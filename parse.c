/*
 * this is the a brute force recursive descent parser
 */

#include "cc1.h"

/* file-scope globals/forward declarations expected by legacy parser code */
static struct name *global = 0;

/* Loop label generation for control flow transformation */
static int loopLblCnt = 0;

/*
 * Generate unique synthetic labels for loops and switch statements
 * Prefix indicates statement type: L=loop (while/for), D=do-while, S=switch
 */
static char *
genLoopLabel(const char *prefix)
{
	char *label = malloc(32);
	sprintf(label, "%s%d", prefix, loopLblCnt++);
	return label;
}

/*
 * Find the enclosing loop or switch statement
 * Used by break/continue to find their target
 * For break: returns nearest WHILE/FOR/DO/SWITCH
 * For continue: returns nearest WHILE/FOR/DO (not SWITCH)
 */
static struct stmt *
findEnclLoop(struct stmt *parent, int is_continue)
{
	while (parent) {
		if (parent->op == WHILE || parent->op == FOR || parent->op == DO) {
			return parent;
		}
		if (parent->op == SWITCH && !is_continue) {
			return parent;
		}
		parent = parent->parent;
	}
	return NULL;
}

/*
 * Simple hash function for generating short unique identifiers
 * Returns a 4-character hex hash string
 */
static unsigned short
hashString(const char *str)
{
	unsigned short hash = 0;
	unsigned char c;

	while ((c = *str++)) {
		hash = hash * 31 + c;
		hash = (hash << 5) ^ (hash >> 11);  /* Mix bits */
	}
	return hash;
}

/*
 * Generate mangled name for static variables
 * Format: <short_name>_<hash> (max 15 chars total)
 * Hash is based on full context to ensure uniqueness:
 * - File-scoped: hash(file_root + "_" + varname)
 * - Function-scoped: hash(file_root + "_" + funcname + "_" + varname + "_" + counter)
 */
static char *
mangleStatNam(struct name *var)
{
	char *mangled;
	char context_buf[256];
	unsigned short hash;
	int name_len;

	if (!srcFileRoot) {
		/* Fallback if no source file set */
		return strdup(var->name);
	}

	/* Build context string for hashing */
	if (var->level == 1) {
		/* File-scoped static */
		snprintf(context_buf, sizeof(context_buf), "%s_%s",
		         srcFileRoot, var->name);
	} else if (curFunc) {
		/* Function-scoped static */
		snprintf(context_buf, sizeof(context_buf), "%s_%s_%s_%d",
		         srcFileRoot, curFunc->name,
		         var->name, staticCtr++);
	} else {
		/* Shouldn't happen, but handle gracefully */
		snprintf(context_buf, sizeof(context_buf), "%s_%s",
		         srcFileRoot, var->name);
	}

	/* Generate hash */
	hash = hashString(context_buf);

	/* Allocate mangled name: <short_name>_<hash> */
	/* Format: up to 9 chars of name + "_" + 4 hex digits = 14 chars max */
	mangled = malloc(15);  /* 14 + null terminator */

	/* Calculate how much of the name we can use (max 9 chars to leave room for _XXXX) */
	name_len = strlen(var->name);
	if (name_len > 9) {
		name_len = 9;
	}

	/* Copy truncated name and append hash */
	snprintf(mangled, 15, "%.*s_%04x", name_len, var->name, hash);

	return mangled;
}
struct stmt *makestmt(unsigned char op, struct expr *left);
char *blockname(void);
struct stmt *asmblock(void);

/*
 * Capture local variables from the current scope level
 * Returns a linked list of name structures (shallow copies)
 * Called before popScope() to preserve variable info
 */
static struct name *
capLocals(void)
{
	struct name *locals_list = NULL;
	struct name *tail = NULL;
	struct name *n, *copy;
	int i;

	/* Iterate through names array looking for variables at current level */
	for (i = 0; i <= lastname; i++) {
		n = names[i];
		if (!n)
			continue;

		/* Only capture variables at current lexical level */
		if (n->level != lexlevel)
			continue;

		/* Skip tags, typedefs, and functions */
		if (n->is_tag || n->kind == tdef || n->kind == fdef)
			continue;

		/* Capture this variable (shallow copy) */
		if (n->kind == var || n->kind == local || n->kind == funarg) {
			copy = malloc(sizeof(struct name));
			memcpy(copy, n, sizeof(struct name));
			copy->next = NULL;

			/* Add to linked list */
			if (!locals_list) {
				locals_list = copy;
				tail = copy;
			} else {
				tail->next = copy;
				tail = copy;
			}
		}
	}

	return locals_list;
}

/* Track variables declared with initializers for local scope */
static struct name *declInits[MAX_DECL_INI];
static unsigned char declInitCnt = 0;

/*
 * Add a variable with an initializer to the deferred initialization list
 *
 * Local variables with initializers (e.g., int x = 10;) need special
 * handling because their initialization must occur as executable code
 * in the function body, not as data in the variable declaration.
 *
 * This function tracks these variables so they can be converted to
 * assignment statements later by statement().
 *
 * Parameters:
 *   v - Variable name entry with initializer in v->u.init field
 */
void
addDeclInit(struct name *v)
{
	if (declInitCnt < MAX_DECL_INI) {
		declInits[declInitCnt++] = v;
	}
}

/*
 * Clear the deferred initialization list
 *
 * Called after processing local variable initializers to reset the
 * tracking array for the next declaration statement.
 */
void
clearDeclIni()
{
	declInitCnt = 0;
}

void declaration();

/*
 * Parse statements recursively - the heart of the compiler frontend
 *
 * This function implements the statement parser for C, handling all control
 * flow structures, expressions, declarations, and blocks. It uses recursive
 * descent to parse nested statements and builds a statement tree.
 *
 * Statement types handled:
 *   - Blocks: { ... } with lexical scoping
 *   - Control flow: if/else, while, do-while, for, switch/case/default
 *   - Jumps: break, continue, return, goto, labels
 *   - Expressions: function calls, assignments, operators
 *   - Declarations: local variables, typedefs (scoped to current block)
 *   - Inline assembly: asm { ... }
 *
 * Lexical scoping:
 *   - Each block pushes a new scope, pops on exit
 *   - Local variables are captured before scope pop to preserve metadata
 *   - Nested blocks can shadow outer names
 *
 * Local variable initialization:
 *   - Initializers (e.g., int x = 10;) are converted to assignment statements
 *   - Arrays use COPY operator for aggregate initialization
 *   - Static locals are initialized in data section, not converted
 *
 * Loop/switch transformation:
 *   - break/continue are transformed to goto statements
 *   - Synthetic labels are generated (L0, D1, S2, etc.)
 *   - Label format: <prefix><number>_<break|continue|test>
 *
 * Parameters:
 *   parent - Enclosing statement (for break/continue target lookup, scope)
 *
 * Returns:
 *   Statement tree head, or NULL if no statements parsed
 */
struct stmt*
statement(struct stmt *parent)
{
    struct stmt *st, **pst = 0;
    struct stmt *head = 0;
    // struct name *v = 0;
    struct stmt *makestmt(unsigned char op, struct expr *left);
    unsigned char block = 1;

    while (block) {
        st = NULL;  // Initialize st to NULL for each iteration
    	switch (cur.type) {

    	case END:   // end a block
    	case E_O_F: // end of file
            block = 0;
            break;

        case BEGIN: // begin a block
            gettoken();
            pushScope(blockname());
            st = makestmt(BEGIN, 0);
            st->parent = parent;
            st->chain = statement(st);

            /* Capture local variables before popping scope */
            st->locals = capLocals();

            popScope();
            expect(END, ER_S_CC);
            // If this is a top-level block (function body), return immediately
            if (parent == NULL) {
                return st;
            }
            break;

        case IF:    // if <condition> <statement>
            gettoken();
            expect(LPAR, ER_S_NP);
            st = makestmt(IF, parseExpr(PRI_ALL, parent));
            st->parent = parent;  /* Set parent before recursive call */
            expect(RPAR, ER_S_NP);
            st->chain = statement(st);
            if (cur.type == ELSE) {   // else <statement>
                gettoken();
                st->otherwise = statement(st);
            }
            break;
        case BREAK:
            gettoken();
            expect(SEMI, ER_S_SN);
            /* Transform break into goto to enclosing loop's break label */
            {
                struct stmt *loop = findEnclLoop(parent, 0);
                if (loop && loop->label) {
                    st = makestmt(GOTO, 0);
                    st->label = malloc(strlen(loop->label) + 10);
                    sprintf(st->label, "%s_break", loop->label);
                } else {
                    /* No enclosing loop - keep as BREAK (will be an error) */
                    st = makestmt(BREAK, 0);
                }
            }
            break;
        case CONTINUE:
            gettoken();
            expect(SEMI, ER_S_SN);
            /*
             * Transform continue into goto to enclosing
             * loop's continue label
             */
            {
                struct stmt *loop = findEnclLoop(parent, 1);
                if (loop && loop->label) {
                    st = makestmt(GOTO, 0);
                    /*
                     * For DO-WHILE, continue goes to test label,
                     * for others go to top
                     */
                    if (loop->op == DO) {
                        st->label = malloc(strlen(loop->label) + 10);
                        sprintf(st->label, "%s_test", loop->label);
                    } else {
                        st->label = malloc(strlen(loop->label) + 15);
                        sprintf(st->label, "%s_continue", loop->label);
                    }
                } else {
                    /*
                     * No enclosing loop - keep as CONTINUE
                     * (will be an error)
                     */
                    st = makestmt(CONTINUE, 0);
                }
            }
            break;
        case RETURN:
            gettoken();
            st = makestmt(RETURN, 0);
            if (cur.type != SEMI) {
                st->left = parseExpr(PRI_ALL, parent);
            }
            expect(SEMI, ER_S_SN);
            break;
        
        /* Local declarations - type keywords */
        case INT:
        case CHAR:
        case SHORT:
        case LONG:
        case FLOAT:
        case DOUBLE:
        case VOID:
        case STRUCT:
        case UNION:
        case ENUM:
        case UNSIGNED:
        case CONST:
        case VOLATILE:
        case STATIC:
        case REGISTER:
        case AUTO:
        case EXTERN:
            clearDeclIni();
            declaration();
            /* Convert local variable initializers to assignment statements */
            if (declInitCnt > 0) {
                unsigned char i;
                for (i = 0; i < declInitCnt; i++) {
                    struct name *v = declInits[i];
                    struct expr *lhs, *assign_expr;
                    struct stmt *assign_st;

                    /* Create lvalue: just the variable symbol */
                    lhs = mkexprI(SYM, 0, v->type, 0, 0);
                    /* Cast name* to var* (field is overloaded) */
                    lhs->var = (struct var *)v;

                    /*
                     * Check if this is an array initialization
                     * requiring memory copy
                     */
                    if (v->type && (v->type->flags & TF_ARRAY) && v->u.init) {
                        /* Create memory copy: COPY dest src length */
                        assign_expr = mkexprI(COPY, lhs, v->type,
                                                     v->type->count, 0);
                        assign_expr->right = v->u.init;
                        /* Clear so it's not output in declaration */
                        v->u.init = NULL;
                    } else {
                        /* Regular scalar assignment: lhs = initializer */
                        assign_expr = mkexprI(ASSIGN, lhs, v->type,
                                                     0, 0);
                        assign_expr->right = v->u.init;
                        /* Clear so it's not output in declaration */
                        v->u.init = NULL;
                    }

                    /* Create expression statement */
                    assign_st = makestmt(EXPR, assign_expr);

                    /* Link this statement into the list */
                    if (!pst) {
                        head = assign_st;
                        assign_st->flags |= S_PARENT;
                    } else {
                        *pst = assign_st;
                    }
                    pst = &assign_st->next;
                    assign_st->parent = parent;
                }
                clearDeclIni();
            }
            st = NULL;  /* Don't create another statement */
            break;

        case TYPEDEF:
            /* typedef inside function body - scoped to current block */
            declaration();
            st = NULL;  /* declaration() doesn't return a statement */
            break;
        
        case SYM:
            /* Check if it's a label */
            if (next.type == COLON) {
                st = makestmt(LABEL, 0);
                st->label = strdup(cur.v.name);
                st->flags |= S_LABEL;
                gettoken();
                gettoken();
                break;
            }
            /* Check if it's a typedef name used in a declaration */
            {
                struct name *poss_typedef =
                    findName(cur.v.name, 0);
                if (poss_typedef && poss_typedef->kind == tdef) {
                    clearDeclIni();
                    declaration();
                    /*
                     * Convert local variable initializers to
                     * assignment statements
                     */
                    if (declInitCnt > 0) {
                        unsigned char i;
                        for (i = 0; i < declInitCnt; i++) {
                            struct name *v = declInits[i];
                            struct expr *lhs, *assign_expr;
                            struct stmt *assign_st;

                            /* Create lvalue: just the variable symbol */
                            lhs = mkexprI(SYM, 0, v->type, 0, 0);
                            /* Cast name* to var* (field is overloaded) */
                            lhs->var = (struct var *)v;

                            /*
                             * Check if this is an array initialization
                             * requiring memory copy
                             */
                            if (v->type && (v->type->flags & TF_ARRAY) &&
                                v->u.init) {
                                /* Create memory copy: COPY dest src length */
                                assign_expr = mkexprI(COPY, lhs, v->type,
                                                             v->type->count, 0);
                                assign_expr->right = v->u.init;
                                /* Clear so it's not output in declaration */
                                v->u.init = NULL;
                            } else {
                                /*
                                 * Regular scalar assignment:
                                 * lhs = initializer
                                 */
                                assign_expr = mkexprI(ASSIGN, lhs,
                                                             v->type, 0, 0);
                                assign_expr->right = v->u.init;
                                /* Clear so it's not output in declaration */
                                v->u.init = NULL;
                            }

                            /* Create expression statement */
                            assign_st = makestmt(EXPR, assign_expr);

                            /* Link this statement into the list */
                            if (!pst) {
                                head = assign_st;
                                assign_st->flags |= S_PARENT;
                            } else {
                                *pst = assign_st;
                            }
                            pst = &assign_st->next;
                            assign_st->parent = parent;
                        }
                        clearDeclIni();
                    }
                    st = NULL;  /* declaration() doesn't return a statement */
                    break;
                }
            }
            /* fall through to expression */
        case NUMBER:    // numeric literals can start expression statements
        case STRING:    // string literals can start expression statements
        case LPAR:
        case STAR:
        case INCR:
        case DECR:
            st = makestmt(EXPR, parseExpr(PRI_ALL, parent));
            expect(SEMI, ER_S_SN);
            break;

        case FOR:   // for (<expr>; <expr>; <expr>) <statement> ;
            gettoken();
            expect(LPAR, ER_S_NP);
            /* Init expression - optional */
            if (cur.type == SEMI) {
                st = makestmt(FOR, NULL);
            } else {
                st = makestmt(FOR, parseExpr(PRI_ALL, parent));
            }
            /* Generate synthetic label */
            st->label = genLoopLabel("L");
            /* Set parent before recursive call */
            st->parent = parent;
            expect(SEMI, ER_S_SN);
            /* Condition expression - optional */
            if (cur.type == SEMI) {
                st->middle = NULL;
            } else {
                st->middle = parseExpr(PRI_ALL, parent);
            }
            expect(SEMI, ER_S_SN);
            /* Increment expression - optional */
            if (cur.type == RPAR) {
                st->right = NULL;
            } else {
                st->right = parseExpr(PRI_ALL, parent);
            }
            expect(RPAR, ER_S_NP);
            st->chain = statement(st);
            break;

        case WHILE:     // while <condition> <statement> ;
            gettoken();
            expect(LPAR, ER_S_NP);
            st = makestmt(WHILE, parseExpr(PRI_ALL, parent));
            /* Generate synthetic label */
            st->label = genLoopLabel("L");
            /* Set parent before recursive call */
            st->parent = parent;
            expect(RPAR, ER_S_NP);
            st->chain = statement(st);
            break;

        case 'E':
            recover(SEMI, ER_S_OE);
            break;

        case SWITCH:    // switch (<expr>) <block> ;
            gettoken();
            expect(LPAR, ER_S_NP);
            st = makestmt(SWITCH, parseExpr(PRI_ALL, parent));
            /* Generate synthetic label */
            st->label = genLoopLabel("S");
            /* Set parent before recursive call */
            st->parent = parent;
            expect(RPAR, ER_S_NP);
            expect(BEGIN, ER_S_SB);
            st->chain = statement(st);
            expect(END, ER_S_CC);
            break;

        case CASE:
            gettoken();
            // Use priority 13 to stop at colon (ternary/colon have priority 13)
            st = makestmt(CASE, parseExpr(13, parent));
            expect(COLON, ER_S_NL);
            break;

        case GOTO:
            gettoken();
            st = makestmt(GOTO, 0);
            if (cur.type != SYM) {
                recover(ER_S_GL, SEMI);
                break;
            } 
                st->label = strdup(cur.v.name);
            gettoken();
            expect(SEMI, ER_S_SN);
            break;

        case DEFAULT:
            gettoken();
            expect(COLON, ER_S_NL);
            st = makestmt(DEFAULT, 0);
            break;

        case ';':
            gettoken();
            st = makestmt(';', 0);
            break;

        case DO:    // do <statement> while <condition> ;
            gettoken();
            st = makestmt(DO, 0);
            /* Generate synthetic label */
            st->label = genLoopLabel("D");
            /* Set parent before recursive call */
            st->parent = parent;
            st->chain = statement(st);
            if (cur.type != WHILE) {
                gripe(ER_S_DO);
                break;
            }
            gettoken();  // advance past WHILE keyword
            expect(LPAR, ER_S_NP);
            st->left = parseExpr(PRI_ALL, parent);
            need(RPAR, SEMI, ER_S_NP);
            expect(SEMI, ER_S_SN);
            break;

        case ASM:
            gettoken();
            st = asmblock();
            break;

        default:
            gripe(ER_E_UO);
            break;
        }
        // Skip if no statement was created (e.g., default case, EOF)
        if (!st) {
            continue;
        }
        if (!pst) {
            head = st;
            st->flags |= S_PARENT;
        } else {
            *pst = st;  // Link previous statement to this one
        }
        pst = &st->next;
        // st->function = v;
        st->parent = parent;

        /*
         * If we're parsing a single-statement body for a control
         * structure (if/while/for/etc), return after parsing one
         * statement. Don't return for block statements (BEGIN),
         * switch statements (SWITCH), or top-level (parent ==
         * NULL/function body)
         */
        if (parent && parent->op != BEGIN && parent->op != 'S' && st) {
            block = 0;  // Exit the while loop
        }
    } // while
    return head;
}

/*
 * Create a new statement node
 *
 * Allocates and zero-initializes a statement structure with the specified
 * operator and left expression. This is the basic statement node allocator
 * used throughout the parser.
 *
 * Parameters:
 *   op   - Statement operator (e.g., IF, WHILE, EXPR, BEGIN)
 *   left - Left expression (condition for IF/WHILE, expression for EXPR, etc.)
 *
 * Returns:
 *   Pointer to newly allocated and initialized statement node
 */
struct stmt*
makestmt(unsigned char op, struct expr *left)
{
	struct stmt *st;

	st = calloc(1, sizeof(*st));  // Zero-initialize all fields
	st->op = op;
	st->left = left;
	return st;
}

/*
 * Parse an inline assembly block
 *
 * Handles the asm { ... } syntax for embedding raw assembly code in C
 * functions. The assembly text is captured verbatim (with proper brace
 * nesting) and stored in the statement tree for later emission.
 *
 * Assembly blocks can contain nested braces, which are tracked to find
 * the matching closing brace. The lexer's ASM_BLOCK flag enables special
 * token capture mode where all tokens are appended to asmCbuf.
 *
 * Post-processing:
 *   - Trailing spaces and semicolons are trimmed
 *   - The captured text is transferred to the statement's label field
 *   - The ASM_BLOCK flag is cleared to restore normal lexing
 *
 * Returns:
 *   ASM statement node with assembly text in label field, or NULL on error
 */
struct stmt *
asmblock(void)
{
    struct stmt *st;
    int depth = 0;
    char *captured_text = NULL;
    int captured_len = 0;

    /* Expect opening brace */
    if (cur.type != BEGIN) {
        gripe(ER_S_SB);  /* Expected { (brace) */
        return NULL;
    }

    /* Initialize asm capture buffer */
    asmCbuf = malloc(256);
    asmCsiz = 256;
    asmClen = 0;
    asmCbuf[0] = 0;

    /* Set ASM_BLOCK flag for special asm processing */
    tflags |= ASM_BLOCK;

    /* Clear lineend flag from any newline after the opening brace */
    lineend = 0;

    /* Consume { and start capturing tokens */
    gettoken();
    depth = 1;

    while (depth > 0 && cur.type != E_O_F) {
        /* Check for nested braces */
        if (cur.type == BEGIN) {
            depth++;
        } else if (cur.type == END) {
            depth--;
            if (depth == 0) {
                /* Found matching closing brace - save captured text and stop */
                captured_text = asmCbuf;
                captured_len = asmClen;
                asmCbuf = NULL;
                asmCsiz = 0;
                asmClen = 0;
                tflags &= ~ASM_BLOCK;  /* Clear ASM_BLOCK flag */
                break;
            }
        }

        /*
         * Get next token (will be captured by lexer if
         * asmCbuf is set)
         */
        gettoken();
    }

    /* Clear ASM_BLOCK flag in case we exited loop due to EOF */
    tflags &= ~ASM_BLOCK;

    /* Trim trailing space and semicolon from captured text */
    while (captured_len > 0 && (captured_text[captured_len-1] == ' ' ||
                                 captured_text[captured_len-1] == ';')) {
        captured_text[captured_len-1] = 0;
        captured_len--;
    }

    /* Consume the closing } */
    if (cur.type == END) {
        gettoken();
    }

    /* Create statement with assembly text */
    st = makestmt(ASM, NULL);
    st->label = captured_text;  /* Transfer ownership to statement */

    return st;
}

/*
 * Parse an initializer list for arrays and structs
 *
 * Handles the { expr, expr, ... } syntax for aggregate initializers.
 * Supports nested initializer lists for multi-dimensional arrays and
 * nested structs. Commas separate elements, and the list is terminated
 * by a closing brace.
 *
 * The parser stops at comma operator priority (15) to treat commas as
 * element separators rather than the comma operator.
 *
 * Examples:
 *   int arr[3] = { 1, 2, 3 };
 *   int matrix[2][2] = { {1, 2}, {3, 4} };
 *   struct { int x, y; } p = { 10, 20 };
 *
 * Returns:
 *   Linked list of initializer expressions (via next pointers), or
 *   NULL on error
 */
static struct expr *parseInitList(void)
{
    struct expr *head = NULL;
    struct expr *tail = NULL;
    struct expr *item;

    gettoken(); /* consume { */

    while (cur.type != END) {
        if (cur.type == BEGIN) {
            /* Nested initializer for struct/array member */
            item = parseInitList();
        } else {
            /* Parse expression but stop before comma operator (priority 15)
             * so that comma is treated as element separator, not an operator */
            item = parseExpr(OP_PRI_COMMA, NULL);
        }

        if (item == NULL) {
            gripe(ER_E_IT); /* Invalid initializer term */
            return NULL;
        }

        /* Link initializer expressions together */
        if (head == NULL) {
            head = item;
            tail = item;
        } else {
            tail->next = item;
            tail = item;
        }

        if (cur.type == END) {
            break;
        } else if (cur.type != COMMA) {
            gripe(ER_S_SN); /* need semicolon/separator */
            return NULL;
        }
        gettoken(); /* consume , */
    }
    
    gettoken(); /* consume } */
    return head;
}

/*
 * Parse a variable initializer
 *
 * Handles both simple expression initializers and aggregate initializer
 * lists for variable declarations. Called when an = token is encountered
 * after a declarator.
 *
 * Initializer forms:
 *   - Simple expression: int x = 10;
 *   - Initializer list: int arr[] = { 1, 2, 3 };
 *   - Nested lists: int matrix[][2] = { {1, 2}, {3, 4} };
 *   - String literals: char str[] = "hello";
 *
 * The parser uses precedence 15 for simple expressions to allow
 * assignment operators (priority 14) but exclude the comma operator
 * (priority 15), ensuring commas in function-like macros work correctly.
 *
 * Returns:
 *   Expression tree representing the initializer, or NULL on error
 */
struct expr *
doInitlzr(void)
{
    struct expr *init;

    gettoken(); /* consume = token */

    if (cur.type == BEGIN) {
        /* Handle {...} style initializer list */
        init = parseInitList();
    } else {
        /*
         * Handle simple expression initializer
         * Use precedence 15 to allow assignment (14) but exclude
         * comma operator (15)
         */
        init = parseExpr(15, NULL);
    }

    if (init == NULL) {
        gripe(ER_E_IT); /* Invalid initializer term */
        return NULL;
    }

    return init;
}

/*
 * Parse a function definition
 *
 * Processes the function body following a function declarator. This function:
 *   1. Sets up function context for static variable mangling
 *   2. Pushes a new scope for the function body
 *   3. Installs function parameters into level 2 scope
 *   4. Parses the function body statement tree
 *   5. Emits the AST for pass 2 (code generation)
 *   6. Pops the function scope and cleans up
 *
 * Parameter handling:
 *   - Parameters are read from f->type->elem (function type signature)
 *   - New name entries are created at level 2 with funarg kind
 *   - This separates type signature (normalized, name-independent) from
 *     actual parameter symbols (visible in function body)
 *
 * Function-scoped static variables:
 *   - Mangled name format: <file>_<func>_<var>_<counter>
 *   - Allocated in global data section with proper scoping
 *
 * Debug assertions:
 *   - Verifies lexlevel returns to 1 (global) after parsing
 *   - Verifies no local names remain in symbol table
 *
 * Parameters:
 *   f - Function name entry with type signature in f->type
 */
void
parsefunc(struct name *f)
{
	struct name *param;

	// Set current function context for static variable name mangling
	curFunc = f;
	staticCtr = 0;

	// Push a new scope for the function body
	pushScope(f->name);

	// Install function parameters into the scope at level 2
	// Read parameter info from f->type->elem but create NEW entries at level 2
	if (f->type && (f->type->flags & TF_FUNC)) {
		for (param = f->type->elem; param; param = param->next) {
			// Only add parameters with actual names (skip anonymous ones)
			if (param->name && param->name[0] != '\0') {
				// Create a NEW name entry at level 2 (don't reuse type->elem)
				newName(param->name, funarg, param->type, 0);
			}
		}
	}

	// Parse the function body
	f->u.body = statement(0);
	if (f->u.body) {
		f->u.body->flags |= S_FUNC;
		// Mark this as a function definition (not just a prototype)
		f->kind = fdef;
	}

	// Emit AST for second pass
	emitFunction(f);

	// Pop the function scope
	popScope();

	/*
	 * Debug assertion: verify we're back at global scope and all
	 * locals are cleaned up
	 */
#ifdef DEBUG
	if (lexlevel != 1) {
		fdprintf(2, "ASSERTION FAILED: lexlevel=%d after parsing "
		         "function %s (expected 1)\n", lexlevel, f->name);
		fatal(0);
	}
	/* Verify no local names remain in symbol table */
	{
		int i;
		for (i = 0; i <= lastname; i++) {
			if (names[i] && names[i]->level > 1) {
				fdprintf(2, "ASSERTION FAILED: found local name "
				         "'%s' at level %d after parsing "
				         "function %s\n",
				         names[i]->name, names[i]->level, f->name);
				fatal(0);
			}
		}
	}
#endif

	// Clear current function context
	curFunc = NULL;
}

/*
 * storage class clauses - many combinations are illogical
 */
char *sclassBitDef[] = { "EXTERN", "REGISTER", "STATIC", "CONST",
	"VOLATILE", "AUTO", "TYPEDEF"
};

/*
 * Parse storage class specifiers in a declaration
 *
 * Storage class specifiers control visibility, lifetime, and storage
 * location of variables and functions. This function parses any combination
 * of storage class keywords and returns them as a bitmask.
 *
 * Recognized keywords:
 *   - extern:   External linkage (defined elsewhere)
 *   - static:   Internal linkage or persistent local storage
 *   - auto:     Automatic (stack) storage (default for locals)
 *   - register: Request register allocation (hint only)
 *   - const:    Type qualifier (ignored by this compiler)
 *   - volatile: Type qualifier (ignored by this compiler)
 *   - typedef:  Type alias declaration
 *
 * Invalid combinations detected:
 *   - extern with static/auto/register (conflicting linkage)
 *   - register with static (conflicting storage)
 *   - static with auto (conflicting storage)
 *   - typedef with any storage class (typedef is not storage)
 *
 * Note: This function only handles parsing. The code generator determines
 * actual storage location based on context (global vs local) and storage
 * class.
 *
 * Returns:
 *   Bitmask of storage class flags (SC_EXTERN, SC_STATIC, etc.)
 */
unsigned char
parseSclass()
{
	unsigned char ret = 0;
	unsigned char bit;
#ifdef DEBUG
	if (VERBOSE(V_SYM)) {
		fdprintf(2,"parseSclass: starting, cur.type=0x%02x\n", cur.type);
	}
#endif

	while (1) {
		switch (cur.type) {
		case EXTERN:
			bit = SC_EXTERN;
			break;
		case REGISTER:
			bit = SC_REGISTER;
			break;
		case STATIC:
			bit = SC_STATIC;
			break;
		case CONST:
			bit = SC_CONST;
			break;
		case VOLATILE:
			bit = SC_VOLATILE;
			break;
		case AUTO:
			bit = SC_AUTO;
			break;
		case TYPEDEF:
			bit = SC_TYPEDEF;
#ifdef DEBUG
			if (VERBOSE(V_SYM)) {
				fdprintf(2,"parseSclass: FOUND TYPEDEF token!\n");
			}
#endif
			break;
		default:
			bit = 0;
			break;
		}
		if (bit) {
			if (ret & bit) {
				gripe(ER_P_SC);
			}
			ret |= bit;
			gettoken();
		} else {
			break;
		}
	}
	// bogosity checks
	if ((ret & SC_EXTERN) &&
        (ret & (SC_STATIC | SC_AUTO | SC_REGISTER))) {
		gripe(ER_P_SC);
	}
	if ((ret & SC_REGISTER) &&
        (ret & (SC_STATIC))) {
		gripe(ER_P_SC);
	}
	if ((ret & SC_STATIC) &&
        (ret & (SC_AUTO))) {
		gripe(ER_P_SC);
	}
	/* const and volatile are ignored by this compiler */
	if ((ret & SC_TYPEDEF) &&
        (ret & (SC_EXTERN | SC_STATIC | SC_AUTO | SC_REGISTER))) {
		gripe(ER_P_SC);
	}
	return ret;
}

/*
 * Parse a complete declaration statement
 *
 * Handles variable and function declarations at global or local scope.
 * A declaration consists of a storage class (optional), base type, and
 * one or more declarators separated by commas.
 *
 * Declaration forms:
 *   - Variables:    int x, *p, arr[10];
 *   - Functions:    int foo(int x);
 *   - Typedefs:     typedef int* intptr;
 *   - Initializers: int x = 10, arr[] = {1, 2, 3};
 *
 * Processing:
 *   1. Parse storage class keywords (extern, static, typedef, etc.)
 *   2. Parse base type once (shared across all declarators)
 *   3. For each declarator:
 *      - Parse declarator (name, pointers, arrays, functions)
 *      - Handle initializers (= expr or = { ... })
 *      - Apply storage class
 *      - Emit to AST if global or static
 *      - If function body present ({ ... }), parse function
 *
 * Typedef handling:
 *   - Changes name kind from var to tdef
 *   - Typedefs cannot have initializers or function bodies
 *   - Creates type alias in current scope
 *
 * Function definitions:
 *   - Detected by BEGIN token after function declarator
 *   - Calls parsefunc() to parse body
 *   - Statement tree freed after emission
 *
 * Local variable initializers:
 *   - Added to declInits list for conversion to assignments
 *   - Static locals are initialized in data section, not converted
 *
 * Array size inference:
 *   - char[] = "string" infers size from string length
 *   - int[] = {1, 2, 3} infers size from initializer count
 */
void
declaration()
{
	unsigned char sclass;
	struct type *basetype;
	struct name *v;

	/* Parse storage class and base type once at the beginning */
	sclass = parseSclass();
	/* Initialize once, then shared across comma-separated declarators */
	basetype = 0;

	while (1) {
        v = declare(&basetype);

        /* error recovery: if declare failed, skip to next ; or , */
        if (!v) {
            while (cur.type != SEMI && cur.type != COMMA && cur.type != E_O_F) {
                gettoken();
            }
            if (cur.type == SEMI) {
                gettoken();
                break;
            }
            if (cur.type == COMMA) {
                gettoken();
                continue;
            }
            /* E_O_F */
            break;
        }

        /* handle typedef declarations */
        if (sclass & SC_TYPEDEF) {
            /* change the name kind from var to tdef */
#ifdef DEBUG
            if (VERBOSE(V_SYM)) {
                fdprintf(2,"CONVERTING %s from var to tdef "
                         "(sclass=0x%02x)\n", v->name, sclass);
            }
#endif
            v->kind = tdef;
            /* typedefs cannot have initializers or function bodies */
            if (cur.type == ASSIGN) {
                gripe(ER_T_TD);
                /* skip the initializer */
                while (cur.type != SEMI && cur.type != COMMA &&
                       cur.type != E_O_F) {
                    gettoken();
                }
            }
            /* continue to next declarator or end of statement */
            if (cur.type == COMMA) {
                gettoken();
                continue;
            }
            if (cur.type == SEMI) {
                gettoken();
                break;
            }
            /* unexpected token */
            break;
        }

        if (v->type) {
        }
        if (v->type && (v->type->flags & TF_FUNC)) {
            if (cur.type == BEGIN) {
                /* Assign storage class BEFORE parsing function body
                 * so it's available when emitting the AST */
                if (sclass & SC_STATIC) {
                    v->sclass = SC_STATIC;
                    v->mangled_name = mangleStatNam(v);
                } else if (sclass & SC_EXTERN) {
                    v->sclass = SC_EXTERN;
                }

                parsefunc(v);

                /* Free the statement tree (dumping now happens in parsefunc) */
                if (v->u.body) {
                    frStmt(v->u.body);
                    v->u.body = 0;  /* Mark as freed */
                }

                v->next = global;
                global = v;
                break;
            }
        }

        /*
         * Assign storage class for variables (non-functions or
         * function prototypes)
         */
        if (sclass & SC_STATIC) {
            v->sclass = SC_STATIC;
            v->mangled_name = mangleStatNam(v);
        } else if (sclass & SC_EXTERN) {
            v->sclass = SC_EXTERN;
        }

        if (cur.type == ASSIGN) {
            cstring str;
            int len;

            v->u.init = doInitlzr();

            /*
             * Fix array size for char[] = "string" syntax
             * (single string, not a list)
             */
            if (v->type && (v->type->flags & TF_ARRAY) &&
                v->type->count == -1 &&
                v->u.init && v->u.init->op == STRING && !v->u.init->next) {
                /* Get string length from counted string */
                str = (cstring)v->u.init->v;
                if (str) {
                    /* First byte is length */
                    len = (unsigned char)str[0];
                    /*
                     * Create new array type with correct size
                     * (length + 1 for null terminator)
                     */
                    v->type = getType(TF_ARRAY|TF_POINTER, v->type->sub,
                                       len + 1);
                }
            }

            /* Fix array size for array[] = { ... } syntax */
            if (v->type && (v->type->flags & TF_ARRAY) &&
                v->type->count == -1 &&
                v->u.init && v->u.init->next) {
                /* Count elements in initializer list */
                struct expr *item;
                int count = 0;
                for (item = v->u.init; item; item = item->next) {
                    count++;
                }
                /* Create new array type with correct size */
                v->type = getType(TF_ARRAY|TF_POINTER, v->type->sub,
                                   count);
            }

            /*
             * Track local variable initializers for conversion
             * to assignments
             */
            if (lexlevel > 1 && v->u.init && !(sclass & SC_STATIC)) {
                addDeclInit(v);
            }
        }

		/* Emit global variables and static locals immediately */
		/*
		 * Emit if: (global scope OR static storage) AND not
		 * typedef AND not function def
		 */
		if ((lexlevel == 1 || (sclass & SC_STATIC)) &&
		    v->kind != tdef && v->kind != fdef) {
			/* Skip function declarations - only emit actual variables */
			if (!(v->type && (v->type->flags & TF_FUNC))) {
				emitGv(v);
			}
		}

		if (cur.type == COMMA) {
			gettoken();
			continue;
		}
		if (cur.type == SEMI) {
			gettoken();
			break;
		}
		/* Error recovery: unexpected token, break out of loop */
		break;
	}
}

char bnbuf[20];

/*
 * Generate a unique name for a block scope
 *
 * Creates a synthetic name for lexical blocks to track scope in the
 * symbol table. Each call returns a new name with an incrementing counter.
 *
 * Note: The returned pointer is to a static buffer that is overwritten
 * on each call. Callers must copy the string if persistence is needed.
 *
 * Returns:
 *   Pointer to static buffer containing "block N" where N is sequential
 */
char*
blockname()
{
	static int blockid = 0;
	sprintf(bnbuf, "block %d", blockid++);
	return bnbuf;
}

/*
 * Parse a C source file at global scope
 *
 * This is the top-level entry point for parsing a translation unit.
 * It processes all global declarations (variables, functions, typedefs)
 * until EOF is reached.
 *
 * Initialization:
 *   1. Push global scope (level 1)
 *   2. Initialize basic types (char, int, long, void, etc.)
 *   3. Process declarations until EOF
 *
 * Declaration recognition:
 *   - Type keywords: int, char, struct, etc.
 *   - Storage class: extern, static, typedef, etc.
 *   - Typedef names: Previously declared type aliases
 *
 * Incremental emission:
 *   - Global variables are emitted to AST as they're parsed
 *   - Functions are emitted after their body is parsed
 *   - This avoids buffering entire program in memory
 *
 * Cleanup:
 *   - Pops global scope on completion
 *   - Debug builds verify all names properly cleaned up
 *   - Only basic types (level 0) should remain after parsing
 *
 * Error recovery:
 *   - Unrecognized tokens are skipped to prevent infinite loops
 *   - NONE tokens (from lexer errors) are consumed and ignored
 */
void
parse()
{
	struct name *poss_typedef;

	pushScope("global");
	initbasictype();
	while (cur.type != E_O_F) {
		while (cur.type == NONE) {
			gettoken();
		}
		/* Check if current token looks like start of a declaration */
		/* Also check if it's a typedef name (SYM that's a typedef) */
		poss_typedef = NULL;
		if (cur.type == SYM) {
			poss_typedef = findName(cur.v.name, 0);
		}

		if (cur.type == INT || cur.type == CHAR || cur.type == SHORT ||
			cur.type == LONG || cur.type == FLOAT || cur.type == DOUBLE ||
			cur.type == VOID || cur.type == UNSIGNED || cur.type == STRUCT ||
			cur.type == UNION || cur.type == ENUM || cur.type == CONST ||
			cur.type == VOLATILE || cur.type == TYPEDEF || cur.type == STATIC ||
			cur.type == REGISTER || cur.type == AUTO || cur.type == EXTERN ||
			(poss_typedef && poss_typedef->kind == tdef)) {
			declaration();
		} else {
			/* Not a declaration - skip this token to avoid getting stuck */
			gettoken();
		}
	}

	/* Emit global variables (no-op, already emitted incrementally) */
	emitGvs();

	popScope();

	/*
	 * Debug assertion: verify all allocations have been freed
	 * after parsing file
	 */
#ifdef DEBUG
	if (lexlevel != 0) {
		fdprintf(2, "ASSERTION FAILED: lexlevel=%d after parsing "
		         "file (expected 0)\n", lexlevel);
		fatal(0);
	}
	/* Verify only basic types remain in symbol table (level 0) */
	{
		int i;
		int nonBasicCnt = 0;
		for (i = 0; i <= lastname; i++) {
			if (names[i] && names[i]->level > 0) {
				fdprintf(2, "WARNING: name '%s' at level %d "
				         "still in symbol table after "
				         "file parse\n",
				         names[i]->name, names[i]->level);
				nonBasicCnt++;
			}
		}
		if (nonBasicCnt > 0) {
			fdprintf(2, "ASSERTION FAILED: found %d non-basic "
			         "names after parsing file\n", nonBasicCnt);
			fatal(0);
		}
	}
#endif
}

/*
 * Free a statement tree recursively
 *
 * Deallocates a statement tree and all its child nodes, including:
 *   - chain: Child/body statement (e.g., body of if/while/for/block)
 *   - otherwise: Else branch (for if statements)
 *   - next: Sibling statement in sequence
 *   - label: Synthetic labels or captured asm text
 *   - locals: Captured local variable list (for blocks)
 *
 * Note: Expression trees (left, right, middle) are NOT freed here as they
 * may be shared or owned elsewhere. A full implementation would need
 * reference counting for expressions.
 *
 * Parameters:
 *   st - Statement tree root to free (NULL-safe)
 */
void
frStmt(struct stmt *st)
{
	if (!st)
		return;

	/* Free child statements */
	if (st->chain)
		frStmt(st->chain);
	if (st->otherwise)
		frStmt(st->otherwise);
	if (st->next)
		frStmt(st->next);

	/* Free label string if present */
	if (st->label)
		free(st->label);

	/* Free locals list */
	if (st->locals) {
		struct name *local = st->locals;
		struct name *next;
		while (local) {
			next = local->next;
			free(local);
			local = next;
		}
	}

	/* Note: Don't free st->left, st->right, st->middle (expressions)
	 * as they may be shared or owned elsewhere.
	 * A full implementation would need expression reference counting. */

	free(st);
}

/*
 * Clean up all allocated memory after parsing
 *
 * Frees all dynamically allocated parser data structures including:
 *   - Name entries (symbol table)
 *   - Statement trees (function bodies)
 *   - Type structures
 *   - Names array itself
 *
 * Cleanup order:
 *   1. Free statement trees attached to function definitions
 *   2. Free name strings (except function parameters owned by types)
 *   3. Free name structures (except function parameters)
 *   4. Free names array
 *   5. Free all types (parameter lists are name structures freed above)
 *   6. Reset global type pointers to NULL
 *
 * Note: This function is typically called on process exit or when
 * multiple files are compiled in sequence. Function parameters
 * (funarg kind) are not freed as their names are owned by function types.
 */
void
cleanupParse(void)
{
	int i;
	struct name *n;

	/* Free all names and their statement trees */
	for (i = 0; i <= lastname; i++) {
		n = names[i];
		if (n) {
			/* Free function body if present */
			if (n->u.body)
				frStmt(n->u.body);

			/*
			 * Free name string (except for function parameters
			 * which are owned by type)
			 */
			if (n->kind != funarg && n->name)
				free(n->name);

			/* Free the name structure itself (except function parameters) */
			if (n->kind != funarg)
				free(n);
		}
	}

	/* Free the names array itself */
	if (names)
		free(names);

	/* Free all type structures */
	{
		struct type *t = types;
		struct type *next;
		while (t) {
			next = t->next;
			/* Note: Don't free t->elem (function parameters) as they're
			 * name structures that were freed above */
			free(t);
			t = next;
		}
		types = NULL;  /* Reset to NULL after freeing all types */
		chartype = NULL;
		inttype = NULL;
		uchartype = NULL;
	}
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
