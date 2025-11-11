/*
 * this is the a brute force recursive descent parser
 */

#include "cc1.h"

/* file-scope globals/forward declarations expected by legacy parser code */
static struct name *global = 0;

/* Loop label generation for control flow transformation */
static int loop_label_counter = 0;

/*
 * Generate unique synthetic labels for loops and switch statements
 * Prefix indicates statement type: L=loop (while/for), D=do-while, S=switch
 */
static char *
generate_loop_label(const char *prefix)
{
	char *label = malloc(32);
	sprintf(label, "%s%d", prefix, loop_label_counter++);
	return label;
}

/*
 * Find the enclosing loop or switch statement
 * Used by break/continue to find their target
 * For break: returns nearest WHILE/FOR/DO/SWITCH
 * For continue: returns nearest WHILE/FOR/DO (not SWITCH)
 */
static struct stmt *
find_enclosing_loop(struct stmt *parent, int is_continue)
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
 * Generate mangled name for static variables
 * File-scoped statics: <file_root>_<varname>
 * Function-scoped statics: <file_root>_<funcname>_<varname>_<counter>
 */
static char *
mangle_static_name(struct name *var)
{
	char *mangled;
	int len;

	if (!source_file_root) {
		/* Fallback if no source file set */
		return strdup(var->name);
	}

	if (var->level == 1) {
		/* File-scoped static: <file_root>_<varname> */
		len = strlen(source_file_root) + 1 + strlen(var->name) + 1;
		mangled = malloc(len);
		sprintf(mangled, "%s_%s", source_file_root, var->name);
	} else if (current_function) {
		/* Function-scoped static: <file_root>_<funcname>_<varname>_<counter> */
		len = strlen(source_file_root) + 1 + strlen(current_function->name) +
		      1 + strlen(var->name) + 1 + 10 + 1;  /* 10 digits for counter */
		mangled = malloc(len);
		sprintf(mangled, "%s_%s_%s_%d", source_file_root, current_function->name,
		        var->name, static_counter++);
	} else {
		/* Shouldn't happen, but handle gracefully */
		len = strlen(source_file_root) + 1 + strlen(var->name) + 1;
		mangled = malloc(len);
		sprintf(mangled, "%s_%s", source_file_root, var->name);
	}

	return mangled;
}
struct stmt *makestmt(unsigned char op, struct expr *left);
char *blockname(void);
struct stmt *asmblock(void);

/*
 * Capture local variables from the current scope level
 * Returns a linked list of name structures (shallow copies)
 * Called before pop_scope() to preserve variable info
 */
static struct name *
capture_locals(void)
{
	extern struct name **names;
	extern int lastname;
	extern int lexlevel;
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
static struct name *decl_inits[MAX_DECL_INITS];
static unsigned char decl_init_count = 0;

void
add_decl_init(struct name *v)
{
	if (decl_init_count < MAX_DECL_INITS) {
		decl_inits[decl_init_count++] = v;
	}
}

void
clear_decl_inits()
{
	decl_init_count = 0;
}

void declaration();

/*
 * parse a statement - this is really the heart of the compiler frontend
 * it recursively calls itself
 * there is some hair here having to do with scope
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
            push_scope(blockname());
            st = makestmt(BEGIN, 0);
            st->parent = parent;
            st->chain = statement(st);

            /* Capture local variables before popping scope */
            st->locals = capture_locals();

            pop_scope();
            expect(END, ER_S_CC);
            // If this is a top-level block (function body), return immediately
            if (parent == NULL) {
                return st;
            }
            break;

        case IF:    // if <condition> <statement>
            gettoken();
            expect(LPAR, ER_S_NP);
            st = makestmt(IF, parse_expr(PRI_ALL, parent));
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
                struct stmt *loop = find_enclosing_loop(parent, 0);
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
            /* Transform continue into goto to enclosing loop's continue label */
            {
                struct stmt *loop = find_enclosing_loop(parent, 1);
                if (loop && loop->label) {
                    st = makestmt(GOTO, 0);
                    /* For DO-WHILE, continue goes to test label, for others go to top */
                    if (loop->op == DO) {
                        st->label = malloc(strlen(loop->label) + 10);
                        sprintf(st->label, "%s_test", loop->label);
                    } else {
                        st->label = malloc(strlen(loop->label) + 15);
                        sprintf(st->label, "%s_continue", loop->label);
                    }
                } else {
                    /* No enclosing loop - keep as CONTINUE (will be an error) */
                    st = makestmt(CONTINUE, 0);
                }
            }
            break;
        case RETURN:
            gettoken();
            st = makestmt(RETURN, 0);
            if (cur.type != SEMI) {
                st->left = parse_expr(PRI_ALL, parent);
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
            clear_decl_inits();
            declaration();
            /* Convert local variable initializers to assignment statements */
            if (decl_init_count > 0) {
                unsigned char i;
                for (i = 0; i < decl_init_count; i++) {
                    struct name *v = decl_inits[i];
                    struct expr *lhs, *assign_expr;
                    struct stmt *assign_st;

                    /* Create lvalue: just the variable symbol */
                    lhs = makeexpr_init(SYM, 0, v->type, 0, 0);
                    lhs->var = (struct var *)v;  /* Cast name* to var* (field is overloaded) */

                    /* Check if this is an array initialization requiring memory copy */
                    if (v->type && (v->type->flags & TF_ARRAY) && v->u.init) {
                        /* Create memory copy: COPY dest src length */
                        assign_expr = makeexpr_init(COPY, lhs, v->type, v->type->count, 0);
                        assign_expr->right = v->u.init;
                        v->u.init = NULL;  /* Clear so it's not output in declaration */
                    } else {
                        /* Regular scalar assignment: lhs = initializer */
                        assign_expr = makeexpr_init(ASSIGN, lhs, v->type, 0, 0);
                        assign_expr->right = v->u.init;
                        v->u.init = NULL;  /* Clear so it's not output in declaration */
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
                clear_decl_inits();
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
                struct name *possible_typedef = lookup_name(cur.v.name, 0);
                if (possible_typedef && possible_typedef->kind == tdef) {
                    clear_decl_inits();
                    declaration();
                    /* Convert local variable initializers to assignment statements */
                    if (decl_init_count > 0) {
                        unsigned char i;
                        for (i = 0; i < decl_init_count; i++) {
                            struct name *v = decl_inits[i];
                            struct expr *lhs, *assign_expr;
                            struct stmt *assign_st;

                            /* Create lvalue: just the variable symbol */
                            lhs = makeexpr_init(SYM, 0, v->type, 0, 0);
                            lhs->var = (struct var *)v;  /* Cast name* to var* (field is overloaded) */

                            /* Check if this is an array initialization requiring memory copy */
                            if (v->type && (v->type->flags & TF_ARRAY) && v->u.init) {
                                /* Create memory copy: COPY dest src length */
                                assign_expr = makeexpr_init(COPY, lhs, v->type, v->type->count, 0);
                                assign_expr->right = v->u.init;
                                v->u.init = NULL;  /* Clear so it's not output in declaration */
                            } else {
                                /* Regular scalar assignment: lhs = initializer */
                                assign_expr = makeexpr_init(ASSIGN, lhs, v->type, 0, 0);
                                assign_expr->right = v->u.init;
                                v->u.init = NULL;  /* Clear so it's not output in declaration */
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
                        clear_decl_inits();
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
            st = makestmt(EXPR, parse_expr(PRI_ALL, parent));
            expect(SEMI, ER_S_SN);
            break;

        case FOR:   // for (<expr>; <expr>; <expr>) <statement> ;
            gettoken();
            expect(LPAR, ER_S_NP);
            /* Init expression - optional */
            if (cur.type == SEMI) {
                st = makestmt(FOR, NULL);
            } else {
                st = makestmt(FOR, parse_expr(PRI_ALL, parent));
            }
            st->label = generate_loop_label("L");  /* Generate synthetic label */
            st->parent = parent;  /* Set parent before recursive call */
            expect(SEMI, ER_S_SN);
            /* Condition expression - optional */
            if (cur.type == SEMI) {
                st->middle = NULL;
            } else {
                st->middle = parse_expr(PRI_ALL, parent);
            }
            expect(SEMI, ER_S_SN);
            /* Increment expression - optional */
            if (cur.type == RPAR) {
                st->right = NULL;
            } else {
                st->right = parse_expr(PRI_ALL, parent);
            }
            expect(RPAR, ER_S_NP);
            st->chain = statement(st);
            break;

        case WHILE:     // while <condition> <statement> ;
            gettoken();
            expect(LPAR, ER_S_NP);
            st = makestmt(WHILE, parse_expr(PRI_ALL, parent));
            st->label = generate_loop_label("L");  /* Generate synthetic label */
            st->parent = parent;  /* Set parent before recursive call */
            expect(RPAR, ER_S_NP);
            st->chain = statement(st);
            break;

        case 'E':
            recover(SEMI, ER_S_OE);
            break;

        case SWITCH:    // switch (<expr>) <block> ;
            gettoken();
            expect(LPAR, ER_S_NP);
            st = makestmt(SWITCH, parse_expr(PRI_ALL, parent));
            st->label = generate_loop_label("S");  /* Generate synthetic label */
            st->parent = parent;  /* Set parent before recursive call */
            expect(RPAR, ER_S_NP);
            expect(BEGIN, ER_S_SB);
            st->chain = statement(st);
            expect(END, ER_S_CC);
            break;

        case CASE:
            gettoken();
            // Use priority 13 to stop at colon (ternary/colon have priority 13)
            st = makestmt(CASE, parse_expr(13, parent));
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
            st->label = generate_loop_label("D");  /* Generate synthetic label */
            st->parent = parent;  /* Set parent before recursive call */
            st->chain = statement(st);
            if (cur.type != WHILE) {
                gripe(ER_S_DO);
                break;
            }
            gettoken();  // advance past WHILE keyword
            expect(LPAR, ER_S_NP);
            st->left = parse_expr(PRI_ALL, parent);
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

        // If we're parsing a single-statement body for a control structure
        // (if/while/for/etc), return after parsing one statement.
        // Don't return for block statements (BEGIN), switch statements (SWITCH),
        // or top-level (parent == NULL/function body)
        if (parent && parent->op != BEGIN && parent->op != 'S' && st) {
            block = 0;  // Exit the while loop
        }
    } // while
    return head;
}

struct stmt*
makestmt(unsigned char op, struct expr *left)
{
	struct stmt *st;

	st = calloc(1, sizeof(*st));  // Zero-initialize all fields
	st->op = op;
	st->left = left;
	return st;
}

/* stubs for missing legacy functions */
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
    asm_capture_buf = malloc(256);
    asm_capture_size = 256;
    asm_capture_len = 0;
    asm_capture_buf[0] = 0;

    /* Set ASM_BLOCK flag for special asm processing */
    tflags |= ASM_BLOCK;

    /* Clear lineend flag from any newline after the opening brace */
    extern unsigned char lineend;
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
                captured_text = asm_capture_buf;
                captured_len = asm_capture_len;
                asm_capture_buf = NULL;
                asm_capture_size = 0;
                asm_capture_len = 0;
                tflags &= ~ASM_BLOCK;  /* Clear ASM_BLOCK flag */
                break;
            }
        }

        /* Get next token (will be captured by lexer if asm_capture_buf is set) */
        gettoken();
    }

    /* Clear ASM_BLOCK flag in case we exited loop due to EOF */
    tflags &= ~ASM_BLOCK;

    /* Trim trailing space and semicolon from captured text */
    while (captured_len > 0 &&
           (captured_text[captured_len-1] == ' ' ||
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



static struct expr *parse_initializer_list(void)
{
    struct expr *head = NULL;
    struct expr *tail = NULL;
    struct expr *item;

    gettoken(); /* consume { */

    while (cur.type != END) {
        if (cur.type == BEGIN) {
            /* Nested initializer for struct/array member */
            item = parse_initializer_list();
        } else {
            /* Parse expression but stop before comma operator (priority 15)
             * so that comma is treated as element separator, not an operator */
            item = parse_expr(OP_PRI_COMMA, NULL);
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

struct expr *
do_initializer(void)
{
    struct expr *init;

    gettoken(); /* consume = token */

    if (cur.type == BEGIN) {
        /* Handle {...} style initializer list */
        init = parse_initializer_list();
    } else {
        /* Handle simple expression initializer */
        init = parse_expr(0, NULL);
    }

    if (init == NULL) {
        gripe(ER_E_IT); /* Invalid initializer term */
        return NULL;
    }

    return init;
}

void
parsefunc(struct name *f)
{
	struct name *param;

	// Set current function context for static variable name mangling
	current_function = f;
	static_counter = 0;

	// Push a new scope for the function body
	push_scope(f->name);

	// Install function parameters into the scope at level 2
	// Read parameter info from f->type->elem but create NEW entries at level 2
	if (f->type && (f->type->flags & TF_FUNC)) {
		for (param = f->type->elem; param; param = param->next) {
			// Only add parameters with actual names (skip anonymous ones)
			if (param->name && param->name[0] != '\0') {
				// Create a NEW name entry at level 2 (don't reuse type->elem entry)
				new_name(param->name, funarg, param->type, 0);
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
	emit_function(f);

	// Pop the function scope
	pop_scope();

	// Clear current function context
	current_function = NULL;
}

/*
 * storage class clauses - many combinations are illogical
 */
char *sclass_bitdefs[] = { "EXTERN", "REGISTER", "STATIC", "CONST", "VOLATILE",
		"AUTO", "TYPEDEF"
};

/*
 * parse the storage class on a declaration.  this is a muddy concept, since
 * we've got visibility and storage lumped together, and context also contributes into where our
 * thing actually will reside.  we're just interested in the parse part.
 * so, let's just eat extern, auto, register, volatile, static and const
 * in other compilers, bizarre stuff like fortran, far and pascal show up here.
 * gripe about bogus combinations.
 *
 * how this actually resolves into a storage space is a code generator issue
 */
unsigned char
parse_sclass()
{
	unsigned char ret = 0;
	unsigned char bit;
#ifdef DEBUG
	if (VERBOSE(V_SYM)) {
		fdprintf(2,"parse_sclass: starting, cur.type=0x%02x\n", cur.type);
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
				fdprintf(2,"parse_sclass: FOUND TYPEDEF token!\n");
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
        (ret & (SC_CONST | SC_STATIC | SC_AUTO | SC_REGISTER))) {
		gripe(ER_P_SC);
	}
	if ((ret & SC_REGISTER) && 
        (ret & (SC_CONST | SC_STATIC))) {
		gripe(ER_P_SC);
	}
	if ((ret & SC_STATIC) && 
        (ret & (SC_CONST | SC_AUTO))) {
		gripe(ER_P_SC);
	}
	if ((ret & SC_CONST) && 
        (ret & (SC_VOLATILE))) {
		gripe(ER_P_SC);
	}
	if ((ret & SC_TYPEDEF) && 
        (ret & (SC_EXTERN | SC_STATIC | SC_AUTO | SC_REGISTER))) {
		gripe(ER_P_SC);
	}
	return ret;
}

/*
 * read a declaration
 */
void
declaration()
{
	unsigned char sclass;
	struct type *basetype;
	struct name *v;

	/* Parse storage class and base type once at the beginning */
	sclass = parse_sclass();
	basetype = 0;  /* Initialize once, then shared across comma-separated declarators */

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
                fdprintf(2,"CONVERTING %s from var to tdef (sclass=0x%02x)\n", v->name, sclass);
            }
#endif
            v->kind = tdef;
            /* typedefs cannot have initializers or function bodies */
            if (cur.type == ASSIGN) {
                gripe(ER_T_TD);
                /* skip the initializer */
                while (cur.type != SEMI && cur.type != COMMA && cur.type != E_O_F) {
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
                parsefunc(v);

                /* Free the statement tree (dumping now happens in parsefunc) */
                if (v->u.body) {
                    free_stmt(v->u.body);
                    v->u.body = 0;  /* Mark as freed */
                }

                /* Assign storage class */
                if (sclass & SC_STATIC) {
                    v->sclass = SC_STATIC;
                    v->mangled_name = mangle_static_name(v);
                } else if (sclass & SC_EXTERN) {
                    v->sclass = SC_EXTERN;
                }
                v->next = global;
                global = v;
                break;
            }
        }

        /* Assign storage class for variables (non-functions or function prototypes) */
        if (sclass & SC_STATIC) {
            v->sclass = SC_STATIC;
            v->mangled_name = mangle_static_name(v);
        } else if (sclass & SC_EXTERN) {
            v->sclass = SC_EXTERN;
        }

        if (cur.type == ASSIGN) {
            cstring str;
            int len;

            v->u.init = do_initializer();

            /* Fix array size for char[] = "string" syntax */
            if (v->type && (v->type->flags & TF_ARRAY) && v->type->count == -1 &&
                v->u.init && v->u.init->op == STRING) {
                /* Get string length from counted string */
                str = (cstring)v->u.init->v;
                if (str) {
                    len = (unsigned char)str[0];  /* First byte is length */
                    /* Create new array type with correct size (length + 1 for null terminator) */
                    v->type = get_type(TF_ARRAY, v->type->sub, len + 1);
                }
            }

            /* Track local variable initializers for conversion to assignments */
            extern int lexlevel;
            if (lexlevel > 1 && v->u.init && !(sclass & SC_STATIC)) {
                add_decl_init(v);
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

char*
blockname()
{
	static int blockid = 0;
	sprintf(bnbuf, "block %d", blockid++);
	return bnbuf;
}

/*
 * global level parse
 */
void
parse()
{
	push_scope("global");
	initbasictype();
	while (cur.type != E_O_F) {
		while (cur.type == NONE) {
			gettoken();
		}
		/* Check if current token looks like start of a declaration */
		/* Also check if it's a typedef name (SYM that's a typedef) */
		struct name *possible_typedef = NULL;
		if (cur.type == SYM) {
			possible_typedef = lookup_name(cur.v.name, 0);
		}

		if (cur.type == INT || cur.type == CHAR || cur.type == SHORT ||
			cur.type == LONG || cur.type == FLOAT || cur.type == DOUBLE ||
			cur.type == VOID || cur.type == UNSIGNED || cur.type == STRUCT ||
			cur.type == UNION || cur.type == ENUM || cur.type == CONST ||
			cur.type == VOLATILE || cur.type == TYPEDEF || cur.type == STATIC ||
			cur.type == REGISTER || cur.type == AUTO || cur.type == EXTERN ||
			(possible_typedef && possible_typedef->kind == tdef)) {
			declaration();
		} else {
			/* Not a declaration - skip this token to avoid getting stuck */
			gettoken();
		}
	}

	/* Emit string literals and global variables before popping scope */
	emit_literals();
	emit_global_vars();

	pop_scope();
}

/*
 * Free a statement tree recursively
 */
void
free_stmt(struct stmt *st)
{
	if (!st)
		return;

	/* Free child statements */
	if (st->chain)
		free_stmt(st->chain);
	if (st->otherwise)
		free_stmt(st->otherwise);
	if (st->next)
		free_stmt(st->next);

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
 */
void
cleanup_parser(void)
{
	int i;
	struct name *n;
	extern struct name **names;
	extern int lastname;

	/* Free all names and their statement trees */
	for (i = 0; i <= lastname; i++) {
		n = names[i];
		if (n) {
			/* Free function body if present */
			if (n->u.body)
				free_stmt(n->u.body);

			/* Free name string (except for function parameters which are owned by type) */
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
		extern struct type *types;
		extern struct type *chartype;
		extern struct type *inttype;
		extern struct type *uchartype;
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
