/*
 * statement parsing
 */

#include "cc1.h"

/* Loop label generation for control flow transformation */
static int loopLblCnt = 0;

void resetLoopLbls(void) { loopLblCnt = 0; }

/* Switch statement collection (phase 1) - defined but not yet used */
struct swhdr *swList = 0;
struct swhdr *curSw = 0;

/* Label stack for break/continue resolution (phase 2) */
struct lblfrm lblStack[MAX_LBLDEPTH];
int lblDepth = 0;

/* FOR loop context (phase 2) */
struct forctx forStack[MAX_FORDEPTH];
int forDepth = 0;

/*
 * Count storage for streaming AST emission
 * Phase 1 records counts (arg counts, case counts, stmt counts).
 * Phase 2 retrieves them in FIFO order (same order they were pushed).
 */
static unsigned char countBuf[MAX_COUNTS];
static int countTop = 0;   /* write pointer for pushing */
static int countIdx = 0;   /* read pointer for popping (FIFO) */

void
pushCount(int c)
{
	if (countTop < MAX_COUNTS) {
#ifdef DEBUG
		fdprintf(2, "pushCount[%d] = %d\n", countTop, c);
#endif
		countBuf[countTop++] = (unsigned char)c;
	}
}

int
popCount(void)
{
	if (countIdx < countTop) {
		int c = countBuf[countIdx++];
#ifdef DEBUG
		fdprintf(2, "popCount[%d] = %d\n", countIdx - 1, c);
#endif
		return c;
	}
#ifdef DEBUG
	fdprintf(2, "popCount: UNDERFLOW!\n");
#endif
	return 0;
}

void
resetCounts(void)
{
	countTop = 0;
	countIdx = 0;
}

/*
 * Generate unique synthetic labels for loops and switch statements
 * Prefix indicates statement type: L=loop (while/for), D=do-while, S=switch
 */
static char *
genLoopLabel(char *prefix)
{
	char *label = malloc(32);
	sprintf(label, "%s%d", prefix, loopLblCnt++);
	return label;
}

/*
 * Switch statement collection helpers (phase 1)
 */
static struct swhdr *swStack[8];
static int swDepth = 0;

void
resetSwitches(void)
{
	/* Free all switch headers and case entries */
	struct swhdr *sh = swList;
	while (sh) {
		struct swhdr *next = sh->next;
		struct caseent *ce = sh->cases;
		while (ce) {
			struct caseent *cnext = ce->next;
			free(ce);
			ce = cnext;
		}
		free(sh);
		sh = next;
	}
	swList = 0;
	curSw = 0;
	swDepth = 0;
}

static struct swhdr *
newSwitch(char *label)
{
	struct swhdr *sh = malloc(sizeof(struct swhdr));
	sh->next = swList;
	sh->cases = 0;
	sh->caseCnt = 0;
	sh->hasDef = 0;
	strncpy(sh->label, label, 7);
	sh->label[7] = 0;
	swList = sh;
	return sh;
}

static void
pushSwitch(struct swhdr *sh)
{
	if (swDepth < 8) {
		swStack[swDepth++] = curSw;
	}
	curSw = sh;
}

static void
popSwitch(void)
{
	if (swDepth > 0) {
		curSw = swStack[--swDepth];
	} else {
		curSw = 0;
	}
}

static void
addCase(long value, unsigned char isDef)
{
	struct caseent *ce;
	if (!curSw) return;
	ce = malloc(sizeof(struct caseent));
	ce->value = value;
	ce->isDef = isDef;
	ce->next = curSw->cases;
	curSw->cases = ce;
	curSw->caseCnt++;
	if (isDef) curSw->hasDef = 1;
}

/*
 * Label stack helpers for phase 2 break/continue resolution
 *
 * Labels are simple: B<n> for break, C<n> for continue.
 * Stack stores type (FOR/WHILE/DO/SWITCH) and label number.
 */
static int loopLblNum = 0;  /* counter for B/C labels */

static void
pushLabel(unsigned char type)
{
	if (lblDepth < MAX_LBLDEPTH) {
		lblStack[lblDepth].type = type;
		lblStack[lblDepth].num = loopLblNum++;
		lblDepth++;
	}
}

static void
popLabel(void)
{
	if (lblDepth > 0)
		lblDepth--;
}

/*
 * Find break target - returns label number or -1
 * Break can target: WHILE, FOR, DO, SWITCH
 */
static int
findBreakLbl(void)
{
	int i;
	for (i = lblDepth - 1; i >= 0; i--) {
		unsigned char t = lblStack[i].type;
		if (t == WHILE || t == FOR || t == DO || t == SWITCH)
			return lblStack[i].num;
	}
	return -1;
}

/*
 * Find continue target - returns label number or -1
 * Continue can target: WHILE, FOR, DO (not SWITCH)
 */
static int
findContLbl(void)
{
	int i;
	for (i = lblDepth - 1; i >= 0; i--) {
		unsigned char t = lblStack[i].type;
		if (t == WHILE || t == FOR || t == DO)
			return lblStack[i].num;
	}
	return -1;
}

/* Forward declaration */
static char *blockname(void);

/*
 * Capture local variables from the current scope level
 * Returns a linked list of name structures (shallow copies)
 */
struct name *
capLocals(void)
{
	struct name *locals_list = NULL;
	struct name *tail = NULL;
	struct name *n, *copy;

	/* Traverse chain - current level names are at head */
	for (n = names; n && n->level == lexlevel; n = n->chain) {
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
 * Convert deferred local variable initializers to assignment statements
 * When parent == NULL (streaming mode), emit and free immediately.
 * Otherwise, link into statement list and return head.
 */
static struct stmt *
emitDeclInits(struct stmt ***ppst, struct stmt *parent)
{
	struct stmt *head = NULL;
	unsigned char i;

	for (i = 0; i < declInitCnt; i++) {
		struct name *v = declInits[i];
		struct expr *lhs, *assign_expr;
		struct stmt *assign_st;

		lhs = mkexprI(SYM, 0, v->type, 0, 0);
		lhs->var = (struct var *)v;

		if (v->type && (v->type->flags & TF_ARRAY) && v->u.init) {
			assign_expr = mkexprI(COPY, lhs, v->type, v->type->count, 0);
		} else {
			assign_expr = mkexprI(ASSIGN, lhs, v->type, 0, 0);
		}
		assign_expr->right = v->u.init;
		v->u.init = NULL;

		assign_st = makestmt(EXPR, assign_expr);

		if (!parent) {
			/* Streaming mode: emit and free immediately */
			emitOneStmt(assign_st);
			frStmt(assign_st);
		} else {
			/* Nested block: link into statement list */
			if (!*ppst) {
				head = assign_st;
				assign_st->flags |= S_PARENT;
			} else {
				**ppst = assign_st;
			}
			*ppst = &assign_st->next;
			assign_st->parent = parent;
		}
	}
	declInitCnt = 0;
	return head;
}

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
    unsigned char block = 1;
    int stmt_count = 0;  /* Count statements for streaming (phase 1) */

    while (block) {
        st = NULL;  // Initialize st to NULL for each iteration

#ifdef DEBUG
        if (phase == 2)
            fdprintf(2, "statement() phase=%d cur.type=%d (%c)\n",
                     phase, cur.type, cur.type > 31 && cur.type < 127 ? cur.type : '?');
#endif

        /*
         * Phase 1: Skip statements, only track scope and declarations
         * This discovers local variables without building trees.
         * Also counts statements for streaming emission in phase 2.
         */
        if (phase == 1) {
            switch (cur.type) {
            case END:
            case E_O_F:
                /* Push statement count only for function body (lexlevel 2).
                 * Nested blocks (lexlevel > 2) are lowered with hardcoded
                 * or computed counts in phase 2, so don't push for them. */
#ifdef DEBUG
                fdprintf(2, "END: lexlevel=%d stmt_count=%d\n", lexlevel, stmt_count);
#endif
                if (lexlevel == 2)
                    pushCount(stmt_count);
                block = 0;
                break;
            case BEGIN:
                gettoken();
                pushScope(blockname());
                statement(0);  /* recurse for nested block */
                popScope();
                expect(END, ER_S_CC);
                stmt_count++;  /* BEGIN counts as one statement */
                if (parent) block = 0;
                break;
            /* Declarations - process for symbol table but don't count */
            case INT: case CHAR: case SHORT: case LONG:
            case FLOAT: case DOUBLE: case VOID:
            case STRUCT: case UNION: case ENUM:
            case UNSIGNED: case CONST: case VOLATILE:
            case STATIC: case REGISTER: case AUTO:
            case EXTERN: case TYPEDEF:
                declaration();
                break;
            case SYM:
                /* Check typedef name for declaration */
                {
                    struct name *pt = findName(cur.v.name, 0);
                    if (pt && pt->kind == tdef) {
                        declaration();
                        break;
                    }
                }
                /* Check for label - labels don't count as statements */
                if (next.type == COLON) {
                    gettoken();
                    gettoken();
                    break;
                }
                /* Fall through to expression statement */
            case NUMBER: case STRING: case LPAR:
            case STAR: case INCR: case DECR:
                parseExpr(PRI_ALL, parent);
                expect(SEMI, ER_S_SN);
                stmt_count++;
                if (parent) block = 0;
                break;
            case IF:
                gettoken();
                expect(LPAR, ER_S_NP);
                parseExpr(PRI_ALL, parent);
                expect(RPAR, ER_S_NP);
                statement((struct stmt*)1);  /* single-statement mode */
                if (cur.type == ELSE) {
                    gettoken();
                    statement((struct stmt*)1);
                }
                stmt_count++;
                if (parent) block = 0;
                break;
            case WHILE:
                gettoken();
                expect(LPAR, ER_S_NP);
                parseExpr(PRI_ALL, parent);
                expect(RPAR, ER_S_NP);
                statement((struct stmt*)1);
                stmt_count++;
                if (parent) block = 0;
                break;
            case DO:
                gettoken();
                statement((struct stmt*)1);  /* single-statement mode */
                if (cur.type == WHILE) {
                    gettoken();
                    expect(LPAR, ER_S_NP);
                    parseExpr(PRI_ALL, parent);
                    expect(RPAR, ER_S_NP);
                }
                expect(SEMI, ER_S_SN);
                stmt_count++;
                if (parent) block = 0;
                break;
            case FOR:
                gettoken();
                expect(LPAR, ER_S_NP);
                if (cur.type != SEMI)
                    parseExpr(PRI_ALL, parent);
                expect(SEMI, ER_S_SN);
                if (cur.type != SEMI)
                    parseExpr(PRI_ALL, parent);
                expect(SEMI, ER_S_SN);
                if (cur.type != RPAR)
                    parseExpr(PRI_ALL, parent);
                expect(RPAR, ER_S_NP);
                statement((struct stmt*)1);
                stmt_count++;
                if (parent) block = 0;
                break;
            case SWITCH: {
                struct swhdr *sh;
                char *label = genLoopLabel("S");
                gettoken();
                expect(LPAR, ER_S_NP);
                parseExpr(PRI_ALL, parent);
                expect(RPAR, ER_S_NP);
                expect(BEGIN, ER_S_SB);
                /* Create and push switch header for case collection */
                sh = newSwitch(label);
                pushSwitch(sh);
                free(label);
                statement(0);  /* switch body - will push its count */
                popSwitch();
                expect(END, ER_S_CC);
                stmt_count++;
                if (parent) block = 0;
                break;
            }
            case CASE: {
                /* Collect case value for current switch */
                long val;
                gettoken();
                val = parseConst(COLON);
                addCase(val, 0);
                expect(COLON, ER_S_NL);
                stmt_count++;
                break;
            }
            case DEFAULT:
                gettoken();
                addCase(0, 1);  /* default case */
                expect(COLON, ER_S_NL);
                stmt_count++;
                break;
            case BREAK: case CONTINUE:
                gettoken();
                expect(SEMI, ER_S_SN);
                stmt_count++;
                if (parent) block = 0;
                break;
            case RETURN:
                gettoken();
                if (cur.type != SEMI)
                    parseExpr(PRI_ALL, parent);
                expect(SEMI, ER_S_SN);
                stmt_count++;
                if (parent) block = 0;
                break;
            case GOTO:
                gettoken();
                if (cur.type == SYM)
                    gettoken();
                expect(SEMI, ER_S_SN);
                stmt_count++;
                if (parent) block = 0;
                break;
            case ASM:
                gettoken();  /* asmblock consumes the text in cur.v.str */
                stmt_count++;
                if (parent) block = 0;
                break;
            case ';':
                gettoken();
                stmt_count++;
                if (parent) block = 0;
                break;
            default:
                gettoken();  /* skip unknown token */
                break;
            }
            continue;  /* phase 1: don't build statement tree */
        }

        /*
         * Phase 2: Normal statement parsing with tree building
         */
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
            break;

        case IF:    // if <condition> <statement>
            gettoken();
            expect(LPAR, ER_S_NP);
            st = makestmt(IF, parseExpr(PRI_ALL, parent));
            expect(RPAR, ER_S_NP);
            st->parent = parent;
            st->chain = statement(st);
            if (cur.type == ELSE) {
                gettoken();
                st->otherwise = statement(st);
            }
            break;
        case BREAK:
        case CONTINUE: {
            unsigned char is_cont = (cur.type == CONTINUE);
            int num;
            gettoken();
            expect(SEMI, ER_S_SN);
            /* Use label stack: B<num> for break, C<num> for continue */
            num = is_cont ? findContLbl() : findBreakLbl();
            if (num >= 0) {
                st = makestmt(GOTO, 0);
                st->label = malloc(16);
                sprintf(st->label, "%c%d", is_cont ? 'C' : 'B', num);
            } else {
                st = makestmt(is_cont ? CONTINUE : BREAK, 0);
            }
            break;
        }
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
            declInitCnt = 0;
            declaration();
            if (declInitCnt > 0) {
                struct stmt *init_head = emitDeclInits(&pst, parent);
                if (init_head && !head)
                    head = init_head;
            }
            st = NULL;
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
                struct name *poss_typedef = findName(cur.v.name, 0);
                if (poss_typedef && poss_typedef->kind == tdef) {
                    declInitCnt = 0;
                    declaration();
                    if (declInitCnt > 0) {
                        struct stmt *init_head = emitDeclInits(&pst, parent);
                        if (init_head && !head)
                            head = init_head;
                    }
                    st = NULL;
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
            pushLabel(FOR);
            st->chain = statement(st);
            popLabel();
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
            pushLabel(WHILE);
            st->chain = statement(st);
            popLabel();
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
            pushLabel(SWITCH);
            st->chain = statement(st);
            popLabel();
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
            pushLabel(DO);
            st->chain = statement(st);
            popLabel();
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
            st = asmblock();  /* asmblock() handles token advancement */
            break;

        default:
            gripe(ER_E_UO);
            break;
        }
        // Skip if no statement was created (e.g., default case, EOF)
        if (!st) {
#ifdef DEBUG
            if (phase == 2)
                fdprintf(2, "  -> st=NULL, continuing\n");
#endif
            continue;
        }
#ifdef DEBUG
        if (phase == 2)
            fdprintf(2, "  -> st=%p op=%d, head=%p\n", st, st->op, head);
#endif

        /*
         * Streaming mode: when parsing function body (parent == NULL)
         * in phase 2, emit and free each statement immediately instead
         * of building a linked list.
         */
        if (!parent) {
            /* Function body - stream emit and free */
            emitOneStmt(st);
            frStmt(st);
#ifdef DEBUG
            if (phase == 2)
                fdprintf(2, "  after free: exprs=%d\n", exprCurCnt);
#endif
        } else {
            /* Nested block - build linked list as usual */
            if (!pst) {
                head = st;
                st->flags |= S_PARENT;
            } else {
                *pst = st;  // Link previous statement to this one
            }
            pst = &st->next;
            st->parent = parent;
        }

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
    char *text;

    /* The ASM token should have raw text in cur.v.str (captured by lexer) */
    if (!cur.v.str) {
        gripe(ER_S_SB);  /* Expected asm block */
        return NULL;
    }

    /* Take ownership of the raw text */
    text = cur.v.str;
    cur.v.str = NULL;

    /* Get next token to continue parsing */
    gettoken();

    /* Create statement with assembly text */
    st = makestmt(ASM, NULL);
    st->label = text;

    return st;
}

static char bnbuf[20];

/*
 * Generate a unique name for a block scope
 */
static char*
blockname()
{
	static int blockid = 0;
	sprintf(bnbuf, "block %d", blockid++);
	return bnbuf;
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
