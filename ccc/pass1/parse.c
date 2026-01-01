/*
 * statement parsing
 */

#include "cc1.h"

/* Loop label generation for control flow transformation */
static unsigned char loopLblCnt = 0;

void resetLoopLbls() { loopLblCnt = 0; }

/* Switch case counting (phase 1) - counts CASE/DEFAULT in each switch */
static unsigned char caseCnt = 0;      /* current switch case count */
static unsigned char caseStack[8];     /* nested switch case counts */
static unsigned char caseDepth = 0;    /* nesting depth */


/* Function body statement count stack (separate from case counts) */
static unsigned char funcCnts[32];   /* stmt counts for functions */
static unsigned char funcCntTop = 0;    /* write pointer (phase 1) */
static unsigned char funcCntIdx = 0;    /* read pointer (phase 2) */

void pushFuncCnt(unsigned char n) {
    if (funcCntTop < 32)
        funcCnts[funcCntTop++] = n;
}

unsigned char popFuncCnt(void) {
    if (funcCntIdx < funcCntTop)
        return funcCnts[funcCntIdx++];
    return 0;
}

/* Reset function stmt count read pointer for phase 2 */
void resetFuncIdx(void) {
    funcCntIdx = 0;
}


/* Label stack for break/continue resolution (phase 2) */
struct lblfrm lblStack[MAX_LBLDEPTH];
unsigned char lblDepth = 0;

/* FOR loop context (phase 2) */
struct forctx forStack[MAX_FORDEPTH];
unsigned char forDepth = 0;

/*
 * Count storage for streaming AST emission
 * Phase 1 records counts (arg counts, case counts, stmt counts).
 * Phase 2 retrieves them in FIFO order (same order they were pushed).
 */
static unsigned char countBuf[MAX_COUNTS];
static unsigned char countTop = 0;   /* write pointer for pushing */
static unsigned char countIdx = 0;   /* read pointer for popping (FIFO) */

void
pushCount(char c)
{
	if (countTop < MAX_COUNTS) {
#ifdef DEBUG
		fdprintf(2, "pushCount[%d] = %d\n", countTop, c);
#endif
		countBuf[countTop++] = (unsigned char)c;
	}
}

char
popCount(void)
{
	if (countIdx < countTop) {
		char c = countBuf[countIdx++];
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
 * Label stack helpers for phase 2 break/continue resolution
 *
 * Labels are simple: B<n> for break, C<n> for continue.
 * Stack stores type (FOR/WHILE/DO/SWITCH) and label number.
 */
static unsigned char loopLblNum = 0;  /* counter for B/C labels */

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


/* Forward declarations */
static char *blockname(void);
struct stmt *statement(struct stmt *parent);

/*
 * Parse a braced block body.
 * Used by control structures that now always have braces.
 * Handles scope push/pop and expects BEGIN...END.
 */
static void
parseBlock(void)
{
	expect(BEGIN, ER_S_SB);
	pushScope(blockname());
	statement(0);
	popScope();
	expect(END, ER_S_CC);
}

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
    char stmt_count = 0;  /* Count statements for streaming (phase 1) */

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
                /* Push statement count only for function body (lexlevel 2)
                 * and not inside a switch (caseDepth == 0). */
#ifdef DEBUG
                fdprintf(2, "END: lexlevel=%d stmt_count=%d caseDepth=%d\n",
                         lexlevel, stmt_count, caseDepth);
#endif
                if (lexlevel == 2 && caseDepth == 0)
                    pushFuncCnt(stmt_count);
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
            case UNSIGNED:
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
            handle_if:
                gettoken();
                expect(LPAR, ER_S_NP);
                parseExpr(PRI_ALL, parent);
                expect(RPAR, ER_S_NP);
                parseBlock();
                if (cur.type == ELSE) {
                    gettoken();
                    if (cur.type == IF)
                        goto handle_if;  /* else if */
                    parseBlock();
                }
                stmt_count++;
                if (parent) block = 0;
                break;
            case WHILE:
                gettoken();
                expect(LPAR, ER_S_NP);
                parseExpr(PRI_ALL, parent);
                expect(RPAR, ER_S_NP);
                parseBlock();
                stmt_count++;
                if (parent) block = 0;
                break;
            case DO:
                gettoken();
                parseBlock();
                expect(WHILE, ER_S_WH);
                expect(LPAR, ER_S_NP);
                parseExpr(PRI_ALL, parent);
                expect(RPAR, ER_S_NP);
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
                parseBlock();
                stmt_count++;
                if (parent) block = 0;
                break;
            case SWITCH: {
                gettoken();
                expect(LPAR, ER_S_NP);
                parseExpr(PRI_ALL, parent);
                expect(RPAR, ER_S_NP);
                expect(BEGIN, ER_S_SB);
                /* Push current case count, start new counter */
                if (caseDepth < 8)
                    caseStack[caseDepth++] = caseCnt;
                caseCnt = 0;
                statement(0);  /* switch body counts cases */
                /* Push case count for phase 2 SWITCH */
                pushCount(caseCnt);
                if (caseDepth > 0)
                    caseCnt = caseStack[--caseDepth];
                expect(END, ER_S_CC);
                stmt_count++;
                if (parent) block = 0;
                break;
            }
            case CASE:
                gettoken();
                parseConst(COLON);
                caseCnt++;  /* count case for current switch */
                expect(COLON, ER_S_NL);
                break;
            case DEFAULT:
                gettoken();
                caseCnt++;  /* count default for current switch */
                expect(COLON, ER_S_NL);
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

        case IF:   /* if <condition> <statement> */
        handle_if2: {
            struct expr *cond;
            gettoken();
            expect(LPAR, ER_S_NP);
            cond = parseExpr(PRI_ALL, parent);
            expect(RPAR, ER_S_NP);
            /* Emit: I has_else nlabels cond then [else] */
            emit1('I');
            emit1(1);  /* always has else slot */
            emit1(cntCondLbls(cond));
            emitExpr(cond);
            FreeExpr(cond);
            parseBlock();
            if (cur.type == ELSE) {
                gettoken();
                if (cur.type == IF)
                    goto handle_if2;  /* else if */
                parseBlock();
            } else {
                emit1(';');  /* empty else */
            }
            st = NULL;
            break;
        }
        case BREAK: {
            /* Emit goto to innermost loop/switch break label */
            char lbl[16];
            int i;
            gettoken();
            expect(SEMI, ER_S_SN);
            /* Find innermost loop or switch */
            for (i = lblDepth - 1; i >= 0; i--) {
                char prefix = 'W';
                if (lblStack[i].type == FOR) prefix = 'F';
                else if (lblStack[i].type == DO) prefix = 'D';
                else if (lblStack[i].type == SWITCH) prefix = 'S';
                sprintf(lbl, "%c%d", prefix, lblStack[i].num);
                break;
            }
            emitGoto(lbl, "B");
            st = NULL;
            break;
        }

        case CONTINUE: {
            /* Emit goto to innermost loop continue label (skip switches) */
            char lbl[16];
            int i;
            gettoken();
            expect(SEMI, ER_S_SN);
            /* Find innermost loop (not switch) */
            for (i = lblDepth - 1; i >= 0; i--) {
                if (lblStack[i].type == SWITCH)
                    continue;  /* continue doesn't apply to switch */
                char prefix = 'W';
                if (lblStack[i].type == FOR) prefix = 'F';
                else if (lblStack[i].type == DO) prefix = 'D';
                sprintf(lbl, "%c%d", prefix, lblStack[i].num);
                break;
            }
            emitGoto(lbl, "C");
            st = NULL;
            break;
        }
        case RETURN: {
            struct expr *ret_expr = NULL;
            gettoken();
            if (cur.type != SEMI)
                ret_expr = parseExpr(PRI_ALL, parent);
            expect(SEMI, ER_S_SN);
            /* Emit: R has_value [expr] */
            emit1('R');
            emit1(ret_expr ? 1 : 0);
            if (ret_expr) {
                emitExpr(ret_expr);
                FreeExpr(ret_expr);
            }
            st = NULL;
            break;
        }

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
                /* Emit: L label */
                emit1('L');
                emitS(cur.v.name);
                gettoken();
                gettoken();
                st = NULL;
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
        case DECR: {
            struct expr *expr = parseExpr(PRI_ALL, parent);
            expect(SEMI, ER_S_SN);
            /* Convert postinc/postdec to preinc/predec since result unused */
            if (expr && (expr->op == INCR || expr->op == DECR) &&
                (expr->flags & E_POSTFIX)) {
                expr->flags &= ~E_POSTFIX;
            }
            /* Emit: E expr */
            emit1('E');
            emitExpr(expr);
            FreeExpr(expr);
            st = NULL;
            break;
        }

        case FOR: { // for (<expr>; <expr>; <expr>) <statement> ;
            struct expr *init_e = NULL, *cond_e = NULL, *incr_e = NULL;
            char lbl[16];
            int num, n;
            gettoken();
            expect(LPAR, ER_S_NP);
            /* Init expression - optional */
            if (cur.type != SEMI)
                init_e = parseExpr(PRI_ALL, parent);
            expect(SEMI, ER_S_SN);
            /* Condition expression - optional */
            if (cur.type != SEMI)
                cond_e = parseExpr(PRI_ALL, parent);
            expect(SEMI, ER_S_SN);
            /* Increment expression - optional */
            if (cur.type != RPAR)
                incr_e = parseExpr(PRI_ALL, parent);
            expect(RPAR, ER_S_NP);
            pushLabel(FOR);
            num = lblStack[lblDepth - 1].num;
            sprintf(lbl, "F%d", num);
            /* Count: top + if + continue + goto + break = 5 base */
            n = 5;
            if (init_e) n++;
            if (incr_e) n++;
            emit1('B');
            emit1(0);
            emit1(n);
            if (init_e) {
                emit1('E');
                emitExpr(init_e);
                FreeExpr(init_e);
            }
            emitLabel(lbl, "T");
            if (cond_e) {
                emit1('I');
                emit1(1);  /* has else */
                emit1(cntCondLbls(cond_e));
                emitExpr(cond_e);
                FreeExpr(cond_e);
                /* then: B 0 2 L<lbl>Y body */
                emit1('B');
                emit1(0);
                emit1(2);
                emitLabel(lbl, "Y");
                parseBlock();
                /* else: B 0 1 G<lbl>B */
                emit1('B');
                emit1(0);
                emit1(1);
                emitGoto(lbl, "B");
            } else {
                /* No condition - body executes unconditionally */
                emit1('B');
                emit1(0);
                emit1(2);
                emitLabel(lbl, "Y");
                parseBlock();
            }
            emitLabel(lbl, "C");
            if (incr_e) {
                emit1('E');
                emitExpr(incr_e);
                FreeExpr(incr_e);
            }
            emitGoto(lbl, "T");
            emitLabel(lbl, "B");
            popLabel();
            st = NULL;
            break;
        }

        case WHILE: {   // while <condition> <statement> ;
            struct expr *cond;
            char lbl[16];
            int num;
            gettoken();
            expect(LPAR, ER_S_NP);
            cond = parseExpr(PRI_ALL, parent);
            expect(RPAR, ER_S_NP);
            pushLabel(WHILE);
            num = lblStack[lblDepth - 1].num;
            sprintf(lbl, "W%d", num);
            /* Structure: B 0 5 L<T> I{B 0 2 L<Y> body}{B 0 1 G<B>} L<C> G<T> L<B> */
            emit1('B');
            emit1(0);
            emit1(5);
            emitLabel(lbl, "T");
            emit1('I');
            emit1(1);  /* has else */
            emit1(cntCondLbls(cond));
            emitExpr(cond);
            FreeExpr(cond);
            /* then: B 0 2 L<lbl>Y body */
            emit1('B');
            emit1(0);
            emit1(2);
            emitLabel(lbl, "Y");
            parseBlock();
            /* else: B 0 1 G<lbl>B */
            emit1('B');
            emit1(0);
            emit1(1);
            emitGoto(lbl, "B");
            emitLabel(lbl, "C");
            emitGoto(lbl, "T");
            emitLabel(lbl, "B");
            popLabel();
            st = NULL;
            break;
        }

        case ELSE:
            recover(SEMI, ER_S_OE);
            break;

        case SWITCH: {  // switch (<expr>) <block> ;
            char lbl[16];
            int num;
            gettoken();
            expect(LPAR, ER_S_NP);
            st = makestmt(SWITCH, parseExpr(PRI_ALL, parent));
            expect(RPAR, ER_S_NP);
            expect(BEGIN, ER_S_SB);
            pushLabel(SWITCH);
            num = lblStack[lblDepth - 1].num;
            sprintf(lbl, "S%d", num);
            st->label = strdup(lbl);
            st->chain = statement(st);
            popLabel();
            expect(END, ER_S_CC);
            break;
        }

        case CASE:
            gettoken();
            st = makestmt(CASE, parseExpr(13, parent));  /* stop at colon */
            expect(COLON, ER_S_NL);
            break;

        case GOTO: {
            char *lbl;
            gettoken();
            if (cur.type != SYM) {
                recover(ER_S_GL, SEMI);
                st = NULL;
                break;
            }
            lbl = cur.v.name;
            gettoken();
            expect(SEMI, ER_S_SN);
            /* Emit: G label */
            emit1('G');
            emitS(lbl);
            st = NULL;
            break;
        }

        case DEFAULT:
            gettoken();
            expect(COLON, ER_S_NL);
            st = makestmt(DEFAULT, NULL);
            break;

        case ';':
            gettoken();
            emit1(';');
            st = NULL;
            break;

        case DO: {  // do <statement> while <condition> ;
            struct expr *cond;
            char lbl[16];
            int num;
            gettoken();
            pushLabel(DO);
            num = lblStack[lblDepth - 1].num;
            sprintf(lbl, "D%d", num);
            /* Structure: B 0 5 L<T> body L<C> I{G<T>} L<B> */
            /* DO structure matches source order: body first, then test */
            emit1('B');
            emit1(0);
            emit1(5);
            emitLabel(lbl, "T");   /* top of body */
            parseBlock();          /* body */
            emitLabel(lbl, "C");   /* continue point (before test) */
            if (cur.type != WHILE) {
                gripe(ER_S_WH);
                popLabel();
                st = NULL;
                break;
            }
            gettoken();
            expect(LPAR, ER_S_NP);
            cond = parseExpr(PRI_ALL, parent);
            need(RPAR, SEMI, ER_S_NP);
            expect(SEMI, ER_S_SN);
            /* if(cond) goto T - no else needed */
            emit1('I');
            emit1(0);  /* no else */
            emit1(cntCondLbls(cond));
            emitExpr(cond);
            FreeExpr(cond);
            emitGoto(lbl, "T");    /* loop back if true */
            emitLabel(lbl, "B");   /* break point */
            popLabel();
            st = NULL;
            break;
        }

        case ASM:
            st = asmblock();  /* asmblock() handles token advancement */
            break;

        default:
            gripe(ER_E_UO);
            break;
        }
        /*
         * Handle statement result
         */
        if (st) {
#ifdef DEBUG
            if (phase == 2)
                fdprintf(2, "  -> st=%p op=%d, head=%p\n", st, st->op, head);
#endif
            /*
             * Streaming mode: when parsing function body (parent == NULL)
             * or single-statement mode (parent == 1), emit and free each
             * statement immediately instead of building a linked list.
             */
            if (!parent || parent == (struct stmt *)1) {
                /* Stream emit and free */
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
        }
#ifdef DEBUG
        else if (phase == 2) {
            fdprintf(2, "  -> st=NULL (streaming)\n");
        }
#endif

        /*
         * If we're parsing a single-statement body for a control
         * structure (if/while/for/etc), return after parsing one
         * statement. Don't return for block statements (BEGIN),
         * switch statements (SWITCH), or top-level (parent ==
         * NULL/function body).
         *
         * For streaming statements (st=NULL), we still need to exit
         * after one statement when in single-statement mode.
         */
        if (parent && parent != (struct stmt *)1 &&
            parent->op != BEGIN && parent->op != SWITCH) {
            block = 0;  // Exit the while loop
        } else if (parent == (struct stmt *)1) {
            /* Special marker for single-statement mode */
            block = 0;
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
	static char blockid = 0;
	sprintf(bnbuf, "block %d", blockid++);
	return bnbuf;
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
