/*
 * statement parsing
 */

#include "cc1.h"

/* Switch statement table tracking (phase 1) */
struct swtab swList[MAX_SWITCHES];
unsigned char swCount = 0;          /* number of switches in function */
unsigned char swStack[MAX_SWDEPTH]; /* nesting stack (indices into swList) */
unsigned char swDepth = 0;          /* nesting depth */

/* Global case pool - all switches share this */
struct swcase casePool[MAX_ALLCASES];
unsigned char casePoolIdx = 0;

/* Phase 2 switch emission tracking */
unsigned char swEmitIdx = 0;                /* next switch to emit */
unsigned char swEmitStack[MAX_SWDEPTH];     /* stack of switch indices */
unsigned char swEmitDepth = 0;              /* emit stack depth */
unsigned char caseEmitIdx[MAX_SWITCHES];    /* case indices per switch */

/* If/else tracking: store has_else flag for each if statement */
#define MAX_IFS 128
static unsigned char ifHasElse[MAX_IFS];    /* 1 if if #N has else, 0 otherwise */
static unsigned char ifCount = 0;           /* phase 1: count of if statements */
static unsigned char ifEmitIdx = 0;         /* phase 2: next if to emit */

void resetSwitch(void) {
    swCount = 0;
    swDepth = 0;
    swEmitIdx = 0;
    swEmitDepth = 0;
    casePoolIdx = 0;
    ifCount = 0;
    ifEmitIdx = 0;
}

void pushSwitch(void) {
    if (swCount < MAX_SWITCHES && swDepth < MAX_SWDEPTH) {
        unsigned char idx = swCount++;
        swList[idx].cases = &casePool[casePoolIdx];
        swList[idx].count = 0;
        swList[idx].num = idx;
        swList[idx].base_stmts = 0;
        swStack[swDepth++] = idx;
    }
}

void popSwitch(void) {
    if (swDepth > 0)
        swDepth--;
}

/* Finalize previous case's stmt count before starting a new case */
void finishCase(unsigned char stmt_cnt) {
    if (swDepth > 0) {
        unsigned char idx = swStack[swDepth - 1];
        struct swtab *sw = &swList[idx];
        if (sw->count > 0) {
            sw->cases[sw->count - 1].stmts = stmt_cnt - sw->base_stmts;
        }
        sw->base_stmts = stmt_cnt;
    }
}

void addCase(long value, unsigned char stmt_cnt) {
    if (swDepth > 0 && casePoolIdx < MAX_ALLCASES) {
        unsigned char idx = swStack[swDepth - 1];
        struct swtab *sw = &swList[idx];
        /* Finalize previous case if any */
        if (sw->count > 0) {
            sw->cases[sw->count - 1].stmts = stmt_cnt - sw->base_stmts;
        }
        sw->base_stmts = stmt_cnt;
        /* Add new case to pool */
        sw->cases[sw->count].value = value;
        sw->cases[sw->count].is_default = 0;
        sw->cases[sw->count].stmts = 0;  /* will be set by next case or popSwitch */
        sw->count++;
        casePoolIdx++;
    }
}

void addDefault(unsigned char stmt_cnt) {
    if (swDepth > 0 && casePoolIdx < MAX_ALLCASES) {
        unsigned char idx = swStack[swDepth - 1];
        struct swtab *sw = &swList[idx];
        /* Finalize previous case if any */
        if (sw->count > 0) {
            sw->cases[sw->count - 1].stmts = stmt_cnt - sw->base_stmts;
        }
        sw->base_stmts = stmt_cnt;
        /* Add default to pool */
        sw->cases[sw->count].value = 0;
        sw->cases[sw->count].is_default = 1;
        sw->cases[sw->count].stmts = 0;  /* will be set by next case or popSwitch */
        sw->count++;
        casePoolIdx++;
    }
}


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

/* Block statement counts - pushed at END in phase 1, popped at BEGIN in phase 2 */
#define MAX_BLKCNTS 256
static unsigned char blkCnts[MAX_BLKCNTS];
static unsigned short blkCntTop = 0;
static unsigned short blkCntIdx = 0;

void pushBlkCnt(unsigned char n) {
    if (blkCntTop < MAX_BLKCNTS)
        blkCnts[blkCntTop++] = n;
}

unsigned char popBlkCnt(void) {
    if (blkCntIdx < blkCntTop)
        return blkCnts[blkCntIdx++];
    return 0;
}

/* Reverse the block counts for phase 2 (LIFO -> FIFO) */
void flipBlkCnts(void) {
    unsigned short i, j;
    unsigned char tmp;
    if (blkCntTop == 0)
        return;
    for (i = 0, j = blkCntTop - 1; i < j; i++, j--) {
        tmp = blkCnts[i];
        blkCnts[i] = blkCnts[j];
        blkCnts[j] = tmp;
    }
    blkCntIdx = 0;
}

void resetBlkCnts(void) {
    blkCntTop = 0;
    blkCntIdx = 0;
}

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
		countBuf[countTop] = (unsigned char)c;
#ifdef DEBUG
		fdprintf(2, "pushCount[%d] = %d @%p (verify=%d)\n", countTop, c,
		         &countBuf[countTop], countBuf[countTop]);
#endif
		countTop++;
	}
}

char
popCount(void)
{
	if (countIdx < countTop) {
		char c = countBuf[countIdx++];
#ifdef DEBUG
		fdprintf(2, "popCount[%d] = %d @%p (top=%d)\n", countIdx - 1, c, &countBuf[countIdx-1], countTop);
#endif
		return c;
	}
#ifdef DEBUG
	fdprintf(2, "popCount: UNDERFLOW! (idx=%d top=%d)\n", countIdx, countTop);
#endif
	return 0;
}

void
resetCounts(void)
{
	countTop = 0;
	countIdx = 0;
}

/* Forward declarations */
static char *blockname(void);

/*
 * Parse a braced block body.
 * Used by control structures that now always have braces.
 * Handles scope push/pop and expects BEGIN...END.
 * If emitHdr is true and phase==2, emit block header.
 */
static void
parseBlockEx(int emitHdr)
{
	expect(BEGIN, ER_S_SB);
	pushScope(blockname());
	/* In phase 2, emit block header since we consumed BEGIN */
	if (emitHdr && phase == 2) {
		unsigned char cnt = popBlkCnt();
		emit1('B');
		emit1(0);  /* no decls - hoisted to function */
		emit1(cnt);
	}
	statement();
	popScope();
	expect(END, ER_S_CC);
}

static void
parseBlock(void)
{
	parseBlockEx(1);  /* emit header by default */
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

/*
 * Parse statements recursively - the heart of the compiler frontend
 *
 * This function implements the statement parser for C, handling all control
 * flow structures, expressions, declarations, and blocks. It uses recursive
 * descent to parse nested statements.
 *
 * Statement types handled:
 *   - Blocks: { ... } with lexical scoping
 *   - Control flow: if/else, switch/case/default (loops handled by cpp)
 *   - Jumps: return, goto, labels (break/continue handled by cpp)
 *   - Expressions: function calls, assignments, operators
 *   - Declarations: local variables, typedefs (scoped to current block)
 *   - Inline assembly: asm { ... }
 *
 * Lexical scoping:
 *   - Each block pushes a new scope, pops on exit
 *   - Local variables are captured before scope pop to preserve metadata
 *   - Nested blocks can shadow outer names
 *
 * Two-phase operation:
 *   Phase 1: Build symbol table, count statements for streaming
 *   Phase 2: Emit AST bytecode using counts from phase 1
 */
void
statement(void)
{
    unsigned char block = 1;
    char stmt_count = 0;       /* Count statements for streaming (phase 1) */
    /* Hoisted locals - shared across cases to reduce stack frame */
    char *lblptr;              /* Label pointer for GOTO */
    char *text;                /* Text pointer for ASM */
    unsigned char cnt;         /* Shared count (body_cnt, case_cnt, etc) */
    unsigned char hasElse;     /* If statement has else branch */
    unsigned char sw_idx, c_idx; /* Switch/case indices */
    struct swcase *sc;         /* Switch case pointer */
    struct expr *e1;           /* Shared expression pointer */

    while (block) {
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
                /* Push statement count for blocks */
#ifdef DEBUG
                fdprintf(2, "END: lexlevel=%d stmt_count=%d swDepth=%d\n",
                         lexlevel, stmt_count, swDepth);
#endif
                /* Finalize last case when ending switch body */
                if (swDepth > 0)
                    finishCase(stmt_count);
                /* Function body uses separate mechanism */
                if (lexlevel == 2 && swDepth == 0)
                    pushFuncCnt(stmt_count);
                /* Nested blocks (lexlevel > 2, not switch body) push to block counts */
                else if (lexlevel > 2 && swDepth == 0)
                    pushBlkCnt(stmt_count);
                block = 0;
                break;
            case BEGIN:
                gettoken();
                pushScope(blockname());
                statement();  /* recurse for nested block */
                popScope();
                expect(END, ER_S_CC);
                stmt_count++;  /* BEGIN counts as one statement */
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
                parseExpr(PRI_ALL);
                expect(SEMI, ER_S_SN);
                stmt_count++;
                break;
            case IF:
            handle_if: {
                unsigned char thisIf = ifCount++;
                gettoken();
                expect(LPAR, ER_S_NP);
                parseExpr(PRI_ALL);
                expect(RPAR, ER_S_NP);
                parseBlock();
                if (cur.type == ELSE) {
                    ifHasElse[thisIf] = 1;
                    gettoken();
                    if (cur.type == IF)
                        goto handle_if;  /* else if */
                    parseBlock();
                } else {
                    ifHasElse[thisIf] = 0;
                }
                stmt_count++;
                break;
            }
            /* WHILE/DO/FOR handled by cpp loop lowering */
            case SWITCH: {
                unsigned char idx;
                gettoken();
                expect(LPAR, ER_S_NP);
                parseExpr(PRI_ALL);
                expect(RPAR, ER_S_NP);
                expect(BEGIN, ER_S_SB);
                pushSwitch();  /* start new switch table */
                statement();  /* switch body - adds cases to table */
                /* Push case count for phase 2 before popping */
                idx = swStack[swDepth - 1];
                pushCount(swList[idx].count);
                popSwitch();
                expect(END, ER_S_CC);
                stmt_count++;
                break;
            }
            case CASE: {
                long val;
                gettoken();
                val = parseConst(COLON);
                addCase(val, stmt_count);  /* add to current switch table */
                expect(COLON, ER_S_NL);
                break;
            }
            case DEFAULT:
                gettoken();
                addDefault(stmt_count);  /* add default to current switch table */
                expect(COLON, ER_S_NL);
                break;
            case BREAK:  /* only in switch - loop breaks handled by cpp */
                gettoken();
                expect(SEMI, ER_S_SN);
                stmt_count++;
                break;
            case RETURN:
                gettoken();
                if (cur.type != SEMI)
                    parseExpr(PRI_ALL);
                expect(SEMI, ER_S_SN);
                stmt_count++;
                break;
            case GOTO:
                gettoken();
                if (cur.type == SYM)
                    gettoken();
                expect(SEMI, ER_S_SN);
                stmt_count++;
                break;
            case ASM:
                gettoken();  /* asmblock consumes the text in cur.v.str */
                stmt_count++;
                break;
            case SEMI:
                gettoken();
                stmt_count++;
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

        case BEGIN:  // begin a block
            gettoken();
            pushScope(blockname());
            /* Emit block header: B 0 stmt_count */
            emit1('B');
            emit1(0);  /* no decls - hoisted to function */
            cnt = popBlkCnt();
            emit1(cnt);
            /* Stream body statements */
            statement();
            popScope();
            expect(END, ER_S_CC);
            break;

        case IF:   /* if <condition> <statement> */
        handle_if2:
            hasElse = ifHasElse[ifEmitIdx++];
            gettoken();
            expect(LPAR, ER_S_NP);
            e1 = parseExpr(PRI_ALL);
            expect(RPAR, ER_S_NP);
            /* Emit: I nlabels cond then has_else [else] */
            emit1('I');
            emit1(cntCondLbls(e1));
            emitExpr(e1);
            FreeExpr(e1);
            parseBlock();
            emit1(hasElse);  /* has_else comes after then block */
            if (cur.type == ELSE) {
                gettoken();
                if (cur.type == IF)
                    goto handle_if2;  /* else if */
                parseBlock();
            }
            break;

        /* BREAK/CONTINUE handled by cpp - lowered to goto */

        case RETURN:
            e1 = NULL;
            gettoken();
            if (cur.type != SEMI)
                e1 = parseExpr(PRI_ALL);
            expect(SEMI, ER_S_SN);
            /* Emit: R has_value [expr] */
            emit1('R');
            emit1(e1 ? 1 : 0);
            if (e1) {
                emitExpr(e1);
                FreeExpr(e1);
            }
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
        case STATIC:
        case REGISTER:
        case AUTO:
        case EXTERN:
            declaration();
            break;

        case TYPEDEF:
            /* typedef inside function body - scoped to current block */
            declaration();
            break;

        case SYM:
            /* Check if it's a label */
            if (next.type == COLON) {
                /* Emit: L label */
                emit1('L');
                emitS(cur.v.name);
                gettoken();
                gettoken();
                    break;
            }
            /* Check if it's a typedef name used in a declaration */
            {
                struct name *poss_typedef = findName(cur.v.name, 0);
                if (poss_typedef && poss_typedef->kind == tdef) {
                    declaration();
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
            struct expr *expr = parseExpr(PRI_ALL);
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
            break;
        }

        /* FOR/WHILE/DO handled by cpp loop lowering - should not appear here */

        case ELSE:
            recover(SEMI, ER_S_OE);
            break;

        case SWITCH:  /* switch (<expr>) <block> ; */
            gettoken();
            expect(LPAR, ER_S_NP);
            e1 = parseExpr(PRI_ALL);
            expect(RPAR, ER_S_NP);
            expect(BEGIN, ER_S_SB);
            /* Get this switch's index and push onto emit stack */
            sw_idx = swEmitIdx++;
            swEmitStack[swEmitDepth++] = sw_idx;
            caseEmitIdx[sw_idx] = 0;
            /* Emit switch header: S has_label case_count expr */
            /* has_label=0 since cpp handles break lowering */
            emit1('S');
            emit1(0);  /* no label - cpp lowered break to goto */
            cnt = popCount();
            emit1(cnt);
            emitExpr(e1);
            FreeExpr(e1);
            /* Parse body - CASE/DEFAULT emit themselves */
            statement();
            swEmitDepth--;
            expect(END, ER_S_CC);
            break;

        case CASE:
            gettoken();
            e1 = parseExpr(13);  /* parse case value expression */
            expect(COLON, ER_S_NL);
            /* Get current switch and case */
            sw_idx = swEmitStack[swEmitDepth - 1];
            c_idx = caseEmitIdx[sw_idx]++;
            sc = &swList[sw_idx].cases[c_idx];
            /* Emit: C stmt_count value_expr */
            emit1('C');
            emit1(sc->stmts);
            emitExpr(e1);
            FreeExpr(e1);
            break;

        case GOTO:
            gettoken();
            if (cur.type != SYM) {
                recover(ER_S_GL, SEMI);
                    break;
            }
            lblptr = cur.v.name;
            gettoken();
            expect(SEMI, ER_S_SN);
            /* Emit: G label */
            emit1('G');
            emitS(lblptr);
            break;

        case DEFAULT:
            gettoken();
            expect(COLON, ER_S_NL);
            /* Get current switch and case */
            sw_idx = swEmitStack[swEmitDepth - 1];
            c_idx = caseEmitIdx[sw_idx]++;
            sc = &swList[sw_idx].cases[c_idx];
            /* Emit: O stmt_count */
            emit1('O');
            emit1(sc->stmts);
            break;

        case SEMI:
            gettoken();
            emit1(';');
            break;

        case ASM:
            /* Get asm text and emit directly */
            text = cur.v.str;
            cur.v.str = NULL;
            gettoken();
            emitGlobalAsm(text);
            free(text);
            break;

        default:
            gripe(ER_E_UO);
            break;
        }
    }
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
