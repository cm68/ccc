/*
 * statement parsing
 */

#include "cc1.h"

/* Switch statement table tracking (phase 1) - dynamically allocated */
struct swtab *swList = 0;           /* dynamically allocated switch array */
unsigned char swCount = 0;          /* number of switches in function */
unsigned char swCapacity = 0;       /* allocated size of swList */
unsigned char swStack[MAX_SWDEPTH]; /* nesting stack (indices into swList) */
unsigned char swStmtDepth[MAX_SWDEPTH]; /* statement() depth at which each switch started */
unsigned char swDepth = 0;          /* nesting depth */
static unsigned char stmtNest = 0;  /* current statement() nesting depth */

/* Phase 2 switch emission tracking */
unsigned char swEmitIdx = 0;                /* next switch to emit */
unsigned char swEmitStack[MAX_SWDEPTH];     /* stack of switch indices */
unsigned char swEmitDepth = 0;              /* emit stack depth */

/* If/else tracking: bitmap of has_else flags, one bit per if statement */
#define MAX_IFS 256
static unsigned char ifHasElse[MAX_IFS / 8]; /* bit N set: if #N has else */
static unsigned short ifCount = 0;          /* phase 1: count of if statements */
static unsigned short ifEmitIdx = 0;        /* phase 2: next if to emit */

void resetSwitch(void) {
    int i;
    /* Free all allocated switch case arrays */
    for (i = 0; i < swCount; i++) {
        if (swList[i].cases)
            free(swList[i].cases);
    }
    /* Free the switch list itself */
    if (swList) {
        free(swList);
        swList = 0;
    }
    swCount = 0;
    swCapacity = 0;
    swDepth = 0;
    swEmitIdx = 0;
    swEmitDepth = 0;
    ifCount = 0;
    ifEmitIdx = 0;
}

void pushSwitch(void) {
    unsigned char idx;
    struct swtab *sw;

    if (swDepth >= MAX_SWDEPTH)
        return;  /* nesting too deep */

    /* Grow swList if needed */
    if (swCount >= swCapacity) {
        unsigned char newcap = swCapacity ? swCapacity * 2 : 8;
        struct swtab *newlist = realloc(swList, newcap * sizeof(struct swtab));
        if (!newlist)
            fatal(ER_NOMEM);
        swList = newlist;
        swCapacity = newcap;
    }

    idx = swCount++;
    sw = &swList[idx];
    /* Allocate case array for this switch */
    sw->cases = (struct swcase *)galloc(SW_INIT_CASES * sizeof(struct swcase));
    sw->count = 0;
    sw->capacity = SW_INIT_CASES;
    sw->num = idx;
    sw->base_stmts = 0;
    sw->emitIdx = 0;
    swStmtDepth[swDepth] = stmtNest;  /* record statement() depth at switch start */
    swStack[swDepth++] = idx;
}

/* Check if we're in the innermost switch's body statement() (not nested deeper) */
static int atSwBodyStmt(void) {
    if (swDepth == 0) return 0;
    /* Switch body is one level deeper than where switch started */
#ifdef DEBUG
    fdprintf(2, "atSwBodyStmt: stmtNest=%d swStmtDepth=%d result=%d\n",
             stmtNest, swStmtDepth[swDepth - 1],
             stmtNest == swStmtDepth[swDepth - 1] + 1);
#endif
    return stmtNest == swStmtDepth[swDepth - 1] + 1;
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
#ifdef DEBUG
            fdprintf(2, "finishCase: sw[%d].cases[%d].stmts = %d - %d = %d\n",
                     idx, sw->count - 1, stmt_cnt, sw->base_stmts,
                     sw->cases[sw->count - 1].stmts);
#endif
        }
        sw->base_stmts = stmt_cnt;
    }
}

void addCase(long value, unsigned char stmt_cnt) {
    unsigned char idx;
    struct swtab *sw;

    if (swDepth == 0)
        return;

    idx = swStack[swDepth - 1];
    sw = &swList[idx];

    /* Grow cases array if needed */
    if (sw->count >= sw->capacity) {
        unsigned char newcap = sw->capacity * 2;
        struct swcase *newcases = realloc(sw->cases, newcap * sizeof(struct swcase));
        if (!newcases)
            fatal(ER_NOMEM);
        sw->cases = newcases;
        sw->capacity = newcap;
    }

    /* Finalize previous case if any */
    if (sw->count > 0) {
        sw->cases[sw->count - 1].stmts = stmt_cnt - sw->base_stmts;
#ifdef DEBUG
        fdprintf(2, "addCase: finalize sw[%d].cases[%d].stmts = %d - %d = %d\n",
                 idx, sw->count - 1, stmt_cnt, sw->base_stmts,
                 sw->cases[sw->count - 1].stmts);
#endif
    }
    sw->base_stmts = stmt_cnt;
    /* Add new case */
    sw->cases[sw->count].value = value;
    sw->cases[sw->count].is_default = 0;
    sw->cases[sw->count].stmts = 0;  /* will be set by next case or popSwitch */
#ifdef DEBUG
    fdprintf(2, "addCase: add sw[%d].cases[%d] val=%ld base=%d\n",
             idx, sw->count, value, sw->base_stmts);
#endif
    sw->count++;
}

void addDefault(unsigned char stmt_cnt) {
    unsigned char idx;
    struct swtab *sw;

    if (swDepth == 0)
        return;

    idx = swStack[swDepth - 1];
    sw = &swList[idx];

    /* Grow cases array if needed */
    if (sw->count >= sw->capacity) {
        unsigned char newcap = sw->capacity * 2;
        struct swcase *newcases = realloc(sw->cases, newcap * sizeof(struct swcase));
        if (!newcases)
            fatal(ER_NOMEM);
        sw->cases = newcases;
        sw->capacity = newcap;
    }

    /* Finalize previous case if any */
    if (sw->count > 0) {
        sw->cases[sw->count - 1].stmts = stmt_cnt - sw->base_stmts;
#ifdef DEBUG
        fdprintf(2, "addDefault: finalize sw[%d].cases[%d].stmts = %d - %d = %d\n",
                 idx, sw->count - 1, stmt_cnt, sw->base_stmts,
                 sw->cases[sw->count - 1].stmts);
#endif
    }
    sw->base_stmts = stmt_cnt;
    /* Add default */
    sw->cases[sw->count].value = 0;
    sw->cases[sw->count].is_default = 1;
    sw->cases[sw->count].stmts = 0;  /* will be set by next case or popSwitch */
#ifdef DEBUG
    fdprintf(2, "addDefault: add sw[%d].cases[%d] base=%d\n",
             idx, sw->count, sw->base_stmts);
#endif
    sw->count++;
}


/* Function body statement count stack (separate from case counts) */
static unsigned char funcCnts[32];   /* stmt counts for functions */
static unsigned char funcCntTop = 0;    /* write pointer (phase 1) */
static unsigned char funcCntIdx = 0;    /* read pointer (phase 2) */

/*
 * Block statement counts - indexed by block ENTRY order (DFS)
 * Phase 1: assign entry ID when block starts, store count at ID when block ends
 * Phase 2: read count at current entry ID (deterministic DFS order matches)
 */
#define MAX_BLKCNTS 256
static unsigned char blkCnts[MAX_BLKCNTS];
static unsigned short blkCntTop = 0;     /* next entry ID to assign (phase 1) */
static unsigned short blkCntIdx = 0;     /* current read index (phase 2) */

/* Stack of entry IDs for active blocks (to know where to store count on exit) */
#define MAX_BLK_DEPTH 32
static unsigned short blkIdStack[MAX_BLK_DEPTH];
static unsigned char blkIdSp = 0;  /* stack pointer */

/* Called when entering a nested block in phase 1 */
void enterBlkCnt(void) {
    if (blkIdSp < MAX_BLK_DEPTH && blkCntTop < MAX_BLKCNTS) {
#ifdef DEBUG
        if (VERBOSE(V_PHASE1))
            fdprintf(2, "enterBlkCnt[%d] sp=%d\n", blkCntTop, blkIdSp);
#endif
        blkIdStack[blkIdSp++] = blkCntTop++;  /* assign entry ID, push to stack */
    }
}

/* Called when exiting a nested block in phase 1 - store count at entry ID */
void storeBlkCnt(unsigned char n) {
    if (blkIdSp > 0) {
        unsigned short id = blkIdStack[--blkIdSp];
#ifdef DEBUG
        if (VERBOSE(V_PHASE1))
            fdprintf(2, "storeBlkCnt[%d] = %d\n", id, n);
#endif
        blkCnts[id] = n;
    }
}

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

/* Legacy names for compatibility - now use entry-order indexing */
void pushBlkCnt(unsigned char n) {
    storeBlkCnt(n);  /* store at entry ID from stack */
}

unsigned char popBlkCnt(void) {
    unsigned char r = 0;
    if (blkCntIdx < blkCntTop)
        r = blkCnts[blkCntIdx++];
#ifdef DEBUG
    if (VERBOSE(V_PHASE2))
        fdprintf(2, "popBlkCnt[%d] = %d\n", blkCntIdx-1, r);
#endif
    return r;
}

/* Prepare block counts for phase 2 - just reset read index */
void flipBlkCnts(void) {
    blkCntIdx = 0;
}

void resetBlkCnts(void) {
    blkCntTop = 0;
    blkCntIdx = 0;
    blkIdSp = 0;
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

/* Reset read pointer for phase 2 (preserves pushed values) */
void
resetCountIdx(void)
{
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
	/* In phase 1, register block entry for nested blocks */
	if (phase == 1 && lexlevel > 2)
		enterBlkCnt();
	/* In phase 2, emit block header since we consumed BEGIN */
	if (emitHdr && phase == 2) {
		unsigned char cnt = popBlkCnt();
		emit1(AST_BLOCK);
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
		if (n->is_tag || n->kind == ktdef || n->kind == kfdef)
			continue;

		/* Capture this variable (shallow copy) */
		if (n->kind == kvar || n->kind == klocal || n->kind == kfunarg) {
			copy = (struct name *)galloc(sizeof(struct name));
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
    char *text;                /* Text pointer for ASM */
    unsigned char cnt;         /* Shared count (body_cnt, case_cnt, etc) */
    unsigned char hasElse;     /* If statement has else branch */
    unsigned char sw_idx, c_idx; /* Switch/case indices */
    struct swcase *sc;         /* Switch case pointer */
    struct expr *e1;           /* Shared expression pointer */

    stmtNest++;  /* Track statement() nesting depth */

    while (block) {
#ifdef DEBUG
        if ((VERBOSE(V_PHASE1) && phase == 1) || (VERBOSE(V_PHASE2) && phase == 2))
            fdprintf(2, "stmt() P%d lev=%d cur.type=%d (%c)\n",
                     phase, lexlevel, cur.type, cur.type > 31 && cur.type < 127 ? cur.type : '?');
#endif

        /*
         * Phase 1: Skip statements, only track scope and declarations
         * This discovers local variables without building trees.
         * Also counts statements for streaming emission in phase 2.
         */
        if (phase == 1) {
#ifdef DEBUG
            if (VERBOSE(V_PHASE1) && cur.type == SWITCH)
                fdprintf(2, "P1 SWITCH at lexlevel=%d\n", lexlevel);
#endif
            switch (cur.type) {
            case END:
            case E_O_F:
                /* Push statement count for blocks */
#ifdef DEBUG
                if (VERBOSE(V_PHASE1))
                    fdprintf(2, "P1 END: lev=%d cnt=%d sw=%d\n",
                             lexlevel, stmt_count, swDepth);
#endif
                /*
                 * Store stmt_count for the innermost switch body.
                 * Only do this when we're at the switch body level (stmtNest),
                 * not when nested blocks inside cases end.
                 */
                if (atSwBodyStmt())
                    swList[swStack[swDepth - 1]].final_cnt = stmt_count;
                /* Function body uses separate mechanism */
                if (lexlevel == 2 && swDepth == 0)
                    pushFuncCnt(stmt_count);
                /* Nested blocks (lexlevel > 2) store to block counts */
                /* Note: this can happen while inside a switch (swDepth > 0) */
                if (lexlevel > 2 && !atSwBodyStmt())
                    pushBlkCnt(stmt_count);
                block = 0;
                break;
            case BEGIN:
                gettoken();
                pushScope(blockname());
                /* Register block entry for nested blocks */
                if (lexlevel > 2)
                    enterBlkCnt();
                statement();  /* recurse for nested block */
                popScope();
                expect(END, ER_S_CC);
                stmt_count++;  /* BEGIN counts as one statement */
                break;
            /* Declarations - process for symbol table but don't count */
            case INT: case CHAR: case SHORT: case LONG:
            case FLOAT: case DOUBLE: case VOID:
            case STRUCT: case UNION:
            case UNSIGNED:
            case STATIC: case REGISTER: case AUTO:
            case EXTERN: case TYPEDEF:
                declaration();
                break;
            case SYM:
                /* Check typedef name for declaration */
                {
                    struct name *pt = findName(cur.v.name, 0);
                    if (pt && pt->kind == ktdef) {
                        declaration();
                        break;
                    }
                }
                /* Fall through to expression statement */
            case NUMBER: case STRING: case LPAR:
            case STAR: case INCR: case DECR:
                parseExpr(PRI_ALL);
                expect(SEMI, ER_S_SN);
                stmt_count++;
                break;
            case LABEL:
                gettoken();
                stmt_count++;
                break;
            case IF:
            handle_if: {
                unsigned short thisIf = ifCount++;
                if (thisIf >= MAX_IFS)
                    fatal(ER_S_IF);
                gettoken();
                expect(LPAR, ER_S_NP);
                parseExpr(PRI_ALL);
                expect(RPAR, ER_S_NP);
                parseBlock();
                if (cur.type == ELSE) {
                    ifHasElse[thisIf >> 3] |= 1 << (thisIf & 7);
                    gettoken();
                    if (cur.type == IF)
                        goto handle_if;  /* else if */
                    parseBlock();
                } else {
                    ifHasElse[thisIf >> 3] &= ~(1 << (thisIf & 7));
                }
                stmt_count++;
                break;
            }
            /* WHILE/DO/FOR handled by cpp loop lowering */
            case SWITCH: {
                unsigned char idx;
#ifdef DEBUG
                fdprintf(2, "P1 SWITCH: cur=%d line=%d\n", cur.type, lineno);
#endif
                gettoken();
                expect(LPAR, ER_S_NP);
                parseExpr(PRI_ALL);
                expect(RPAR, ER_S_NP);
                expect(BEGIN, ER_S_SB);
                pushSwitch();  /* start new switch table */
                statement();  /* switch body - adds cases to table */
                /* Finalize last case using stmt_count stored by END handler */
                idx = swStack[swDepth - 1];
                finishCase(swList[idx].final_cnt);
                /* Push case count for phase 2 before popping */
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
            /* Emit block header: AST_BLOCK 0 stmt_count */
            emit1(AST_BLOCK);
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
            hasElse = (ifHasElse[ifEmitIdx >> 3] >> (ifEmitIdx & 7)) & 1;
            ifEmitIdx++;
            gettoken();
            expect(LPAR, ER_S_NP);
            e1 = parseExpr(PRI_ALL);
            expect(RPAR, ER_S_NP);
            /* Emit: IF nlabels cond then has_else [else] */
            emit1(IF);
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
            /* Emit: RETURN has_value [expr] */
            emit1(RETURN);
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

        case LABEL:
            /* Label (from cpp) */
            emit1(LABEL);
            emitS(cur.v.name);
            gettoken();
            break;

        case SYM:
            /* Check if it's a typedef name used in a declaration */
            {
                struct name *poss_typedef = findName(cur.v.name, 0);
                if (poss_typedef && poss_typedef->kind == ktdef) {
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
            /* Emit expression statement directly (no EXPR wrapper) */
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
            swList[sw_idx].emitIdx = 0;
            /* Emit switch header: SWITCH has_label case_count expr */
            /* has_label=0 since cpp handles break lowering */
            emit1(SWITCH);
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
            c_idx = swList[sw_idx].emitIdx++;
            sc = &swList[sw_idx].cases[c_idx];
#ifdef DEBUG
            fdprintf(2, "P2 CASE: sw_idx=%d c_idx=%d stmts=%d\n",
                     sw_idx, c_idx, sc->stmts);
#endif
            /* Emit: CASE stmt_count value_expr */
            emit1(CASE);
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
            /* Copy label before gettoken overwrites cur.v.name */
            {
                char lblbuf[16];
                strncpy(lblbuf, cur.v.name, 15);
                lblbuf[15] = 0;
                gettoken();
                expect(SEMI, ER_S_SN);
                /* Emit: GOTO label */
                emit1(GOTO);
                emitS(lblbuf);
            }
            break;

        case DEFAULT:
            gettoken();
            expect(COLON, ER_S_NL);
            /* Get current switch and case */
            sw_idx = swEmitStack[swEmitDepth - 1];
            c_idx = swList[sw_idx].emitIdx++;
            sc = &swList[sw_idx].cases[c_idx];
#ifdef DEBUG
            fdprintf(2, "P2 DEFAULT: sw_idx=%d c_idx=%d stmts=%d\n",
                     sw_idx, c_idx, sc->stmts);
#endif
            /* Emit: DEFAULT stmt_count */
            emit1(DEFAULT);
            emit1(sc->stmts);
            break;

        case SEMI:
            gettoken();
            emit1(SEMI);
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
#ifdef DEBUG
            fdprintf(2, "bad op: %d\n", cur.type);
#endif
            gripe(ER_E_UO);
            break;
        }
    }

    stmtNest--;  /* Restore statement() nesting depth */
}

/*
 * Generate a unique name for a block scope (dummy for debugging)
 */
static char*
blockname()
{
	return "blk";
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
