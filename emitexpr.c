/*
 * emitexpr.c - Expression emission for cc2
 *
 * Walks expression trees, emitting assembly code and freeing nodes.
 */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "cc2.h"
#include "emithelper.h"
#include "regcache.h"

/*
 * Emit expression tree, emit assembly, and free nodes
 *
 * Binary operators need special handling to emit accumulator move between
 * children:
 *   1. Emit left child (result in PRIMARY)
 *   2. Emit move instruction (PRIMARY to SECONDARY)
 *   3. Emit right child (result in PRIMARY)
 *   4. Emit call instruction (operates on SECONDARY and PRIMARY)
 */
static int exprCount = 0;
/*
 * Execute a single scheduled instruction from e->ins[]
 * Returns 1 if instruction was handled, 0 if not
 */
static int
execIns(struct expr *e, unsigned char ins)
{
    switch (ins) {
    case EO_NOP:
        return 1;

    /* Register moves */
    case EO_HL_BC:
        emit(S_BCHL);
        return 1;
    case EO_HL_DE:
    case EO_DE_HL:
        emit(S_EXDEHL);
        return 1;
    case EO_BC_HL:
        fdprintf(outFd, "\tld b, h\n\tld c, l\n");
        return 1;
    case EO_A_L:
        emit(S_AL);
        return 1;
    case EO_A_C:
        fdprintf(outFd, "\tld a, c\n");
        return 1;

    /* Word loads to HL */
    case EO_HL_IYW:
        loadWordIY(e->offset);
        return 1;
    case EO_HL_MEM:
        if (e->left && e->left->symbol) {
            fdprintf(outFd, "\tld hl, (%s)\n", stripVarPfx(e->left->symbol));
            clearHL();
        }
        return 1;
    case EO_HL_CONST:
        if (cacheFindWord(e) == 'H')
            return 1;  /* Already in HL */
        fdprintf(outFd, "\tld hl, %ld\n", e->value);
        cacheSetHL(e);  /* Cache constant for potential reuse */
        return 1;

    /* Byte loads to A */
    case EO_A_IY:
        loadByteIY(e->offset, 0);
        return 1;
    case EO_A_MEM:
        if (e->left && e->left->symbol) {
            fdprintf(outFd, "\tld a, (%s)\n", stripVarPfx(e->left->symbol));
            clearA();
        }
        return 1;
    case EO_A_CONST:
        if ((e->value & 0xff) == 0) {
            if (!fnAZero) {
                emit(S_XORA);
                fnAZero = 1;
            }
        } else {
            fdprintf(outFd, "\tld a, %ld\n", e->value & 0xff);
            fnAZero = 0;
        }
        clearA();
        return 1;
    case EO_A_BC_IND:
        fdprintf(outFd, "\tld a, (bc)\n");
        return 1;
    case EO_A_IX:
        emitIndexDrf('x', e->offset, 1, R_A, NULL);
        return 1;

    /* Word stores from HL */
    case EO_IYW_HL:
        storeWordIY(e->offset);
        return 1;

    /* Word stores from BC/DE to global (ED-prefixed) */
    case EO_MEM_BC:
        if (e->left && e->left->symbol) {
            fdprintf(outFd, "\tld (%s), bc\n", stripVarPfx(e->left->symbol));
        }
        return 1;
    case EO_MEM_DE:
        if (e->left && e->left->symbol) {
            fdprintf(outFd, "\tld (%s), de\n", stripVarPfx(e->left->symbol));
        }
        return 1;

    /* Byte stores from A */
    case EO_IY_A:
        storeByteIY(e->offset, 0);
        return 1;

    /* Arithmetic */
    case EO_ADD_HL_DE:
        emit(S_ADDHLDE);
        return 1;
    case EO_ADD_HL_BC:
        emit(S_ADDHLBC);
        return 1;
    case EO_SBC_HL_DE:
        emit(S_SBCHLDE);
        return 1;
    case EO_ADD_IX_DE:
        fdprintf(outFd, "\tadd ix, de\n");
        return 1;
    case EO_ADD_IX_BC:
        fdprintf(outFd, "\tadd ix, bc\n");
        return 1;
    case EO_IX_CONST:
        fdprintf(outFd, "\tld ix, %d\n", e->value);
        return 1;
    case EO_DE_IX:
        fdprintf(outFd, "\tpush ix\n\tpop de\n");
        return 1;
    case EO_INC_HL:
        emit(S_INCHL);
        return 1;
    case EO_DEC_HL:
        emit(S_DECHL);
        return 1;

    /* Type conversions */
    case EO_WIDEN:
        fdprintf(outFd, "\tld l, a\n\tld h, 0\n");
        return 1;
    case EO_SEXT:
        fdprintf(outFd, "\tld l, a\n\trla\n\tsbc a, a\n\tld h, a\n");
        return 1;

    /* Stack ops */
    case EO_PUSH_HL:
        emit(S_PUSHHL);
        return 1;
    case EO_POP_HL:
        emit(S_POPHL);
        return 1;
    case EO_PUSH_DE:
        emit(S_PUSHDE);
        return 1;
    case EO_POP_DE:
        emit(S_POPDE);
        return 1;

    default:
        return 0;  /* Not handled */
    }
}

/*
 * Check if expression has simple scheduled instructions we can execute
 * Returns 1 if fully handled, 0 if should use old emit path
 */
static int
trySched(struct expr *e)
{
    int i;

    /* Must have scheduled instructions */
    if (e->nins == 0) return 0;

    /* Handle simple DEREF of variable: (M $var) */
    if (e->op == 'M' && e->left && e->left->op == '$' && !e->right) {
        /* Simple variable load - execute scheduled instructions */
        for (i = 0; i < e->nins; i++) {
            if (!execIns(e, e->ins[i])) {
                return 0;
            }
        }
        /* Free the child $ node */
        freeExpr(e->left);
        e->left = NULL;
        return 1;
    }

    /* Handle constants */
    if (e->op == 'C') {
        for (i = 0; i < e->nins; i++) {
            if (!execIns(e, e->ins[i])) {
                return 0;
            }
        }
        return 1;
    }

    /* Handle other leaf nodes */
    if (e->left || e->right) return 0;

    /* Execute all scheduled instructions */
    for (i = 0; i < e->nins; i++) {
        if (!execIns(e, e->ins[i])) {
            return 0;  /* Instruction not handled */
        }
    }

    return 1;  /* Fully handled */
}

void emitExpr(struct expr *e)
{
    if (!e) return;
    exprCount++;
#ifdef DEBUG
    if (TRACE(T_EXPR)) {
        fdprintf(2, "emitExpr: %d calls, op=%c (0x%x) nins=%d\n", exprCount, e->op, e->op, e->nins);
    }
    if (exprCount > 100000) {
        fdprintf(2, "emitExpr: exceeded 100000 calls, op=%c\n", e->op);
        exit(1);
    }
#endif

    /* Try new scheduled emit for simple cases */
    if (trySched(e)) {
        freeNode(e);
        return;
    }

    /* Handle BC indirect load with caching - use opflags */
    if (e->op == 'M' && (e->opflags & OP_BCINDIR)) {
        emitBCIndir();
        freeNode(e);
        return;
    }
    /* Handle increment/decrement - Pattern 1 (e->symbol) or Pattern 2/3 (e->left) */
    else if ((e->op == AST_PREINC || e->op == AST_POSTINC || e->op == AST_PREDEC || e->op == AST_POSTDEC) &&
             (e->symbol || e->left)) {
        emitIncDec(e);
        return;
    }
    /* Handle ASSIGN - use op check */
    else if (e->op == '=') {
        /* Check for scheduled direct store (EO_MEM_BC, etc) */
        if (e->nins > 0 && (e->ins[0] == EO_MEM_BC || e->ins[0] == EO_MEM_DE)) {
            int i;
            for (i = 0; i < e->nins; i++)
                execIns(e, e->ins[i]);
            freeExpr(e->right);
            freeExpr(e->left);
            freeNode(e);
            return;
        }
#ifdef DEBUG
        if (TRACE(T_EXPR)) {
            fdprintf(2, "  calling emitAssign\n");
        }
#endif
        emitAssign(e);
#ifdef DEBUG
        if (TRACE(T_EXPR)) {
            fdprintf(2, "  emitAssign returned, about to return from emitExpr\n");
        }
#endif
        return;
    }
    /* Binary operators with accumulator management need special handling */
    else if (isBinopWAccum(e->op) && e->left && e->right) {
        emitBinop(e);
        return;
    }
    /* CALL operator - children handled by emitCall */
    else if (e->op == '@') {
        emitCall(e);
        return;
    }
    /* Handle ternary operator (? :) */
    else if (e->op == '?') {
        emitTernary(e);
        return;
    }
    /* Handle NARROW - truncate to smaller type */
    else if (e->op == 'N') {
        if (e->left && e->size == 1) {
            /* Narrowing to byte */
            if (e->left->op == 'C') {
                /* Constant - just load low byte directly */
                int val = e->left->value & 0xff;
                if (val == 0 && !fnAZero) {
                    emit(S_XORA);
                    fnAZero = 1;
                } else if (val != 0) {
                    fdprintf(outFd, "\tld a, %d\n", val);
                    fnAZero = 0;
                }
                freeExpr(e->left);
                clearA();
            } else {
                /* Non-constant - emit child then move L to A */
                emitExpr(e->left);
                emit(S_AL);
                clearA();
            }
        } else if (e->left) {
            /* Not narrowing to byte - just emit child */
            emitExpr(e->left);
        }
        freeNode(e);
        return;
    }
    /* Handle DEREF of register variable with caching - use opflags */
    else if (e->op == 'M' && (e->opflags & OP_REGVAR) &&
             e->left && e->left->op == '$') {
        emitRegVarDrf(e);
        return;
    }
    /* Handle DEREF of global with caching */
    else if (e->op == 'M' && (e->opflags & OP_GLOBAL) &&
             e->left && e->left->op == '$') {
        emitGlobDrf(e);
        return;
    }
    /* Handle DEREF of stack variable (IY-indexed) */
    else if (e->op == 'M' && (e->opflags & OP_IYMEM) &&
             e->left && e->left->op == '$') {
        emitStackDrf(e);
        return;
    }
    /* Handle DEREF of local member: (M (+ $var C)) with pre-computed offset */
    else if (e->op == 'M' && (e->opflags & OP_IYMEM) &&
             e->left && e->left->op == '+' && e->cached_var) {
        /* offset field has combined var offset + member offset */
        emitIndexDrf('y', e->offset, e->size, e->dest, NULL);
        freeExpr(e->left);
        freeNode(e);
        return;
    }
    /* Handle DEREF with indirect addressing (loc=LOC_INDIR) */
    else if (e->op == 'M' && e->loc == LOC_INDIR) {
        /* Emit address calculation first */
        emitExpr(e->left);
        /* Then load through HL */
        if (e->size == 1) {
            emit(S_AHL);
            clearA();
        } else if (e->size == 2) {
            emit(S_LDHLIND);
            clearHL();
        }
        freeNode(e);
        return;
    }
    /* Handle DEREF of IX-allocated pointer: (M (M $ptr)) or (M (+ (M $ptr) ofs)) */
    else if (e->op == 'M' && e->loc == LOC_IX) {
        emitIndexDrf('x', e->offset, e->size, e->dest, NULL);
        freeExpr(e->left);
        freeNode(e);
        return;
    }
    /* Handle symbol address - check if global or local */
    else if (e->op == '$' && e->symbol) {
        const char *sym_name = stripVarPfx(e->symbol);
        struct local_var *var = findVar(sym_name);

        if (!var) {
            /* Global symbol - load address */
            if (e->dest == R_DE) {
                fdprintf(outFd, "\tld de, %s\n", sym_name);
                fnDEValid = 1;
            } else {
                int cached = cacheFindWord(e);
                if (cached == 'H') {
                    /* HL already has this address - skip load */
                } else {
                    fdprintf(outFd, "\tld hl, %s\n", sym_name);
                    cacheSetHL(e);
                }
            }
        } else if (var->reg != REG_NO) {
            /* Register variable - load register value to HL
             * (the variable holds a pointer, we need its value as address) */
            if (var->reg == REG_BC)
                emit(S_BCHL);
            else if (var->reg == REG_IX)
                emit(S_IXHL);
            clearHL();
        } else {
            /* Stack variable - compute address (IY + offset) */
            int ofs = var->offset;
            emit(S_IYHL);
            if (ofs != 0) {
                fdprintf(outFd, "\tld de, %d\n\tadd hl, de\n", ofs);
            }
            clearHL();
        }
        free(e);
        return;
    }
    /* Handle constants with scheduler */
    else if (e->op == 'C' && e->loc == LOC_CONST) {
        if (e->size == 1) {
            if ((e->value & 0xff) == 0) {
                if (!fnAZero) {
                    emit(S_XORA);
                    fnAZero = 1;
                }
            } else if (cacheFindByte(e) == 'A') {
                /* A already has this value */
            } else {
                fdprintf(outFd, "\tld a, %ld\n", e->value & 0xff);
                fnAZero = 0;
                cacheSetA(e);
            }
        } else {
            fdprintf(outFd, "\tld hl, %ld\n", e->value);
            clearHL();
        }
        freeNode(e);
        return;
    }
    /* Handle specialized BYTE shift ops (LSHIFTEQ/RSHIFTEQ with register var) */
    else if ((e->op == '0' || e->op == '6') && e->size == 1 &&
             (e->opflags & OP_REGVAR) && e->cached_var) {
        struct local_var *var = e->cached_var;
        const char *rn = byteRegName(var->reg);
        const char *inst = (e->op == '0') ? "sla" : "srl";
        int count = e->value, i;
        if (var->reg == REG_BC) rn = "c";
        for (i = 0; i < count; i++)
            fdprintf(outFd, "\t%s %s\n", inst, rn);
        freeNode(e);
        return;
    }
    /* Handle specialized bit ops (OREQ=set, ANDEQ=res) */
    else if ((e->op == '1' || e->op == AST_ANDEQ) && !e->left && !e->right && e->cached_var &&
             e->value >= 0 && e->value <= 7) {
        /* Simple variable patterns - kids were freed, bitnum stored in e->value */
        struct local_var *var = e->cached_var;
        const char *inst = (e->op == '1') ? "set" : "res";
        int bitnum = e->value;
        if (e->opflags & OP_REGVAR) {
            const char *rn = byteRegName(var->reg);
            if (var->reg == REG_BC) rn = "c";
            if (rn)
                fdprintf(outFd, "\t%s %d, %s\n", inst, bitnum, rn);
        } else if (e->opflags & OP_IYMEM) {
            int ofs = varIYOfs(var);
            fdprintf(outFd, "\t%s %d, (iy %c %d)\n", inst, bitnum,
                     ofs >= 0 ? '+' : '-', ofs >= 0 ? ofs : -ofs);
        }
        freeNode(e);
        return;
    }
    /* Handle specialized bit ops with IX-indexed or (hl) addressing */
    else if ((e->op == '1' || e->op == AST_ANDEQ) && (e->opflags & OP_IXMEM) && !e->left) {
        const char *inst = (e->op == '1') ? "set" : "res";
        int bitnum = (e->value >> 8) & 0xff;
        int ofs = (char)(e->value & 0xff);
        fdprintf(outFd, "\t%s %d, (ix %c %d)\n", inst, bitnum,
                 ofs >= 0 ? '+' : '-', ofs >= 0 ? ofs : -ofs);
        freeNode(e);
        return;
    }
    else if ((e->op == '1' || e->op == AST_ANDEQ) && e->left && !e->right) {
        /* (hl) addressing - emit left for address, then bit op */
        const char *inst = (e->op == '1') ? "set" : "res";
        int bitnum = e->value;
        emitExpr(e->left);
        fdprintf(outFd, "\t%s %d, (hl)\n", inst, bitnum);
        freeNode(e);
        return;
    }
    /* Handle specialized OREQ/ANDEQ/XOREQ for byte register variables */
    else if ((e->op == '1' || e->op == AST_ANDEQ || e->op == 'X') && (e->opflags & OP_REGVAR) &&
             !e->left && e->right && e->cached_var) {
        struct local_var *var = e->cached_var;
        const char *rn = byteRegName(var->reg);
        const char *inst = (e->op == '1') ? "or" : (e->op == AST_ANDEQ) ? "and" : "xor";
        if (var->reg == REG_BC) rn = "c";
        emitExpr(e->right);
        fdprintf(outFd, "\t%s %s\n\tld %s, a\n", inst, rn, rn);
        freeNode(e);
        return;
    }
    /* Handle specialized PLUSEQ/SUBEQ for byte register variables */
    else if ((e->op == 'P' || e->op == AST_SUBEQ) && (e->opflags & OP_REGVAR) &&
             !e->left && e->right && e->cached_var) {
        struct local_var *var = e->cached_var;
        const char *rn = byteRegName(var->reg);
        if (var->reg == REG_BC) rn = "c";
        emitExpr(e->right);
        if (e->op == 'P')
            fdprintf(outFd, "\tadd a, %s\n\tld %s, a\n", rn, rn);
        else
            fdprintf(outFd, "\tld e, a\n\tld a, %s\n\tsub e\n\tld %s, a\n", rn, rn);
        freeNode(e);
        return;
    }
    /* Handle word LSHIFTEQ for BC register variable with constant shift */
    else if (e->op == '0' && e->size == 2 && (e->opflags & OP_REGVAR) &&
             e->cached_var && e->cached_var->reg == REG_BC &&
             e->right && e->right->op == 'C' &&
             e->right->value >= 1 && e->right->value <= 8) {
        int i, cnt = e->right->value;
        emit(S_BCHL);
        for (i = 0; i < cnt; i++)
            emit(S_ADDHLHL);
        emit(S_BCHLX);
        freeExpr(e->left);
        freeExpr(e->right);
        freeNode(e);
        return;
    }
    /* Handle word OREQ for BC register variable */
    else if (e->op == '1' && e->size == 2 && (e->opflags & OP_REGVAR) &&
             e->cached_var && e->cached_var->reg == REG_BC && e->right) {
        int rhs_byte = (e->right->op == 'W');  /* WIDEN means high byte is 0 */
        freeExpr(e->left);
        emitExpr(e->right);  /* Result in HL */
        emit(S_ALORCC);
        if (!rhs_byte)
            emit(S_AHORBBA);
        freeNode(e);
        return;
    }
    /* Handle word PLUSEQ for BC register variable */
    else if (e->op == 'P' && e->size == 2 && (e->opflags & OP_REGVAR) &&
             e->cached_var && e->cached_var->reg == REG_BC && e->right) {
        freeExpr(e->left);
        emitExpr(e->right);  /* Result in HL */
        emit(S_ADDHLBCBC);
        freeNode(e);
        return;
    }
    /* Handle WIDEN - zero extend byte to word */
    else if (e->op == 'W' && e->left) {
        emitExpr(e->left);
        /* Child puts byte result in A, zero-extend to HL */
        emit(S_WIDEN);
        clearHL();
        freeNode(e);
        return;
    }
    /* Handle long (4-byte) LSHIFTEQ on IY-indexed variable */
    else if (e->op == '0' && e->size == 4 && (e->opflags & OP_IYMEM) && e->cached_var) {
        int ofs = varIYOfs(e->cached_var);
        int count = e->right ? e->right->value : 0;
        freeExpr(e->left);
        freeExpr(e->right);
        /* Set up EA in DE, then shift */
        fdprintf(outFd, "\tld a, %d\n\tcall lea_iy\n", ofs);
        fdprintf(outFd, "\tld a, %d\n\tcall lshift32\n", count);
        freeNode(e);
        return;
    }
    /* Handle long (4-byte) OREQ on IY-indexed variable */
    else if (e->op == '1' && e->size == 4 && (e->opflags & OP_IYMEM) && e->cached_var) {
        int ofs = varIYOfs(e->cached_var);
        freeExpr(e->left);
        /* Emit RHS - result in HL (widened to HL:HL' with high word = 0) */
        emitExpr(e->right);
        emit(S_EXX0);  /* Clear high word */
        /* Set up EA in DE, then OR */
        fdprintf(outFd, "\tld a, %d\n\tcall lea_iy\n", ofs);
        emit(S_CALLLOR32);
        freeNode(e);
        return;
    }
    else {
        /* Normal postorder traversal for other operators */
        if (e->left) emitExpr(e->left);
        if (e->right) emitExpr(e->right);

        /* Emit deferred cleanup (for CALL stack cleanup after result used) */
        if (e->cleanup_block) {
            fdprintf(outFd, "%s", e->cleanup_block);
        }
    }

    /* Free this node (children already freed by recursive emit calls above) */
    xfree(e->cleanup_block);
    if (e->jump) freeJump(e->jump);
    free(e);
}

/*
 * New emit: Execute scheduled instructions
 * This is the "dumb" emit that just does what the scheduler told it to.
 */
void emit2Expr(struct expr *e)
{
    int i;

    if (!e) return;

    /* Process children first (post-order) */
    emit2Expr(e->left);
    emit2Expr(e->right);

    /* Execute scheduled instructions */
    for (i = 0; i < e->nins; i++) {
        switch (e->ins[i]) {
        case EO_NOP:
            break;

        /* Register-to-register moves */
        case EO_HL_BC:
            emit(S_BCHL);
            break;
        case EO_HL_DE:
        case EO_DE_HL:
            emit(S_EXDEHL);
            break;
        case EO_BC_HL:
            fdprintf(outFd, "\tld b, h\n\tld c, l\n");
            break;
        case EO_A_L:
            emit(S_AL);
            break;
        case EO_A_C:
            fdprintf(outFd, "\tld a, c\n");
            break;
        case EO_L_A:
            fdprintf(outFd, "\tld l, a\n\tld h, 0\n");
            break;

        /* Memory loads - word to HL */
        case EO_HL_IYW:
            loadWordIY(e->offset);
            break;
        case EO_HL_MEM:
            if (e->left && e->left->symbol) {
                const char *sym = stripVarPfx(e->left->symbol);
                fdprintf(outFd, "\tld hl, (%s)\n", sym);
            }
            break;
        case EO_HL_CONST:
            fdprintf(outFd, "\tld hl, %ld\n", e->value);
            break;

        /* Memory loads - byte to A */
        case EO_A_IY:
            loadByteIY(e->offset, 0);
            break;
        case EO_A_MEM:
            if (e->left && e->left->symbol) {
                const char *sym = stripVarPfx(e->left->symbol);
                fdprintf(outFd, "\tld a, (%s)\n", sym);
            }
            break;
        case EO_A_CONST:
            if ((e->value & 0xff) == 0) {
                emit(S_XORA);
            } else {
                fdprintf(outFd, "\tld a, %ld\n", e->value & 0xff);
            }
            break;
        case EO_A_BC_IND:
            fdprintf(outFd, "\tld a, (bc)\n");
            break;

        /* Memory stores - word from HL */
        case EO_IYW_HL:
            storeWordIY(e->offset);
            break;
        case EO_MEM_HL:
            if (e->left && e->left->left && e->left->left->symbol) {
                const char *sym = stripVarPfx(e->left->left->symbol);
                fdprintf(outFd, "\tld (%s), hl\n", sym);
            }
            break;
        case EO_MEM_BC:
            /* For (= $glob (M $bcvar)), e->left is $glob directly */
            if (e->left && e->left->symbol) {
                const char *sym = stripVarPfx(e->left->symbol);
                fdprintf(outFd, "\tld (%s), bc\n", sym);
            }
            break;
        case EO_MEM_DE:
            if (e->left && e->left->symbol) {
                const char *sym = stripVarPfx(e->left->symbol);
                fdprintf(outFd, "\tld (%s), de\n", sym);
            }
            break;

        /* Memory stores - byte from A */
        case EO_IY_A:
            storeByteIY(e->offset, 0);
            break;
        case EO_MEM_A:
            if (e->left && e->left->left && e->left->left->symbol) {
                const char *sym = stripVarPfx(e->left->left->symbol);
                fdprintf(outFd, "\tld (%s), a\n", sym);
            }
            break;

        /* Word arithmetic */
        case EO_ADD_HL_DE:
            emit(S_ADDHLDE);
            break;
        case EO_ADD_HL_BC:
            emit(S_ADDHLBC);
            break;
        case EO_SBC_HL_DE:
            emit(S_SBCHLDE);
            break;

        /* Byte arithmetic */
        case EO_ADD_A_N:
            if (e->right && e->right->op == 'C') {
                fdprintf(outFd, "\tadd a, %ld\n", e->right->value & 0xff);
            }
            break;
        case EO_SUB_A_N:
            if (e->right && e->right->op == 'C') {
                fdprintf(outFd, "\tsub %ld\n", e->right->value & 0xff);
            }
            break;
        case EO_AND_A_N:
            if (e->right && e->right->op == 'C') {
                fdprintf(outFd, "\tand %ld\n", e->right->value & 0xff);
            }
            break;
        case EO_OR_A_N:
            if (e->right && e->right->op == 'C') {
                fdprintf(outFd, "\tor %ld\n", e->right->value & 0xff);
            }
            break;
        case EO_XOR_A_N:
            if (e->right && e->right->op == 'C') {
                fdprintf(outFd, "\txor %ld\n", e->right->value & 0xff);
            }
            break;
        case EO_CP_N:
            if (e->right && e->right->op == 'C') {
                fdprintf(outFd, "\tcp %ld\n", e->right->value & 0xff);
            }
            break;

        /* Inc/Dec */
        case EO_INC_HL:
            emit(S_INCHL);
            break;
        case EO_DEC_HL:
            emit(S_DECHL);
            break;
        case EO_INC_A:
            fdprintf(outFd, "\tinc a\n");
            break;
        case EO_DEC_A:
            fdprintf(outFd, "\tdec a\n");
            break;

        /* Flag tests */
        case EO_OR_A:
            emit(S_ORA);
            break;
        case EO_AHORL:
            emit(S_AHORL);
            break;

        /* Stack operations */
        case EO_PUSH_HL:
            emit(S_PUSHHL);
            break;
        case EO_PUSH_DE:
            emit(S_PUSHDE);
            break;
        case EO_POP_HL:
            emit(S_POPHL);
            break;
        case EO_POP_DE:
            emit(S_POPDE);
            break;

        /* Type conversions */
        case EO_WIDEN:
            fdprintf(outFd, "\tld l, a\n\tld h, 0\n");
            break;
        case EO_SEXT:
            /* Sign extend A to HL: ld l,a; rla; sbc a,a; ld h,a */
            fdprintf(outFd, "\tld l, a\n\trla\n\tsbc a, a\n\tld h, a\n");
            break;

        /* Calls */
        case EO_CALL:
            if (e->left && e->left->symbol) {
                fdprintf(outFd, "\tcall %s\n", stripVarPfx(e->left->symbol));
            }
            break;

        default:
            /* Unknown instruction - skip */
            break;
        }
    }

    /* Free this node */
    xfree(e->cleanup_block);
    if (e->jump) freeJump(e->jump);
    free(e);
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
