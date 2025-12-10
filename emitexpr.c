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
#ifdef DEBUG
static int exprCount = 0;
#endif
/*
 * Execute a single scheduled instruction from e->ins[]
 * Returns 1 if instruction was handled, 0 if not
 */
static int
execIns(struct expr *e, unsigned char ins)
{
    const char *symbol = (e->left && e->left->symbol) ? e->left->symbol : e->symbol;
    unsigned char s = 0;

    switch (ins) {
    /* Simple emit cases - set s and break */
    case EO_HL_BC: s = S_BCHL; break;
    case EO_HL_DE:
    case EO_DE_HL: s = S_EXDEHL; break;
    case EO_BC_HL: s = S_BCHLX; break;
    case EO_A_L: s = S_AL; break;
    case EO_A_BC_IND: s = S_LDABC; break;
    case EO_ADD_HL_DE: s = S_ADDHLDE; break;
    case EO_ADD_HL_BC: s = S_ADDHLBC; break;
    case EO_SBC_HL_DE: s = S_SBCHLDE; break;
    case EO_ADD_IX_DE: s = S_ADDIXDE; break;
    case EO_ADD_IX_BC: s = S_ADDIXBC; break;
    case EO_DE_IX: s = S_IXDE; break;
    case EO_INC_HL: s = S_INCHL; break;
    case EO_DEC_HL: s = S_DECHL; break;
    case EO_WIDEN: s = S_WIDEN; break;
    case EO_SEXT: s = S_SEXT; break;
    case EO_PUSH_HL: s = S_PUSHHL; break;
    case EO_POP_HL: s = S_POPHL; break;
    case EO_PUSH_DE: s = S_PUSHDE; break;
    case EO_POP_DE: s = S_POPDE; break;

    /* Complex cases with additional logic */
    case EO_NOP:
        return 1;
    case EO_A_C:
        if (fnARegvar == REG_C)
            return 1;
        emit(S_LDAC);
        fnARegvar = REG_C;
        return 1;
    case EO_A_B:
        if (fnARegvar == REG_B)
            return 1;
        emit(S_LDAB);
        fnARegvar = REG_B;
        return 1;
    case EO_HL_IYW:
        loadWordIY(e->offset);
        return 1;
    case EO_HL_MEM:
        if (symbol) {
            fdprintf(outFd, "\tld hl, (%s)\n", symbol);
            clearHL();
        }
        return 1;
    case EO_HL_CONST:
        if (e->value == 0 && fnHLZero)
            return 1;
        fdprintf(outFd, "\tld hl, %ld\n", e->value);
        fnHLZero = (e->value == 0);
        return 1;
    case EO_HLHL_IYL:
        fdprintf(outFd, "\tld a, %d\n\tcall getLiy\n", (int)e->offset);
        clearHL();
        return 1;
    case EO_HLHL_IXL:
        fdprintf(outFd, "\tld a, %d\n\tcall getLix\n", (int)e->offset);
        clearHL();
        return 1;
    case EO_DE_CONST:
        if (symbol)
            fdprintf(outFd, "\tld de, %s\n", symbol);
        else
            fdprintf(outFd, "\tld de, %ld\n", e->value);
        return 1;
    case EO_A_IY:
        loadByteIY(e->offset, 0);
        return 1;
    case EO_A_MEM:
        if (symbol) {
            fdprintf(outFd, "\tld a, (%s)\n", symbol);
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
            emit1(F_LDA, e->value & 0xff);
            fnAZero = 0;
        }
        clearA();
        return 1;
    case EO_A_IX:
        emitIndexDrf('x', e->offset, 1, R_A, NULL);
        return 1;
    case EO_IYW_HL:
        storeWordIY(e->offset);
        return 1;
    case EO_MEM_BC:
        if (symbol)
            fdprintf(outFd, "\tld (%s), bc\n", symbol);
        return 1;
    case EO_MEM_DE:
        if (symbol)
            fdprintf(outFd, "\tld (%s), de\n", symbol);
        return 1;
    case EO_IY_A:
        storeByteIY(e->offset, 0);
        return 1;
    case EO_IX_CONST:
        fdprintf(outFd, "\tld ix, %d\n", e->value);
        return 1;

    default:
        return 0;
    }

    emit(s);
    return 1;
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

void
emitExpr(struct expr *e)
{
    struct expr *left;
    unsigned char op, opflags;
    const char *symbol;
    long value;

    if (!e) return;
    op = e->op;
    opflags = e->opflags;
    symbol = e->symbol;
    value = e->value;
    left = e->left;
#ifdef DEBUG
    exprCount++;
    if (TRACE(T_EXPR)) {
        fdprintf(2, "emitExpr: %d calls, op=%c (0x%x) nins=%d\n", exprCount, op, op, e->nins);
    }
    if (exprCount > 100000) {
        fdprintf(2, "emitExpr: exceeded 100000 calls, op=%c\n", op);
        exit(1);
    }
#endif

    /* Try new scheduled emit for simple cases */
    if (trySched(e)) {
        freeNode(e);
        return;
    }

    /* Handle BC indirect load with caching - use opflags */
    if (op == 'M' && (opflags & OP_BCINDIR)) {
        emitBCIndir();
        freeNode(e);
        return;
    }
    /* Handle increment/decrement - Pattern 1 (symbol) or Pattern 2/3 (left) */
    else if ((op == AST_PREINC || op == AST_POSTINC || op == AST_PREDEC || op == AST_POSTDEC) &&
             (symbol || left)) {
        emitIncDec(e);
        return;
    }
    /* Handle ASSIGN - use op check */
    else if (op == '=') {
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
    else if (isBinopWAccum(op) && left && e->right) {
        emitBinop(e);
        return;
    }
    /* CALL operator - children handled by emitCall */
    else if (op == '@') {
        emitCall(e);
        return;
    }
    /* Handle ternary operator (? :) */
    else if (op == '?') {
        emitTernary(e);
        return;
    }
    /* Handle NARROW - truncate to smaller type */
    else if (op == 'N') {
        if (left && e->size == 1) {
            /* Narrowing to byte */
            if (left->op == 'C') {
                /* Constant - just load low byte directly */
                int val = left->value & 0xff;
                if (val == 0 && !fnAZero) {
                    emit(S_XORA);
                    fnAZero = 1;
                } else if (val != 0) {
                    emit1(F_LDA, val);
                    fnAZero = 0;
                }
                freeExpr(left);
                clearA();
            } else {
                /* Non-constant - emit child then move L to A */
                emitExpr(left);
                emit(S_AL);
                clearA();
            }
        } else if (left) {
            /* Not narrowing to byte - just emit child */
            emitExpr(left);
        }
        freeNode(e);
        return;
    }
    /* Handle DEREF of register variable with caching - use opflags */
    else if (op == 'M' && (opflags & OP_REGVAR) &&
             left && left->op == '$') {
        emitRegVarDrf(e);
        return;
    }
    /* Handle DEREF of global with caching */
    else if (op == 'M' && (opflags & OP_GLOBAL) &&
             left && left->op == '$') {
        emitGlobDrf(e);
        return;
    }
    /* Handle DEREF of stack variable (IY-indexed) */
    else if (op == 'M' && (opflags & OP_IYMEM) &&
             left && left->op == '$') {
        emitStackDrf(e);
        return;
    }
    /* Handle DEREF of local member: (M (+ $var C)) with pre-computed offset */
    else if (op == 'M' && (opflags & OP_IYMEM) &&
             left && left->op == '+' && e->cached_var) {
        /* offset field has combined var offset + member offset */
        emitIndexDrf('y', e->offset, e->size, e->dest, NULL);
        freeExpr(left);
        freeNode(e);
        return;
    }
    /* Handle DEREF with indirect addressing (loc=LOC_INDIR) */
    else if (op == 'M' && e->loc == LOC_INDIR) {
        /* Emit address calculation first */
        emitExpr(left);
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
    else if (op == 'M' && e->loc == LOC_IX) {
        emitIndexDrf('x', e->offset, e->size, e->dest, NULL);
        freeExpr(left);
        freeNode(e);
        return;
    }
    /* Handle symbol address - check if global or local */
    else if (op == '$' && symbol) {
        const char *sym_name = symbol;
        struct local_var *var = findVar(sym_name);

        if (!var) {
            /* Global symbol - load address */
            if (e->dest == R_DE) {
                fdprintf(outFd, "\tld de, %s\n", sym_name);
                fnDEValid = 1;
            } else {
                fdprintf(outFd, "\tld hl, %s\n", sym_name);
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
    else if (op == 'C' && e->loc == LOC_CONST) {
        if (e->size == 1) {
            if ((value & 0xff) == 0) {
                if (!fnAZero) {
                    emit(S_XORA);
                    fnAZero = 1;
                }
            } else {
                emit1(F_LDA, value & 0xff);
                fnAZero = 0;
            }
        } else {
            if (value == 0 && fnHLZero) {
                /* HL already zero */
            } else {
                fdprintf(outFd, "\tld hl, %ld\n", value);
                fnHLZero = (value == 0);
                if (!fnHLZero) clearHL();
            }
        }
        freeNode(e);
        return;
    }
    /* Handle specialized BYTE shift ops (LSHIFTEQ/RSHIFTEQ with register var) */
    else if ((op == '0' || op == '6') && e->size == 1 &&
             (opflags & OP_REGVAR) && e->cached_var) {
        struct local_var *var = e->cached_var;
        const char *rn = byteRegName(var->reg);
        const char *inst = (op == '0') ? "sla" : "srl";
        int count = value, i;
        if (var->reg == REG_BC) rn = "c";
        for (i = 0; i < count; i++)
            emit2S(FS2_OP, inst, rn);
        freeNode(e);
        return;
    }
    /* Handle specialized bit ops (OREQ=set, ANDEQ=res) */
    else if ((op == '1' || op == AST_ANDEQ) && !left && !e->right && e->cached_var &&
             value >= 0 && value <= 7) {
        /* Simple variable patterns - kids were freed, bitnum stored in value */
        struct local_var *var = e->cached_var;
        const char *inst = (op == '1') ? "set" : "res";
        int bitnum = value;
        if (opflags & OP_REGVAR) {
            const char *rn = byteRegName(var->reg);
            if (var->reg == REG_BC) rn = "c";
            if (rn)
                fdprintf(outFd, "\t%s %d, %s\n", inst, bitnum, rn);
        } else if (opflags & OP_IYMEM) {
            int ofs = varIYOfs(var);
            fdprintf(outFd, "\t%s %d, (iy %c %d)\n", inst, bitnum,
                     ofs >= 0 ? '+' : '-', ofs >= 0 ? ofs : -ofs);
        }
        freeNode(e);
        return;
    }
    /* Handle specialized bit ops with IX-indexed or (hl) addressing */
    else if ((op == '1' || op == AST_ANDEQ) && (opflags & OP_IXMEM) && !left) {
        const char *inst = (op == '1') ? "set" : "res";
        int bitnum = (value >> 8) & 0xff;
        int ofs = (char)(value & 0xff);
        fdprintf(outFd, "\t%s %d, (ix %c %d)\n", inst, bitnum,
                 ofs >= 0 ? '+' : '-', ofs >= 0 ? ofs : -ofs);
        freeNode(e);
        return;
    }
    else if ((op == '1' || op == AST_ANDEQ) && left && !e->right) {
        /* (hl) addressing - emit left for address, then bit op */
        const char *inst = (op == '1') ? "set" : "res";
        int bitnum = value;
        emitExpr(left);
        fdprintf(outFd, "\t%s %d, (hl)\n", inst, bitnum);
        freeNode(e);
        return;
    }
    /* Handle specialized OREQ/ANDEQ/XOREQ for byte register variables */
    else if ((op == '1' || op == AST_ANDEQ || op == 'X') && (opflags & OP_REGVAR) &&
             !left && e->right && e->cached_var) {
        struct local_var *var = e->cached_var;
        const char *rn = byteRegName(var->reg);
        const char *inst = (op == '1') ? "or" : (op == AST_ANDEQ) ? "and" : "xor";
        if (var->reg == REG_BC) rn = "c";
        emitExpr(e->right);
        fdprintf(outFd, "\t%s %s\n\tld %s, a\n", inst, rn, rn);
        freeNode(e);
        return;
    }
    /* Handle specialized PLUSEQ/SUBEQ for byte register variables */
    else if ((op == 'P' || op == AST_SUBEQ) && (opflags & OP_REGVAR) &&
             !left && e->right && e->cached_var) {
        struct local_var *var = e->cached_var;
        const char *rn = byteRegName(var->reg);
        if (var->reg == REG_BC) rn = "c";
        emitExpr(e->right);
        if (op == 'P')
            fdprintf(outFd, "\tadd a, %s\n\tld %s, a\n", rn, rn);
        else
            fdprintf(outFd, "\tld e, a\n\tld a, %s\n\tsub e\n\tld %s, a\n", rn, rn);
        freeNode(e);
        return;
    }
    /* Handle word LSHIFTEQ for BC register variable with constant shift */
    else if (op == '0' && e->size == 2 && (opflags & OP_REGVAR) &&
             e->cached_var && e->cached_var->reg == REG_BC &&
             e->right && e->right->op == 'C' &&
             e->right->value >= 1 && e->right->value <= 8) {
        int i, cnt = e->right->value;
        emit(S_BCHL);
        for (i = 0; i < cnt; i++)
            emit(S_ADDHLHL);
        emit(S_BCHLX);
        freeExpr(left);
        freeExpr(e->right);
        freeNode(e);
        return;
    }
    /* Handle word OREQ for BC register variable */
    else if (op == '1' && e->size == 2 && (opflags & OP_REGVAR) &&
             e->cached_var && e->cached_var->reg == REG_BC && e->right) {
        /* Check if RHS is byte-sized (high byte is implicitly 0) */
        int rhs_byte = (e->right->size == 1);
        freeExpr(left);
        emitExpr(e->right);  /* Result in A (byte) or HL (word) */
        if (rhs_byte) {
            /* RHS in A, just OR with C */
            emit(S_ORCCA);
        } else {
            /* RHS in HL, OR both bytes */
            emit(S_ALORCC);
            emit(S_AHORBBA);
        }
        freeNode(e);
        return;
    }
    /* Handle word PLUSEQ for BC register variable */
    else if (op == 'P' && e->size == 2 && (opflags & OP_REGVAR) &&
             e->cached_var && e->cached_var->reg == REG_BC && e->right) {
        freeExpr(left);
        emitExpr(e->right);  /* Result in HL */
        emit(S_ADDHLBCBC);
        freeNode(e);
        return;
    }
    /* Handle WIDEN - zero extend byte to word */
    else if (op == 'W' && left) {
        emitExpr(left);
        /* Child puts byte result in A, zero-extend to HL */
        emit(S_WIDEN);
        clearHL();
        freeNode(e);
        return;
    }
    /* Handle long (4-byte) LSHIFTEQ on IY-indexed variable */
    else if (op == '0' && e->size == 4 && (opflags & OP_IYMEM) && e->cached_var) {
        int ofs = varIYOfs(e->cached_var);
        int count = e->right ? e->right->value : 0;
        freeExpr(left);
        freeExpr(e->right);
        /* Set up EA in DE, then shift */
        fdprintf(outFd, "\tld a, %d\n\tcall lea_iy\n", ofs);
        fdprintf(outFd, "\tld a, %d\n\tcall lshift32\n", count);
        freeNode(e);
        return;
    }
    /* Handle long (4-byte) OREQ on IY-indexed variable */
    else if (op == '1' && e->size == 4 && (opflags & OP_IYMEM) && e->cached_var) {
        int ofs = varIYOfs(e->cached_var);
        int rhs_size = e->right ? e->right->size : 2;
        freeExpr(left);
        /* Emit RHS - result in A (byte) or HL (word) */
        emitExpr(e->right);
        /* Widen to HLHL': byte->A needs ld l,a; ld h,0, word already in HL */
        if (rhs_size == 1) {
            emit(S_WIDEN);
        }
        emit(S_EXX0);  /* Clear high word */
        /* Set up EA in DE, then OR */
        fdprintf(outFd, "\tld a, %d\n\tcall lea_iy\n", ofs);
        emit(S_CALLLOR32);
        freeNode(e);
        return;
    }
    else {
        /* Normal postorder traversal for other operators */
        emitExpr(left);
        emitExpr(e->right);

        /* Emit deferred cleanup (for CALL stack cleanup after result used) */
        if (e->cleanup_block) {
            fdprintf(outFd, "%s", e->cleanup_block);
        }
    }

    /* Free this node (children already freed by recursive emit calls above) */
    xfree(e->cleanup_block);
    free(e);
}

/*
 * New emit: Execute scheduled instructions
 * This is the "dumb" emit that just does what the scheduler told it to.
 */
void
emit2Expr(struct expr *e)
{
    struct expr *left, *ll;
    int i;

    if (!e) return;
    left = e->left;
    ll = left ? left->left : NULL;

    /* Process children first (post-order) */
    emit2Expr(e->left);
    emit2Expr(e->right);

    /* Execute scheduled instructions */
    for (i = 0; i < e->nins; i++) {
        unsigned char s = 0, f = 0;
        switch (e->ins[i]) {
        /* Simple emit cases - set s and break */
        case EO_HL_BC: s = S_BCHL; break;
        case EO_HL_DE:
        case EO_DE_HL: s = S_EXDEHL; break;
        case EO_BC_HL: s = S_BCHLX; break;
        case EO_A_L: s = S_AL; break;
        case EO_A_C: s = S_LDAC; break;
        case EO_L_A: s = S_WIDEN; break;
        case EO_A_BC_IND: s = S_LDABC; break;
        case EO_ADD_HL_DE: s = S_ADDHLDE; break;
        case EO_ADD_HL_BC: s = S_ADDHLBC; break;
        case EO_SBC_HL_DE: s = S_SBCHLDE; break;
        case EO_INC_HL: s = S_INCHL; break;
        case EO_DEC_HL: s = S_DECHL; break;
        case EO_INC_A: s = S_INCA; break;
        case EO_DEC_A: s = S_DECA; break;
        case EO_OR_A: s = S_ORA; break;
        case EO_AHORL: s = S_AHORL; break;
        case EO_PUSH_HL: s = S_PUSHHL; break;
        case EO_PUSH_DE: s = S_PUSHDE; break;
        case EO_POP_HL: s = S_POPHL; break;
        case EO_POP_DE: s = S_POPDE; break;
        case EO_WIDEN: s = S_WIDEN; break;
        case EO_SEXT: s = S_SEXT; break;

        /* Byte arithmetic - set f and break */
        case EO_ADD_A_N: f = F_ADDA; break;
        case EO_SUB_A_N: f = F_SUB; break;
        case EO_AND_A_N: f = F_AND; break;
        case EO_OR_A_N: f = F_OR; break;
        case EO_XOR_A_N: f = F_XOR; break;
        case EO_CP_N: f = F_CP; break;

        /* Complex cases */
        case EO_NOP: continue;
        case EO_HL_IYW: loadWordIY(e->offset); continue;
        case EO_HL_MEM:
            if (left && left->symbol)
                fdprintf(outFd, "\tld hl, (%s)\n", left->symbol);
            continue;
        case EO_HL_CONST:
            fdprintf(outFd, "\tld hl, %ld\n", e->value);
            continue;
        case EO_A_IY: loadByteIY(e->offset, 0); continue;
        case EO_A_MEM:
            if (left && left->symbol)
                fdprintf(outFd, "\tld a, (%s)\n", left->symbol);
            continue;
        case EO_A_CONST:
            if ((e->value & 0xff) == 0)
                emit(S_XORA);
            else
                emit1(F_LDA, e->value & 0xff);
            continue;
        case EO_IYW_HL: storeWordIY(e->offset); continue;
        case EO_MEM_HL:
            if (ll && ll->symbol)
                fdprintf(outFd, "\tld (%s), hl\n", ll->symbol);
            continue;
        case EO_MEM_BC:
            if (left && left->symbol)
                fdprintf(outFd, "\tld (%s), bc\n", left->symbol);
            continue;
        case EO_MEM_DE:
            if (left && left->symbol)
                fdprintf(outFd, "\tld (%s), de\n", left->symbol);
            continue;
        case EO_IY_A: storeByteIY(e->offset, 0); continue;
        case EO_MEM_A:
            if (ll && ll->symbol)
                fdprintf(outFd, "\tld (%s), a\n", ll->symbol);
            continue;
        case EO_CALL:
            if (left && left->symbol)
                fdprintf(outFd, "\tcall %s\n", left->symbol);
            continue;
        default: continue;
        }
        if (s) emit(s);
        else if (f && e->right && e->right->op == 'C')
            emit1(f, e->right->value & 0xff);
    }

    /* Free this node */
    xfree(e->cleanup_block);
    free(e);
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
