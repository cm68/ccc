#include "cgen.h"

/*
 * File - cgen1.c
 */

/*********************************************************
 * genExprCode - Main code generation entry point for expressions
 *
 * Generates code for an expression tree:
 *   1. Selects code patterns via matchEmitPat
 *   2. Emits code via emitExprTree
 *   3. Handles function prologue for COLON_U nodes
 *   4. Frees the expression tree
 *
 * Fatal error if expression is too complex for available registers.
 *********************************************************/
void genExprCode(register node_t *expr) {
    int usedRegs;

    expr->nPat = 0;

    if (matchEmitPat(expr, 0x48, availRegs, 0, &usedRegs) <= 0)
        fatalErr("Expression too complicated");

    emitExprTree(expr);
    if (expr->op == COLON_U) {
        if (expr->tFlags & T_FUNC) {
            prFrameHead(funcSymbol[lvlidx]->offset); /* emit_call_ncsv */
            saveRegVars();
        }
        if (expr->info.np[0]->op == IDOP)
            expr->info.np[0]->info.mp[0]->sflags |= B_SLOC_EMITTED;
    }
    freeExprTree(expr);
}

/*********************************************************
 * signExtend - Sign-extend value based on node type size
 *
 * If node size < 4 bytes, extends sign bit to full long.
 *********************************************************/
long signExtend(node_t *node, long value) {
    char bitWidth;

    if ((bitWidth = nodesize(node) * 8) >= 32)
        return value;
    if (value & (1L << (bitWidth - 1)))
        value |= ~((1L << bitWidth) - 1L);
    return value;
}

/*********************************************************
 * prSignedVal - Print sign-extended value for node
 *********************************************************/
void prSignedVal(node_t *node, long value) {

    printf("%ld", signExtend(node, value));
}
/* end of cgen1.c */
