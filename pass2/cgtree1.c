#include "cgen.h"

/*
 * File - tree1.c Created 09.03.2019 Last Modified 17.06.2020
 */
/*
 * emitExpr - Emit code for an expression statement
 *
 * Optimizes the expression tree, handles special cases
 * (constant conditionals, increment/decrement operators),
 * then emits code via genExprCode. Deferred side-effect
 * expressions (from INCR/DECR conversion) are emitted after.
 */
void emitExpr(register node_t *node) {
    uint16_t idx;

    node = optimizeExpr(node);
    if (node->op == DOLLAR && node->info.np[0]->op == CONST) {
        prWarning("constant conditional branch");
        if (node->info.np[0]->info.l != 0) {
            node->op = DOLLAR_U;
            freeNode(node->info.np[0]);
            node->info.np[0] = node->info.np[1];
        } else {
            freeExprTree(node);
            return;
        }
    }
    deferCnt = 0;
    if (node->op == INCR)
        node->op = ASADD;
    else if (node->op == DECR)
        node->op = ASSUB;

    node = deferPostInc(node);
    if (deferCnt != 0)
        node = optimizeExpr(node);
    genExprCode(node);
    for (idx = 0; idx < deferCnt; idx++)
        genExprCode(deferList[idx]);
}

/*
 * constFold - Fold constant expressions at compile time
 *
 * Recursively evaluates constant sub-expressions and
 * reduces them to single CONST nodes where possible.
 */
node_t *constFold(register node_t *node) {
    node_t *childNode;
    int opCount;

    if ((opCount = dopetab[node->op] & DOPE_OPCOUNT))
        node->info.np[0] = constFold(node->info.np[0]);
    if (opCount == DOPE_BINARY)
        node->info.np[1] = constFold(node->info.np[1]);
    if (node->op == CONV && node->info.np[0]->op == FCONST && getTypeClass(node) == 3) {
        childNode     = node->info.np[0];
        childNode->pm = node->pm;
        freeNode(node->info.np[1]);
        freeNode(node);
        return childNode;
    }
    if (opCount == 0 || node->info.np[0]->op != CONST)
        return node;

    if (node->op == CONV)
        return castConst(node);

    if (opCount == 8 && node->info.np[1]->op != CONST)
        return node;

    if (getTypeClass(node) == 2) {
        switch (node->op) {
        case LT:
        case LEQ:
        case GT:
        case GEQ:
            warningMsg = "constant relational expression";
            return node;
        case MOD:
        case DIV:
        case RSHIFT:
            unsignedOp(&node->info.np[0]->info.ul, node->info.np[1]->info.l, node->op);
        finish:
            if (opCount == 8)
                freeExprTree(node->info.np[1]);
            setNodeType(node, node->info.np[0]);
            freeNode(node);
            return node->info.np[0];
        }
    }
    switch (node->op) { /* m10: */
    case NEQL:
    case LT:
    case LEQ:
    case EQL:
    case GT:
    case GEQ:
        warningMsg = "constant relational expression";
        return node;
    case BAND:
    case LAND:
    case SUB:
    case BXOR:
    case BOR:
    case LOR:
    case ADD:
    case MUL:
    case DIV:
    case MOD:
    case RSHIFT:
    case LSHIFT:
        signedOp(&node->info.np[0]->info.l, node->info.np[1]->info.l, node->op);
        goto finish;
    case MINUS_U:
        node->info.np[0]->info.l = -node->info.np[0]->info.l;
        goto finish;
    case NOT:
        node->info.np[0]->info.l = node->info.np[0]->info.l == 0;
        goto finish;
    case BNOT:
        node->info.np[0]->info.l = ~node->info.np[0]->info.l;
        goto finish;
    default:
        return node;
    }
}

/*
 * mkConstNode - Create a CONST node with given value
 *
 * Allocates and initializes a new constant expression node
 * with type 'long' and the specified value.
 */
node_t *mkConstNode(long number) {
    register node_t *node;
    node         = allocNode();
    node->op   = CONST;
    node->pm     = typeLong;
    node->info.l = number;
    return node;
}

/* end of file tree1.c */

/* vim: tabstop=4 shiftwidth=4 noexpandtab: */
