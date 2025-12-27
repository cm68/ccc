/*
 * File - local1s.c
 */
#include "cgen.h"

/*
 * localOptimize - Local algebraic optimization pass
 *
 * Performs local (peephole-style) optimizations on expression trees:
 *   - Operand reordering for commutative ops (complex operand first)
 *   - Associativity reorganization for better code gen
 *   - Algebraic identities: x+0=x, x*1=x, x&~0=x, x|0=x, etc.
 *   - Strength reduction: mul/div/mod by power of 2 → shifts/masks
 *   - Comparison simplification for unsigned types
 *   - Type narrowing through CONV nodes
 *   - Address/dereference cancellation (&* and *&)
 *
 * Called iteratively by optimizeExpr until no changes made.
 */
node_t *localOptimize(register node_t *node) {
    node_t *tempNode;
    node_t *convNode;
    int opCount;
    uint32_t bitMask;

    opCount = dopetab[node->op] & DOPE_OPCOUNT;
    if (opCount != 0)
        node->info.np[0] = localOptimize(node->info.np[0]);

    if (opCount == DOPE_BINARY)
        node->info.np[1] = localOptimize(node->info.np[1]);

    if (dopetab[node->op] & DOPE_COMMUTE) {
        if (addrLevel(node->info.np[0]) < addrLevel(node->info.np[1])) {
            treeChanged      = true;
            tempNode            = node->info.np[0];
            node->info.np[0] = node->info.np[1];
            node->info.np[1] = tempNode;
        }
        if ((dopetab[node->op] & DOPE_ASSOC) && node->op == node->info.np[0]->op) {
            if (isAddrable(node->info.np[1]) && isAddrable((tempNode = node->info.np[0])->info.np[1])) {
                treeChanged       = true;
                node->info.np[0]  = tempNode->info.np[1];
                tempNode->info.np[1] = node;
                node              = tempNode;
            } else if (addrLevel(node->info.np[0]->info.np[1]) < addrLevel(node->info.np[1])) {
                treeChanged                  = true;
                tempNode                        = node->info.np[1];
                node->info.np[1]             = node->info.np[0]->info.np[1];
                node->info.np[0]->info.np[1] = tempNode;
            }
        }
    }

    if ((dopetab[node->op] & DOPE_OPCLASS) == DOPE_BITWISE && node->info.np[0]->op == CONV &&
        nodesize(node->info.np[0]->info.np[0]) < nodesize(node) &&
        constFits(node->info.np[1], node->info.np[0]->info.np[0])) {

        convNode                   = node->info.np[0];
        node->info.np[0]        = convNode->info.np[0];
        node->pm                = node->info.np[0]->pm;
        node->tFlags             = node->info.np[0]->tFlags;
        node->info.np[1]->pm    = node->pm;
        node->info.np[1]->tFlags = node->tFlags;
        convNode->info.np[0]       = node;
        node                    = convNode;
    }

    if ((dopetab[node->op] & (DOPE_COMMUTE | DOPE_LOGICAL)) == (DOPE_COMMUTE | DOPE_LOGICAL) && (tempNode = node->info.np[0])->op == CONV &&
        nodesize(tempNode->info.np[0]) != 0 && nodesize(tempNode->info.np[0]) < nodesize(tempNode)) {
        if (node->info.np[1]->op == CONST &&
            (getTypeClass(tempNode) == 1 || getTypeClass(tempNode->info.np[0]) == 2)) {
            if (constFitsType(node->info.np[0], node->info.np[1])) {
                freeNode(node->info.np[0]);
                freeNode(node->info.np[0]->info.np[1]);
                node->info.np[0]        = node->info.np[0]->info.np[0];
                node->info.np[1]->pm    = node->info.np[0]->pm;
                node->info.np[1]->tFlags = node->info.np[0]->tFlags;
            } else
                warningMsg = "mismatched comparision"; /* m8: */
        } else if (node->info.np[1]->op == CONV &&
                   sameType(node->info.np[0]->info.np[0], node->info.np[1]->info.np[0])) {
            tempNode            = node->info.np[0];
            node->info.np[0] = tempNode->info.np[0];
            freeNode(tempNode->info.np[1]);
            freeNode(tempNode);
            tempNode            = node->info.np[1];
            node->info.np[1] = tempNode->info.np[0];
            freeNode(tempNode->info.np[1]);
            freeNode(tempNode);
        }
    }

    switch (node->op) {
    case IDOP:
        return expandId(node);
    case DOT:
        return mkArrayOp(node);
    case NOT:
        return simplifyNot(node);
    case LT:
    case GEQ:
        if (getTypeClass(node->info.np[0]) == 2 && isZeroConst(node->info.np[1])) {
            warningMsg = "degenerate unsigned comparision";
            freeExprTree(node);
            node = mkConstNode(node->op == GEQ ? 1L : 0L);
        }
        break;
    case LEQ:
    case GT:
        if (getTypeClass(node->info.np[0]) == 2 && isZeroConst(node->info.np[1]))
            node->op = node->op == GT ? NEQL : EQL;
        else if (node->info.np[1]->op == CONST) {
            node->info.np[1]->info.l += 1;
            node->op = node->op == GT ? GEQ : LT;
        } else {
            tempNode            = node->info.np[0];
            node->info.np[0] = node->info.np[1];
            node->info.np[1] = tempNode;
            node->op       = node->op != GT ? GEQ : LT;
        }
        break;
    case ASEOR:
    case BOR:
        if (node->info.np[1]->op == CONST && node->info.np[1]->info.l == -1L) {
            freeExprTree(node->info.np[0]);
            freeNode(node);
            return node->info.np[1];
        }
        /* fall through */
    case ADD:
    case SUB:
    case LSHIFT:
    case ASADD:
    case ASSUB:
    case ASLSHIFT:
    case ASRSHIFT:
    case RSHIFT:
        if (node->info.np[1]->op == CONST && node->info.np[1]->info.l == 0L)
            return dropRightOp(node);

        if ((dopetab[node->op] & DOPE_SHIFT) && nodesize(node->info.np[1]) != 1) {
            tempNode            = allocNode();
            tempNode->pm        = getTypeClass(node->info.np[1]) == 2 ? typeUChar : typeChar;
            tempNode->op      = TYPE;
            node->info.np[1] = mkNode(CONV, node->info.np[1], tempNode);
        }
        break;
    case MUL:
    case DIV:
    case ASMUL:
    case ASDIV:
        if (node->info.np[1]->op == CONST && node->info.np[1]->info.l == 1L)
            return dropRightOp(node);
        /* fall through */
    case MOD:
    case ASMOD:
        if (node->info.np[1]->op == CONST && isPow2Bit(node->info.np[1]->info.l) != 0)
            return pow2ToShift(node);
        /* fall through */
    case BAND:
    case ASAND:
        if (node->info.np[1]->op == CONST && node->info.np[1]->info.l == 0L)
            switch (node->op) {
            case MOD:
            case DIV:
            case ASMOD:
            case ASDIV:
                prWarning("Division by zero");
                return node;
            case BAND:
            case MUL:
                freeExprTree(node->info.np[0]);
                freeExprTree(node->info.np[1]);
                node->op   = CONST;
                node->info.l = 0;
                return node;
            case ASAND:
            case ASMUL:
                node->op = ASSIGN;
                return node;
            }
        bitMask = nodesize(node) >= 4 ? -1L : (1L << (nodesize(node) * 8)) - 1;
        if ((node->op == BAND || node->op == ASAND) && node->info.np[1]->op == CONST &&
            bitMask == (node->info.np[1]->info.l & bitMask))
            return dropRightOp(node);
        break;
    case CONV:
        return optConv(node);
    case PLUS_U:
        freeNode(node);
        node = node->info.np[0];
        break;
    case GADDR:
        if (node->info.np[0]->op == MUL_U)
            return canAddrDeref(node);
        break;
    case MUL_U:
        if (node->info.np[0]->op == GADDR) {
            tempNode = node->info.np[0]->info.np[0];
            if (tempNode->op != LPAREN && tempNode->op != ASSIGN && tempNode->op != QUEST)
                return canAddrDeref(node);
        }
        break;
    }
    return node;
}

/* end of file local1s.c */

/* vim: set tabstop=4 shiftwidth=4 noexpandtab: */
