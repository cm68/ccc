#include "cgen.h"

/*
 * File - cgen3.c Created 09.03.2019 Last Modified 30.05.2020
 */

/*
 * emitNodeCode - Emit all code fragments for an expression node
 *
 * Iterates through the code fragments assigned to the node
 * (stored in pat[] array with nPat count) and emits each
 * one via emitCodePat.
 */
void emitNodeCode(register node_t *node) {
    int patIdx;
    struct codeFrag_t *frag;

    emitOffset = 0;
    for (patIdx = 0; patIdx != node->nPat; patIdx++) {
        if ((frag = &codeFrag[node->pat[patIdx]])->emitCode) {
            emitCodePat(node, frag->emitCode, patIdx);
            fputchar('\n');
        }
    }
}

/*
 * emitExprTree - Recursively emit code for expression tree
 *
 * Walks the expression tree and emits code in correct order:
 *   - Binary ops: order controlled by flags flags from reg alloc
 *   - If flags & 2: push/pop to save intermediate result
 *   - Calls emitNodeCode to emit actual code fragments per node
 *
 * The flags flags handle evaluation order and stack spills
 * when registers are insufficient.
 */
void emitExprTree(register node_t *node) {
    int opCount;

    if (dopetab[node->op] & DOPE_SIGNED)
        exprNestDepth++;
    opCount = dopetab[node->op] & DOPE_OPCOUNT;
    if (DOPE_BINARY == opCount) {
        if (node->flags & NF_EVALORDER) {
            emitExprTree(node->info.np[0]);
            emitExprTree(node->info.np[1]);
        } else {
            emitExprTree(node->info.np[1]);
            if (node->flags & NF_STACKSPILL)
                prPush(node->info.np[1]->resReg[node->info.np[1]->nPat - 1]);
            emitExprTree(node->info.np[0]);
        }
    }
    if (opCount == DOPE_UNARY)
        emitExprTree(node->info.np[0]);
    if (node->flags & NF_STACKSPILL)
        prPop(node->info.np[1]->resReg[node->info.np[1]->nPat - 1]);
    emitNodeCode(node);
    if (dopetab[node->op] & DOPE_SIGNED)
        exprNestDepth--;
}

/*
 * freeNode - Free a single expression tree node
 *
 * Adds the node to the free list for reuse. For FCONST
 * (float constant) nodes, also frees the string data.
 */
void freeNode(register node_t *node) {

    treeChanged = true;
    if (node->op == FCONST)
        free(node->info.np[0]);
    node->pm     = nodeFreeList;
    nodeFreeList = node;
}

/*
 * allocNode - Allocate a new expression tree node
 *
 * Returns a node from the free list if available, otherwise
 * allocates a new node. Always zeros the node before returning.
 */
node_t *allocNode(void) {
    register node_t *node;

    treeChanged = true;
    if (nodeFreeList) {
        node         = nodeFreeList;
        nodeFreeList = node->pm;
        blkclr((char *)node, sizeof(node_t));
    } else { /* create node_t */
        node = allocMem(sizeof(node_t));
    }

    return node;
}

/*
 * relNodeFrList - Release nodes from free list to system
 */
bool relNodeFrList(void) {
    register node_t *node;

    if (nodeFreeList == 0)
        return false;
    while ((node = nodeFreeList)) {
        nodeFreeList = node->pm;
        free(node);
    }
    return true;
}

/*
 * copyTree - Deep copy an expression tree
 *
 * Recursively duplicates a tree node and all its children.
 * Used by deferPostInc to copy operand before deferring increment.
 */
node_t *copyTree(node_t *src) {
    register node_t *copy;

    copy  = allocNode();
    *copy = *src;

    if (dopetab[copy->op] & DOPE_OPCOUNT)
        copy->info.np[0] = copyTree(copy->info.np[0]);
    if ((dopetab[copy->op] & DOPE_OPCOUNT) == DOPE_BINARY)
        copy->info.np[1] = copyTree(copy->info.np[1]);
    return copy;
}

/*
 * peelType - Unwrap one level of pointer/array indirection
 *
 * Shifts tFlags right by 2 bits, or walks type chain to
 * find the pointed-to type.
 */
void peelType(register node_t *node) {
    member_t *typePtr;

    typePtr = node->pm;
    if (node->tFlags != 0)
        node->tFlags >>= 2;
    else {
        while (typePtr->refl == 0)
            typePtr = typePtr->type;
        node->pm    = typePtr->type;
        node->tFlags = typePtr->refl >> 2;
    }
}

/*
 * addPtrType - Add pointer indirection to node's type
 *
 * Sets tFlags = (tFlags * 4) | 1 to add pointer level.
 */
void addPtrType(register node_t *node) {
    node->tFlags = (node->tFlags * 4) | 1;
}

/*
 * derefSize - Get size of dereferenced type
 *
 * Peels one level of indirection, gets the size, then
 * restores the pointer type.
 */
uint16_t derefSize(register node_t *node) {
    uint8_t size;

    if (node->tFlags == 0 && node->pm->type == 0 && node->pm->refl == 0)
        return 1;

    peelType(node);
    size = (uint8_t)nodesize(node);
    addPtrType(node);
    return size;
}

/*
 * hasTypeFlag - Check if type chain has specified flag
 *
 * Tests tFlags and walks type chain checking b_refl for flag.
 */
bool hasTypeFlag(node_t *node, int flag) {
    register member_t *typePtr;

    if (node->tFlags & flag)
        return true;
    if (node->tFlags != 0)
        return false;
    typePtr = node->pm;
    do {
        if (typePtr->refl & flag)
            return true;
        if (typePtr->refl != 0)
            return false;
    } while ((typePtr = typePtr->type) != 0);
    return false;
}

/*
 * isPointer - Check if node is a pointer type (not used)
 */
bool isPointer(node_t *node) {
    return hasTypeFlag(node, 1);
}

/*
 * isFuncType - Check if node is a function type (not used)
 */
bool isFuncType(node_t *node) {
    return hasTypeFlag(node, 2);
}

/*
 * isStructVal - Check if node is a struct value (not ptr)
 */
bool isStructVal(register node_t *node) {
    return node->pm->sclass == STRUCT && (node->tFlags & T_PTR) == 0;
}

/*
 * nodesize - Get size in bytes for a node's type
 */
uint16_t nodesize(register node_t *node) {

    if (node->tFlags & T_FUNC)
        prError("can\'t take sizeof func");

    if (node->tFlags & T_PTR)
        return 2;
    return node->pm->size;
}

/*
 * castConst - Cast constant node to target type
 *
 * Converts integer to float or propagates type with masking.
 */
node_t *castConst(register node_t *node) {
    char buf[50];

    freeNode(node->info.np[1]);
    if (getTypeClass(node) == 3) {
        sprintf(buf, "%ld.", node->info.np[0]->info.l);
        freeNode(node->info.np[0]);
        node->info.sv.s = allocMem(strlen(buf) + 1); /* create string */
        strcpy(node->info.sv.s, buf);
        node->info.sv.v = newLocal();
        node->op      = FCONST;
        return node;
    }
    setNodeType(node, node->info.np[0]);
    freeNode(node);
    return node->info.np[0];
}

/*
 * setNodeType - Copy type from source to dest with value masking
 *
 * Masks/sign-extends value based on type size, then copies
 * type info (pm, tFlags) from source to destination node.
 */
void setNodeType(register node_t *typeNode, node_t *destNode) {
    int bitWidth;

    bitWidth = nodesize(typeNode);
    if (bitWidth != 0 && bitWidth < 4) {
        bitWidth <<= 3;
        destNode->info.l &= (1L << bitWidth) - 1L;
        if (getTypeClass(typeNode) == 1) {
            if (destNode->info.l & (long)(1 << (bitWidth - 1))) { /* sign extend */
                destNode->info.l |= ~((1L << bitWidth) - 1L);
            }
        }
    }
    destNode->pm    = typeNode->pm;
    destNode->tFlags = typeNode->tFlags;
}

/*
 * unsignedOp - Apply unsigned arithmetic operation in-place
 */
void unsignedOp(register unsigned long *ptr, long operand, int operator) {

    switch (operator) {
    case DIV:
        *ptr /= operand;
        break;
    case MOD:
        *ptr %= operand;
        break;

    case RSHIFT:
        *ptr >>= operand;
        break;
    }
}

/*
 * signedOp - Apply signed arithmetic operation in-place
 */
void signedOp(register long *ptr, long operand, int operator) {

    switch (operator) {
    case ADD:
        *ptr += operand;
        break;
    case SUB:
        *ptr -= operand;
        break;
    case MUL:
        *ptr *= operand;
        break;
    case DIV:
        *ptr /= operand;
        break;
    case MOD:
        *ptr %= operand;
        break;
    case BOR:
        *ptr |= operand;
        break;
    case BAND:
        *ptr &= operand;
        break;
    case BXOR:
        *ptr ^= operand;
        break;
    case LSHIFT:
        *ptr <<= operand;
        break;
    case RSHIFT:
        *ptr >>= operand;
        break;
    }
}

/*
 * deferPostInc - Extract and defer post-increment/decrement
 *
 * Walks expression tree separating post-inc/dec side effects:
 *   - SCOLON: emit left operand, continue with right
 *   - INCR/DECR on USEREG: save to array_AFFD[], convert to
 *     ASADD/ASSUB, return copy of operand value
 *   - Stops at 0x4000 ops, recurses on children
 *
 * After this pass, emitExpr generates main expr first,
 * then emits deferred side effects from array_AFFD[].
 */
node_t *deferPostInc(register node_t *node) {
    int dopeFlags;

    if (node->op == SCOLON) {
        genExprCode(node->info.np[0]);
        freeNode(node);
        node = node->info.np[1];
    }
    if (deferCnt != 10) {
        if (dopetab[node->op] & DOPE_CTRL)
            return node;
        if ((node->op == INCR || node->op == DECR) && hasRegChild(node)) {
            deferList[deferCnt++] = node;
            node->op              = node->op == INCR ? ASADD : ASSUB;
            node                  = copyTree(node->info.np[0]);
        }
        dopeFlags = dopetab[node->op];
        if (dopeFlags & DOPE_OPCOUNT)
            node->info.np[0] = deferPostInc(node->info.np[0]);
        if ((dopeFlags & DOPE_OPCOUNT) == DOPE_BINARY)
            node->info.np[1] = deferPostInc(node->info.np[1]);
    }
    return node;
}

/* end of file cgen3.c */

/* vim: tabstop=4 shiftwidth=4 noexpandtab: */
