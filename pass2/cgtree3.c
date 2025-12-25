#include "cgen.h"

/*
 * File - tree3.c Created 09.03.2019 Last Modified 17.06.2020
 */

/*
 * mkNode - Create expression tree node
 *
 * Allocates a new node, sets operator and children, and
 * determines result type based on operator properties.
 *
 * Parameters:
 *   p1:  Operator code (NOT, ADD, MUL_U, etc.)
 *   p2a: Left/only child (NULL for leaves)
 *   p3a: Right child (NULL for unary ops)
 *
 * Type inference:
 *   DOPE_LEAF (0x10): result is long (constants)
 *   DOPE_LOGICAL (0x20): result is bool (comparisons)
 *   Both (0x30): result is variable type
 *   Default: inherit type from left child
 *
 * Special handling for DOT, CONV, GADDR, MUL_U, HASHSIGN, etc.
 */
node_t *mkNode(uint8_t opCode, node_t *leftChild, node_t *rightChild) {
    member_t *member;
    long sizeVal;
    register node_t *node;

    node             = allocNode();
    node->op       = opCode;
    node->info.np[0] = leftChild;
    node->info.np[1] = rightChild;
    switch (dopetab[node->op] & (DOPE_LEAF | DOPE_LOGICAL)) {
    case DOPE_LEAF:
        node->pm = typeLong; /* long	  */
        break;
    case DOPE_LOGICAL:
        node->pm = typeB; /* b	  */
        break;

    case (DOPE_LEAF | DOPE_LOGICAL):
        node->pm = typeVar; /* variable */
        break;

    default:
        node->pm    = leftChild->pm;
        node->tFlags = leftChild->tFlags;
    }
    switch (node->op) {
    case DOT:
        if (leftChild->op == TYPE) {
            member = leftChild->pm;
            freeNode(node);
            freeNode(leftChild);
            node          = rightChild;
            node->pm      = member;
            rightChild->info.l = (long)member->u.ilist->vals[node->info.l];
            break;
        }
        member         = node->pm->u.mlist->vals[rightChild->info.l];
        node->pm      = member->type;
        node->tFlags   = member->refl;
        rightChild->info.l = (long)member->offset;
        rightChild->pm     = typeX; /* x      */
        if (member->sflags & B_SLOC_BITFIELD) {
            leftChild             = allocNode();
            *leftChild            = *node;
            leftChild->info.np[0] = node;
            node              = leftChild;
            node->op        = BFIELD;
            node->info.mp[1]  = member;
        }
        break;
    case CONV:
    case SCOLON:
    case QUEST:
        node->pm    = node->info.np[1]->pm;
        node->tFlags = node->info.np[1]->tFlags;
        break;
    case GADDR:
        if (node->tFlags == 0 && node->pm->nelem > 1) {
            node->tFlags = node->pm->refl;
            node->pm    = node->pm->type;
        }
        addPtrType(node);
        break;
    case LPAREN:
    case MUL_U:
        peelType(node);
        break;
    case COLON_S:
        node->pm    = typeChar; /* char   */
        node->tFlags = 1;
        break;
    case HASHSIGN:
        if (node->info.np[0]->op == IDOP)
            sizeVal = node->info.np[0]->info.mp[0]->size;
        else
            sizeVal = (uint16_t)nodesize(node->info.np[0]);
        if (sizeVal == 0)
            prWarning("Sizeof yields 0");
        freeExprTree(node->info.np[0]);
        node->op   = CONST;
        node->pm     = typeLong; /* long	  */
        node->tFlags  = 0;
        node->info.l = sizeVal;
        break;
    }
    return node;
}

/*
 * isPow2Bit - Check if value is power of 2, return bit position
 *
 * Returns 0 if p1 is not a power of 2 (or < 1).
 * Returns (log2(p1) + 1) if p1 is a power of 2:
 *   1 -> 1, 2 -> 2, 4 -> 3, 8 -> 4, etc.
 *
 * Used for strength reduction (div/mod -> shift/mask) and
 * bit offset calculations during code emission.
 */
uint8_t isPow2Bit(long value) {
    uint8_t bitPos;

    if ((value & (value - 1)) || value < 1)
        return 0;
    for (bitPos = 0; value >>= 1; bitPos++)
        ;

    return (bitPos + 1);
}

/*
 * freeExprTree OK++ PMO
 *
 * Recursively frees an expression tree node and all its children.
 * Uses dopetab[op] & DOPE_OPCOUNT to determine operand count:
 *   0 = leaf node (no children)
 *   4 = unary operator (one child in np[0])
 *   8 = binary operator (two children in np[0] and np[1])
 */
void freeExprTree(register node_t *node) {
    int opCount;

    if ((opCount = dopetab[node->op] & DOPE_OPCOUNT)) {
        freeExprTree(node->info.np[0]);
        if (opCount == DOPE_BINARY)
            freeExprTree(node->info.np[1]);
    }
    freeNode(node);
}

/*
 * testPattern - Test if node matches pattern requirements
 * The code optimisers generate many differences in the code
 * however the vast majority relate to the choice of the code
 * fragments to share for returning true/false and generating
 * true/false from != condition
 * I have tried multiple variants of code to get better matches
 * for the current code, the other differences are
 * 1) at m49dc:, code for getTypeClass(sa) != 3 is moved
 * 2) case SUB: nodesize(sa) == 2 code moved and missed sharing
 *    of code for or a ! sbc hl,de ! jp nz false
 * 3) case DECR: similar to (2)
 * 4) case GADDR: similar to (2)
 * 5) an instance of missed sharing of code
 * 6) instances of inversion of jp conditions with labels also
 *    flipped
 * Note removing the gotos was initially tried, but the optimiser
 * match was poorer. Similarly using compound returns rather
 * than if / break, also produced a poorer match.
 */
uint8_t testPattern(register node_t *node, int pattern) {
    long value;

    switch (pattern) {
    case MUL_U:
        node = node->info.np[1];
    case T_SCOLON:
        return node->info.l >= 1L && node->info.l < 3L;
    case RPAREN:
        node = node->info.np[0];
    case MOD:
        if (nodesize(node) == 2 && !isStructVal(node))
            return true;
        break;
    case BAND:
        if (getTypeClass(node) == 3)
            return false;
        goto m485f;
    case MUL:
        node = node->info.np[0];
    case DOLLAR_U:
    m485f:
        if (nodesize(node) == 4 && !isStructVal(node))
            return true;
        break;
    case SCOLON:
        node = node->info.np[0];
    case DOLLAR:
        return isStructVal(node);
    case ADD:
        return (uint8_t)node->info.l == 8 || (uint8_t)node->info.l == 9;
    case INCR:
        value = signExtend(node, node->info.l);
        return -128L <= value && value < 125L;
    case COLON_U:
        return 1L <= node->info.l && node->info.l < 5L;
    case COLON_S:
        value = node->info.l;
        if (getTypeClass(node) == 2 && (value & (uint16_t)(1 << ((int16_t)nodesize(node) << 3))))
            value |= (uint16_t) ~((1 << ((int16_t)nodesize(node) << 3)) - 1);
        return value < 0 && value >= -4L;
    case LPAREN:
        node = node->info.np[1];
    case HASHSIGN:
        return nodesize(node) == 1 && !isStructVal(node);
    case MINUS_U:
        if (nodesize(node) == 4)
            goto m49dc;
        break;
    case COMMA:
        if (nodesize(node) == 2)
        m49dc:
            if (getTypeClass(node) != 3 && getTypeClass(node->info.np[0]) != 3)
                return getTypeClass(node) == 2 || getTypeClass(node->info.np[0]) == 2;
        break;
    case LT:
        return node->info.l == 0;
    case CONV:
        if (nodesize(node) == 4)
        m4a48:
            return getTypeClass(node) == 1 && getTypeClass(node->info.np[0]) == 1;
        return false;

    case SUB:
        if (nodesize(node) == 2)
            goto m4a48;
        break;
    case PLUS_U:
        return 1L == node->info.l;
    case NOT:
        return isPow2Bit(node->info.np[1]->info.l);
    case NEQL:
        return isPow2Bit(~node->info.np[1]->info.l);
    case COLON:
        if (nodesize(node) == 4)
            goto m4b02;
        break;
    case DECR:
        if (nodesize(node) == 2)
            goto m4b02;
        break;

    case GADDR:
        if (nodesize(node) != 1)
            return false;
    m4b02:
        return (getTypeClass(node) == 1 || getTypeClass(node) == 2) && getTypeClass(node->info.np[0]) == 3;
    case DIV:
        if (nodesize(node->info.np[0]) == 1)
            goto dotp;
        break;
    case DOT_DOT:
        if (nodesize(node->info.np[0]) == 2)
            goto dotp;
        break;
    case DOT:
        if (nodesize(node->info.np[0]) == 4) {
        dotp:
            if (getTypeClass(node) != 3)
                return false;
            else
                return getTypeClass(node->info.np[0]) == 1 || getTypeClass(node->info.np[0]) == 2;
        }
        break;
    case LAND:
        return getTypeClass(node) == 3;
    }
    return 0;
}

/*
 * isAddrable - Check if node is a simple addressable expr
 *
 * Returns true for ID nodes, &ID, or ID+CONST combinations.
 */
bool isAddrable(register node_t *node) {
    /* Any operator except "#", "..", "CONST" */
    return (dopetab[node->op] & DOPE_LEAF) || (node->op == GADDR && node->info.np[0]->op == IDOP) ||
           (node->op == ADD && node->info.np[1]->op == CONST && isAddrable(node->info.np[0]) != 0);
}

/*
 * isZeroConst - Check if node is a zero constant
 */
bool isZeroConst(register node_t *node) {

    return node->op == CONST && node->info.l == 0;
}

/*
 * addrLevel - Get address complexity level for a node
 */
int addrLevel(register node_t *node) {
    if (node->op == GADDR && node->info.np[0]->op == IDOP)
        return 1;
    if (dopetab[node->op] & DOPE_LEAF)
        return 0; /* if "#", "..", "CONST"  */
    if (isAddrable(node))
        return 1;
    return dopetab[node->op] & (DOPE_OPCOUNT | DOPE_RESCAT);
}

/*
 * hasRegChild - Check if child operand uses a register
 */
bool hasRegChild(node_t *node) {
    return node->info.np[0]->op == USEREG;
}

/*
 * simplifyNot - Simplify NOT expressions
 *
 * Transforms: !!x -> x, !(a==b) -> a!=b, !(a&&b) -> !a||!b
 */
node_t *simplifyNot(register node_t *node) {
    node_t *child;

    child = node->info.np[0];
    if (node->info.np[0]->op == NOT) {
        child = child->info.np[0];
        freeNode(node->info.np[0]);
        freeNode(node);
        return child;
    }
    if ((dopetab[child->op] & (DOPE_SIDEEFF | DOPE_OPCOUNT | DOPE_LOGICAL)) == (DOPE_BINARY | DOPE_LOGICAL)) {
        freeNode(node);
        child->op = invertTest(child->op);
        return child;
    }
    if (dopetab[child->op] & DOPE_LOGICAL) {
        child->info.np[0] = mkNode(NOT, child->info.np[0], (node_t *)0);
        child->info.np[1] = mkNode(NOT, child->info.np[1], (node_t *)0);
        freeNode(node);
        child->op = child->op == LAND ? LOR : LAND;
        return child;
    }
    return node;
}

/*
 * expandId - Expand identifier to address expression
 *
 * Converts auto/register variable references to address
 * calculations (IX+offset or register direct).
 */
node_t *expandId(register node_t *node) {
    node_t *offsetNode;

    if (node->info.mp[0]->tflag == 1) {
        offsetNode        = mkConstNode(node->info.mp[0]->offset);
        offsetNode->pm    = node->pm;
        offsetNode->tFlags = node->tFlags;
        freeExprTree(node);
        addPtrType(offsetNode);
        node         = allocNode();
        *node        = *offsetNode;
        node->op   = USEREG;
        node->info.l = 8L;
        node         = mkNode(ADD, node, offsetNode);
        return mkNode(MUL_U, node, 0);
    }
    if (node->info.mp[0]->tflag == 2) {
        node->op   = USEREG;
        node->info.l = node->info.mp[0]->u.i;
    }
    return node;
}

/*
 * mkArrayOp - Convert array subscript to pointer arithmetic
 *
 * Transforms array[index] into *(array + index) for codegen.
 */
node_t *mkArrayOp(register node_t *node) {
    node_t *childNode;

    if (dopetab[(childNode = node->info.np[0])->op] & DOPE_INDIR) {
        childNode->pm        = node->pm;
        childNode->tFlags     = node->tFlags;
        node->info.np[0] = childNode = mkNode(GADDR, childNode, 0);
    } else {
        childNode       = allocNode();
        *childNode      = *node;
        childNode->op = TYPE;
        addPtrType(childNode);
        node->info.np[0] = mkNode(CONV, mkNode(GADDR, node->info.np[0], 0), childNode);
    }
    node->pm                = childNode->pm;
    node->tFlags             = childNode->tFlags;
    node->info.np[1]->tFlags = node->tFlags;
    node->info.np[1]->pm    = node->pm;
    node->op              = ADD;
    return mkNode(MUL_U, node, 0);
}

/*
 * dropRightOp - Drop right operand, return left
 *
 * Used for identity elimination: frees the right operand
 * subtree and operator node, returning just the left operand.
 * E.g., x+0 -> x, x*1 -> x, x&~0 -> x
 */
node_t *dropRightOp(register node_t *node) {

    freeExprTree(node->info.np[1]);
    freeNode(node);
    return node->info.np[0];
}

/*
 * pow2ToShift - Strength reduce power-of-2 multiply/divide/mod
 *
 * Converts operations by powers of 2 into faster equivalents:
 *   MUL/ASMUL  -> LSHIFT/ASLSHIFT  (x * 2^n -> x << n)
 *   DIV/ASDIV  -> RSHIFT/ASRSHIFT  (x / 2^n -> x >> n, unsigned only)
 *   MOD/ASMOD  -> BAND/ASAND       (x % 2^n -> x & (2^n - 1))
 *
 * For shifts, converts constant from power value to shift amount.
 * Division optimization only applies to unsigned types.
 */
node_t *pow2ToShift(register node_t *node) {

    switch (node->op) {
    case MUL:
        node->op = LSHIFT;
        break;
    case ASMUL:
        node->op = ASLSHIFT;
        break;
    case DIV:
        if (getTypeClass(node) != 2)
            return node;
        node->op = RSHIFT;
        break;
    case ASDIV:
        if (getTypeClass(node) != 2)
            return node;
        node->op = ASRSHIFT;
        break;
    case ASMOD:
        node->op = ASAND;
        node->info.np[1]->info.l--;
        return node;
    case MOD:
        node->op = BAND;
        node->info.np[1]->info.l--;
        return node;
    }
    treeChanged              = true;
    node->info.np[1]->info.l = isPow2Bit(node->info.np[1]->info.l) - 1;
    return node;
}

/*
 * optConv - Optimize type conversion expressions
 *
 * Eliminates redundant conversions and propagates types.
 */
node_t *optConv(register node_t *node) {
    node_t *childNode;
    node_t *newNode;
    char buf[15];
    long constVal;

    if (getTypeClass(node) == 3 && node->info.np[0]->op == CONST) {
        constVal = node->info.np[0]->info.l;
        freeNode(node->info.np[0]);
        freeNode(node->info.np[1]);
        sprintf(buf, "%ld", constVal);
        node->info.sv.s = allocMem(strlen(buf) + 1); /* create string */
        strcpy(node->info.sv.s, buf);
        node->info.sv.v = newLocal();
        node->op      = FCONST;
        return node;
    }
    if ((dopetab[(childNode = node->info.np[0])->op] & DOPE_INDIR) && nodesize(childNode) >= nodesize(node) &&
        getTypeClass(node) != 3 && getTypeClass(childNode) != 3) {
        childNode->pm    = node->pm;
        childNode->tFlags = node->tFlags;
        freeNode(node->info.np[1]);
        freeNode(node);
        return childNode;
    }
    if (sameType(node, childNode)) {
        freeExprTree(node->info.np[1]);
        childNode->pm    = node->pm;
        childNode->tFlags = node->tFlags;
        freeNode(node);
        return childNode;
    }
    if (nodesize(childNode) < nodesize(node))
        return node;
    if (childNode->op == CONV && nodesize(childNode->info.np[0]) >= nodesize(node)) {
        if (sameType(node, childNode->info.np[0]) ||
            (getTypeClass(node) == getTypeClass(childNode) && getTypeClass(node) == getTypeClass(childNode->info.np[0]))) {
            node->info.np[0] = childNode->info.np[0];
            freeNode(childNode->info.np[1]);
            freeNode(childNode);
            return node;
        }
    }
    if ((dopetab[childNode->op] & DOPE_OPCLASS) == 0 || getTypeClass(childNode) == 3)
        return node;

    if ((dopetab[childNode->op] & DOPE_DIVMOD) &&
        (childNode->info.np[0]->op != CONV || nodesize(childNode->info.np[0]->info.np[0]) >= nodesize(childNode)))
        return node;

    newNode = mkNode(CONV, childNode->info.np[0], node->info.np[1]);
    *node = *node->info.np[1];
    if (dopetab[childNode->op] & DOPE_BINARY) {
        node = mkNode(CONV, childNode->info.np[1], node);
    } else {
        freeNode(node);
        node = 0;
    }
    newNode = mkNode((uint8_t)childNode->op, newNode, node);
    freeNode(childNode);
    return newNode;
}

/*
 * canAddrDeref - Cancel address-of/dereference pairs
 *
 * Simplifies &(*p) and *(&x) expressions:
 *   - If inner node is an lvalue (IDOP, DOT, etc.), cancels
 *     the &* or *& entirely, returning the inner node
 *   - Otherwise converts to a CONV (type cast) node
 *
 * Called from localOptimize for GADDR/MUL_U combinations.
 */
node_t *canAddrDeref(register node_t *node) {
    node_t *innerNode;

    if (dopetab[(innerNode = node->info.np[0])->info.np[0]->op] & DOPE_INDIR) {
        innerNode        = innerNode->info.np[0];
        innerNode->pm    = node->pm;
        innerNode->tFlags = node->tFlags;
        freeNode(node->info.np[0]);
        freeNode(node);
        return innerNode;
    }
    node->op       = CONV;
    node->info.np[1] = innerNode;
    node->info.np[0] = innerNode->info.np[0];
    innerNode->op      = TYPE;
    innerNode->pm        = node->pm;
    innerNode->tFlags     = node->tFlags;
    treeChanged      = true;
    return node;
}

/* end of file tree3.c */

/* vim: tabstop=4 shiftwidth=4 noexpandtab: */
