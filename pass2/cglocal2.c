#include "cgen.h"

/*
 * File - local2.c
 */

/*********************************************************
 * allocVar - Allocate storage for a local variable
 *
 * Assigns stack offset or register based on storage class.
 *********************************************************/
void allocVar(register member_t *symbol, int storageClass) {

    switch (symbol->tflag) {
    case 1:
        if (tryAllocReg(symbol, storageClass) == 0) {
            funcLocalSize[lvlidx] -= symbol->size;
            if ((symbol->offset = funcLocalSize[lvlidx]) < -128 && symbol->size <= 4)
                prWarning("%s: large offset", symbol->name);
        }
        break;
    case 5:
        symbol->offset = funcParamOffset[lvlidx];
        funcParamOffset[lvlidx] += symbol->size;
        if (symbol->size == 1 && symbol->type->sclass != STRUCT && symbol->type->sclass != UNION)
            funcParamOffset[lvlidx]++;

        if (!tryAllocReg(symbol, storageClass))
            symbol->tflag = 1;
        break;
    case 3:
    case 4:
        emitBssDef(symbol);
        break;
    }
}

/*********************************************************
 * subToAdd - Normalize subtraction to addition with negation
 *
 * Pre-optimization pass that converts all subtractions to
 * additions with negated operands for easier optimization:
 *   SUB(x, const)  -> ADD(x, -const)
 *   SUB(x, expr)   -> ADD(x, MINUS_U(expr))
 *   MINUS_U(MINUS_U(x)) -> x  (cancel double negation)
 *   MINUS_U(FCONST) -> prepend "-" to float string
 *
 * Called first in optimizeExpr; negAddToSub reverses at end.
 *********************************************************/
node_t *subToAdd(register node_t *node) {
    char opCount;
    char *str;

    opCount = dopetab[node->op] & DOPE_OPCOUNT;
    if (opCount == DOPE_BINARY)
        node->info.np[1] = subToAdd(node->info.np[1]);
    if (opCount != 0)
        node->info.np[0] = subToAdd(node->info.np[0]);

    switch (node->op) {
    case SUB:
    case ASSUB:
        if (node->info.np[1]->op == CONST)
            node->info.np[1]->info.l = -node->info.np[1]->info.l;
        else
            node->info.np[1] = mkNode(MINUS_U, node->info.np[1], 0);
        node->op = node->op == SUB ? ADD : ASADD;
        break;
    case MINUS_U:
        if (node->info.np[0]->op == MINUS_U) {
            freeNode(node);
            node = node->info.np[0];
            freeNode(node);
            node = node->info.np[0];
        }
        if (node->info.np[0]->op == FCONST) {
            freeNode(node);
            node = node->info.np[0];
            str = allocMem(strlen(node->info.sv.s) + 2);
            strcat(strcpy(str, "-"), node->info.sv.s);
            free(node->info.sv.s);
            node->info.sv.s = str;
        }
    }
    return node;
}

/*********************************************************
 * negAddToSub - Convert additions with negated operands to subtractions
 *
 * Final cleanup pass that recursively transforms:
 *   ADD(-x, y)   -> swap to ADD(y, -x), then convert
 *   ADD(x, -y)   -> SUB(x, y)
 *   ASADD(x, -y) -> ASSUB(x, y)
 *
 * Called at end of optimizeExpr after constant folding.
 *********************************************************/
node_t *negAddToSub(register node_t *node) {
    char opCount;
    node_t *temp;

    opCount = dopetab[node->op] & DOPE_OPCOUNT;
    if (opCount == DOPE_BINARY)
        node->info.np[1] = negAddToSub(node->info.np[1]);
    if (opCount != 0)
        node->info.np[0] = negAddToSub(node->info.np[0]);

    switch (node->op) {
    case ADD:
        if (node->info.np[0]->op == MINUS_U) {
            temp             = node->info.np[0];
            node->info.np[0] = node->info.np[1];
            node->info.np[1] = temp;
        }
    case ASADD:
        if (node->info.np[1]->op == MINUS_U) {
            freeNode(node->info.np[1]);
            node->info.np[1] = node->info.np[1]->info.np[0];
            node->op       = node->op == ASADD ? ASSUB : SUB;
        }
    }

    return node;
}

/*********************************************************
 * optimizeExpr OK++ PMO
 *
 * Main expression optimizer - simplifies and optimizes expression trees.
 *
 * Optimization pipeline:
 *   1. subToAdd: Normalize (convert SUB to ADD+negate, simplify --x)
 *   2. Loop until no changes:
 *      - localOptimize: Local opts (reordering, identity elimination, strength reduction)
 *      - constFold: Constant folding (evaluate constants at compile time)
 *   3. negAddToSub: Final cleanup pass
 *
 * Sets treeChanged when transformations are made, loops until stable.
 * Collects and reports optimization warnings.
 *
 * Returns: optimized expression tree
 *********************************************************/
node_t *optimizeExpr(register node_t *node) {

    int unused; /* Not used */

    warningMsg = 0;
    node         = subToAdd(node);
    unused         = 0; /* Not used */
    do {
        treeChanged = false;
        node = constFold(localOptimize(node));
    } while (treeChanged);

    if (warningMsg)
        prWarning(warningMsg);

    return negAddToSub(node);
}

/*********************************************************
 * invertTest - Invert comparison operator
 *
 *********************************************************/

int invertTest(int op) {

#ifdef DEBUG
    printf("\tinvertTest(%d)\n", op);
#endif

    switch (op) {
    case EQL:
        return NEQL;
    case NEQL:
        break;
    case LT:
        return GEQ;
    case LEQ:
        return GT;
    case GEQ:
        return LT;
    case GT:
        return LEQ;
    }
    return EQL;
}

/*********************************************************
 * constFits - Check if constant fits in target type range
 *********************************************************/
bool constFits(register node_t *constNode, node_t *targetNode) {
    long maxVal;

    if (constNode->op != 'C')
        return 0;
    if (nodesize(targetNode) >= 4)
        return constNode->info.l >= 0 || getTypeClass(targetNode) == 1;

    maxVal = (uint16_t)(1 << (nodesize(targetNode) * 8));
    if (getTypeClass(targetNode) == 2) {
        if (constNode->info.l < 0)
            return 0;
    } else {
        maxVal /= 2;
        if (constNode->info.l < 0)
            return constNode->info.l >= -maxVal;
    }
    return constNode->info.l < maxVal;
}

/*********************************************************
 * tryAllocReg - Try to allocate a register for variable
 *
 * Returns true if register was allocated, false otherwise.
 *********************************************************/
bool tryAllocReg(register member_t *symbol, int storageClass) {

    if (!rflag && isupper(storageClass) && symbol->refl == 1 && symbol->nelem <= 1) {
        if (findAvailReg(availRegs, 9) == 0)
            return false;
        if (symbol->tflag == 5)
            symbol->sflags |= B_SLOC_SAVE;
        symbol->tflag   = 2;
        symbol->u.i = findAvailReg(availRegs, 9);
        availRegs &= ~regBitMask[symbol->u.i];
        return true;
    }

    return false;
}

/*********************************************************
 * sameType - Check if two nodes have matching types
 *
 * Returns true if both nodes have the same size and the
 * same type class (signed/unsigned/float).
 *********************************************************/
bool sameType(node_t *node1, node_t *node2) {

    return nodesize(node1) == nodesize(node2) && getTypeClass(node1) == getTypeClass(node2);
}

/*********************************************************
 * bothSigned - Check if both nodes are signed integers
 *
 * Returns true if both nodes have type class 1 (signed).
 *********************************************************/
bool bothSigned(node_t *node1, node_t *node2) {
    return getTypeClass(node1) == 1 && getTypeClass(node2) == 1;
}

/*********************************************************
 * constFitsType - Check if constant fits in type's range
 *
 * Returns true if the constant value in p2a fits within
 * the range of sa's target type (sa->info.np[0]).
 *
 * Range for unsigned: [0, 2^bits)
 * Range for signed: [-2^(bits-1), 2^(bits-1))
 * 32-bit types: always returns true
 *********************************************************/
bool constFitsType(register node_t *typeNode, node_t *constNode) {
    long maxVal, minVal;
    char bitWidth;

    bitWidth = nodesize(typeNode->info.np[0]) * 8;
    if (bitWidth >= 32)
        return true;
    maxVal = 1L << bitWidth;
    minVal = 0;
    if (bothSigned(typeNode, typeNode->info.np[0])) {
        maxVal /= 2;
        minVal = -maxVal;
    }
    return minVal <= constNode->info.l && constNode->info.l < maxVal;
}

/* macros to make reading the code easier */
/* clang-format off */
/* don't reformat as hitech cpp includes the extra spaces
   which can cause line overflow
*/
#ifndef TOPBIT
#define TOPBIT REG_INVMASK
#define TopBitSet(n) ((n)&TOPBIT)
#define MapVal(n) (TopBitSet(n)?~(n):regBitMask[n])
#endif
/* clang-format on */
/*********************************************************
 * selCompatReg - Select compatible register
 *
 * Finds a register matching requirement p3 that is both
 * available (in bitmask p1) and compatible with target p2.
 *
 * Parameters:
 *   p1: Available registers bitmask
 *   p2: Target constraint (0=none, 0x8000=bitmask, else reg index)
 *   p3: Required register or class (bit 6 modifier cleared)
 *
 * Returns: Register index if found, 0 otherwise
 *********************************************************/
uint8_t selCompatReg(int availMask, int targetReg, int reqReg) {
    uint8_t resultReg;
    uint8_t tempReg;
    int altReg;
    register uint8_t *classPtr;

    reqReg &= ~CF_REGCONSTRAINT;
    resultReg = 0;

    if (targetReg == 0)
        resultReg = findAvailReg(availMask, reqReg);
    else if (targetReg & REG_INVMASK)
        resultReg = findAvailReg(MapVal(targetReg) & availMask, reqReg);
    else if (reqReg < 24) {
        if ((resultReg = findAvailReg(MapVal(targetReg) & availMask, reqReg)) == 0 && (altReg = findAvailReg(availMask, reqReg)) != 0 &&
            findAvailReg(MapVal(altReg), targetReg) != 0)
            resultReg = altReg;
    } else {
        altReg = 6;
        classPtr = &regClassRegs[(reqReg - 24) * 6];
        while (*classPtr && altReg-- != 0)
            if ((tempReg = findAvailReg(availMask, *classPtr++)) != 0 &&
                (findAvailReg(regBitMask[tempReg], targetReg) != 0 || findAvailReg(regBitMask[targetReg], tempReg) != 0)) {
                resultReg = tempReg;
                break;
            }
    }
    return resultReg;
}

/*********************************************************
 * findAvailReg - Find available register matching requirement
 *
 * Given a bitmask of available registers (p1) and a register
 * requirement (p2), finds a matching register.
 *
 * Parameters:
 *   p1: Bitmask of available registers
 *   p2: Register index (0-23) or class code (24+), bit 6 is modifier
 *
 * For single registers (p2 < 24): returns p2 if available, else 0
 * For register classes (p2 >= 24): returns first available register
 *   from the class, or 0 if none available
 *********************************************************/
uint8_t findAvailReg(int availMask, int regSpec) {
    char count;
    uint8_t *classPtr;

    regSpec &= ~CF_REGCONSTRAINT; /* Clear bit 6 */
    if (regSpec < 24)
        if ((availMask & regBitMask[regSpec]) == regBitMask[regSpec])
            return regSpec;
        else
            return 0;
    regSpec -= 24;
    count = 6;
    classPtr = &regClassRegs[regSpec * 6];
    do {
        if (*classPtr != 0 && (availMask & regBitMask[*classPtr]) == regBitMask[*classPtr])
            return *classPtr;
    } while (*++classPtr && --count != 0);
    return 0;
}

/*********************************************************
 * findRegPair - Find register pair containing a component
 *
 * Inverse of regPairHiLo lookup. Given a component register,
 * finds which register pair contains it.
 *
 * Parameters:
 *   p1: Component register index (e.g., 2='c', 6='l')
 *   p2: '0' for low byte position, '1' for high byte position
 *
 * Returns: Register pair index, or 0 if not found
 *
 * Examples: findRegPair(2,'0')=12 (c is low of bc)
 *           findRegPair(7,'1')=14 (h is high of hl)
 *********************************************************/
uint16_t findRegPair(uint16_t compReg, uint8_t position) {
    uint16_t pairIdx;

    if (compReg == 0)
        return 0;
    position -= 48;
    for (pairIdx = 1; pairIdx < 24; pairIdx++)
        if (regPairHiLo[pairIdx * 2 + position] == compReg)
            return pairIdx;
    return 0;
}

/*********************************************************
 * selResultReg - Select result register for code pattern
 *
 * Register selection for matchEmitPat. Finds suitable register
 * given constraints:
 *   p1: Required register class
 *   p2: Preferred reg or constraint mask (0x8000 = mask mode)
 *   p3: Available registers bitmask
 *   p4: Pattern string for register pair lookups ("01" = hi/lo)
 *
 * Returns selected register index, or 0 if none found.
 *********************************************************/
int selResultReg(int reqClass, int prefReg, int availMask, char *patStr) {
    int16_t intersect, result;
    char *patPtr;

    result = 0;
    if ((prefReg & REG_INVMASK) != 0) {
        if (reqClass == 0)
            result = prefReg;
        else if ((intersect = findAvailReg(((prefReg & REG_INVMASK) ? ~prefReg : regBitMask[prefReg]) & availMask, reqClass)) != 0)
            result = intersect;
        else
            result = findAvailReg(availMask, reqClass);
    } else if (reqClass == 0) {
        if (prefReg == 0 || patStr == 0)
            result = prefReg;
        else {
            for (patPtr = patStr; isdigit(patPtr[1]); patPtr++)
                ;
            if (prefReg >= 24 && (prefReg = findAvailReg(availMask, prefReg)) == 0)
                result = 0;
            else {
                do {
                    prefReg = findRegPair(prefReg, (uint8_t)*patPtr);
                } while (patPtr-- != patStr);
                result = prefReg;
            }
        }
    } else if (prefReg == 0)
        result = reqClass;
    else {
        intersect = ((reqClass & REG_INVMASK) ? ~reqClass : regBitMask[reqClass]) & ((prefReg & REG_INVMASK) ? ~prefReg : regBitMask[prefReg]);
        if (((reqClass & REG_INVMASK) ? ~reqClass : regBitMask[reqClass]) == intersect)
            result = reqClass;
        else if (((prefReg & REG_INVMASK) ? ~prefReg : regBitMask[prefReg]) == intersect)
            result = prefReg;
        else
            result = findAvailReg(intersect, reqClass);
    }
    return result;
}
/* end of local2.c*/
