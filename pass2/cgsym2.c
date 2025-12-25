#include "cgen.h"


/*
 * File - sym2s.c
 */

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

/*
 * codeFragLists - Code fragment index lookup table
 *
 * Contains packed lists of codeFrag[] indices. Each list is a
 * sequence of indices terminated by a negative value.
 * The negative value is also a valid index (negated).
 *
 * opEmitValues[] contains starting indices into this array.
 * matchEmitPat() iterates through the list starting at that
 * index, trying each code fragment until one matches or
 * the list ends (negative value encountered).
 *
 * Example: list starting at index 1: {9, -10} means try
 * codeFrag[9] then codeFrag[10], then stop.
 */
static int codeFragLists[] = {
    0,    9,    -10,  25,   -28, 21,   25,   -28,  7,    21,   25,   28,   -29,  11,   12,
    13,   14,   -15,  4,    5,   21,   25,   -28,  6,    21,   25,   -28,  1,    21,   -28,
    17,   20,   21,   22,   25,  26,   28,   -29,  3,    21,   25,   -28,  4,    5,    20,
    21,   25,   -28,  2,    21,  25,   -28,  19,   23,   24,   -27,  20,   -28,  239,  -240,
    133,  -134, 46,   -47,  43,  44,   46,   -47,  43,   44,   46,   47,   -48,  43,   44,
    -47,  43,   44,   45,   46,  47,   -48,  43,   44,   45,   46,   -47,  45,   -47,  116,
    117,  118,  119,  120,  121, 122,  123,  124,  127,  130,

    131,  -132, 117,  118,  120, 125,  128,  131,  -132, 117,  118,  120,  126,  129,  131,
    -132, 245,  -246, 212,  214, -217, 218,  -221, 228,  229,  230,  231,  232,  233,  234,
    235,  -236, 211,  -227, 224, 225,  -226, 214,  -219, 213,  -214, 135,  -136, 71,   72,
    73,   -97,  93,   -94,  77,  78,   79,   80,   81,   -82,  85,   -86,  75,   -83,  73,
    -91,  87,   -88,  30,   -31, 145,  146,  -147, 143,  -144, 175,  -178, 161,  -207, 185,
    -204, 162,  163,  182,  183, 184,  -185, 161,  198,  199,  200,  -201, 173,  -180, 172,
    -181, 161,  185,  186,  187, 205,  206,  208,  209,  -210,

    169,  -171, 159,  165,  185, 194,  195,  196,  -203, 174,  -179, 163,  165,  188,  189,
    191,  -193, 190,  -192, 176, -179, 168,  -171, 177,  -178, 162,  -185, 161,  -163, 248,
    -249, 40,   41,   -42,  38,  39,   40,   41,   -42,  40,   -41,  37,   38,   39,   40,
    41,   -42,  38,   39,   40,  -41,  109,  -110, 243,  -244, 51,   -52,  152,  -153, 53,
    55,   -56,  54,   -55,  107, -108, 59,   60,   -61,  58,   -61,  58,   59,   -61,  105,
    -106, 101,  -102, 101,  102, -103, 156,  -157, 138,  -140
};

/*
 * matchEmitPat OK++ PMO	Used in: genExprCode
 *
 * Matches expression node against code fragment patterns for code generation.
 * Iterates through codeFragLists[] trying each fragment until one matches.
 * Handles register allocation, pattern validation, and recursive matching.
 *
 * Parameters:
 *   node        - expression node to match
 *   emitCode    - emit pattern code to use
 *   availRegs   - bitmask of available registers
 *   wantReg     - preferred result register (0 = any, TOPBIT set = mask)
 *   usedRegsOut - output: bitmask of registers used by matched pattern
 *
 * Returns: 1 on match, -1 if no pattern matches
 *
 * Note: one small block of code moved by optimiser otherwise code identical.
 * Needs standalone ported optimiser to optimise.
 */
int matchEmitPat(node_t *node, int emitCode, int availRegs, int wantReg, int *usedRegsOut) {
    int listIdx, leftUsed, rightUsed, rightRegs, fragVal, leftReg;
    uint16_t rightMask, leftMask;
    register struct codeFrag_t *frag;
    static int fragIdx;
    static char *patStr;
    static char *digitPtr;
    static char patCmd;
    static int regMask;
    static int selReg;

    if (node->nPat == 0)
        node->flags = 0;
    if ((listIdx = lookupEmitCode(emitCode, node->op)) == 0)
        return (-1);

    if (wantReg != 0 && TopBitSet(wantReg) == 0 && findAvailReg(availRegs, wantReg) == 0 &&
        (node->op != USEREG || findAvailReg(regBitMask[node->info.l], wantReg) == 0))
        return (-1);

    do {
        if (listIdx > 0)
            fragIdx = fragVal = codeFragLists[listIdx++];
        else
            fragIdx = fragVal = listIdx;

        if (fragIdx < 0)
            fragIdx = -fragIdx;

        frag = &codeFrag[fragIdx];

        if (frag->resultReg != 0 && findAvailReg(availRegs, frag->resultReg) == 0)
            continue;

        if ((uint8_t)frag->subMatch < 'H' && frag->leftPat != 0 &&
            (lookupEmitCode((uint8_t)frag->leftPat, node->info.np[0]->op) == 0 ||
             (frag->rightPat != 0 && lookupEmitCode(frag->rightPat, node->info.np[1]->op) == 0)))
            continue;
        if (frag->nodeTest != 0 && testPattern(node, frag->nodeTest) == 0)
            continue;

        leftReg = frag->leftReg;
        rightMask = 0;
        if (wantReg == 0 && frag->subMatch == 'E')
            wantReg = frag->leftReg;

        if (wantReg != 0 && (!TopBitSet(wantReg) || leftReg == 0 || findAvailReg(wantReg & REG_MASKMASK, leftReg) != 0)) {
            if ((patStr = frag->auxCode) != 0) {
                if (patStr[0] == 'L' && patStr[1] == 0)
                    patStr = "GL";
                while (*patStr) {
                    patCmd = *patStr;
                    if (patCmd == 'X' || patCmd == 'G' || (patCmd == 'S' && node->op == USEREG)) {
                        if (isdigit(*++patStr))
                            digitPtr = patStr;
                        else
                            digitPtr = 0;
                        while (*patStr < 'A')
                            patStr++;
                        switch (*patStr) {
                        case 'L':
                            if ((leftReg = selResultReg(leftReg, wantReg, availRegs, digitPtr)) == 0)
                                leftReg = -1;
                            break;
                        case 'R':
                            rightMask = wantReg;
                            break;
                        case 'N':
                            if (patCmd == 'S') {
                                if (node->op == USEREG &&
                                    selResultReg(leftReg, wantReg, regBitMask[node->info.l], digitPtr) == 0)
                                    leftReg = -1;
                            } else if (selCompatReg(availRegs, wantReg, frag->resultReg) == 0 &&
                                       (!TopBitSet(wantReg) || selResultReg(frag->resultReg, wantReg, availRegs, digitPtr) == 0))
                                leftReg = -1;
                            break;
                        default:
                            continue;
                        }
                        break;
                    } else
                        patStr++;
                }
            } else if ((leftReg = selResultReg(leftReg, wantReg, availRegs, 0)) == 0)
                leftReg = -1;
        }

        if (leftReg == -1)
            continue;
        rightRegs = 0;
        if ((uint8_t)frag->subMatch >= 'H') {
            node->nPat = 0;
            if (matchEmitPat(node, (uint8_t)frag->subMatch, availRegs, leftReg, &leftUsed) < 0) {
                node->nPat = 0;
                continue;
            }
        } else if (frag->leftPat != 0) {
            node->info.np[0]->nPat = 0;
            if (matchEmitPat(node->info.np[0], (uint8_t)frag->leftPat, availRegs, leftReg, &leftUsed) < 0)
                continue;
            if (frag->rightPat != 0) {
                node->info.np[1]->nPat = 0;
                leftMask                    = getUsedRegs(node->info.np[0]);
                if (rightMask == 0) {
                    if (frag->resultReg != 0)
                        rightMask = regBitMask[(uint8_t)selCompatReg(availRegs, wantReg, frag->resultReg)];
                    else
                        rightMask = 0;
                    rightMask = (leftMask | rightMask) ? (leftMask | rightMask | REG_INVMASK) : 0;
                }
                if (matchEmitPat(node->info.np[1], frag->rightPat, availRegs, rightMask, &rightUsed) < 0)
                    continue;
                rightRegs = getUsedRegs(node->info.np[1]);
                if (rightRegs & leftMask) {
                    node->info.np[1]->nPat = 0;
                    if (matchEmitPat(node->info.np[1], frag->rightPat, availRegs & ~leftMask, rightMask, &rightUsed) <= 0)
                        continue;
                    rightRegs = getUsedRegs(node->info.np[1]);
                }
                if ((dopetab[node->op] & DOPE_SIDEEFF) || (rightRegs & leftUsed)) {
                    if (leftMask & rightUsed) {
                        if (dopetab[node->op] & DOPE_SIDEEFF)
                            continue;
                        node->flags = 2;
                        rightRegs        = 0;
                    } else
                        node->flags = 1;
                } else
                    node->flags = 0;
            }
        } else if (leftReg != 0)
            wantReg = leftReg;

        if (frag->resultReg != 0) {
            regMask = availRegs;
            if (!(frag->resultReg & CF_REGCONSTRAINT) || (selReg = selCompatReg(getUsedRegs(node), wantReg, frag->resultReg)) == 0) {
                if ((frag->resultReg & CF_REGCONSTRAINT) == 0)
                    regMask &= ~getUsedRegs(node);
                regMask &= ~rightRegs;
                selReg = selCompatReg(regMask, wantReg, frag->resultReg);
                if (selReg == 0 && (selReg = findAvailReg(regMask, frag->resultReg)) == 0)
                    continue;
            }
            node->wantReg[node->nPat] = selReg;
        } else
            node->wantReg[node->nPat] = 0;

        node->pat[node->nPat++]    = (uint8_t)(frag - codeFrag);
        selReg                         = (uint8_t)getResultReg(node);
        node->resReg[node->nPat - 1] = selReg;
        if (selReg == 0 && node->op == USEREG)
            node->resReg[node->nPat - 1] = selReg = node->info.l;
        /* clang-format off */
        if ((wantReg != 0 && !TopBitSet(wantReg) && (selReg == 0 || findAvailReg(MapVal(wantReg), selReg) != selReg)) ||
            (TopBitSet(wantReg) && (MapVal(wantReg) & MapVal(selReg)) != MapVal(selReg))) {
                node->nPat--;
                continue;
            }
        /* clang-format on */
        *usedRegsOut = regBitMask[node->wantReg[node->nPat - 1]];

        if ((uint8_t)frag->subMatch >= 'H' || frag->leftPat != 0)
            *usedRegsOut |= leftUsed;
        if (frag->rightPat != 0)
            *usedRegsOut |= rightUsed;
        return 1;
    } while (fragVal >= 0);
    return -1;
}

/* end of file sym2s.c */

/* vim: tabstop=4 shiftwidth=4 noexpandtab: */
