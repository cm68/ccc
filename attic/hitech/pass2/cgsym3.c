#include "cgen.h"

/*
 * File - sym3.c
 */

/*
 * calcUsedRegs - Calculate registers used by a code pattern
 *
 * Parses the code fragment pattern string at index 'par' and
 * accumulates which registers are referenced. Handles:
 *   X,G: Extract register from node, decompose pairs if needed
 *   L: Recurse into left child or previous pattern level
 *   R: Recurse into right child
 *   Other letters: Skip to next position marker
 *
 * Returns bitmask of used registers ANDed with availRegs
 * (available registers).
 */
int calcUsedRegs(register node_t *node, int patIdx) {
    char *ptr;
    struct codeFrag_t *frag;
    node_t *targetNode FORCEINIT;
    char *digitPtr;
    uint8_t cmd;
    int targetPatIdx;
    int usedRegs;

    frag = &codeFrag[node->pat[patIdx]];
    usedRegs = 0;

    for (ptr = frag->auxCode; *ptr;) {
        switch (cmd = *(ptr++)) {
        case 'X':
        case 'G':
            if (isdigit(*ptr)) {
                digitPtr = ptr;
                while (isdigit(*ptr))
                    ptr++;
            } else
                digitPtr = 0;

            switch (*ptr++) {
            case 'N':
                targetPatIdx = patIdx;
                targetNode = node;
                break;
            case 'L':
                if (patIdx > 0) {
                    targetNode = node;
                    targetPatIdx = patIdx - 1;
                } else {
                    targetNode = node->info.np[0];
                    targetPatIdx = targetNode->nPat - 1;
                }
                break;
            case 'R':
                targetNode = node->info.np[1];
                targetPatIdx = targetNode->nPat - 1;
                break;
            }
            if (cmd == 'S') {
                if (targetNode->op == USEREG)
                    cmd = (uint8_t)targetNode->info.l;
                else
                    cmd = 0;
            } else {
                cmd = cmd == 'G' ? targetNode->resReg[targetPatIdx] : targetNode->wantReg[targetPatIdx];
                if (digitPtr)
                    while (isdigit(*digitPtr) != 0)
                        cmd = regPairHiLo[cmd * 2 + (*digitPtr++ - '0')];
            }
            usedRegs |= regBitMask[cmd];
            if (cmd != 0)
                lastResultReg = cmd;
            break;
        case 'A':
        case 'B':
        case 'C':
        case 'F':
        case 'M':
        case 'O':
        case 'P':
        case 'Q':
        case 'S':
        case 'T':
        case 'V':
        case 'W':
        case 'Z':
            while (*ptr && *ptr != 'N' && *ptr != 'L' && *ptr != 'R')
                ptr++;
            break;
        case '~':
            while (*ptr && *ptr != 'L' && *ptr != 'R')
                ptr++;
            break;
        case 'L':
            if (patIdx > 0)
                usedRegs |= calcUsedRegs(node, patIdx - 1);
            else
                usedRegs |= getUsedRegs(node->info.np[0]);

            break;
        case 'R':
            usedRegs |= getUsedRegs(node->info.np[1]);
            break;
        case 'D':
        case 'U':
            break;
        }
    }
    return usedRegs & availRegs;
}

/*
 * getUsedRegs - Get registers used by an expression node
 *
 * Returns bitmask of registers used by the node's selected
 * code pattern. For unmatched nodes (nPat == 0), recurses
 * into left child for LOGOP/QUEST nodes.
 */
int getUsedRegs(register node_t *node) {

    if (node->nPat == 0) {
        if ((dopetab[node->op] & DOPE_OPCOUNT) == 0)
            return 0;
        return getUsedRegs(node->info.np[0]); /* LOGOP & QUEST */
    }
    return calcUsedRegs(node, node->nPat - 1);
}

/*
 * getResultReg - Get the result register for an expression
 *
 * Determines which register will hold the result of evaluating
 * the expression. Uses the register constraint from the last
 * pattern match (wantReg), or falls back to lastResultReg set during
 * getUsedRegs traversal.
 */
uint8_t getResultReg(register node_t *node) {
    uint8_t wantedReg;
    int usedRegs;

    wantedReg     = node->wantReg[node->nPat - 1];
    lastResultReg = 0;
    usedRegs      = getUsedRegs(node);
    if (wantedReg != 0 && findAvailReg(usedRegs, wantedReg) == wantedReg)
        return wantedReg;
    return lastResultReg;
}
/* end of file sym3.c */

/* vim: set tabstop=4 shiftwidth=4 noexpandtab: */
