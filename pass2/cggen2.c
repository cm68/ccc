/*
 * File - cgen2s.c	BIG size - problems compiling v3.09
 */
#include "cgen.h"
/*********************************************************
 * emitCodePat - Emit assembly code from a pattern string
 *
 * Interprets code pattern strings and emits assembly output.
 * Pattern characters include:
 *   D: Print accumulated emitOffset
 *   S: Print symbol name, register, or constant
 *   N: Reference operand node (L=left, R=right)
 *   Z/B: Print node/element sizes
 *   ~: Stack offset calculations
 *   G/X: Other code generation directives
 *********************************************************/
void emitCodePat(register node_t *node, char *pattern, char patIdx) {
    struct codeFrag_t *frag FORCEINIT;
    node_t *targetNode FORCEINIT;
    char *digitPtr FORCEINIT;
    uint8_t regNum;
    char offset;
    char operand;
    char cmd;
    int modifier;
    long sizeVal;
    int targetPatIdx;
    char loopFlag;
    char *lineStart;
    uint32_t accumVal;
    bool indented;

    lineStart = pattern;
    loopFlag = indented = false;
    for (;;) {
        if ((cmd = *pattern++) == 0)
            return;
        switch (cmd) {
        case ' ':
            if (!indented) {
                fputchar('\t');
                indented = true;
            } else
                fputchar(cmd);
            continue;
        case ';':
            return;
        case '\n':
        case '\r':
            fputchar('\n');
            indented = false;
            continue;
        case 'G':
        case 'X':
            if (isdigit(*pattern)) {
                digitPtr = pattern;
                while (isdigit(*pattern))
                    pattern++;
            } else
                digitPtr = 0;
            operand = *pattern++;
            modifier  = 'G';
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
        case '~':
            if ((modifier = *pattern) == '-' || modifier == '+') {
                if (isdigit(*++pattern)) {
                    offset = atoi(pattern);
                    if (modifier == '-')
                        offset = -offset;
                    modifier = 0;
                } else
                    offset = 0;
                while (isdigit(*pattern) || *pattern == '+' || *pattern == '-')
                    pattern++;
            } else if (modifier == '>') {
                for (offset = 0; *pattern == '>'; pattern++, offset++)
                    ;
            } else {
                modifier  = 0;
                offset = 0;
            }

            operand = *pattern++;
            break;
        case 'L':
        case 'R':
        case 'U':
            operand = cmd;
            cmd = '~';
            modifier  = 0;
            offset = 0;
            break;
        case 'D':
            operand = cmd;
            modifier  = 0;
            offset = 0;
            break;

        default:
            fputchar(cmd);
            continue;
        }

        switch (operand) {
        case 'L':
            if (patIdx > 0) { /* m27: */
                targetNode  = node;
                targetPatIdx = patIdx - 1;
                frag  = &codeFrag[node->pat[targetPatIdx]];
            } else {
                targetNode  = node->info.np[0];
                targetPatIdx = targetNode->nPat - 1;
                frag  = &codeFrag[targetNode->pat[targetPatIdx]];
            }
            break;
        case 'R':
            targetNode  = node->info.np[1];
            targetPatIdx = targetNode->nPat - 1;
            frag  = &codeFrag[targetNode->pat[targetPatIdx]];
            break;
        case 'N':
            targetNode  = node;
            targetPatIdx = patIdx;
            break;
        }
        switch (cmd) {
        case 'D':
            if (emitOffset != 0) {
                printf("%d", emitOffset);
                emitOffset = 0;
            }
            break;
        case 'S':
            if (targetNode->op == USEREG)
                printf("%s", regNames[targetNode->info.l]);
            else if (targetNode->op == CONST)
                prSignedVal(targetNode, targetNode->info.l);
            else if (targetNode->op == FCONST)
                printf("%s", targetNode->info.sv.s);
            else
                printf("%s", targetNode->info.mp[0]->name);
        proff:
            if (modifier == 0) {
                offset += emitOffset;
                emitOffset = 0;
                if (offset > 0)
                    printf("+%d", offset);
                else if (offset < 0)
                    printf("%d", offset);
            }
            break;
        case 'F':
            printf("%d", targetNode->info.sv.v);
            goto proff;

        case 'Z':
            sizeVal = nodesize(targetNode);
            goto m46;
        case 'B':
            sizeVal = derefSize(targetNode);
        m46:
            if (modifier == '-') {
                if (loopFlag == 0) {
                    accumVal      = sizeVal + (long)emitOffset;
                    emitOffset = 0;
                    loopFlag      = 1;
                }
                goto m54;
            }
            printf("%lu", sizeVal);
            break;
        case 'O':
            printf("%s", otherFrag[targetNode->op]);
            break;
        case 'A':
            prTypeChar(targetNode);
            break;
        case 'V':
            if (modifier == '-' || modifier == '+') {
                if (loopFlag == 0) {
                    accumVal      = targetNode->info.l + (long)emitOffset;
                    emitOffset = 0;
                    loopFlag      = 1;
                }
            m54:
                accumVal += modifier == '-' ? -1L : 1L;
                if (getTypeClass(targetNode) == 2 && nodesize(targetNode) < 4)
                    accumVal &= (1 << (int)(nodesize(targetNode) * 8)) - 1;

                if (accumVal != 0) {
                    while (lineStart < pattern && pattern[-1] != '\n')
                        pattern--;
                    fputchar('\n');
                } else
                    loopFlag = 0;
            } else {
                offset += emitOffset;
                emitOffset = 0;
                prSignedVal(targetNode, targetNode->info.l >> (offset * 8));
            }
            break;

        case 'C':
            if (targetNode->op == BFIELD)
                printf("%d", targetNode->info.mp[1]->bOffset);
            else {
                sizeVal = targetNode->info.l;
                if (isPow2Bit(sizeVal) == 0)
                    sizeVal = ~sizeVal;
                emitOffset += (isPow2Bit(sizeVal) - 1) / 8;
            }
            break;
        case 'T':
            if (targetNode->op == BFIELD)
                printf("%d", targetNode->info.mp[1]->bWidth);
            else {
                sizeVal = targetNode->info.l;
                if (isPow2Bit(sizeVal) == 0)
                    sizeVal = ~sizeVal;
                printf("%d", isPow2Bit(sizeVal) - 1);
            }
            break;
        case 'G':
        case 'X':
            regNum = cmd == 'G' ? targetNode->resReg[targetPatIdx] : targetNode->wantReg[targetPatIdx];

            if (digitPtr != 0)
                while (isdigit(*digitPtr))
                    regNum = regPairHiLo[regNum * 2 + (*digitPtr++ - '0')];

            if (*pattern != '+') {
                regNum += emitOffset;
                emitOffset = 0;
            }
            printf("%s", regNames[regNum]);
            break;
        case '~':
            if (operand == 'U') {
                offset += exprNestDepth;
                if ((dopetab[node->op] & DOPE_SIGNED) && patIdx != 0)
                    offset--;
                printf("%d", offset);
            } else {
                emitOffset += offset;
                emitCodePat(targetNode, frag->auxCode, targetPatIdx);
            }
            break;
        }
    }
}

/* end of file cgen2s.c */
