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
#include "regcache.h"

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
static int exprCount = 0;
void emitExpr(struct expr *e)
{
    if (!e) return;
    exprCount++;
    if (TRACE(T_EXPR)) {
        fdprintf(2, "emitExpr: %d calls, op=%c (0x%x)\n", exprCount, e->op, e->op);
    }
    if (exprCount > 100000) {
        fdprintf(2, "emitExpr: exceeded 100000 calls, op=%c\n", e->op);
        exit(1);
    }

    /* Handle DEREF specially for tracing */
    if (TRACE(T_EXPR)) {
        fdprintf(2, "  asm_block=%p\n", (void*)e->asm_block);
    }
    /* Handle BC indirect load with caching - use opflags */
    if (e->op == 'M' && (e->opflags & OP_BCINDIR)) {
        emitBCIndir();
        freeNode(e);
        return;
    }
    /* Handle increment/decrement - Pattern 1 (e->symbol) or Pattern 2/3 (e->left) */
    else if ((e->op == 0xcf || e->op == 0xef || e->op == 0xd6 || e->op == 0xf6) &&
             (e->symbol || e->left)) {
        emitIncDec(e);
        return;
    }
    /* Handle ASSIGN - use op check */
    else if (e->op == '=') {
        if (TRACE(T_EXPR)) {
            fdprintf(2, "  calling emitAssign\n");
        }
        emitAssign(e);
        if (TRACE(T_EXPR)) {
            fdprintf(2, "  emitAssign returned, about to return from emitExpr\n");
        }
        return;
    }
    /* Optimize ADD with constant where left is register-allocated variable */
    else if (e->op == '+' && e->left && e->left->op == 'M' && e->size == 2 &&
             e->left->left && e->left->left->op == '$' && e->left->left->symbol &&
             !e->right) {
        emitAddConst(e);
        return;
    }
    /* Binary operators with accumulator management need special handling */
    else if (isBinopWAccum(e->op) && e->left && e->right) {
        emitBinop(e);
        return;
    }
    /* CALL operator - don't emit children, they're in the asm_block */
    else if (e->op == '@') {
        emitCall(e);
        return;
    }
    /* Handle ternary operator (? :) */
    else if (e->op == '?') {
        emitTernary(e);
        return;
    }
    /* Handle DEREF of register variable with caching - use opflags */
    else if (e->op == 'M' && (e->opflags & OP_REGVAR) &&
             e->left && e->left->op == '$') {
        emitRegVarDrf(e);
        return;
    }
    /* Handle DEREF of global with caching */
    else if (e->op == 'M' && (e->opflags & OP_GLOBAL) &&
             e->left && e->left->op == '$') {
        emitGlobDrf(e);
        return;
    }
    /* Handle DEREF of stack variable (IY-indexed) */
    else if (e->op == 'M' && (e->opflags & OP_IYMEM) &&
             e->left && e->left->op == '$') {
        emitStackDrf(e);
        return;
    }
    /* Handle DEREF with indirect addressing (loc=LOC_INDIR) */
    else if (e->op == 'M' && e->loc == LOC_INDIR) {
        /* Emit address calculation first */
        emitExpr(e->left);
        /* Then load through HL */
        if (e->size == 1) {
            fdprintf(outFd, "\tld a, (hl)\n");
            clearA();
        } else if (e->size == 2) {
            fdprintf(outFd, "\tld a, (hl)\n\tinc hl\n\tld h, (hl)\n\tld l, a\n");
            clearHL();
        }
        freeNode(e);
        return;
    }
    /* Handle symbol address - check if global or local */
    else if (e->op == '$' && e->symbol) {
        const char *sym_name = stripVarPfx(e->symbol);
        struct local_var *var = findVar(sym_name);

        if (!var) {
            /* Global symbol - load address */
            addRefSym(sym_name);  /* Track for extern declaration */
            if (e->dest == R_DE) {
                fdprintf(outFd, "\tld de, %s\n", sym_name);
                fnDEValid = 1;
            } else {
                int cached = cacheFindWord(e);
                if (cached == 'H') {
                    /* HL already has this address - skip load */
                } else {
                    fdprintf(outFd, "\tld hl, %s\n", sym_name);
                    cacheSetHL(e);
                }
            }
        } else {
            /* Local variable - compute address (IY + offset) */
            int ofs = var->offset;
            fdprintf(outFd, "\tpush iy\n\tpop hl\n");
            if (ofs != 0) {
                fdprintf(outFd, "\tld de, %d\n\tadd hl, de\n", ofs);
            }
            clearHL();
        }
        xfree(e->asm_block);
        free(e);
        return;
    }
    /* Handle constants with scheduler */
    else if (e->op == 'C' && e->loc == LOC_CONST) {
        if (e->size == 1) {
            if ((e->value & 0xff) == 0) {
                if (!fnAZero) {
                    fdprintf(outFd, "\txor a\n");
                    fnAZero = 1;
                }
            } else if (cacheFindByte(e) == 'A') {
                /* A already has this value */
            } else {
                fdprintf(outFd, "\tld a, %ld\n", e->value & 0xff);
                fnAZero = 0;
                cacheSetA(e);
            }
        } else {
            fdprintf(outFd, "\tld hl, %ld\n", e->value);
            clearHL();
        }
        freeNode(e);
        return;
    }
    else {
        /* Normal postorder traversal for other operators */
        if (e->left) emitExpr(e->left);
        if (e->right) emitExpr(e->right);

        if (e->asm_block) {
            if (e->asm_block[0]) {  /* Only if non-empty */
                fdprintf(outFd, "%s\n", e->asm_block);
            }
            /* Empty asm_block - don't emit anything */

            /* Check if this is a call to a comparison function that sets Z flag */
            if (strstr(e->asm_block, "call")) {
                char *call_pos;
                call_pos = strstr(e->asm_block, "call");
                /* Skip "call" and any whitespace/newlines to get function name */
                call_pos += 4;  /* Skip "call" */
                while (*call_pos && (*call_pos == ' ' || *call_pos == '\t' || *call_pos == '\n' || *call_pos == '\r')) {
                    call_pos++;
                }
                if (*call_pos && isCmpFunc(call_pos)) {
                    fnZValid = 1;
                }
            }
        }

        /* Emit deferred cleanup (for CALL stack cleanup after result used) */
        if (e->cleanup_block) {
            fdprintf(outFd, "%s", e->cleanup_block);
        }
    }

    /* Free this node (children already freed by recursive emit calls above) */
    xfree(e->asm_block);
    xfree(e->cleanup_block);
    if (e->jump) freeJump(e->jump);
    free(e);
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
