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
    /* Handle increment/decrement placeholders - need to check register allocation */
    if (e->asm_block && strstr(e->asm_block, "INCDEC_PLACEHOLDER:")) {
        emitIncDec(e);
        return;
    }
    /* Handle ASSIGN specially - need to check register allocation */
    else if (e->op == '=' && e->asm_block &&
            strstr(e->asm_block, "ASSIGN_PLACEHOLDER")) {
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
    else if (isBinopWAccum(e->op) && e->left && e->right &&
            e->asm_block) {
        emitBinop(e);
        return;
    }
    /* CALL operator - don't emit children, they're in the asm_block */
    else if (e->op == '@') {
        emitCall(e);
        return;
    }
    /* Handle SYM - load variable value to PRIMARY */
    else if (e->op == '$' && e->symbol) {
        loadVar(e->symbol, 2, 0);
    }
    /* Handle ternary operator (? :) */
    else if (e->op == '?') {
        emitTernary(e);
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
    if (e->asm_block) free(e->asm_block);
    if (e->cleanup_block) free(e->cleanup_block);
    if (e->jump) freeJump(e->jump);
    free(e);
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
