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
void emitExpr(struct function_ctx *ctx, struct expr *e)
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

    /* Handle DEREF specially - need to check register allocation */
    if (TRACE(T_EXPR)) {
        fdprintf(2, "  asm_block=%p\n", (void*)e->asm_block);
    }
    if (e->op == 'M' && e->asm_block &&
            strstr(e->asm_block, "DEREF_PLACEHOLDER")) {
        struct local_var *var = NULL;
        const char *global_sym = NULL;
        struct expr *temp;
        if (TRACE(T_EXPR)) {
            fdprintf(2, "  => DEREF_PLACEHOLDER case\n");
            fdprintf(2, "  hl_cache=%p\n", (void*)ctx->hl_cache);
            fdprintf(2, "  calling matchesCache\n");
        }

        /* Check cache first - if value already in HL, no code needed */
        if (ctx->hl_cache && matchesCache(e, ctx->hl_cache)) {
            if (TRACE(T_EXPR)) {
                fdprintf(2, "  matched hl_cache\n");
            }
            /* Value already in HL - mark as generated and return */
            e->flags |= E_GENERATED;
            freeExpr(e->left);
            freeNode(e);
            return;
        }
        if (TRACE(T_EXPR)) {
            fdprintf(2, "  checking de_cache\n");
        }

        /* Check if value is in DE - swap if so */
        if (ctx->de_cache && matchesCache(e, ctx->de_cache)) {
            /* Value in DE - swap DE and HL */
            emit(S_CACHESWP);

            /* Swap caches */
            temp = ctx->hl_cache;
            ctx->hl_cache = ctx->de_cache;
            ctx->de_cache = temp;

            /* Mark as generated and return */
            e->flags |= E_GENERATED;
            freeExpr(e->left);
            freeNode(e);
            return;
        }

        /* Look up variable BEFORE emitting child (which frees it) */
        if (TRACE(T_EXPR)) {
            fdprintf(2, "  looking up var, e->left=%p\n", (void*)e->left);
        }

        if (e->left && e->left->op == '$' && e->left->symbol) {
            if (TRACE(T_EXPR)) {
                fdprintf(2, "  found symbol: %s\n", e->left->symbol);
            }
            var = findVar(ctx, e->left->symbol);
            /* If not found as local var, it's a global - save symbol name */
            if (!var) {
                if (TRACE(T_EXPR)) {
                    fdprintf(2, "  not a local, saving as global_sym\n");
                }
                global_sym = e->left->symbol;
            }
        }

        /* Free child without emitting (we'll emit the load ourselves) */
        if (TRACE(T_EXPR)) {
            fdprintf(2, "  calling freeExpr on e->left\n");
        }
        freeExpr(e->left);
        if (TRACE(T_EXPR)) {
            fdprintf(2, "  after freeExpr\n");
        }

        /* Now emit the load inst based on current register allocation */
        if (var) {
            if (var->reg != REG_NO) {
                /* Variable is in a register - move to PRIMARY */
                if (e->size == 1) {
                    /* Byte: move register to A */
                    if (var->reg == REG_B) {
                        emit(S_LDAB);
                    } else if (var->reg == REG_C) {
                        emit(S_LDAC);
                    } else if (var->reg == REG_Bp) {
                        emit(S_EXXLDAB);
                    } else if (var->reg == REG_Cp) {
                        emit(S_EXXLDAC);
                    }
                } else {
                    /* Word: move register pair to HL */
                    if (var->reg == REG_BC) {
                        emit(S_BCHL);
                    } else if (var->reg == REG_IX) {
                        emit(S_IXHL);
                    }
                }
            } else {
                /* Variable is on stack - load from (iy + offset) */
                if (e->size == 1) {
                    loadByteIY(var->offset, var->offset >= 0);
                } else if (e->size == 2) {
                    loadWordIY(var->offset);
                } else {
                    fdprintf(outFd, "\tld a, %d\n", var->offset);
                    emit(S_CALLGL);
                }
            }
        } else if (global_sym) {
            /* Global variable - direct memory access */
            const char *sym = stripDollar(global_sym);
            if (e->size == 1) {
                fdprintf(outFd, "\tld a, (%s)\n", sym);
            } else if (e->size == 2) {
                fdprintf(outFd, "\tld hl, (%s)\n", sym);
            } else if (e->size == 4) {
                fdprintf(outFd, "\tld hl, (%s)\n", sym);
                emit(S_EXX);
                fdprintf(outFd, "\tld hl, (%s+2)\n", sym);
                emit(S_EXX);
            }
        }

        /* Value now in HL (TOS) - Z flag may be invalid */
        ctx->zflag_valid = 0;

        /* Save expression to cache */
        if (TRACE(T_EXPR)) {
            fdprintf(2, "  clearing HL cache\n");
        }
        clearHL(ctx);
        if (TRACE(T_EXPR)) {
            fdprintf(2, "  making shallow copy\n");
        }
        ctx->hl_cache = shallowCopy(e);
        if (TRACE(T_EXPR)) {
            fdprintf(2, "  freeing node and returning\n");
        }

        /* Free this node and return */
        freeNode(e);
        if (TRACE(T_EXPR)) {
            fdprintf(2, "  DEREF case done\n");
        }
        return;
    }
    /* Handle DEREF with struct member access (marked by flags in codegen) */
    else if (e->op == 'M' && (e->flags & 2) && e->left && e->left->left &&
             e->left->left->left && e->left->left->left->op == '$' &&
             e->left->left->left->symbol) {
        /* Pattern: (M (+ (M:p $var) const)) where $var is in IX */
        const char *var_symbol;
        long offset;
        const char *var_name;
        struct local_var *var;

        var_symbol = e->left->left->left->symbol;
        offset = e->value;  /* Saved by codegen phase */
        var_name = stripVarPfx(var_symbol);
        var = findVar(ctx, var_name);

        if (var && var->reg == REG_IX) {
            /* Variable is in IX - use IX-indexed addressing */
            if (e->size == 1) {
                fdprintf(outFd, "\tld a, (ix + %d)\n", (char)offset);
            } else if (e->size == 2) {
                loadWordIX((char)offset);
            } else if (e->size == 4) {
                loadWordIX((char)offset);
                emit(S_EXX);
                loadWordIX((char)(offset + 2));
                emit(S_EXX);
            }

            /* Free child expression tree without emitting code */
            freeExpr(e->left);

            /* Free this node */
            freeNode(e);
            return;
        } else {
            /* Not IX-allocated - fall back to computing address and dereferencing */
            /* Emit child expression (computes address to HL) */
            emitExpr(ctx, e->left);

            /* Emit load from (HL) */
            if (e->size == 1) {
                emit(S_AHL);
            } else if (e->size == 2) {
                emit(S_LDEDHLSWP);
            } else if (e->size == 4) {
                emit(S_CALLL32I);
            }

            /* Free this node */
            freeNode(e);
            return;
        }
    }
    /* Handle DEREF_INDIRECT_B - byte load through pointer variable */
    else if (e->op == 'M' && e->asm_block &&
            strstr(e->asm_block, "DEREF_INDIRECT_B:")) {
        /* Extract symbol name from placeholder */
        const char *sym_start;
        char symbol[256];
        int i;
        const char *var_name;
        struct local_var *var;

        sym_start = strstr(e->asm_block, "DEREF_INDIRECT_B:") + 17;
        i = 0;
        while (sym_start[i] && sym_start[i] != '\n' && i < 255) {
            symbol[i] = sym_start[i];
            i++;
        }
        symbol[i] = '\0';
        var_name = stripVarPfx(symbol);
        var = findVar(ctx, var_name);

        if (var && var->reg != REG_NO) {
            /* Pointer is register-allocated - use indirect addressing */
            if (var->reg == REG_BC) {
                emit(S_LDABC);
            } else if (var->reg == REG_IX) {
                emit(S_LDAIXZ);
            } else {
                /* Register type can't do indirect addressing - fall back */
                /* Load pointer to HL first, then indirect load */
                if (var->reg == REG_B || var->reg == REG_C ||
                    var->reg == REG_Bp || var->reg == REG_Cp) {
                    /* Single byte register shouldn't hold pointer, but handle it */
                    emit(S_WARNBPTR);
                }
                emit(S_FBKNORM);
                /* Emit the inner DEREF to load pointer */
                emitExpr(ctx, e->left);
                emit(S_AHL);
            }
        } else {
            /* Pointer not register-allocated - load to HL then indirect */
            emitExpr(ctx, e->left);
            emit(S_AHL);
        }

        /* Free outer node (don't free e->left, already freed above if emitted) */
        if (!var || var->reg == REG_NO ||
            (var->reg != REG_BC && var->reg != REG_IX)) {
            /* e->left was emitted and freed, just free this node */
        } else {
            /* e->left was not emitted, need to free it manually */
            freeExpr(e->left);
        }
        freeNode(e);
        return;
    }
    /* Handle increment/decrement placeholders - need to check register allocation */
    else if (e->asm_block && strstr(e->asm_block, "INCDEC_PLACEHOLDER:")) {
        emitIncDec(ctx, e);
        return;
    }
    /* Handle ASSIGN specially - need to check register allocation */
    else if (e->op == '=' && e->asm_block &&
            strstr(e->asm_block, "ASSIGN_PLACEHOLDER")) {
        if (TRACE(T_EXPR)) {
            fdprintf(2, "  calling emitAssign\n");
        }
        emitAssign(ctx, e);
        if (TRACE(T_EXPR)) {
            fdprintf(2, "  emitAssign returned, about to return from emitExpr\n");
        }
        return;
    }
    /* Optimize ADD with constant where left is register-allocated variable */
    else if (e->op == '+' && e->left && e->left->op == 'M' && e->size == 2 &&
             e->left->left && e->left->left->op == '$' && e->left->left->symbol &&
             !e->right) {
        emitAddConst(ctx, e);
        return;
    }
    /* Binary operators with accumulator management need special handling */
    else if (isBinopWAccum(e->op) && e->left && e->right &&
            e->asm_block) {
        emitBinop(ctx, e);
        return;
    }
    /* CALL operator - don't emit children, they're in the asm_block */
    else if (e->op == '@') {
        emitCall(ctx, e);
        return;
    }
    /* Handle SYM - load variable value to PRIMARY */
    else if (e->op == '$' && e->symbol) {
        loadVar(ctx, e->symbol, 2, 0);
    }
    /* Handle ternary operator (? :) */
    else if (e->op == '?') {
        emitTernary(ctx, e);
        return;
    }
    else {
        /* Normal postorder traversal for other operators */
        if (e->left) emitExpr(ctx, e->left);
        if (e->right) emitExpr(ctx, e->right);

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
                    ctx->zflag_valid = 1;
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
