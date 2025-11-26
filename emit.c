/*
 * emit.c - Code emission phase for cc2
 *
 * Walks statement trees, emitting assembly code and freeing nodes.
 * Expression emission is in emitexpr.c, helpers in emitops.c and emithelper.c.
 */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "cc2.h"
#include "emithelper.h"

/*
 * Walk statement tree, emit assembly, and free nodes
 */
static void emitStmt(struct function_ctx *ctx, struct stmt *s)
{
    if (!s) return;

    /* For ASM nodes, emit the assembly block directly */
    if (s->type == 'A' && s->asm_block) {
        fdprintf(outFd, "%s\n", s->asm_block);
    }

    /* Handle IF statements specially */
    if (s->type == 'I') {
        int invertCond = 0;
        int use_dir_jump = 0;
        struct expr *cond = s->expr;

        /* Check if condition has ! wrapper */
        if (cond && cond->op == '!') {
            invertCond = 1;
            cond = cond->left;  /* unwrap ! */
        }

        /* Check if this is a byte operation that sets Z flag */
        if (cond && (cond->op == '&' || cond->op == '|' || cond->op == '^') &&
            cond->left && cond->left->size == 1 &&
            cond->right && cond->right->op == 'C' &&
            cond->right->value >= 0 && cond->right->value <= 255) {
            use_dir_jump = 1;
        }
        else if (cond && cond->size == 1) {
            use_dir_jump = 1;
        }

        if (use_dir_jump) {
            emitExpr(ctx, cond);

            if (invertCond && s->expr != cond) {
                free(s->expr);
            }

            if (s->label2 > 0) {
                if (invertCond) {
                    emitJump("jp nz,", "_if_", s->label);
                } else {
                    emitJump("jp z,", "_if_", s->label);
                }
            } else {
                if (invertCond) {
                    emitJump("jp nz,", "_if_end_", s->label);
                } else {
                    emitJump("jp z,", "_if_end_", s->label);
                }
            }
        } else {
            /* Non-byte or complex expression */
            struct local_var *var = NULL;
            if (cond && cond->op == 'M' && cond->size == 2 &&
                cond->left && cond->left->op == '$' && cond->left->symbol) {
                var = findVar(ctx, stripVarPfx(cond->left->symbol));
            }

            if (var && (var->reg == REG_BC || var->reg == REG_BCp)) {
                if (var->reg == REG_BC) {
                    emit(S_ABCORC);
                } else {
                    emit(S_EXXABCORC);
                }
                freeExpr(cond);
                if (invertCond && s->expr != cond) {
                    free(s->expr);
                }
            } else {
                emitExpr(ctx, s->expr);
                if (!ctx->zflag_valid) {
                    emit(S_AHORL);
                }
                ctx->zflag_valid = 0;
            }

            if (s->label2 > 0) {
                emitJump(invertCond ? "jp nz," : "jp z,", "_if_", s->label);
            } else {
                emitJump(invertCond ? "jp nz," : "jp z,", "_if_end_", s->label);
            }
        }

        /* Emit then branch */
        if (s->then_branch) emitStmt(ctx, s->then_branch);

        if (s->label2 > 0) {
            emitJump("jp", "_if_end_", s->label2);
            fdprintf(outFd, "_if_%d:\n", s->label);
            if (s->else_branch) emitStmt(ctx, s->else_branch);
        }

        if (s->next) emitStmt(ctx, s->next);

        if (s->asm_block) free(s->asm_block);
        if (s->jump) freeJump(s->jump);
        free(s);
        return;
    }
    /* Handle RETURN statements specially */
    else if (s->type == 'R') {
        if (s->expr) {
            emitExpr(ctx, s->expr);
            if (strcmp(ctx->rettype, "_long_") == 0 && s->expr->size == 2) {
                emit(S_ZEXTSL);
                emit(S_EXX);
                emit(S_HLZERO);
                emit(S_EXX);
            }
        }
        fdprintf(outFd, "\tjp %sX\n", ctx->name);
    } else {
        /* Emit expressions (this frees them) */
        if (s->expr) emitExpr(ctx, s->expr);
        if (s->expr2) emitExpr(ctx, s->expr2);
        if (s->expr3) emitExpr(ctx, s->expr3);
    }

    /* Emit child statements (this frees them) */
    if (s->then_branch) emitStmt(ctx, s->then_branch);
    if (s->else_branch) emitStmt(ctx, s->else_branch);

    /* Emit next statement in chain (this frees it) */
    if (s->next) emitStmt(ctx, s->next);

    /* Free this node only (children already freed by recursive emit calls) */
    if (s->asm_block) free(s->asm_block);
    if (s->jump) freeJump(s->jump);
    free(s);
}

/*
 * Emit assembly for entire function and free tree
 */
void emitAssembly(struct function_ctx *ctx, int fd)
{
    struct local_var *var, *next;
    int has_params;

    if (!ctx || !ctx->body) return;

    /* Initialize label map for jump optimization */
    lblMapCnt = 0;

    /* Scan statement tree to build label map */
    scanLabJumps(ctx->body);

    has_params = (ctx->params && ctx->params[0]);

    /* Emit function prologue */
    emitFnProlog(ctx->name, ctx->params, ctx->rettype,
        ctx->frame_size, ctx->locals);

    /* Emit function body */
    emitStmt(ctx, ctx->body);

    /* Emit function exit label */
    fdprintf(outFd, "%sX:\n", ctx->name);

    /* Restore callee-saved registers */
    {
        int used = getUsedRegs(ctx->locals);
        if (used & 2) emit(S_POPIX);
        if (used & 1) emit(S_POPBC);
    }

    /* Emit function epilogue */
    if (ctx->frame_size > 0 || has_params) {
        emit(S_JPFF);
    } else {
        emit(S_RET);
    }

    /* Free local variables list */
    var = ctx->locals;
    while (var) {
        next = var->next;
        free(var->name);
        free(var);
        var = next;
    }
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
