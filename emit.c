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
static int stmt_count = 0;
static void emitStmt(struct function_ctx *ctx, struct stmt *s)
{
    if (!s) return;
    stmt_count++;
    if (TRACE(T_EMIT)) {
        fdprintf(2, "emitStmt #%d type=%c\n", stmt_count, s->type);
    }
    if (stmt_count > 100000) {
        fdprintf(2, "emitStmt: exceeded 100000 statements\n");
        exit(1);
    }

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
        if (TRACE(T_EMIT)) {
            fdprintf(2, "  emitStmt: has expr=%p\n", (void*)s->expr);
        }
        if (s->expr) emitExpr(ctx, s->expr);
        if (TRACE(T_EMIT)) {
            fdprintf(2, "  emitStmt: after expr, expr2=%p\n", (void*)s->expr2);
        }
        if (s->expr2) emitExpr(ctx, s->expr2);
        if (TRACE(T_EMIT)) {
            fdprintf(2, "  emitStmt: after expr2, expr3=%p\n", (void*)s->expr3);
        }
        if (s->expr3) emitExpr(ctx, s->expr3);
        if (TRACE(T_EMIT)) {
            fdprintf(2, "  emitStmt: after expr3\n");
        }
    }

    /* Emit child statements (this frees them) */
    if (TRACE(T_EMIT)) {
        fdprintf(2, "  emitStmt: checking then_branch=%p\n", (void*)s->then_branch);
    }
    if (s->then_branch) emitStmt(ctx, s->then_branch);
    if (TRACE(T_EMIT)) {
        fdprintf(2, "  emitStmt: checking else_branch=%p\n", (void*)s->else_branch);
    }
    if (s->else_branch) emitStmt(ctx, s->else_branch);

    /* Emit next statement in chain (this frees it) */
    if (TRACE(T_EMIT)) {
        fdprintf(2, "  emitStmt: checking next=%p\n", (void*)s->next);
    }
    if (s->next) emitStmt(ctx, s->next);
    if (TRACE(T_EMIT)) {
        fdprintf(2, "  emitStmt: about to free stmt %p\n", (void*)s);
    }

    /* Free this node only (children already freed by recursive emit calls) */
    if (TRACE(T_EMIT)) {
        fdprintf(2, "  freeing asm_block=%p\n", (void*)s->asm_block);
    }
    if (s->asm_block) free(s->asm_block);
    if (TRACE(T_EMIT)) {
        fdprintf(2, "  freeing jump=%p\n", (void*)s->jump);
    }
    if (s->jump) freeJump(s->jump);
    if (TRACE(T_EMIT)) {
        fdprintf(2, "  freeing stmt\n");
    }
    free(s);
    if (TRACE(T_EMIT)) {
        fdprintf(2, "  emitStmt DONE\n");
    }
}

/*
 * Emit assembly for entire function and free tree
 */
void emitAssembly(struct function_ctx *ctx, int fd)
{
    struct local_var *var, *next;
    int has_params;

    if (!ctx || !ctx->body) return;
    stmt_count = 0;
    if (TRACE(T_EMIT)) {
        fdprintf(2, "emit: function %s\n", ctx->name);
    }

    /* Initialize label map for jump optimization */
    lblMapCnt = 0;

    /* Scan statement tree to build label map */
    scanLabJumps(ctx->body);

    has_params = (ctx->params && ctx->params[0]);

    /* Emit function prologue */
    emitFnProlog(ctx->name, ctx->params, ctx->rettype,
        ctx->frame_size, ctx->locals);

    /* Emit function body */
    if (TRACE(T_EMIT)) {
        fdprintf(2, "emit: calling emitStmt\n");
    }
    emitStmt(ctx, ctx->body);
    if (TRACE(T_EMIT)) {
        fdprintf(2, "emit: emitStmt returned\n");
    }

    /* Emit function exit label */
    fdprintf(outFd, "%sX:\n", ctx->name);
    if (TRACE(T_EMIT)) {
        fdprintf(2, "emit: emitted exit label\n");
    }

    /* Restore callee-saved registers (reverse order of push) */
    {
        int used = getUsedRegs(ctx->locals);
        if (used & 4) emit(S_EXXPOPBC);  /* restore BC' via exx; pop bc; exx */
        if (used & 2) emit(S_POPIX);
        if (used & 1) emit(S_POPBC);
    }
    if (TRACE(T_EMIT)) {
        fdprintf(2, "emit: restored callee regs\n");
    }

    /* Emit function epilogue */
    if (ctx->frame_size > 0 || has_params) {
        emit(S_JPFF);
    } else {
        emit(S_RET);
    }
    if (TRACE(T_EMIT)) {
        fdprintf(2, "emit: emitted epilogue\n");
    }

    /* Free local variables list */
    if (TRACE(T_EMIT)) {
        fdprintf(2, "emit: freeing locals\n");
    }
    var = ctx->locals;
    while (var) {
        if (TRACE(T_EMIT)) {
            fdprintf(2, "emit: freeing var %s\n", var->name);
        }
        next = var->next;
        free(var->name);
        free(var);
        var = next;
    }
    if (TRACE(T_EMIT)) {
        fdprintf(2, "emit: function %s done\n", ctx->name);
    }
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
