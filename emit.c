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
 * Check if else branch is just a GOTO, return target symbol or NULL
 */
static const char *elseGotoTgt(struct stmt *else_branch) {
    struct stmt *s = else_branch;
    /* Unwrap block: (B (G label)) */
    if (s && s->type == 'B' && s->then_branch && !s->then_branch->next) {
        s = s->then_branch;
    }
    /* Check for GOTO */
    if (s && s->type == 'G' && s->symbol) {
        return s->symbol;
    }
    return NULL;
}

/*
 * Emit conditional expression with explicit true/false targets.
 * For AND/OR, recursively handles nested short-circuit logic.
 * true_lbl/true_num: where to jump when condition is true (-1 = fall through)
 * false_lbl/false_num: where to jump when condition is false (-1 = fall through)
 */
static void emitCond(struct expr *e, int invert,
                     const char *true_lbl, int true_num,
                     const char *false_lbl, int false_num)
{
    int e_size;
    if (!e) return;
    e_size = e->size;

    /* Handle NOT - invert sense and unwrap */
    if (e->op == '!') {
        emitCond(e->left, !invert, true_lbl, true_num, false_lbl, false_num);
        freeNode(e);
        return;
    }

    /* Handle OR (||) */
    if (e->op == 'h') {
        /* For OR: if left is true, skip rest of OR; if false, try right */
        int skip_lbl = fnLblCnt++;
        /* Left: if true -> skip to end of OR; if false -> continue to right */
        /* Pass skip_lbl as true target so we jump past the OR on success */
        emitCond(e->left, invert, "_or_end_", skip_lbl, NULL, -1);
        /* Right: if true -> original true_target; if false -> original false_target */
        emitCond(e->right, invert, true_lbl, true_num, false_lbl, false_num);
        fdprintf(outFd, "_or_end_%d:\n", skip_lbl);
        freeNode(e);
        return;
    }

    /* Handle AND (&&) */
    if (e->op == 'j') {
        /* For AND: if left is false, fail immediately; if true, try right */
        /* Left: if false -> false_target; if true -> continue to right */
        emitCond(e->left, invert, NULL, -1, false_lbl, false_num);
        /* Right: if true -> true_target; if false -> false_target */
        emitCond(e->right, invert, true_lbl, true_num, false_lbl, false_num);
        freeNode(e);
        return;
    }

    /* Leaf condition - emit comparison and jump */
    emitExpr(e);

    /* Determine which way to jump based on Z flag semantics and invert */
    if (fnZValid) {
        /* fnZValid==1: Z=1 means true; fnZValid==2: Z=1 means false */
        int zMeansTrue = (fnZValid == 1);
        if (invert) zMeansTrue = !zMeansTrue;

        /* We need to jump to false on false, or true on true */
        /* If Z means true: jp z -> true, jp nz -> false */
        /* If Z means false: jp z -> false, jp nz -> true */
        if (false_num >= 0) {
            emitJump(zMeansTrue ? "jp nz," : "jp z,", false_lbl, false_num);
        } else if (true_num >= 0) {
            emitJump(zMeansTrue ? "jp z," : "jp nz,", true_lbl, true_num);
        }
        fnZValid = 0;
    } else {
        /* No Z flag info - need to test the value */
        if (e_size == 1) {
            fdprintf(outFd, "\tor a\n");
        } else {
            emit(S_AHORL);
        }
        /* Z=1 means zero/false, Z=0 means nonzero/true */
        if (invert) {
            /* Inverted: nonzero is false */
            if (false_num >= 0) {
                emitJump("jp nz,", false_lbl, false_num);
            } else if (true_num >= 0) {
                emitJump("jp z,", true_lbl, true_num);
            }
        } else {
            /* Normal: zero is false */
            if (false_num >= 0) {
                emitJump("jp z,", false_lbl, false_num);
            } else if (true_num >= 0) {
                emitJump("jp nz,", true_lbl, true_num);
            }
        }
    }
}

/*
 * Walk statement tree, emit assembly, and free nodes
 */
static int stmt_count = 0;
static void emitStmt(struct stmt *s)
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

    /* Handle LABEL statements - emit a label */
    if (s->type == 'L' && s->symbol) {
        fdprintf(outFd, "%s:\n", s->symbol);
    }

    /* Handle GOTO statements - emit unconditional jump */
    if (s->type == 'G' && s->symbol) {
        fdprintf(outFd, "\tjp %s\n", s->symbol);
    }

    /* Handle IF statements specially */
    if (s->type == 'I') {
        int invertCond = 0;
        int use_dir_jump = 0;
        int skip_else = 0;
        struct expr *cond = s->expr;

        /* Handle constant conditions - skip condition evaluation entirely */
        if (cond && cond->op == 'C') {
            int is_true = (cond->value != 0);
            freeExpr(cond);
            s->expr = NULL;
            /* Emit only the taken branch */
            if (is_true) {
                if (s->then_branch) emitStmt(s->then_branch);
            } else {
                if (s->else_branch) emitStmt(s->else_branch);
            }
            if (s->next) emitStmt(s->next);
            if (s->asm_block) free(s->asm_block);
            if (s->jump) freeJump(s->jump);
            free(s);
            return;
        }

        /* Handle logical OR/AND with recursive short-circuit */
        if (cond && (cond->op == 'h' || cond->op == 'j' || cond->op == '!')) {
            const char *else_goto = s->else_branch ? elseGotoTgt(s->else_branch) : NULL;
            const char *false_lbl = else_goto ? else_goto : (s->label2 > 0 ? "_if_" : "_if_end_");
            int false_num = else_goto ? -1 : s->label;

            /* Use recursive emitCond: fall through on true, jump on false */
            emitCond(cond, 0, NULL, -1, false_lbl, false_num);
            skip_else = (else_goto != NULL);
            goto emit_if_body;
        }

        /* Check if else branch is just a GOTO - can jump directly to target */
        {
        const char *else_goto = s->else_branch ? elseGotoTgt(s->else_branch) : NULL;
        skip_else = (else_goto != NULL);

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
            emitExpr(cond);

            if (invertCond && s->expr != cond) {
                free(s->expr);
            }

            /* Check if this is a comparison (Z=1 means true) vs bitwise (Z=1 means zero/false) */
            if (fnZValid == 1) {
                /* Z from EQ comparison: Z=1 means true, invert jump sense */
                invertCond = !invertCond;
            }
            /* fnZValid == 2: NE comparison, Z=1 means false, no invert needed */
            fnZValid = 0;

            if (s->label2 > 0) {
                if (skip_else) {
                    /* Jump directly to else's goto target */
                    fdprintf(outFd, "\t%s %s\n", invertCond ? "jp nz," : "jp z,", else_goto);
                } else {
                    emitJump(invertCond ? "jp nz," : "jp z,", "_if_", s->label);
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
                var = findVar(stripVarPfx(cond->left->symbol));
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
                int cmpSense = 0;  /* 1 if Z from comparison (Z=1 means true) */
                emitExpr(s->expr);
                if (fnZValid == 1) {
                    /* Z from EQ comparison: Z=1 means true, need opposite jump */
                    cmpSense = 1;
                } else if (!fnZValid) {
                    /* Need to test HL: Z=1 means HL=0 (false) */
                    emit(S_AHORL);
                }
                /* fnZValid == 2: NE comparison, Z=1 means false, no invert needed */
                fnZValid = 0;
                /* With cmpSense: jp nz skips when false (Z=0)
                 * Without:       jp z skips when false (Z=1, HL=0) */
                if (cmpSense) invertCond = !invertCond;
            }

            if (s->label2 > 0) {
                if (skip_else) {
                    /* Jump directly to else's goto target */
                    fdprintf(outFd, "\t%s %s\n", invertCond ? "jp nz," : "jp z,", else_goto);
                } else {
                    emitJump(invertCond ? "jp nz," : "jp z,", "_if_", s->label);
                }
            } else {
                emitJump(invertCond ? "jp nz," : "jp z,", "_if_end_", s->label);
            }
        }
        }

emit_if_body:
        /* Emit label for OR short-circuit jumps to then body */
        fdprintf(outFd, "_if_then_%d:\n", s->label);
        /* Emit then branch */
        if (s->then_branch) emitStmt(s->then_branch);

        if (s->label2 > 0 && !skip_else) {
            emitJump("jp", "_if_end_", s->label2);
            fdprintf(outFd, "_if_%d:\n", s->label);
            if (s->else_branch) emitStmt(s->else_branch);
        }

        if (s->next) emitStmt(s->next);

        if (s->asm_block) free(s->asm_block);
        if (s->jump) freeJump(s->jump);
        free(s);
        return;
    }
    /* Handle RETURN statements specially */
    else if (s->type == 'R') {
        if (s->expr) {
            emitExpr(s->expr);
            if (strcmp(fnRettype, "_long_") == 0 && s->expr->size == 2) {
                emit(S_ZEXTSL);
                emit(S_EXX);
                emit(S_HLZERO);
                emit(S_EXX);
            }
        }
        fdprintf(outFd, "\tjp %sX\n", fnName);
    } else {
        /* Emit expressions (this frees them) */
        if (TRACE(T_EMIT)) {
            fdprintf(2, "  emitStmt: has expr=%p\n", (void*)s->expr);
        }
        if (s->expr) emitExpr(s->expr);
        if (TRACE(T_EMIT)) {
            fdprintf(2, "  emitStmt: after expr, expr2=%p\n", (void*)s->expr2);
        }
        if (s->expr2) emitExpr(s->expr2);
        if (TRACE(T_EMIT)) {
            fdprintf(2, "  emitStmt: after expr2, expr3=%p\n", (void*)s->expr3);
        }
        if (s->expr3) emitExpr(s->expr3);
        if (TRACE(T_EMIT)) {
            fdprintf(2, "  emitStmt: after expr3\n");
        }
    }

    /* Emit child statements (this frees them) */
    if (TRACE(T_EMIT)) {
        fdprintf(2, "  emitStmt: checking then_branch=%p\n", (void*)s->then_branch);
    }
    if (s->then_branch) emitStmt(s->then_branch);
    if (TRACE(T_EMIT)) {
        fdprintf(2, "  emitStmt: checking else_branch=%p\n", (void*)s->else_branch);
    }
    if (s->else_branch) emitStmt(s->else_branch);

    /* Emit next statement in chain (this frees it) */
    if (TRACE(T_EMIT)) {
        fdprintf(2, "  emitStmt: checking next=%p\n", (void*)s->next);
    }
    if (s->next) emitStmt(s->next);
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
void emitAssembly(int fd)
{
    struct local_var *var, *next;
    int has_params;

    if (0 || !fnBody) return;
    stmt_count = 0;
    if (TRACE(T_EMIT)) {
        fdprintf(2, "emit: function %s\n", fnName);
    }

    /* Initialize label map for jump optimization */
    lblMapCnt = 0;

    /* Scan statement tree to build label map */
    scanLabJumps(fnBody);

    has_params = (fnParams && fnParams[0]);

    /* Emit function prologue */
    emitFnProlog(fnName, fnParams, fnRettype,
        fnFrmSize, fnLocals);

    /* Emit function body */
    if (TRACE(T_EMIT)) {
        fdprintf(2, "emit: calling emitStmt\n");
    }
    emitStmt(fnBody);
    if (TRACE(T_EMIT)) {
        fdprintf(2, "emit: emitStmt returned\n");
    }

    /* Emit function exit label */
    fdprintf(outFd, "%sX:\n", fnName);
    if (TRACE(T_EMIT)) {
        fdprintf(2, "emit: emitted exit label\n");
    }

    /* Restore callee-saved registers (reverse order of push) */
    {
        int used = getUsedRegs(fnLocals);
        if (used & 4) emit(S_EXXPOPBC);  /* restore BC' via exx; pop bc; exx */
        if (used & 2) emit(S_POPIX);
        if (used & 1) emit(S_POPBC);
    }
    if (TRACE(T_EMIT)) {
        fdprintf(2, "emit: restored callee regs\n");
    }

    /* Emit function epilogue */
    if (fnFrmSize > 0 || has_params) {
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
    var = fnLocals;
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
        fdprintf(2, "emit: function %s done\n", fnName);
    }
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
