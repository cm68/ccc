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
#include "regcache.h"

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
        /* For OR: if left is true, skip rest; if false, try right */
        if (true_lbl || true_num >= 0) {
            /* Inherited true target - pass it through to both children */
            emitCond(e->left, invert, true_lbl, true_num, NULL, -1);
            emitCond(e->right, invert, true_lbl, true_num, false_lbl, false_num);
        } else {
            /* No inherited target - create our own _or_end_ label */
            int skip_lbl = fnLblCnt++;
            emitCond(e->left, invert, "_or_end_", skip_lbl, NULL, -1);
            emitCond(e->right, invert, true_lbl, true_num, false_lbl, false_num);
            fdprintf(outFd, "_or_end_%d:\n", skip_lbl);
        }
        freeNode(e);
        return;
    }

    /* Handle AND (&&) */
    if (e->op == 'j') {
        /* With invert (De Morgan): !(a && b) = !a || !b
         * So AND with invert behaves like OR: if left is true (inverted), short-circuit */
        if (invert) {
            /* Inverted AND = OR semantics */
            if (true_num >= 0 || true_lbl) {
                emitCond(e->left, invert, true_lbl, true_num, NULL, -1);
                emitCond(e->right, invert, true_lbl, true_num, false_lbl, false_num);
            } else {
                int skip_lbl = fnLblCnt++;
                emitCond(e->left, invert, "_and_end_", skip_lbl, NULL, -1);
                emitCond(e->right, invert, "_and_end_", skip_lbl, false_lbl, false_num);
                fdprintf(outFd, "_and_end_%d:\n", skip_lbl);
            }
        } else {
            /* Normal AND: if left is false, fail immediately; if true, try right */
            if (false_num >= 0 || false_lbl) {
                emitCond(e->left, invert, NULL, -1, false_lbl, false_num);
                emitCond(e->right, invert, true_lbl, true_num, false_lbl, false_num);
            } else {
                int skip_lbl = fnLblCnt++;
                emitCond(e->left, invert, NULL, -1, "_and_end_", skip_lbl);
                emitCond(e->right, invert, true_lbl, true_num, "_and_end_", skip_lbl);
                fdprintf(outFd, "_and_end_%d:\n", skip_lbl);
            }
        }
        freeNode(e);
        return;
    }

    /* Leaf condition - emit comparison and jump */
    emitExpr(e);

    /* Check for dual-test pattern: <= 0 and > 0 */
    if (fnDualCmp) {
        /* After bit 7, h: NZ = negative
         * <= 0: negative=true OR zero=true
         * > 0: negative=false AND zero=false (only positive true) */
        int have_target = (false_num >= 0 || (false_num == -1 && false_lbl) ||
                          true_num >= 0 || (true_num == -1 && true_lbl));
        if (have_target) {
            if (fnDualCmp == 'L') {
                /* x <= 0: jump to true if NZ (negative), else test zero */
                if (!invert) {
                    if (true_num >= 0 || (true_num == -1 && true_lbl))
                        emitJump("jp nz,", true_lbl, true_num);
                    else
                        fdprintf(outFd, "\tjp nz, $+8\n");
                    fdprintf(outFd, "\tld a, h\n\tor l\n");
                    if (false_num >= 0 || (false_num == -1 && false_lbl))
                        emitJump("jp nz,", false_lbl, false_num);
                } else {
                    /* inverted <= is > : false if negative, false if zero */
                    if (false_num >= 0 || (false_num == -1 && false_lbl))
                        emitJump("jp nz,", false_lbl, false_num);
                    else
                        fdprintf(outFd, "\tjp nz, $+8\n");
                    fdprintf(outFd, "\tld a, h\n\tor l\n");
                    if (false_num >= 0 || (false_num == -1 && false_lbl))
                        emitJump("jp z,", false_lbl, false_num);
                }
            } else {  /* fnDualCmp == '>' */
                /* x > 0: jump to false if NZ (negative), else test zero */
                if (!invert) {
                    if (false_num >= 0 || (false_num == -1 && false_lbl))
                        emitJump("jp nz,", false_lbl, false_num);
                    else
                        fdprintf(outFd, "\tjp nz, $+8\n");
                    fdprintf(outFd, "\tld a, h\n\tor l\n");
                    if (false_num >= 0 || (false_num == -1 && false_lbl))
                        emitJump("jp z,", false_lbl, false_num);
                } else {
                    /* inverted > is <= : true if negative or zero */
                    if (true_num >= 0 || (true_num == -1 && true_lbl))
                        emitJump("jp nz,", true_lbl, true_num);
                    else
                        fdprintf(outFd, "\tjp nz, $+8\n");
                    fdprintf(outFd, "\tld a, h\n\tor l\n");
                    if (false_num >= 0 || (false_num == -1 && false_lbl))
                        emitJump("jp nz,", false_lbl, false_num);
                }
            }
        }
        fnDualCmp = 0;
    }
    /* Check for carry-based comparison (byte cmp with constant) */
    else if (fnCmpFlag) {
        /* fnCmpFlag == 'c': nc = true (ge, gt)
         * fnCmpFlag == 'C': c = true (lt, le) */
        int cMeansTrue = (fnCmpFlag == 'C');
        if (invert) cMeansTrue = !cMeansTrue;

        if (false_num >= 0 || (false_num == -1 && false_lbl)) {
            emitJump(cMeansTrue ? "jp nc," : "jp c,", false_lbl, false_num);
        } else if (true_num >= 0 || (true_num == -1 && true_lbl)) {
            emitJump(cMeansTrue ? "jp c," : "jp nc,", true_lbl, true_num);
        }
        fnCmpFlag = 0;
    }
    /* Determine which way to jump based on Z flag semantics and invert */
    else if (fnZValid) {
        /* fnZValid==1: Z=1 means true; fnZValid==2: Z=1 means false */
        int zMeansTrue = (fnZValid == 1);
        if (invert) zMeansTrue = !zMeansTrue;

        /* We need to jump to false on false, or true on true */
        /* If Z means true: jp z -> true, jp nz -> false */
        /* If Z means false: jp z -> false, jp nz -> true */
        if (false_num >= 0 || (false_num == -1 && false_lbl)) {
            emitJump(zMeansTrue ? "jp nz," : "jp z,", false_lbl, false_num);
        } else if (true_num >= 0 || (true_num == -1 && true_lbl)) {
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
        /* false_num >= 0 means numbered label, false_num == -1 with false_lbl means named label */
        if (invert) {
            /* Inverted: nonzero is false */
            if (false_num >= 0 || (false_num == -1 && false_lbl)) {
                emitJump("jp nz,", false_lbl, false_num);
            } else if (true_num >= 0 || (true_num == -1 && true_lbl)) {
                emitJump("jp z,", true_lbl, true_num);
            }
        } else {
            /* Normal: zero is false */
            if (false_num >= 0 || (false_num == -1 && false_lbl)) {
                emitJump("jp z,", false_lbl, false_num);
            } else if (true_num >= 0 || (true_num == -1 && true_lbl)) {
                emitJump("jp nz,", true_lbl, true_num);
            }
        }
    }
}

/*
 * Walk statement tree, emit assembly, and free nodes
 * tailPos: if true and return has no next, can fall through to exit
 */
static int stmt_count = 0;
static void emitStmtTail(struct stmt *s, int tailPos);
static void emitStmt(struct stmt *s) { emitStmtTail(s, 0); }
static void emitStmtTail(struct stmt *s, int tailPos)
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

    /* For ASM nodes, emit the raw assembly text */
    if (s->type == 'A' && s->asm_block) {
        fdprintf(outFd, "%s\n", s->asm_block);
    }

    /* Handle end-label statements (type 'Y') - emit _if_end_N: */
    if (s->type == 'Y') {
        fdprintf(outFd, "_if_end_%d:\n", s->label);
    }

    /* Handle LABEL statements - emit a label */
    if (s->type == 'L' && s->symbol && s->symbol[0]) {
        if (TRACE(T_EMIT)) fdprintf(2, "LABEL: %s\n", s->symbol);
        /* Invalidate cache at loop labels (backward jump targets) */
        if (s->symbol[0] == 'L' && (strstr(s->symbol, "_top") || strstr(s->symbol, "_continue"))) {
            cacheInvalAll();
        }
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
            xfree(s->asm_block);
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
            fnCondOnly = 1;
            emitExpr(cond);
            fnCondOnly = 0;

            if (invertCond && s->expr != cond) {
                free(s->expr);
            }

            /* Check for carry-based comparison first */
            if (fnCmpFlag) {
                /* fnCmpFlag == 'c': nc = true, 'C': c = true */
                int cMeansTrue = (fnCmpFlag == 'C');
                if (invertCond) cMeansTrue = !cMeansTrue;
                fnCmpFlag = 0;

                if (s->label2 > 0) {
                    if (skip_else) {
                        fdprintf(outFd, "\t%s %s\n", cMeansTrue ? "jp nc," : "jp c,", else_goto);
                    } else {
                        emitJump(cMeansTrue ? "jp nc," : "jp c,", "_if_", s->label);
                    }
                } else {
                    emitJump(cMeansTrue ? "jp nc," : "jp c,", "_if_end_", s->label);
                }
            }
            /* Check if this is a comparison (Z=1 means true) vs bitwise (Z=1 means zero/false) */
            else if (fnZValid == 1) {
                /* Z from EQ comparison: Z=1 means true, invert jump sense */
                invertCond = !invertCond;
                fnZValid = 0;
                goto emit_z_jump;
            } else if (!fnZValid) {
                /* Z flag not set - need or a to test byte value */
                emit(S_ORA);
                goto emit_z_jump;
            } else {
                /* fnZValid == 2: NE comparison, Z=1 means false, no invert needed */
                fnZValid = 0;
emit_z_jump:
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
            }
        } else {
            /* Non-byte or complex expression */
            struct local_var *var = NULL;
            if (cond && cond->op == 'M' && cond->size == 2 &&
                cond->left && cond->left->op == '$' && cond->left->symbol) {
                var = findVar(stripVarPfx(cond->left->symbol));
            }

            if (var && bcOrCIdx(var->reg)) {
                emit(bcOrCIdx(var->reg));
                freeExpr(cond);
                if (invertCond && s->expr != cond) {
                    free(s->expr);
                }
            } else {
                int cmpSense = 0;  /* 1 if Z from comparison (Z=1 means true) */
                int useCarry = 0;  /* 1 if using carry flag */
                int cMeansTrue = 0;
                fnCondOnly = 1;
                emitExpr(s->expr);
                fnCondOnly = 0;

                /* Handle dual-test pattern (<=0 and >0) */
                if (fnDualCmp) {
                    /* bit 7 test already emitted by emitExpr */
                    const char *tgt = (s->label2 > 0) ? "_if_" : "_if_end_";
                    const char *hi = "h", *lo = "l";
                    int num = s->label;
                    if (fnDualReg == R_BC) { hi = "b"; lo = "c"; }
                    else if (fnDualReg == R_DE) { hi = "d"; lo = "e"; }
                    if (fnDualCmp == 'L') {
                        /* x <= 0: negative=true, zero=true */
                        emitJump("jp nz,", "_if_then_", num);  /* neg -> then */
                        fdprintf(outFd, "\tld a, %s\n\tor %s\n", hi, lo);
                        emitJump("jp nz,", tgt, num);  /* pos -> else/end */
                    } else {
                        /* x > 0: negative=false, zero=false */
                        emitJump("jp nz,", tgt, num);  /* neg -> else/end */
                        fdprintf(outFd, "\tld a, %s\n\tor %s\n", hi, lo);
                        emitJump("jp z,", tgt, num);  /* zero -> else/end */
                    }
                    fnDualCmp = 0;
                    fnDualReg = 0;
                    goto emit_if_body;
                }

                if (fnCmpFlag) {
                    useCarry = 1;
                    cMeansTrue = (fnCmpFlag == 'C');
                    fnCmpFlag = 0;
                } else if (fnZValid == 1) {
                    /* Z from EQ comparison: Z=1 means true, need opposite jump */
                    cmpSense = 1;
                } else if (!fnZValid) {
                    /* Need to test HL: Z=1 means HL=0 (false) */
                    emit(S_AHORL);
                }
                /* fnZValid == 2: NE comparison, Z=1 means false, no invert needed */
                fnZValid = 0;

                if (useCarry) {
                    if (invertCond) cMeansTrue = !cMeansTrue;
                    if (s->label2 > 0) {
                        if (skip_else) {
                            fdprintf(outFd, "\t%s %s\n", cMeansTrue ? "jp nc," : "jp c,", else_goto);
                        } else {
                            emitJump(cMeansTrue ? "jp nc," : "jp c,", "_if_", s->label);
                        }
                    } else {
                        emitJump(cMeansTrue ? "jp nc," : "jp c,", "_if_end_", s->label);
                    }
                } else {
                    /* With cmpSense: jp nz skips when false (Z=0)
                     * Without:       jp z skips when false (Z=1, HL=0) */
                    if (cmpSense) invertCond = !invertCond;
                    if (s->label2 > 0) {
                        if (skip_else) {
                            fdprintf(outFd, "\t%s %s\n", invertCond ? "jp nz," : "jp z,", else_goto);
                        } else {
                            emitJump(invertCond ? "jp nz," : "jp z,", "_if_", s->label);
                        }
                    } else {
                        emitJump(invertCond ? "jp nz," : "jp z,", "_if_end_", s->label);
                    }
                }
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

        if (TRACE(T_EMIT)) {
            fdprintf(2, "IF: s->next=%p skip_else=%d label2=%d\n",
                     (void*)s->next, skip_else, s->label2);
            if (s->next) fdprintf(2, "IF: s->next->type=%c s->next->next=%p\n",
                                  s->next->type, (void*)s->next->next);
        }
        if (s->next) emitStmtTail(s->next, tailPos);

        xfree(s->asm_block);
        if (s->jump) freeJump(s->jump);
        free(s);
        return;
    }
    /* Handle SWITCH statements - emit labels around cases */
    else if (s->type == 'S') {
        /* Emit top label if we have one */
        if (s->symbol)
            fdprintf(outFd, "%s_top:\n", s->symbol);
        /* Emit switch expression (evaluated but result discarded for now) */
        /* TODO: implement proper switch jump table */
        if (s->expr) emitExpr(s->expr);
        /* Emit cases */
        if (s->then_branch) emitStmt(s->then_branch);
        /* Emit break label if we have one */
        if (s->symbol)
            fdprintf(outFd, "%s_break:\n", s->symbol);
        if (s->next) emitStmtTail(s->next, tailPos);
        xfree(s->symbol);
        xfree(s->asm_block);
        if (s->jump) freeJump(s->jump);
        free(s);
        return;
    }
    /* Handle RETURN statements specially */
    else if (s->type == 'R') {
        if (s->expr) {
            int expr_size = s->expr->size;
            int ret_size = getSizeFTStr(fnRettype[0]);
            /* For byte return, set constant size so emit uses A not HL */
            if (ret_size == 1 && s->expr->op == 'C')
                s->expr->size = 1;
            emitExpr(s->expr);
            /* Widen byte to word if needed */
            if (expr_size == 1 && ret_size == 2) {
                fdprintf(outFd, "\tld l, a\n\tld h, 0\n");
            }
            /* Widen word to long if needed */
            if (expr_size == 2 && ret_size == 4) {
                emit(S_ZEXTSL);
                emit(S_EXX);
                emit(S_HLZERO);
                emit(S_EXX);
            }
        }
        /* Skip jump if in tail position with no next - can fall through */
        if (!(tailPos && !s->next)) {
            fdprintf(outFd, "\tjp %sX\n", fnName);
        }
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
    /* Block's then_branch is in tail position if block is and has no next */
    if (s->then_branch) {
        int branchTail = (s->type == 'B' && tailPos && !s->next);
        emitStmtTail(s->then_branch, branchTail);
    }
    if (TRACE(T_EMIT)) {
        fdprintf(2, "  emitStmt: checking else_branch=%p\n", (void*)s->else_branch);
    }
    if (s->else_branch) emitStmt(s->else_branch);

    /* Emit next statement in chain (this frees it) */
    if (TRACE(T_EMIT)) {
        fdprintf(2, "  emitStmt: checking next=%p\n", (void*)s->next);
    }
    if (s->next) emitStmtTail(s->next, tailPos);
    if (TRACE(T_EMIT)) {
        fdprintf(2, "  emitStmt: about to free stmt %p\n", (void*)s);
    }

    /* Free this node only (children already freed by recursive emit calls) */
    if (TRACE(T_EMIT)) {
        fdprintf(2, "  freeing asm_block=%p\n", (void*)s->asm_block);
    }
    xfree(s->asm_block);
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
    emitStmtTail(fnBody, 1);  /* tail position - returns can fall through */
    if (TRACE(T_EMIT)) {
        fdprintf(2, "emit: emitStmt returned\n");
    }

    /* Emit function exit label */
    fdprintf(outFd, "%sX:\n", fnName);
    if (TRACE(T_EMIT)) {
        fdprintf(2, "emit: emitted exit label\n");
    }

    /* Clean up call argument stack (when not using framealloc) */
    if (fnCallStk > 0 && fnFrmSize == 0 && !has_params) {
        int i;
        for (i = 0; i < fnCallStk; i += 2)
            fdprintf(outFd, "\tpop de\n");
    }

    /* Restore callee-saved registers (reverse order of push) */
    {
        int used = getUsedRegs(fnLocals);
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
