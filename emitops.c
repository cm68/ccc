/*
 * emitops.c - Expression emission helpers for complex operations
 *
 * Helper functions for emitting inc/dec, assign, binop, call, and ternary.
 */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "cc2.h"
#include "emithelper.h"

/*
 * Emit increment/decrement operation
 */
void emitIncDec(struct function_ctx *ctx, struct expr *e)
{
    /* Parse placeholder format: INCDEC_PLACEHOLDER:op:size:amount:unused:symbol */
    int op, size, unused;
    long amount;
    char symbol[256];
    const char *p;
    const char *var_name;
    struct local_var *var;
    int is_post, is_dec;

    p = strstr(e->asm_block, "INCDEC_PLACEHOLDER:") + 19;
    if (sscanf(p, "%d:%d:%ld:%d:%255s", &op, &size, &amount, &unused, symbol) != 5) {
        emit(S_ERRPARS);
        freeNode(e);
        return;
    }

    is_post = (op == 0xef || op == 0xf6);
    is_dec = (op == 0xd6 || op == 0xf6);

    /* If result is unused, treat postfix like prefix (simpler code) */
    if (unused && is_post) is_post = 0;

    var_name = stripVarPfx(symbol);
    var = findVar(ctx, var_name);

    if (var && var->reg != REG_NO) {
        /* Variable is register-allocated */
        if (size == 1) {
            /* Byte register */
            const char *reg_name = (var->reg == REG_B || var->reg == REG_Bp) ? "b" : "c";
            int use_alt = (var->reg == REG_Bp || var->reg == REG_Cp);

            if (use_alt) emit(S_EXX);
            if (is_post) fdprintf(outFd, "\tld a, %s\n", reg_name);
            if (amount == 1) {
                fdprintf(outFd, "\t%s %s\n", is_dec ? "dec" : "inc", reg_name);
            } else {
                fdprintf(outFd, "\t%s a, %ld\n", is_dec ? "sub" : "add", amount);
                fdprintf(outFd, "\tld %s, a\n", reg_name);
            }
            if (!is_post) fdprintf(outFd, "\tld a, %s\n", reg_name);
            if (use_alt) emit(S_EXX);
        } else {
            /* Word register */
            const char *reg_pair = (var->reg == REG_IX) ? "ix" : "bc";
            int use_alt = (var->reg == REG_BCp);

            if (use_alt) emit(S_EXX);
            if (is_post) emit(var->reg == REG_IX ? S_IXHL : S_BCHL);
            if (amount == 1) {
                fdprintf(outFd, "\t%s %s\n", is_dec ? "dec" : "inc", reg_pair);
            } else {
                fdprintf(outFd, "\tpush %s\n", reg_pair);
                fdprintf(outFd, "\tld de, %ld\n", amount);
                fdprintf(outFd, is_dec ? "\tor a\n\tsbc %s, de\n" : "\tadd %s, de\n", reg_pair);
                emit(S_POPHPOST);
            }
            if (!is_post) emit(var->reg == REG_IX ? S_IXHL : S_BCHL);
            if (use_alt) emit(S_EXX);
        }
    } else if (var) {
        /* Variable is on stack */
        char sign = (var->offset >= 0) ? '+' : '-';
        int abs_offset = (var->offset >= 0) ? var->offset : -var->offset;
        int byte_adj = (size == 1 && var->offset >= 0) ? 1 : 0;

        if (size == 1) {
            if (is_post) fdprintf(outFd, "\tld a, (iy %c %d)\n", sign, abs_offset + byte_adj);
            if (amount == 1) {
                fdprintf(outFd, "\t%s (iy %c %d)\n", is_dec ? "dec" : "inc", sign, abs_offset + byte_adj);
            } else {
                fdprintf(outFd, "\tld a, (iy %c %d)\n", sign, abs_offset + byte_adj);
                fdprintf(outFd, "\t%s a, %ld\n", is_dec ? "sub" : "add", amount);
                fdprintf(outFd, "\tld (iy %c %d), a\n", sign, abs_offset + byte_adj);
            }
            if (!is_post) fdprintf(outFd, "\tld a, (iy %c %d)\n", sign, abs_offset + byte_adj);
        } else {
            if (is_post) loadWordIY(var->offset);
            if (amount == 1) {
                const char *op_str = is_dec ? "dec" : "inc";
                if (is_dec) fdprintf(outFd, "\tld a, (iy %c %d)\n", sign, abs_offset);
                fdprintf(outFd, "\t%s (iy %c %d)\n", op_str, sign, abs_offset);
                if (is_dec) emit(S_ORA);
                fdprintf(outFd, "\tjr nz, $+3\n\t%s (iy %c %d)\n", op_str, sign, abs_offset + 1);
            } else {
                loadWordIY(var->offset);
                fdprintf(outFd, "\tld de, %ld\n", amount);
                emit(is_dec ? S_SBCHLDE : S_ADDHLDE);
                storeWordIY(var->offset);
            }
            if (!is_post) loadWordIY(var->offset);
        }
    } else {
        /* Global variable */
        const char *sym = stripDollar(symbol);

        if (size == 1) {
            fdprintf(outFd, "\tld a, (%s)\n", sym);
            if (is_post) emit(S_PUSHAFSV);
            if (amount == 1) {
                fdprintf(outFd, "\t%s a\n", is_dec ? "dec" : "inc");
            } else {
                fdprintf(outFd, "\t%s a, %ld\n", is_dec ? "sub" : "add", amount);
            }
            fdprintf(outFd, "\tld (%s), a\n", sym);
            if (is_post) emit(S_POPAFRET);
        } else {
            fdprintf(outFd, "\tld hl, (%s)\n", sym);
            if (is_post) emit(S_PUSHHLOV);
            if (amount == 1) {
                fdprintf(outFd, "\t%s hl\n", is_dec ? "dec" : "inc");
            } else {
                fdprintf(outFd, "\tld de, %ld\n", amount);
                emit(is_dec ? S_SBCHLDE : S_ADDHLDE);
            }
            fdprintf(outFd, "\tld (%s), hl\n", sym);
            if (is_post) emit(S_POPHLRET);
        }
    }

    freeNode(e);
}

/*
 * Emit assignment operation
 */
void emitAssign(struct function_ctx *ctx, struct expr *e)
{
    /* Emit right child first (value goes to PRIMARY) */
    emitExpr(ctx, e->right);

    /* Check for IX-indexed store */
    if ((e->flags & 1) && e->left && e->left->op == '+' &&
        e->left->left && e->left->left->op == 'M' &&
        e->left->left->type_str && strcmp(e->left->left->type_str, ":p") == 0 &&
        e->left->left->left && e->left->left->left->op == '$' &&
        e->left->left->left->symbol) {
        const char *var_symbol = e->left->left->left->symbol;
        long offset = e->value;
        const char *var_name = stripVarPfx(var_symbol);
        struct local_var *var = findVar(ctx, var_name);

        if (var && var->reg == REG_IX) {
            if (e->size == 1) {
                fdprintf(outFd, "\tld (ix + %d), a\n", (char)offset);
            } else if (e->size == 2) {
                storeWordIX((char)offset);
            } else if (e->size == 4) {
                storeWordIX((char)offset);
                emit(S_EXX);
                storeWordIX((char)(offset + 2));
                emit(S_EXX);
            }
            freeExpr(e->left);
            freeNode(e);
            return;
        }
    }

    /* Simple variable assignment */
    if (e->left && e->left->op == '$' && e->left->symbol) {
        storeVar(ctx, e->left->symbol, e->size, 1);
    }
    /* Pointer dereference */
    else if (e->left && e->left->op == 'M') {
        if (e->size == 1) {
            emit(S_ESAVE);
            emitExpr(ctx, e->left->left);
            emit(S_HLDE);
        } else if (e->size == 2) {
            emit(S_DESAVE);
            emitExpr(ctx, e->left->left);
            emit(S_HLDE);
            emit(S_INCHL);
            emit(S_HLD);
        }
    }
    /* Complex lvalue */
    else if (e->left && e->left->op == '+') {
        if (e->size == 1) {
            emit(S_ESAVE);
            emitExpr(ctx, e->left);
            emit(S_HLDE);
        } else if (e->size == 2) {
            emit(S_DESAVE);
            emitExpr(ctx, e->left);
            emit(S_HLDE);
            emit(S_INCHL);
            emit(S_HLD);
        } else if (e->size == 4) {
            emit(S_PUSHHLLOW);
            emit(S_EXX);
            emit(S_PUSHHLUPP);
            emit(S_EXX);
            emitExpr(ctx, e->left);
            emit(S_DEADR);
            emit(S_POPHLUPP);
            emit(S_PUSHDESV);
            emit(S_EXX);
            emit(S_POPDEADR);
            emit(S_POPHLLOW);
            emit(S_AL);
            emit(S_DEA);
            emit(S_INCDE);
            emit(S_AH);
            emit(S_DEA);
            emit(S_INCDE);
            emit(S_EXX);
            emit(S_AL);
            emit(S_DEA);
            emit(S_INCDE);
            emit(S_AH);
            emit(S_DEA);
            emit(S_EXX);
        }
    }
}

/*
 * Emit ADD with constant where left is register-allocated variable
 */
void emitAddConst(struct function_ctx *ctx, struct expr *e)
{
    const char *var_name = stripVarPfx(e->left->left->symbol);
    struct local_var *var = findVar(ctx, var_name);

    if (var && (var->reg == REG_BC || var->reg == REG_BCp || var->reg == REG_IX)) {
        long const_val = 0;
        int is_small = 0;

        if (strstr(e->asm_block, "inc hl")) {
            const char *p = e->asm_block;
            while ((p = strstr(p, "inc hl")) != NULL) {
                const_val++;
                p += 6;
            }
            is_small = 1;
        } else if (strstr(e->asm_block, "ld de, ")) {
            sscanf(strstr(e->asm_block, "ld de, ") + 7, "%ld", &const_val);
        }

        freeExpr(e->left);

        if (is_small && const_val <= 4) {
            if (var->reg == REG_BC) {
                emit(S_BCHL);
            } else if (var->reg == REG_BCp) {
                emit(S_EXXBCHL);
            } else {
                emit(S_IXHL);
            }
            fdprintf(outFd, "%s\n", e->asm_block);
        } else {
            fdprintf(outFd, "\tld hl, %ld\n", const_val);
            if (var->reg == REG_BC) {
                emit(S_ADDHLBC);
            } else if (var->reg == REG_BCp) {
                emit(S_EXXBCPOPHL);
            } else {
                emit(S_IXSWPHL);
            }
        }
    } else {
        emitExpr(ctx, e->left);
        fdprintf(outFd, "%s\n", e->asm_block);
    }

    freeNode(e);
}

/*
 * Emit binary operation
 */
void emitBinop(struct function_ctx *ctx, struct expr *e)
{
    /* Check for inline byte operations with immediate */
    int isInlineImm = 0;
    if (!strchr(e->asm_block, '\n') &&
        (e->op == '&' || e->op == '|' || e->op == '^') &&
        e->left && e->left->size == 1 && e->right && e->right->op == 'C' &&
        e->right->value >= 0 && e->right->value <= 255) {
        isInlineImm = 1;
    }

    if (isInlineImm) {
        emitExpr(ctx, e->left);
        freeExpr(e->right);
        fdprintf(outFd, "%s\n", e->asm_block);
    } else {
        char *call_inst = NULL;
        char *newline = strchr(e->asm_block, '\n');
        int init_saves = ctx->de_save_count;

        if (newline) {
            call_inst = strdup(newline + 1);
        }

        emitExpr(ctx, e->left);
        pushStack(ctx);
        emitExpr(ctx, e->right);

        while (ctx->de_save_count > init_saves) {
            emit(S_POPDERES);
            ctx->de_save_count--;
        }

        if (call_inst) {
            fdprintf(outFd, "%s\n", call_inst);
            if (strstr(call_inst, "call")) {
                char *call_pos = strstr(call_inst, "call");
                call_pos += 4;
                while (*call_pos && (*call_pos == ' ' || *call_pos == '\t')) call_pos++;
                if (*call_pos && isCmpFunc(call_pos)) {
                    ctx->zflag_valid = 1;
                }
            }
            free(call_inst);
        }
    }

    {
        int zflag_saved = ctx->zflag_valid;
        popStack(ctx);
        ctx->zflag_valid = zflag_saved;
    }
}

/*
 * Emit function call
 */
void emitCall(struct function_ctx *ctx, struct expr *e)
{
    if (e->asm_block) {
        char *line_start = e->asm_block;
        char *line_end;
        char line_buf[512];
        int len;

        while (*line_start) {
            line_end = strchr(line_start, '\n');
            if (line_end) {
                len = line_end - line_start;
                if (len >= sizeof(line_buf)) len = sizeof(line_buf) - 1;
                memcpy(line_buf, line_start, len);
                line_buf[len] = '\0';
                line_start = line_end + 1;
            } else {
                strncpy(line_buf, line_start, sizeof(line_buf) - 1);
                line_buf[sizeof(line_buf) - 1] = '\0';
                line_start += strlen(line_buf);
            }

            if (strstr(line_buf, "DEREF_PLACEHOLDER:") ||
                strstr(line_buf, "; load arg")) {
                char *sym_start;
                char symbol[256];
                const char *var_name;
                struct local_var *var;
                int j = 0;

                if (strstr(line_buf, "DEREF_PLACEHOLDER:")) {
                    sym_start = strstr(line_buf, "DEREF_PLACEHOLDER:") + 18;
                } else {
                    sym_start = strchr(line_buf, ':');
                    if (sym_start) {
                        sym_start++;
                        while (*sym_start == ' ') sym_start++;
                    }
                }

                if (sym_start) {
                    while (sym_start[j] && sym_start[j] != ' ' &&
                           sym_start[j] != '\t' && sym_start[j] != '\n' && j < 255) {
                        symbol[j] = sym_start[j];
                        j++;
                    }
                }
                symbol[j] = '\0';
                var_name = stripVarPfx(symbol);
                var = findVar(ctx, var_name);

                /* Look ahead for push hl optimization */
                if (var && var->reg != REG_NO && var->size == 2 && line_end) {
                    char *next_line_st = line_end + 1;
                    char next_buf[512];
                    char *next_end = strchr(next_line_st, '\n');
                    int next_len;

                    if (next_end) {
                        next_len = next_end - next_line_st;
                        if (next_len >= sizeof(next_buf)) next_len = sizeof(next_buf) - 1;
                        memcpy(next_buf, next_line_st, next_len);
                        next_buf[next_len] = '\0';
                    } else {
                        strncpy(next_buf, next_line_st, sizeof(next_buf) - 1);
                        next_buf[sizeof(next_buf) - 1] = '\0';
                        next_len = strlen(next_buf);
                    }

                    if (strstr(next_buf, "push hl")) {
                        if (var->reg == REG_BC) {
                            emit(S_PUSHBC);
                        } else if (var->reg == REG_BCp) {
                            emit(S_EXXBC);
                        } else if (var->reg == REG_IX) {
                            emit(S_PUSHIX);
                        }
                        line_start = next_end ? next_end + 1 : next_line_st + next_len;
                        continue;
                    }
                }

                if (var && var->reg != REG_NO) {
                    if (var->reg == REG_BC) {
                        emit(S_BCHL);
                    } else if (var->reg == REG_BCp) {
                        emit(S_EXXBCHL);
                    } else if (var->reg == REG_IX) {
                        emit(S_IXHL);
                    }
                } else if (var) {
                    loadWordIY(var->offset);
                }
            } else {
                fdprintf(outFd, "%s\n", line_buf);
                if (strstr(line_buf, "call")) {
                    char *call_pos = strstr(line_buf, "call");
                    call_pos += 4;
                    while (*call_pos == ' ' || *call_pos == '\t') call_pos++;
                    if (isCmpFunc(call_pos)) {
                        ctx->zflag_valid = 1;
                    }
                }
            }
        }
    }

    if (e->cleanup_block) {
        fdprintf(outFd, "%s", e->cleanup_block);
    }

    {
        int zflag_saved = ctx->zflag_valid;
        invalStack(ctx);
        ctx->zflag_valid = zflag_saved;
    }

    freeExpr(e->left);
    freeExpr(e->right);
}

/*
 * Emit ternary conditional operator
 */
void emitTernary(struct function_ctx *ctx, struct expr *e)
{
    unsigned char cond_size;

    cond_size = e->left ? e->left->size : 2;

    if (e->left) emitExpr(ctx, e->left);

    if (cond_size == 1) {
        emit(S_ORA);
    } else {
        if (!ctx->zflag_valid) {
            emit(S_AHORL);
        }
    }
    ctx->zflag_valid = 0;

    if (e->jump) {
        emitJump("jp z,", "_tern_false_", e->label);
    }

    if (e->right && e->right->left) {
        emitExpr(ctx, e->right->left);
    }

    if (e->right && e->right->jump) {
        emitJump("jp", "_tern_end_", e->right->label);
    }

    fdprintf(outFd, "_tern_false_%d:\n", e->label);

    if (e->right && e->right->right) {
        emitExpr(ctx, e->right->right);
    }

    if (e->right) {
        fdprintf(outFd, "_tern_end_%d:\n", e->right->label);
    }

    if (e->jump) freeJump(e->jump);
    if (e->right && e->right->jump) freeJump(e->right->jump);
    if (e->right) free(e->right);

    freeNode(e);
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
