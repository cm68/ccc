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
 * Check if expr is an IY-indexed local byte variable.
 * Returns the IY offset if so, or 0 if not.
 * (IY+0 is the saved frame pointer, so locals are never at offset 0)
 */
static int getIYOffset(struct expr *e) {
    struct local_var *v;
    const char *sym;
    if (!e || e->op != 'M' || e->size != 1) return 0;
    if (!e->left || e->left->op != '$' || !e->left->symbol) return 0;
    sym = e->left->symbol;
    v = findVar(stripVarPfx(sym));
    if (!v || v->reg != REG_NO) return 0;
    return v->offset;
}

/*
 * Emit increment/decrement operation
 */
void emitIncDec(struct expr *e)
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
    var = findVar(var_name);

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
void emitAssign(struct expr *e)
{
    if (TRACE(T_ASSIGN)) {
        fdprintf(2, "emitAssign: enter\n");
    }
    /* Emit right child first (value goes to PRIMARY) */
    emitExpr(e->right);
    if (TRACE(T_ASSIGN)) {
        fdprintf(2, "emitAssign: after emitExpr(right)\n");
    }

    /* Check for IX-indexed store */
    if ((e->flags & 1) && e->left && e->left->op == '+' &&
        e->left->left && e->left->left->op == 'M' &&
        e->left->left->type_str && strcmp(e->left->left->type_str, ":p") == 0 &&
        e->left->left->left && e->left->left->left->op == '$' &&
        e->left->left->left->symbol) {
        const char *var_symbol = e->left->left->left->symbol;
        long offset = e->value;
        const char *var_name = stripVarPfx(var_symbol);
        struct local_var *var = findVar(var_name);

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
        if (TRACE(T_ASSIGN)) {
            fdprintf(2, "emitAssign: simple variable\n");
        }
        storeVar(e->left->symbol, e->size, 1);
        if (TRACE(T_ASSIGN)) {
            fdprintf(2, "emitAssign: after storeVar\n");
        }
    }
    /* Pointer dereference */
    else if (e->left && e->left->op == 'M') {
        if (e->size == 1) {
            emit(S_ESAVE);
            emitExpr(e->left->left);
            emit(S_HLDE);
        } else if (e->size == 2) {
            emit(S_DESAVE);
            emitExpr(e->left->left);
            emit(S_HLDE);
            emit(S_INCHL);
            emit(S_HLD);
        }
    }
    /* Complex lvalue */
    else if (e->left && e->left->op == '+') {
        if (e->size == 1) {
            emit(S_ESAVE);
            emitExpr(e->left);
            emit(S_HLDE);
        } else if (e->size == 2) {
            emit(S_DESAVE);
            emitExpr(e->left);
            emit(S_HLDE);
            emit(S_INCHL);
            emit(S_HLD);
        } else if (e->size == 4) {
            emit(S_PUSHHLLOW);
            emit(S_EXX);
            emit(S_PUSHHLUPP);
            emit(S_EXX);
            emitExpr(e->left);
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
    if (TRACE(T_ASSIGN)) {
        fdprintf(2, "emitAssign: returning\n");
    }
}

/*
 * Emit ADD with constant where left is register-allocated variable
 */
void emitAddConst(struct expr *e)
{
    const char *var_name = stripVarPfx(e->left->left->symbol);
    struct local_var *var = findVar(var_name);

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
        emitExpr(e->left);
        fdprintf(outFd, "%s\n", e->asm_block);
    }

    freeNode(e);
}

/*
 * Emit binary operation
 */
void emitBinop(struct expr *e)
{
    /* Check for inline byte operations with immediate */
    int isInlineImm = 0;
    int isByteCmp = 0;
    int iyOffset = 0;

    if (e->left && e->left->size == 1 && e->right && e->right->op == 'C' &&
        e->right->value >= 0 && e->right->value <= 255) {
        if (!strchr(e->asm_block, '\n') &&
            (e->op == '&' || e->op == '|' || e->op == '^')) {
            isInlineImm = 1;
        } else if (e->op == 'Q' || e->op == 'n') {
            /* Byte EQ/NE with constant - use cp instruction */
            isByteCmp = 1;
        }
    }

    /* Check for byte EQ/NE with IY-indexed local */
    if (!isByteCmp && (e->op == 'Q' || e->op == 'n') &&
        e->left && e->left->size == 1) {
        iyOffset = getIYOffset(e->right);
    }

    if (isInlineImm) {
        emitExpr(e->left);
        freeExpr(e->right);
        fdprintf(outFd, "%s\n", e->asm_block);
    } else if (isByteCmp) {
        emitExpr(e->left);
        fdprintf(outFd, "\tcp %ld\n", e->right->value);
        freeExpr(e->right);
        fnZValid = 1;  /* cp sets Z flag */
    } else if (iyOffset) {
        /* Byte EQ/NE with IY-indexed local - use cp (iy + offset) */
        emitExpr(e->left);
        if (iyOffset > 0) {
            fdprintf(outFd, "\tcp (iy + %d)\n", iyOffset);
        } else {
            fdprintf(outFd, "\tcp (iy - %d)\n", -iyOffset);
        }
        freeExpr(e->right);
        fnZValid = 1;  /* cp sets Z flag */
    } else {
        char *call_inst = NULL;
        char *newline = strchr(e->asm_block, '\n');
        int init_saves = fnDESaveCnt;

        if (newline) {
            call_inst = strdup(newline + 1);
        }

        emitExpr(e->left);
        /* For byte ops, move A to E; for word ops, use standard pushStack */
        if (e->left && e->left->size == 1) {
            emit(S_ESAVE);  /* ld e, a */
        } else {
            pushStack();
        }
        emitExpr(e->right);

        while (fnDESaveCnt > init_saves) {
            emit(S_POPDERES);
            fnDESaveCnt--;
        }

        if (call_inst) {
            fdprintf(outFd, "%s\n", call_inst);
            if (strstr(call_inst, "call")) {
                char *call_pos = strstr(call_inst, "call");
                call_pos += 4;
                while (*call_pos && (*call_pos == ' ' || *call_pos == '\t')) call_pos++;
                if (*call_pos && isCmpFunc(call_pos)) {
                    fnZValid = 1;
                }
            }
            free(call_inst);
        }
    }

    /* Only pop stack for word ops (byte ops didn't push) */
    if (!e->left || e->left->size != 1) {
        int zflag_saved = fnZValid;
        popStack();
        fnZValid = zflag_saved;
    }
}

/*
 * Emit function call directly from AST
 * e->left = function (SYM node)
 * e->right = wrapper chain of arguments
 * e->value = argument count
 */
void emitCall(struct expr *e)
{
    struct expr *args[32];
    struct expr *arg;
    int arg_count, i;
    const char *func_name;

    /* Collect arguments from wrapper chain */
    arg_count = e->value;
    arg = e->right;
    for (i = 0; i < arg_count && i < 32 && arg; i++) {
        args[i] = arg->left;
        arg = arg->right;
    }
    arg_count = i;

    /* Emit arguments in reverse order (C calling convention) */
    for (i = arg_count - 1; i >= 0; i--) {
        struct expr *a = args[i];
        int arg_size;
        if (!a) continue;

        /* Save size before emitExpr frees the node */
        arg_size = a->size;

        /* Emit code to load argument value */
        emitExpr(a);

        /* Push onto stack */
        if (arg_size == 1) {
            fdprintf(outFd, "\tpush af  ; arg %d\n", i);
        } else {
            fdprintf(outFd, "\tpush hl  ; arg %d\n", i);
        }
    }

    /* Emit the call */
    if (e->left && e->left->op == '$' && e->left->symbol) {
        func_name = e->left->symbol;
        if (func_name[0] == '$') func_name++;
        addRefSym(func_name);
        fdprintf(outFd, "\tcall %s\n", func_name);
        if (isCmpFunc(func_name)) {
            fnZValid = 1;
        }
    } else {
        fdprintf(outFd, "\t; TODO: indirect call\n");
    }

    /* Stack cleanup in loops (framefree handles it otherwise) */
    if (e->cleanup_block) {
        fdprintf(outFd, "%s", e->cleanup_block);
    }

    invalStack();
    /* Don't free children - they're part of the AST tree */
}

/*
 * Emit ternary conditional operator
 */
void emitTernary(struct expr *e)
{
    unsigned char cond_size;

    cond_size = e->left ? e->left->size : 2;

    if (e->left) emitExpr(e->left);

    if (cond_size == 1) {
        emit(S_ORA);
    } else {
        if (!fnZValid) {
            emit(S_AHORL);
        }
    }
    fnZValid = 0;

    if (e->jump) {
        emitJump("jp z,", "_tern_false_", e->label);
    }

    if (e->right && e->right->left) {
        emitExpr(e->right->left);
    }

    if (e->right && e->right->jump) {
        emitJump("jp", "_tern_end_", e->right->label);
    }

    fdprintf(outFd, "_tern_false_%d:\n", e->label);

    if (e->right && e->right->right) {
        emitExpr(e->right->right);
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
