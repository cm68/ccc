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
#include "regcache.h"

/* Helper: emit IY-indexed instruction with proper sign handling */
static void iyFmt(const char *fmt, int offset) {
    if (offset >= 0)
        fdprintf(outFd, fmt, '+', offset);
    else
        fdprintf(outFd, fmt, '-', -offset);
}

/* Helper: emit store word from HL to (DE) and increment DE */
static void storeHLtoDE(void) {
    emit(S_AL); emit(S_DEA); emit(S_INCDE);
    emit(S_AH); emit(S_DEA); emit(S_INCDE);
}

/*
 * Byte comparison info for inline cp instructions
 */
enum cmp_kind { CMP_NONE, CMP_CONST, CMP_IY, CMP_REG, CMP_IX, CMP_GLOBAL };

struct bytecmp {
    enum cmp_kind kind;
    int offset;          /* IY or IX offset */
    int reg;             /* Register (REG_B, REG_C, etc.) */
    const char *global;  /* Global/static symbol name */
};

/*
 * Check what kind of byte comparison operand we have.
 * Returns CMP_NONE if not optimizable, else fills in cmp struct.
 */
static enum cmp_kind getByteCmp(struct expr *e, struct bytecmp *cmp) {
    struct local_var *v;
    const char *sym;

    cmp->kind = CMP_NONE;
    cmp->offset = 0;
    cmp->reg = REG_NO;
    cmp->global = NULL;

    if (!e) return CMP_NONE;

    /* Constant */
    if (e->op == 'C' && e->value >= 0 && e->value <= 255) {
        cmp->kind = CMP_CONST;
        cmp->offset = e->value;
        return CMP_CONST;
    }

    /* Must be byte DEREF */
    if (e->op != 'M' || e->size != 1) return CMP_NONE;
    if (!e->left || e->left->op != '$' || !e->left->symbol) return CMP_NONE;

    sym = e->left->symbol;
    v = findVar(stripVarPfx(sym));

    if (v) {
        if (v->reg != REG_NO) {
            /* Register-allocated variable */
            cmp->kind = CMP_REG;
            cmp->reg = v->reg;
            return CMP_REG;
        } else {
            /* Stack variable - IY-indexed */
            cmp->kind = CMP_IY;
            cmp->offset = v->offset;
            return CMP_IY;
        }
    }

    /* Global or static */
    cmp->kind = CMP_GLOBAL;
    cmp->global = stripDollar(sym);
    return CMP_GLOBAL;
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

    /* Use actual variable size if available (placeholder size may be wrong for SYM nodes) */
    if (var) size = var->size;

    if (var && var->reg != REG_NO) {
        /* Variable is register-allocated */
        if (size == 1) {
            /* Byte register */
            const char *rn = byteRegName(var->reg);
            if (isAltReg(var->reg)) emit(S_EXX);
            if (is_post) fdprintf(outFd, "\tld a, %s\n", rn);
            if (amount == 1) {
                fdprintf(outFd, "\t%s %s\n", is_dec ? "dec" : "inc", rn);
            } else {
                fdprintf(outFd, "\t%s a, %ld\n\tld %s, a\n",
                         is_dec ? "sub" : "add", amount, rn);
            }
            if (!is_post) fdprintf(outFd, "\tld a, %s\n", rn);
            if (isAltReg(var->reg)) emit(S_EXX);
        } else {
            /* Word register */
            const char *rp = wordRegName(var->reg);
            if (isAltReg(var->reg)) emit(S_EXX);
            if (is_post) emitWordLoad(var->reg);
            if (amount == 1) {
                fdprintf(outFd, "\t%s %s\n", is_dec ? "dec" : "inc", rp);
            } else {
                fdprintf(outFd, "\tpush %s\n\tld de, %ld\n", rp, amount);
                fdprintf(outFd, is_dec ? "\tor a\n\tsbc %s, de\n" : "\tadd %s, de\n", rp);
                emit(S_POPHPOST);
            }
            /* Invalidate BC indirect cache if BC was modified */
            if (var->reg == REG_BC) {
                fnABCValid = 0;
                cacheInvalA();  /* A may hold *(bc) which is now invalid */
            }
            if (!is_post && !unused) emitWordLoad(var->reg);
            if (isAltReg(var->reg)) emit(S_EXX);
        }
    } else if (var) {
        /* Variable is on stack */
        int ofs = var->offset;
        int byte_ofs = ofs + (size == 1 && ofs >= 0 ? 1 : 0);

        if (size == 1) {
            if (is_post) iyFmt("\tld a, (iy %c %d)\n", byte_ofs);
            if (amount == 1) {
                iyFmt(is_dec ? "\tdec (iy %c %d)\n" : "\tinc (iy %c %d)\n", byte_ofs);
            } else {
                iyFmt("\tld a, (iy %c %d)\n", byte_ofs);
                fdprintf(outFd, "\t%s a, %ld\n", is_dec ? "sub" : "add", amount);
                iyFmt("\tld (iy %c %d), a\n", byte_ofs);
            }
            if (!is_post) iyFmt("\tld a, (iy %c %d)\n", byte_ofs);
        } else {
            if (is_post) loadWordIY(ofs);
            if (amount == 1) {
                if (is_dec) iyFmt("\tld a, (iy %c %d)\n", ofs);
                iyFmt(is_dec ? "\tdec (iy %c %d)\n" : "\tinc (iy %c %d)\n", ofs);
                if (is_dec) emit(S_ORA);
                fdprintf(outFd, "\tjr nz, $+3\n");
                iyFmt(is_dec ? "\tdec (iy %c %d)\n" : "\tinc (iy %c %d)\n", ofs + 1);
            } else {
                loadWordIY(ofs);
                fdprintf(outFd, "\tld de, %ld\n", amount);
                emit(is_dec ? S_SBCHLDE : S_ADDHLDE);
                storeWordIY(ofs);
            }
            if (!is_post) loadWordIY(ofs);
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

    /* Optimize: constant to register-allocated variable */
    if (e->right && e->right->op == 'C' && e->size == 2 &&
        e->left && e->left->op == '$' && e->left->symbol) {
        struct local_var *v = findVar(stripVarPfx(e->left->symbol));
        if (v && (v->reg == REG_BC || v->reg == REG_IX)) {
            fdprintf(outFd, "\tld %s, %ld\n",
                v->reg == REG_BC ? "bc" : "ix", e->right->value);
            freeExpr(e->right);
            freeExpr(e->left);
            freeNode(e);
            return;
        }
    }

    /* Optimize: byte constant to pointer target - use ld (hl), N */
    if (e->size == 1 && e->right && e->right->op == 'C' &&
        e->left && (e->left->op == 'M' || e->left->op == '+')) {
        int left_is_deref = (e->left->op == 'M');
        struct expr *addr = left_is_deref ? e->left->left : e->left;
        struct expr *deref_node = left_is_deref ? e->left : NULL;
        emitExpr(addr);  /* emitExpr loads address to HL and sets cache */
        fdprintf(outFd, "\tld (hl), %ld\n", e->right->value & 0xff);
        /* Don't free addr - it was already freed by emitExpr */
        if (deref_node) freeNode(deref_node);  /* Free just the M wrapper */
        freeExpr(e->right);
        freeNode(e);
        return;
    }

    /* Emit right child first (value goes to PRIMARY) */
    emitExpr(e->right);
    if (TRACE(T_ASSIGN)) {
        fdprintf(2, "emitAssign: after emitExpr(right)\n");
    }

    /* Check for IX-indexed store */
    if ((e->flags & E_IXASSIGN) && e->left && e->left->op == '+' &&
        e->left->left && e->left->left->op == 'M' &&
        e->left->left->type_str == 'p' &&
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
    /* Pointer dereference or complex lvalue */
    else if (e->left && (e->left->op == 'M' || e->left->op == '+')) {
        struct expr *addr = (e->left->op == 'M') ? e->left->left : e->left;
        if (e->size == 1) {
            emit(S_ESAVE);
            emitExpr(addr);
            emit(S_HLDE);
        } else if (e->size == 2) {
            emit(S_DESAVE);
            emitExpr(addr);
            emit(S_HLDE);
            emit(S_INCHL);
            emit(S_HLD);
        } else if (e->left->op == '+' && e->size == 4) {
            emit(S_PUSHHLLOW); emit(S_EXX); emit(S_PUSHHLUPP); emit(S_EXX);
            emitExpr(e->left);
            emit(S_DEADR); emit(S_POPHLUPP); emit(S_PUSHDESV);
            emit(S_EXX); emit(S_POPDEADR); emit(S_POPHLLOW);
            storeHLtoDE();
            emit(S_EXX);
            storeHLtoDE();
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

    if (var && wordRegName(var->reg)) {
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
            emitWordLoad(var->reg);
            fdprintf(outFd, "%s\n", e->asm_block);
        } else {
            fdprintf(outFd, "\tld hl, %ld\n", const_val);
            emitAddHLReg(var->reg);
        }
    } else {
        emitExpr(e->left);
        fdprintf(outFd, "%s\n", e->asm_block);
    }

    freeNode(e);
}

/*
 * Emit inline byte comparison (cp instruction)
 * Returns 1 if emitted, 0 if not applicable
 */
static int emitByteCp(struct expr *e)
{
    struct bytecmp cmp;
    int val;

    /* Only for byte comparisons */
    if (!e->left || e->left->size != 1)
        return 0;

    /* For >=, <=, >, < with constant, use carry flag directly */
    if ((e->op == 'g' || e->op == 'L' || e->op == '>' || e->op == '<') &&
        e->right && e->right->op == 'C') {
        val = e->right->value & 0xff;

        /* Emit left operand to A */
        emitExpr(e->left);
        cacheInvalA();  /* A was loaded, invalidate cache */

        /* Emit cp with adjusted constant based on comparison */
        switch (e->op) {
        case 'g':  /* GE (>=): cp N, nc = true */
            fdprintf(outFd, "\tcp %d\n", val);
            fnCmpFlag = 'c';  /* nc = true */
            break;
        case '<':  /* LT (<): cp N, c = true */
            fdprintf(outFd, "\tcp %d\n", val);
            fnCmpFlag = 'C';  /* c = true */
            break;
        case '>':  /* GT (>): cp N+1, nc = true */
            if (val >= 255) {
                /* A > 255 always false - emit compare that always fails */
                fdprintf(outFd, "\tor a\n\tscf\n");  /* set carry */
                fnCmpFlag = 'c';  /* nc = true, but carry is set so always false */
            } else {
                fdprintf(outFd, "\tcp %d\n", val + 1);
                fnCmpFlag = 'c';  /* nc = true */
            }
            break;
        case 'L':  /* LE (<=): cp N+1, c = true */
            if (val >= 255) {
                /* A <= 255 always true - emit compare that always succeeds */
                fdprintf(outFd, "\tor a\n");  /* clears carry */
                fnCmpFlag = 'c';  /* nc = true, always true */
            } else {
                fdprintf(outFd, "\tcp %d\n", val + 1);
                fnCmpFlag = 'C';  /* c = true */
            }
            break;
        }

        freeExpr(e->right);
        return 1;
    }

    /* Only EQ/NE for non-constant comparisons */
    if (e->op != 'Q' && e->op != 'n')
        return 0;

    if (getByteCmp(e->right, &cmp) == CMP_NONE)
        return 0;

    /* Emit left operand to A */
    emitExpr(e->left);
    cacheInvalA();  /* A was loaded, invalidate cache */

    /* Emit cp instruction based on right operand type */
    switch (cmp.kind) {
    case CMP_CONST:
        fdprintf(outFd, "\tcp %d\n", cmp.offset);
        break;
    case CMP_IY:
        iyFmt("\tcp (iy %c %d)\n", cmp.offset);
        break;
    case CMP_REG:
        if (cmp.reg == REG_IX) {
            fdprintf(outFd, "\tcp (ix + 0)\n");
        } else if (byteRegName(cmp.reg) || cmp.reg == REG_BC) {
            /* BC low byte is C */
            const char *rn = cmp.reg == REG_BC ? "c" : byteRegName(cmp.reg);
            if (isAltReg(cmp.reg))
                fdprintf(outFd, "\texx\n\tcp %s\n\texx\n", rn);
            else
                fdprintf(outFd, "\tcp %s\n", rn);
        } else {
            return 0;
        }
        break;
    case CMP_IX:
        fdprintf(outFd, "\tcp (ix + %d)\n", cmp.offset);
        break;
    case CMP_GLOBAL:
        /* Swap: save A, load global, cp saved */
        fdprintf(outFd, "\tld e, a\n\tld a, (%s)\n\tcp e\n", cmp.global);
        /* A now contains right operand, invalidate cache */
        clearA();
        break;
    default:
        return 0;
    }

    freeExpr(e->right);

    /* cp sets Z=1 if equal: EQ wants Z=1 true, NE wants Z=1 false */
    fnZValid = (e->op == 'Q') ? 1 : 2;
    return 1;
}

/*
 * Emit binary operation
 */
void emitBinop(struct expr *e)
{
    /* Check for inline byte operations with immediate */
    int isInlineImm = 0;

    if (e->left && e->left->size == 1 && e->right && e->right->op == 'C' &&
        e->right->value >= 0 && e->right->value <= 255) {
        if (e->op == '&' || e->op == '|' || e->op == '^' ||
            e->op == '+' || e->op == '-') {
            isInlineImm = 1;
        }
    }

    if (isInlineImm) {
        int val = e->right->value & 0xff;
        emitExpr(e->left);
        freeExpr(e->right);
        /* Emit inline instruction based on operator */
        switch (e->op) {
        case '&': fdprintf(outFd, "\tand %d\n", val); break;
        case '|': fdprintf(outFd, "\tor %d\n", val); break;
        case '^': fdprintf(outFd, "\txor %d\n", val); break;
        case '+': fdprintf(outFd, "\tadd a, %d\n", val); break;
        case '-': fdprintf(outFd, "\tsub %d\n", val); break;
        }
        /* These ops set Z flag: Z=1 means result is 0 */
        fnZValid = 2;
    } else if (emitByteCp(e)) {
        /* Byte comparison emitted inline */
    } else {
        char *call_inst = NULL;
        char *newline = strchr(e->asm_block, '\n');
        int init_saves = fnDESaveCnt;
        int left_size = e->left ? e->left->size : 2;  /* Save before emitExpr frees */
        int is_inline_cmp = 0;
        int usedDirDE = 0;

        if (newline) {
            call_inst = strdup(newline + 1);
            /* Check if this is inline sbc comparison (no call) */
            is_inline_cmp = strstr(call_inst, "sbc hl") && !strstr(call_inst, "call");
        }

        /* Check for signed compare with 0: "bit 7, h" pattern */
        if (strstr(e->asm_block, "bit 7, h")) {
            /* Inline signed compare with 0:
             * Only emit left child, skip pushStack and right child */
            emitExpr(e->left);
            fdprintf(outFd, "%s\n", e->asm_block);
            /* Set Z flag meaning: for < and >=, bit 7 sets Z appropriately
             * x < 0: NZ if true (Z=0 means true) -> fnZValid = 2
             * x >= 0: Z if true (Z=1 means true) -> fnZValid = 1
             * For <= and >, more complex: already handled by jump in asm_block
             */
            switch (e->op) {
            case '<': fnZValid = 2; break;  /* x < 0: NZ = true */
            case 'g': fnZValid = 1; break;  /* x >= 0: Z = true */
            case 'L': fnDualCmp = 'L'; break;  /* x <= 0: two-test */
            case '>': fnDualCmp = '>'; break;  /* x > 0: two-test */
            }
            /* Free the right child (constant 0) without emitting, set to NULL to prevent double-free */
            if (e->right) {
                freeExpr(e->right);
                e->right = NULL;
            }
            xfree(call_inst);
            return;
        }

        /* Check if left is simple global word deref - can load directly to DE */
        if (left_size == 2 && e->left && e->left->op == 'M' &&
            !e->left->asm_block && e->left->left &&
            e->left->left->op == '$' && e->left->left->symbol) {
            fnTargetDE = 1;
            emitExpr(e->left);
            /* fnTargetDE cleared by emitGlobDrf, fnDEValid set */
            usedDirDE = 1;
        } else {
            emitExpr(e->left);
            /* For byte ops, move A to E; for word ops, use standard pushStack */
            if (left_size == 1) {
                emit(S_ESAVE);  /* ld e, a */
            } else {
                pushStack();
            }
        }
        emitExpr(e->right);

        while (fnDESaveCnt > init_saves) {
            emit(S_POPDERES);
            fnDESaveCnt--;
        }

        if (is_inline_cmp) {
            /* Emit full inline code: ex de,hl + or a + sbc hl,de
             * After pushStack: DE=left, HL=right
             * ex de,hl: HL=left, DE=right
             * sbc hl,de = left - right */
            if (usedDirDE) {
                /* Skip ex de,hl - already have DE=left, HL=right */
                char *p = e->asm_block;
                if (strncmp(p, "\tex de, hl", 10) == 0) {
                    p += 10;
                    while (*p == ' ' || *p == ';') {
                        while (*p && *p != '\n') p++;
                    }
                    if (*p == '\n') p++;
                }
                fdprintf(outFd, "%s\n", p);
            } else {
                fdprintf(outFd, "%s\n", e->asm_block);
            }
            /* Set flag based on operator */
            switch (e->op) {
            case 'Q': fnZValid = 1; break;       /* EQ: Z=1 true */
            case 'n': fnZValid = 2; break;       /* NE: Z=1 false */
            case 'g': fnCmpFlag = 'c'; break;    /* GE: NC=1 true */
            case '<': fnCmpFlag = 'C'; break;    /* LT: C=1 true */
            }
        } else if (call_inst) {
            fdprintf(outFd, "%s\n", call_inst);
            cacheInvalA();  /* Function calls clobber A */
            if (strstr(call_inst, "call")) {
                char *call_pos = strstr(call_inst, "call");
                call_pos += 4;
                while (*call_pos && (*call_pos == ' ' || *call_pos == '\t')) call_pos++;
                if (*call_pos && isCmpFunc(call_pos)) {
                    fnZValid = 1;
                }
            }
        }
        xfree(call_inst);

        /* Only pop stack for word ops (byte ops didn't push) */
        if (left_size != 1 && !usedDirDE) {
            int zflag_saved = fnZValid;
            popStack();
            fnZValid = zflag_saved;
        }
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

        /* Check for word DEREF of register variable - can push directly */
        if (arg_size == 2 && a->op == 'M' && a->left && a->left->op == '$') {
            const char *sym = a->left->symbol;
            struct local_var *var;
            if (sym && sym[0] == '$') sym++;
            var = findVar(sym);
            if (var && var->reg == REG_BC) {
                fdprintf(outFd, "\tpush bc  ; arg %d\n", i);
                freeExpr(a);
                continue;
            } else if (var && var->reg == REG_IX) {
                fdprintf(outFd, "\tpush ix  ; arg %d\n", i);
                freeExpr(a);
                continue;
            }
        }

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

    if (!fnZValid) {
        if (cond_size == 1) {
            emit(S_ORA);
        } else {
            emit(S_AHORL);
        }
    }

    if (e->jump) {
        /* fnZValid: 1=Z means true, 2=Z means false, 0=Z means zero */
        const char *jmp = (fnZValid == 1) ? "jp nz," : "jp z,";
        emitJump(jmp, "_tern_false_", e->label);
    }
    fnZValid = 0;

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
    xfree(e->right);

    freeNode(e);
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
