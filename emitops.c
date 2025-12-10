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

/*
 * Sentinel expr - used instead of NULL to eliminate null checks.
 * Has op=0 and size=0 which won't match any real conditions.
 */
struct expr nullExpr;

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
 * Uses expr fields: e->op, e->size, e->value (amount), e->flags, e->symbol, e->left
 *
 * Algorithm:
 * 1. Set up addressing strings based on location type
 * 2. If post && !unused: save old value
 * 3. Do inc/dec (amount==1: direct; else load/modify/store)
 * 4. If post: restore old; else if !unused: load new
 */
void
emitIncDec(struct expr *e)
{
    int size, unused, is_post, is_dec;
    long amount;
    struct local_var *var;
    const char *rn;     /* byte register name */
    const char *rp;     /* word register pair name */
    const char *sym;    /* global symbol name */
    int ofs;            /* IY offset */
    enum { ID_REG, ID_STACK, ID_GLOBAL, ID_HL } loc;

    size = e->size;
    amount = e->value;
    unused = (e->flags & E_UNUSED) ? 1 : 0;
    is_post = (e->op == AST_POSTINC || e->op == AST_POSTDEC);
    is_dec = (e->op == AST_PREDEC || e->op == AST_POSTDEC);
    if (unused) is_post = 0;

    /* Initialize */
    var = NULL; rn = NULL; rp = NULL; sym = NULL; ofs = 0;

    /* Determine location type and set up addressing */
    if (e->symbol) {
        const char *var_name = stripVarPfx(e->symbol);
        var = findVar(var_name);
        if (var) size = var->size;

        if (var && var->reg != REG_NO) {
            loc = ID_REG;
            rn = byteRegName(var->reg);
            rp = wordRegName(var->reg);
        } else if (var) {
            loc = ID_STACK;
            ofs = var->offset;
        } else {
            loc = ID_GLOBAL;
            sym = stripDollar(e->symbol);
        }
    } else if (e->left) {
        loc = ID_HL;
        emitExpr(e->left);
        e->left = NULL;
    } else {
        freeNode(e);
        return;
    }

    /* === BYTE operations === */
    if (size == 1) {
        /* 1. If post: load old value to A */
        if (is_post) {
            switch (loc) {
            case ID_REG:    fdprintf(outFd, "\tld a, %s\n", rn); break;
            case ID_STACK:  iyFmt("\tld a, (iy %c %d)\n", ofs); break;
            case ID_GLOBAL: emitS(FS_LDAM, sym); emit(S_PUSHAFSV); break;
            case ID_HL:     emit(S_LDAHLPUSH); break;
            }
        }

        /* 2. Do inc/dec */
        if (amount == 1 && loc != ID_GLOBAL) {
            /* Direct inc/dec on location - sets Z flag for zero test */
            switch (loc) {
            case ID_REG:   fdprintf(outFd, "\t%s %s\n", is_dec ? "dec" : "inc", rn); break;
            case ID_STACK: iyFmt(is_dec ? "\tdec (iy %c %d)\n" : "\tinc (iy %c %d)\n", ofs); break;
            case ID_HL:    fdprintf(outFd, "\t%s (hl)\n", is_dec ? "dec" : "inc"); break;
            default: break;
            }
            fnZValid = 2;  /* Z=1 means result is zero (false in bool context) */
        } else {
            /* Load to A, modify, store */
            switch (loc) {
            case ID_REG:    if (!is_post) fdprintf(outFd, "\tld a, %s\n", rn); break;
            case ID_STACK:  if (!is_post) iyFmt("\tld a, (iy %c %d)\n", ofs); break;
            case ID_GLOBAL: if (!is_post) emitS(FS_LDAM, sym); break;
            case ID_HL:     if (!is_post) emit(S_LDAHL); break;
            }
            fdprintf(outFd, "\t%s a, %ld\n", is_dec ? "sub" : "add", amount);
            switch (loc) {
            case ID_REG:    fdprintf(outFd, "\tld %s, a\n", rn); break;
            case ID_STACK:  iyFmt("\tld (iy %c %d), a\n", ofs); break;
            case ID_GLOBAL: emitS(FS_STAM, sym); break;
            case ID_HL:     emit(S_LDHLA); break;
            }
        }

        /* 3. Return value - skip if only condition flags needed and Z is valid */
        if (is_post) {
            if (loc == ID_GLOBAL) emit(S_POPAFRET);
            else if (loc == ID_HL) emit(S_POPAF);
            /* else: A already has old value */
        } else if (!unused && !(fnCondOnly && fnZValid)) {
            /* Load value to A unless only testing condition (fnCondOnly + fnZValid) */
            switch (loc) {
            case ID_REG:    fdprintf(outFd, "\tld a, %s\n", rn); break;
            case ID_STACK:  iyFmt("\tld a, (iy %c %d)\n", ofs); break;
            case ID_GLOBAL: /* A already has new value */ break;
            case ID_HL:     emit(S_LDAHL); break;
            }
        }
    }
    /* === WORD operations === */
    else if (size == 2) {
        /* 1. If post: save old value */
        if (is_post) {
            switch (loc) {
            case ID_REG:    emitWordLoad(var->reg); break;
            case ID_STACK:  loadWordIY(ofs); break;
            case ID_GLOBAL: emitS(FS_LDHLM, sym); emit(S_PUSHHLOV); break;
            case ID_HL:
                emit(S_HLTODE);
                emit(S_LDAHLINC);
                emit(S_LDAHLHIGH);
                emit(S_PUSHHLEXDE);
                break;
            }
        } else if (loc == ID_HL) {
            emit(S_HLTODE);
        }

        /* 2. Do inc/dec */
        if (amount <= 4) {
            int i;
            switch (loc) {
            case ID_REG:
                for (i = 0; i < amount; i++)
                    fdprintf(outFd, "\t%s %s\n", is_dec ? "dec" : "inc", rp);
                break;
            case ID_STACK:
                /* 8-bit dec/inc affects C flag, so we can use borrow/carry */
                for (i = 0; i < amount; i++) {
                    iyFmt(is_dec ? "\tdec (iy %c %d)\n" : "\tinc (iy %c %d)\n", ofs);
                    emit(S_JRNC3);
                    iyFmt(is_dec ? "\tdec (iy %c %d)\n" : "\tinc (iy %c %d)\n", ofs + 1);
                }
                break;
            case ID_GLOBAL:
                if (!is_post) emitS(FS_LDHLM, sym);
                for (i = 0; i < amount; i++)
                    fdprintf(outFd, "\t%s hl\n", is_dec ? "dec" : "inc");
                emitS(FS_STHLM, sym);
                break;
            case ID_HL:
                for (i = 0; i < amount; i++) {
                    fdprintf(outFd, "\t%s (hl)\n", is_dec ? "dec" : "inc");
                    emit(S_JRNZ4INC);
                    fdprintf(outFd, "\t%s (hl)\n\tdec hl\n", is_dec ? "dec" : "inc");
                }
                break;
            }
        } else {
            /* Use DE as addend */
            switch (loc) {
            case ID_REG:
                fdprintf(outFd, "\tld de, %ld\n\tadd %s, de\n", is_dec ? -amount : amount, rp);
                break;
            case ID_STACK:
                loadWordIY(ofs);
                fdprintf(outFd, "\tld de, %ld\n", is_dec ? -amount : amount);
                emit(S_ADDHLDE);
                storeWordIY(ofs);
                break;
            case ID_GLOBAL:
                if (!is_post) emitS(FS_LDHLM, sym);
                fdprintf(outFd, "\tld de, %ld\n", is_dec ? -amount : amount);
                emit(S_ADDHLDE);
                emitS(FS_STHLM, sym);
                break;
            case ID_HL:
                emit(S_LDCHL);
                emit(S_PUSHHLBCHL);
                fdprintf(outFd, "\tld de, %ld\n\tadd hl, de\n", is_dec ? -amount : amount);
                emit(S_EXDEHLPOPHL);
                emit(S_STDEHL);
                break;
            }
        }

        /* 3. Return value */
        if (is_post) {
            switch (loc) {
            case ID_REG: /* HL already has old value */ break;
            case ID_STACK: /* HL already has old value */ break;
            case ID_GLOBAL: emit(S_POPHLRET); break;
            case ID_HL: emit(S_POPHL); break;
            }
        } else if (!unused) {
            switch (loc) {
            case ID_REG:    emitWordLoad(var->reg); break;
            case ID_STACK:  loadWordIY(ofs); break;
            case ID_GLOBAL: /* HL already has new value */ break;
            case ID_HL:
                emit(S_LDAHLINC);
                emit(S_LDAHLHIGH);
                break;
            }
        }

        if (loc == ID_REG && var->reg == REG_BC) {
            fnABCValid = 0;
            cacheInvalA();
        }
    }
    /* === LONG operations - call helpers === */
    else if (size == 4) {
        /* Load address of long to HL for helper */
        switch (loc) {
        case ID_GLOBAL:
            emitS(FS_LDHL, sym);
            break;
        case ID_STACK:
            emit1(F_LEAIY, ofs);
            break;
        case ID_HL:
            /* emitExpr computed address of pointer variable into HL.
             * Need to load through it to get the pointer value (address of long). */
            emit(S_LDHLIND);
            break;
        default: break;
        }
        /* Call linc or ldec helper - takes address in HL, always increments by 1 */
        fdprintf(outFd, "\tcall %s\n",
                 is_post ? (is_dec ? "ldecp" : "lincp")
                         : (is_dec ? "ldec" : "linc"));
    }

    freeNode(e);
}

/*
 * Emit assignment operation
 */
void
emitAssign(struct expr *e)
{
    struct expr *left = e->left ? e->left : &nullExpr;
    struct expr *right = e->right ? e->right : &nullExpr;
    struct expr *ll, *lleft, *lright;
    unsigned char rhs_dest, rhs_op;

#ifdef DEBUG
    if (TRACE(T_ASSIGN)) {
        fdprintf(2, "emitAssign: enter\n");
    }
#endif

    /* Optimize: constant to register-allocated variable */
    if (right->op == 'C' && e->size == 2 &&
        left->op == '$' && left->symbol) {
        struct local_var *v = findVar(stripVarPfx(left->symbol));
        if (v && (v->reg == REG_BC || v->reg == REG_IX)) {
            fdprintf(outFd, "\tld %s, %ld\n",
                v->reg == REG_BC ? "bc" : "ix", right->value);
            freeExpr(right);
            freeExpr(left);
            freeNode(e);
            return;
        }
    }

    /* Optimize: constant to (ptr + offset) where ptr is stack/global
     * Pattern: (= (+ (M $ptr) offset) const_value)
     * Emit: load ptr to HL, add offset inline, store const directly */
    lleft = left->left ? left->left : &nullExpr;
    lright = left->right ? left->right : &nullExpr;
    ll = lleft->left ? lleft->left : &nullExpr;
    if (right->op == 'C' &&
        left->op == '+' &&
        lleft->op == 'M' &&
        ll->op == '$' &&
        lright->op == 'C') {
        const char *sym = ll->symbol;
        const char *vn = stripVarPfx(sym);
        struct local_var *var = findVar(vn);
        long offset = lright->value;
        long value = right->value;

        /* Skip if ptr is in IX - handled by E_IXASSIGN path */
        if (var && var->reg == REG_IX)
            goto notConstStore;

        /* Load pointer to HL */
        if (var) {
            /* Stack variable */
            loadWordIY(var->offset);
        } else {
            /* Global */
            fdprintf(outFd, "\tld hl, (%s)\n", stripDollar(sym));
        }

        /* Add offset inline: ld a,ofs; add l; ld l,a; adc h; sub l; ld h,a */
        if (offset != 0) {
            if (offset >= 1 && offset <= 4) {
                /* Small offset - use inc hl */
                while (offset--) emit(S_INCHL);
            } else if (offset < 256) {
                fdprintf(outFd, "\tld a, %ld\n\tadd a, l\n\tld l, a\n", offset);
                emit(S_ADCH0);
            } else {
                fdprintf(outFd, "\tld de, %ld\n\tadd hl, de\n", offset);
            }
        }

        /* Store constant directly */
        if (e->size == 1) {
            fdprintf(outFd, "\tld (hl), %ld\n", value & 0xff);
        } else if (e->size == 2) {
            fdprintf(outFd, "\tld (hl), %ld\n", value & 0xff);
            emit(S_INCHL);
            fdprintf(outFd, "\tld (hl), %ld\n", (value >> 8) & 0xff);
        }

        clearHL();  /* HL is modified, don't let cache reuse it */
        freeExpr(left);
        freeExpr(right);
        freeNode(e);
        return;
    }
notConstStore:
    /* Capture RHS info before emitExpr frees it */
    rhs_dest = right->dest;
    rhs_op = right->op;

    /* Optimize: IX = IX->member (self-referential IX load)
     * Pattern: lhs = $p where p is IX, rhs = (M (+ (M $p) offset))
     * Emit: ld h,(ix+ofs+1); ld l,(ix+ofs); push hl; pop ix */
    if (e->size == 2 && left->op == '$' && left->symbol &&
        right->op == 'M' && right->loc == LOC_IX) {
        struct local_var *lv = findVar(stripVarPfx(left->symbol));
        if (lv && lv->reg == REG_IX) {
            int ofs = right->offset;
            /* Load H first, then L (natural order for push) */
            fdprintf(outFd, "\tld h, (ix %c %d)\n",
                     ofs + 1 >= 0 ? '+' : '-', ofs + 1 >= 0 ? ofs + 1 : -(ofs + 1));
            fdprintf(outFd, "\tld l, (ix %c %d)\n",
                     ofs >= 0 ? '+' : '-', ofs >= 0 ? ofs : -ofs);
            emit(S_HLPIX);
            freeExpr(right);
            freeExpr(left);
            freeNode(e);
            return;
        }
    }

    /* Long (4-byte) constant assignment: load all 32 bits into HLHL' */
    if (e->size == 4 && right->op == 'C') {
        long val = right->value;
        fdprintf(outFd, "\tld hl, %d\n", (int)(val & 0xffff));
        emit(S_EXX);
        fdprintf(outFd, "\tld hl, %d\n", (int)((val >> 16) & 0xffff));
        emit(S_EXX);
        freeExpr(right);
        e->right = NULL;
        goto do_store;
    }

    /* Optimize: word = WIDEN(byte) for IY-indexed variable
     * Pattern: =s $var Ws expr  ->  ld (iy+ofs),a; ld (iy+ofs+1),0
     * Avoids: ld l,a; ld h,0; ld (iy+ofs),l; ld (iy+ofs+1),h */
    if (e->size == 2 && right->op == 'W' &&
        left->op == '$' && left->symbol) {
        struct local_var *lv = findVar(stripVarPfx(left->symbol));
        if (lv && lv->reg == REG_NO) {
            int ofs = lv->offset;
            emitExpr(right->left);  /* byte result in A; emitExpr frees it */
            right->left = NULL;     /* prevent double-free */
            fdprintf(outFd, "\tld (iy %c %d), a\n",
                     ofs >= 0 ? '+' : '-', ofs >= 0 ? ofs : -ofs);
            fdprintf(outFd, "\tld (iy %c %d), 0\n",
                     ofs + 1 >= 0 ? '+' : '-', ofs + 1 >= 0 ? ofs + 1 : -(ofs + 1));
            freeExpr(right);
            freeExpr(left);
            freeNode(e);
            return;
        }
    }

    /* Emit right child first (value goes to PRIMARY) */
    emitExpr(right);
    e->right = NULL;  /* Mark as consumed - emitExpr freed it */
#ifdef DEBUG
    if (TRACE(T_ASSIGN)) {
        fdprintf(2, "emitAssign: after emitExpr(right)\n");
    }
#endif

do_store:
    /* After do_store: left is still valid, right has been consumed */
    left = e->left ? e->left : &nullExpr;
    lleft = left->left ? left->left : &nullExpr;
    ll = lleft->left ? lleft->left : &nullExpr;

    /* Check for IX-indexed store */
    if ((e->flags & E_IXASSIGN) && left->op == '+' &&
        lleft->op == 'M' &&
        lleft->type_str == 'p' &&
        ll->op == '$' && ll->symbol) {
        const char *var_symbol = ll->symbol;
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
                fnIXHLOfs = offset;  /* After EXX pair, HL has low word */
                fnIXHL32 = 1;        /* HL' also has high word */
            }
            freeExpr(left);
            freeNode(e);
            return;
        }
    }

    /* Simple variable assignment */
    if (left->op == '$' && left->symbol) {
#ifdef DEBUG
        if (TRACE(T_ASSIGN)) {
            fdprintf(2, "emitAssign: simple variable\n");
        }
#endif
        /* Skip store if RHS was loaded directly into target register */
        {
            struct local_var *lv = findVar(stripVarPfx(left->symbol));
            int skip = 0;
            /* Skip only if RHS destination is BC and RHS is not a call
             * (calls always return in HL regardless of dest) */
            if (lv && lv->reg == REG_BC && rhs_dest == R_BC &&
                rhs_op != '@')
                skip = 1;
            if (!skip)
                storeVar(left->symbol, e->size, 1);
        }
#ifdef DEBUG
        if (TRACE(T_ASSIGN)) {
            fdprintf(2, "emitAssign: after storeVar\n");
        }
#endif
    }
    /* Pointer dereference or complex lvalue */
    else if (left->op == 'M' || left->op == '+') {
        /* For (= (M $ptr) val): emit (M $ptr) to get target address
         * For (= (+ ...) val): emit the + expression to get address */
        if (e->size == 1) {
            emit(S_ESAVE);
            emitExpr(left);
            e->left = NULL;
            emit(S_HLDE);
        } else if (e->size == 2) {
            emit(S_DESAVE);
            emitExpr(left);
            e->left = NULL;
            emit(S_HLDE);
            emit(S_INCHL);
            emit(S_HLD);
        } else if (left->op == '+' && e->size == 4) {
            emit(S_PUSHHLLOW); emit(S_EXX); emit(S_PUSHHLUPP); emit(S_EXX);
            emitExpr(left);
            e->left = NULL;
            emit(S_DEADR); emit(S_POPHLUPP); emit(S_PUSHDESV);
            emit(S_EXX); emit(S_POPDEADR); emit(S_POPHLLOW);
            storeHLtoDE();
            emit(S_EXX);
            storeHLtoDE();
            emit(S_EXX);
        }
    }
#ifdef DEBUG
    if (TRACE(T_ASSIGN)) {
        fdprintf(2, "emitAssign: returning\n");
    }
#endif
}

/*
 * Emit inline byte comparison (cp instruction)
 * Returns 1 if emitted, 0 if not applicable
 */
static int
emitByteCp(struct expr *e)
{
    struct expr *left = e->left;
    struct expr *right = e->right;
    int op = e->op;
    struct bytecmp cmp;
    int val;

    /* Only for byte comparisons */
    if (!left || left->size != 1)
        return 0;

    /* For >=, <=, >, < with constant, use carry flag directly */
    if ((op == 'g' || op == 'L' || op == '>' || op == '<') &&
        right && right->op == 'C') {
        val = right->value & 0xff;

        /* Emit left operand to A */
        emitExpr(left);
        /* Note: cp does NOT change A, so don't invalidate cache */

        /* Emit cp with adjusted constant based on comparison */
        switch (op) {
        case 'g':  /* GE (>=): cp N, nc = true */
            emit1(F_CP, val);
            fnCmpFlag = 'c';  /* nc = true */
            break;
        case '<':  /* LT (<): cp N, c = true */
            emit1(F_CP, val);
            fnCmpFlag = 'C';  /* c = true */
            break;
        case '>':  /* GT (>): cp N+1, nc = true */
            if (val >= 255) {
                /* A > 255 always false - emit compare that always fails */
                emit(S_ORASCF);  /* set carry */
                fnCmpFlag = 'c';  /* nc = true, but carry is set so always false */
            } else {
                emit1(F_CP, val + 1);
                fnCmpFlag = 'c';  /* nc = true */
            }
            break;
        case 'L':  /* LE (<=): cp N+1, c = true */
            if (val >= 255) {
                /* A <= 255 always true - emit compare that always succeeds */
                emit(S_ORA);  /* clears carry */
                fnCmpFlag = 'c';  /* nc = true, always true */
            } else {
                emit1(F_CP, val + 1);
                fnCmpFlag = 'C';  /* c = true */
            }
            break;
        }

        freeExpr(right);
        return 1;
    }

    /* Only EQ/NE for non-constant comparisons */
    if (op != 'Q' && op != 'n')
        return 0;

    if (getByteCmp(right, &cmp) == CMP_NONE)
        return 0;

    /* Emit left operand to A */
    emitExpr(left);
    /* Note: cp does NOT change A, so don't invalidate cache */

    /* Emit cp instruction based on right operand type */
    switch (cmp.kind) {
    case CMP_CONST:
        emit1(F_CP, cmp.offset);
        break;
    case CMP_IY:
        iyFmt("\tcp (iy %c %d)\n", cmp.offset);
        break;
    case CMP_REG:
        if (cmp.reg == REG_IX) {
            emit(S_CPIXZ);
        } else if (byteRegName(cmp.reg) || cmp.reg == REG_BC) {
            /* BC low byte is C */
            const char *rn = cmp.reg == REG_BC ? "c" : byteRegName(cmp.reg);
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

    freeExpr(right);

    /* cp sets Z=1 if equal: EQ wants Z=1 true, NE wants Z=1 false */
    fnZValid = (op == 'Q') ? 1 : 2;
    return 1;
}

/* Long (32-bit) binary operation */
static void
emitLongBinop(struct expr *e, int is_cmp)
{
    const char *fn = NULL;
    emitExpr(e->left);
    emit(S_EXX); emit(S_PUSHHL); emit(S_EXX); emit(S_PUSHHL);
    emitExpr(e->right);
    emit(S_HLTODE);
    emit(S_EXX); emit(S_HLTODE); emit(S_EXX);
    emit(S_POPHL);
    emit(S_EXX); emit(S_POPHL); emit(S_EXX);
    switch (e->op) {
    case '+': fn = "add32"; break;
    case '-': fn = "sub32"; break;
    case '*': fn = "mul3232"; break;
    case '/': fn = "div3232"; break;
    case '%': fn = "mod3232"; break;
    case '&': fn = "and32"; break;
    case '|': fn = "or32"; break;
    case '^': fn = "xor32"; break;
    case 'w': fn = "shr3232"; break;
    case 'y': fn = "shl3232"; break;
    case 'Q': fn = "eq3232"; break;
    case 'n': fn = "ne3232"; break;
    case '<': fn = "lt3232"; break;
    case '>': fn = "gt3232"; break;
    case 'g': fn = "ge3232"; break;
    case 'L': fn = "le3232"; break;
    default: break;
    }
    if (fn) emitS(FS_CALL, fn);
    if (is_cmp) fnZValid = 2;
    freeNode(e);
}

/* Signed comparison with 0 using bit 7 test. Returns 1 if handled. */
static int
emitSignCmp0(struct expr *e)
{
    struct expr *l = e->left;
    int ofs;
    const char *sym;

    if (e->op == 'g' || e->op == '<') {
        switch (l->loc) {
        case LOC_REG:
            if (l->reg == R_BC) emit(S_BIT7B);
            else if (l->reg == R_HL) emit(S_BIT7H);
            else if (l->reg == R_DE) emit(S_BIT7D);
            else if (l->reg == R_IX) emit(S_IXHBIT7);
            else return 0;
            break;
        case LOC_STACK:
            ofs = l->offset + 1;
            fdprintf(outFd, "\tbit 7, (iy %c %d)\n",
                     ofs >= 0 ? '+' : '-', ofs >= 0 ? ofs : -ofs);
            break;
        case LOC_IX:
            ofs = l->offset + 1;
            fdprintf(outFd, "\tbit 7, (ix %c %d)\n",
                     ofs >= 0 ? '+' : '-', ofs >= 0 ? ofs : -ofs);
            break;
        case LOC_MEM:
            if (!l->left || !l->left->symbol) return 0;
            sym = stripDollar(l->left->symbol);
            fdprintf(outFd, "\tld a, (%s+1)\n\tbit 7, a\n", sym);
            break;
        default:
            return 0;
        }
        fnZValid = (e->op == 'g') ? 1 : 2;
    } else if (e->op == 'L' || e->op == '>') {
        switch (l->loc) {
        case LOC_REG:
            if (l->reg == R_BC) { emit(S_BIT7B); fnDualReg = R_BC; }
            else if (l->reg == R_HL) { emit(S_BIT7H); fnDualReg = R_HL; }
            else if (l->reg == R_DE) { emit(S_BIT7D); fnDualReg = R_DE; }
            else return 0;
            break;
        case LOC_STACK:
            ofs = l->offset;
            fdprintf(outFd, "\tld l, (iy %c %d)\n\tld h, (iy %c %d)\n",
                     ofs >= 0 ? '+' : '-', ofs >= 0 ? ofs : -ofs,
                     ofs + 1 >= 0 ? '+' : '-', ofs + 1 >= 0 ? ofs + 1 : -(ofs + 1));
            emit(S_BIT7H);
            fnDualReg = R_HL;
            break;
        case LOC_MEM:
            if (!l->left || !l->left->symbol) return 0;
            sym = stripDollar(l->left->symbol);
            fdprintf(outFd, "\tld hl, (%s)\n\tbit 7, h\n", sym);
            fnDualReg = R_HL;
            break;
        default:
            return 0;
        }
        fnDualCmp = e->op;
    } else {
        return 0;
    }
    freeExpr(e->left);
    freeExpr(e->right);
    e->left = e->right = NULL;
    freeNode(e);
    return 1;
}

/* Word comparison. Returns 1 if handled. */
static int
emitWordCmp(struct expr *e)
{
    struct expr *left = e->left ? e->left : &nullExpr;
    struct expr *right = e->right ? e->right : &nullExpr;
    int op = e->op;

    /* Special case: compare with 0 */
    if (right->op == 'C' && right->value == 0) {
        if (!emitSimplLd(left)) emitExpr(left);
        switch (op) {
        case 'Q':  /* == 0 */
        case 'n':  /* != 0 */
            emit(S_AHORL);
            fnZValid = (op == 'Q') ? 1 : 2;
            break;
        case 'L':  /* <= 0: zero or negative */
            fnDualCmp = 'L';
            fnDualReg = R_HL;
            emit(S_AHORL);
            emit(S_BIT7H);
            break;
        case '<':  /* < 0: negative */
            emit(S_BIT7H);
            fnZValid = 2;  /* NZ if negative */
            break;
        case 'g':  /* >= 0: non-negative */
            emit(S_BIT7H);
            fnZValid = 1;  /* Z if non-negative */
            break;
        case '>':  /* > 0: positive (non-zero and non-negative) */
            fnDualCmp = '>';
            fnDualReg = R_HL;
            emit(S_AHORL);
            emit(S_BIT7H);
            break;
        }
        freeNode(e);
        return 1;
    }
    /* General case: promote byte constant to word if needed */
    if (right->op == 'C' && right->size == 1)
        right->size = 2;
    if (!emitSimplLd(left)) emitExpr(left);
    if (!emitSimplLd(right)) emitExpr(right);
    emit(S_SBCHLDE);
    switch (op) {
    case 'Q': fnZValid = 1; break;
    case 'n': fnZValid = 2; break;
    case 'g': fnCmpFlag = 'c'; break;
    case '<': fnCmpFlag = 'C'; break;
    case '>': fnCmpFlag = 'C'; break;
    case 'L': fnCmpFlag = 'c'; break;
    }
    freeNode(e);
    return 1;
}

/* Word left shift by constant. Returns 1 if handled. */
static int
emitWordShift(struct expr *e)
{
    int i, cnt;
    struct expr *left = e->left;
    struct expr *right = e->right;
    struct expr *ll;  /* left->left */

    if (e->op != 'y' || !right || right->op != 'C' ||
        right->value < 1 || right->value > 8)
        return 0;
    cnt = right->value;
    ll = left ? left->left : NULL;
    if (left && left->op == 'M' && (left->opflags & OP_REGVAR) &&
        ll && ll->op == '$') {
        struct local_var *v = left->cached_var;
        if (v && v->reg == REG_BC) {
            emit(S_BCHL);
            freeExpr(left);
        } else {
            emitExpr(left);
            emit(S_EXDEHL);
        }
    } else {
        emitExpr(left);
        emit(S_EXDEHL);
    }
    for (i = 0; i < cnt; i++)
        emit(S_ADDHLHL);
    freeExpr(right);
    freeNode(e);
    return 1;
}

/*
 * Emit binary operation - generates code directly
 */
void
emitBinop(struct expr *e)
{
    /* Use sentinels for null-safe condition checks */
    struct expr *left = e->left ? e->left : &nullExpr;
    struct expr *right = e->right ? e->right : &nullExpr;
    struct expr *rl, *rr;   /* right->left, right->right */
    int op = e->op;
    int left_size = left->size ? left->size : 2;
    int result_size = e->size ? e->size : left_size;
    int is_cmp = (op == '>' || op == '<' || op == 'g' ||
                  op == 'L' || op == 'Q' || op == 'n');

    /* Long (32-bit) operations */
    if (left_size == 4 || result_size == 4) {
        emitLongBinop(e, is_cmp);
        return;
    }

    /* Cache right->left and right->right with sentinels */
    rl = right->left ? right->left : &nullExpr;
    rr = right->right ? right->right : &nullExpr;

    /* Optimize: ptr + (byte << const) for array indexing */
    if (op == '+' && left->op == '$' && left->symbol &&
        right->op == 'y' &&
        rl->size == 1 &&
        rr->op == 'C' && rr->value >= 1 && rr->value <= 7) {
        int i, cnt = rr->value;
        const char *sym = stripDollar(left->symbol);
        emitExpr(rl);
        for (i = 0; i < cnt; i++)
            out("\tadd a, a\n");
        fdprintf(outFd, "\tld hl, %s\n", sym);
        out("\tadd a, l\n\tld l, a\n\tjr nc, $+3\n\tinc h\n");
        freeExpr(left);
        freeExpr(rr);
        freeNode(right);
        freeNode(e);
        return;
    }

    /* Byte operations with immediate constant */
    if (left_size == 1 && result_size == 1 && !is_cmp &&
        op != 'y' && op != 'w' &&
        right->op == 'C' &&
        right->value >= 0 && right->value <= 255) {
        int val = right->value & 0xff;
        emitExpr(left);
        freeExpr(right);
        switch (op) {
        case '&': emit1(F_AND, val); break;
        case '|': emit1(F_OR, val); break;
        case '^': emit1(F_XOR, val); break;
        case '+': emit1(F_ADDA, val); break;
        case '-': emit1(F_SUB, val); break;
        default: break;
        }
        fnZValid = 2;
        fnARegvar = 0;  /* A no longer holds regvar value */
        return;
    }

    /* Byte comparisons */
    if (left_size == 1 && result_size == 1 && is_cmp && emitByteCp(e))
        return;

    /* Signed comparisons with 0 */
    if (left_size == 2 && is_cmp && !(left->flags & E_UNSIGNED) &&
        right->op == 'C' && right->value == 0) {
        if (emitSignCmp0(e))
            return;
    }

    /* Word comparisons */
    if (left_size == 2 && is_cmp) {
        emitWordCmp(e);
        return;
    }

    /* Byte left shift by constant */
    if (left_size == 1 && result_size == 1 && op == 'y' &&
        right->op == 'C' &&
        right->value >= 1 && right->value <= 7) {
        int i, cnt = right->value;
        emitExpr(left);
        for (i = 0; i < cnt; i++)
            out("\tadd a, a\n");
        freeExpr(right);
        freeNode(e);
        return;
    }

    /* Byte binary ops */
    if (left_size == 1) {
        const char *fn = NULL;
        emitExpr(left);
        emit(S_ESAVE);
        emitExpr(right);
        switch (op) {
        case '*': fn = "bmul"; break;
        case '/': fn = "bdiv"; break;
        case '%': fn = "bmod"; break;
        case 'w': fn = "brsh"; break;
        case 'y': fn = "blsh"; break;
        default: break;
        }
        if (fn) emitS(FS_CALL, fn);
        return;
    }

    /* Word left shift by constant */
    if (emitWordShift(e))
        return;

    /* Word binary ops */
    if (!emitSimplLd(left)) emitExpr(left);
    if (!emitSimplLd(right)) emitExpr(right);

    switch (op) {
    case '+': emit(S_ADDHLDE); break;
    case '-': emit(S_EXDEHL); emit(S_SBCHLDE); break;
    case '*': emit(S_CALLIMUL); break;
    case '/': emit(S_CALLIDIV); break;
    case '%': fdprintf(outFd, "\tex de, hl\n\tcall imod\n"); break;
    case '&': emit(S_ANDHLDE); break;
    case '|': emit(S_ORHLDE); break;
    case '^': emit(S_XORHLDE); break;
    case 'w': emit(S_CALLIRSH); break;
    case 'y': emit(S_CALLILSH); break;
    default: break;
    }
    freeNode(e);
}

/*
 * Emit function call directly from AST
 * e->left = function (SYM node)
 * e->right = wrapper chain of arguments
 * e->value = argument count
 */
void
emitCall(struct expr *e)
{
    struct expr *args[32];
    struct expr *left = e->left;
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
        struct expr *al;  /* a->left */
        int arg_size;
        if (!a) continue;

        /* Save size before emitExpr frees the node */
        arg_size = a->size;
        al = a->left;

        /* Check for word DEREF of register variable - can push directly */
        if (arg_size == 2 && a->op == 'M' && al && al->op == '$') {
            const char *sym = al->symbol;
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
    if (left && left->op == '$' && left->symbol) {
        func_name = left->symbol;
        if (func_name[0] == '$') func_name++;
        emitS(FS_CALL, func_name);
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
void
emitTernary(struct expr *e)
{
    struct expr *left = e->left;
    struct expr *right = e->right;
    struct expr *rl, *rr;
    unsigned char cond_size;

    cond_size = left ? left->size : 2;
    rl = right ? right->left : NULL;
    rr = right ? right->right : NULL;

    emitExpr(left);

    if (e->flags & E_JUMP) {
        const char *jmp;
        if (fnCmpFlag) {
            /* Carry-based comparison: 'c' = NC true, 'C' = C true */
            /* Jump to false when condition is false */
            jmp = (fnCmpFlag == 'c') ? "jp c," : "jp nc,";
            fnCmpFlag = 0;
        } else if (fnZValid) {
            /* Z-flag comparison: 1 = Z true, 2 = Z false */
            jmp = (fnZValid == 1) ? "jp nz," : "jp z,";
            fnZValid = 0;
        } else {
            /* No comparison - test for zero */
            if (cond_size == 1) {
                emit(S_ORA);
            } else {
                emit(S_AHORL);
            }
            jmp = "jp z,";
        }
        emitJump(jmp, "tF", e->label);
    }
    fnZValid = 0;

    if (rl) {
        emitExpr(rl);
    }

    if (right && (right->flags & E_JUMP)) {
        emitJump("jp", "tE", right->label);
    }

    emit1(F_TERNF, e->label);

    if (rr) {
        emitExpr(rr);
    }

    if (right) {
        emit1(F_TERNE, right->label);
    }

    xfree(right);

    freeNode(e);
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
