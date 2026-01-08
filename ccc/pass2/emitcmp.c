/*
 * emitcmp.c - Comparison code emission
 */
#include <stdio.h>
#include "cc2.h"
#include "templates.h"
#include "../cpp/lexeme.h"

/*
 * Emit conditional jump for comparison result
 * op: comparison operator (LT, EQ, NEQ) - pass1 normalizes all others
 * aux2: label info (negative = TRUE jump to ht, positive = FALSE jump to no)
 */
void
emitCondJmp(unsigned char op, int aux2)
{
    if (aux2 < 0) {
        /* TRUE jump to ht{target} */
        int target = -aux2;
        switch (op) {
        case EQ: emit("jp z,ht%d_%d", target, fnIndex); break;
        case NEQ: emit("jp nz,ht%d_%d", target, fnIndex); break;
        case LT: emit("jp c,ht%d_%d", target, fnIndex); break;
        }
    } else {
        /* FALSE jump to no{aux2} */
        switch (op) {
        case EQ: emit("jp nz,no%d_%d", aux2, fnIndex); break;
        case NEQ: emit("jp z,no%d_%d", aux2, fnIndex); break;
        case LT: emit("jp nc,no%d_%d", aux2, fnIndex); break;
        }
    }
}

/*
 * Emit comparison operators: <, Q(==), n(!=)
 * Pass1 normalizes >, <=, >= to these three forms
 */
void
emitCompare(struct expr *e)
{
    comment("%c%c%s [", e->op, e->type, e->cond ? " C" : "");
    indent += 2;
    if (e->special == SP_CMPHL) {
        /* byte cmp with (hl): left is complex (addr in hl), right is simple */
        struct expr *simple = e->right;
        struct expr *complex = e->left;
        comment("CMPHL");
        emitExpr(complex->left);  /* address to HL */
        emit("ld a,(hl)");        /* load left operand */
        if (simple->op == AST_CONST) {
            emit("cp %d", simple->v.c & 0xff);
        } else if (simple->op == REGVAR) {
            emit("cp %s", regnames[simple->aux]);
        } else if (simple->op == LOCALVAR) {
            char *rn = (simple->aux == R_IX) ? "ix" : "iy";
            emit("cp (%s%o)", rn, simple->offset);
        } else if (simple->op == DEREF && simple->left->op == SYM) {
            emit("ld hl,%s", simple->left->sym);
            emitT(T_CPHL);
        } else if (simple->op == DEREF && simple->left->op == LOCALVAR) {
            char *rn = (simple->left->aux == R_IX) ? "ix" : "iy";
            emit("cp (%s%o)", rn, simple->left->offset);
        } else {
            emit("XXXXXXXXX simple op=%c", simple->op);
        }
        if (e->cond)
            emitCondJmp(e->op, e->aux2);
        else
            goto cmpresult;
    } else if (e->special == SP_CMPV) {
        /* byte cmp V node with constant: ld a,const; cp (iy/ix+off) */
        char *rn = (e->aux == R_IX) ? "ix" : "iy";
        comment("CMPV %s%+d #%d", rn, e->offset, e->incr);
        emit("ld a,%d", e->incr);
        emit("cp (%s%o)", rn, e->offset);
        if (e->cond)
            emitCondJmp(e->op, e->aux2);
        else
            goto cmpresult;
    } else if (e->special == SP_CMPR) {
        /* byte cmp regvar with constant: ld a,const; cp reg */
        char *rn = (e->aux == R_B) ? "b" : "c";
        comment("CMPR %s #%d", rn, e->incr);
        emit("ld a,%d", e->incr);
        emit("cp %s", rn);
        if (e->cond)
            emitCondJmp(e->op, e->aux2);
        else
            goto cmpresult;
    } else if (e->special == SP_SIGN) {
        /* sign test: bit 7 of high byte */
        comment("SIGN $%s ofs=%d", e->sym ? e->sym : "?", e->offset);
        if (e->offset > 0)
            emit("ld hl,%s+%d", e->sym, e->offset);
        else
            emit("ld hl,%s", e->sym);
        emitT(T_BIT7HL);
        if (!e->cond) {
            int lbl = labelCnt++;
            emitT(T_LDA0);
            emit("jp nz,sg%d_%d", lbl, fnIndex);
            emitT(T_INCA);
            emit("sg%d_%d:", lbl, fnIndex);
        }
    } else if (e->special == SP_SIGNREG) {
        /* sign test regvar BC: bit 7,b */
        comment("SIGNREG bc");
        emitT(T_BIT7B);
        if (!e->cond) {
            int lbl = labelCnt++;
            emitT(T_LDA0);
            emit("jp nz,sg%d_%d", lbl, fnIndex);
            emitT(T_INCA);
            emit("sg%d_%d:", lbl, fnIndex);
        }
    } else if (e->special == SP_CMPEQ) {
        /* Word equality with 0, 1, -1: zero demand */
        comment("CMPEQ %d", e->incr);
        emitExpr(e->left);
        if (e->incr == 1)
            emit("dec hl");
        else if (e->incr == -1 || e->incr == 0xffff)
            emit("inc hl");
        /* incr == 0: no modification, just test HL */
        if (!e->cond) {
            /* Not conditional - convert HL to boolean in A */
            /* HL==0 means equal (true for EQ, false for NEQ) */
            int lbl = labelCnt++;
            emit("ld a,h");
            emit("or l");
            if (e->op == EQ) {
                /* == : result is 1 if HL==0, else 0 */
                emitT(T_LDA1);
                emit("jr z,eq%d_%d", lbl, fnIndex);
                emit("xor a");
            } else {
                /* != : result is 0 if HL==0, else 1 */
                emitT(T_LDA0);
                emit("jr z,eq%d_%d", lbl, fnIndex);
                emitT(T_INCA);
            }
            emit("eq%d_%d:", lbl, fnIndex);
        }
        /* In conditional context, IF handler will test HL */
    } else {
        /* General comparison - use operand type, not result type */
        unsigned char ctype = e->left->type;
        /* Check if comparing subtraction result against 0 - sub sets flags */
        int subCmpZero = (e->left->op == MINUS && e->right->op == AST_CONST &&
                          e->right->v.s == 0);
        if (ISLONG(ctype)) {
            /* Long comparison: emit both, call lcmp */
            emitExpr(e->left);
            emitExpr(e->right);
            emit("call lcmp");
        } else if (subCmpZero) {
            /* Subtraction compared to 0: sub already sets flags */
            emitExpr(e->left);
            if (ISBYTE(ctype) && e->left->size == 2)
                emitT(T_LDAL);
            /* No comparison needed - sub/sbc already set Z flag */
            if (e->cond) {
                emitCondJmp(e->op, e->aux2);
                indent -= 2;
                comment("]");
                return;
            }
            goto cmpresult;
        } else {
            /* Depth-first: emit left, save, emit right, compare */
            emitExpr(e->left);
            if (ISBYTE(ctype)) {
                if (e->left->size == 2)
                    emitT(T_LDAL);
                /* Byte compare with constant: use cp immediate */
                if (e->right->op == AST_CONST) {
                    emit("cp %d", e->right->v.c & 0xff);
                } else {
                    emit("ld e,a");  /* save left to E */
                    emitExpr(e->right);
                    /* A has right, E has left. Compare left - right = E - A */
                    emit("ld b,a");
                    emit("ld a,e");
                    emitT(T_CPB);
                }
            } else if (ISWORD(ctype)) {
                emitT(T_EXDE);  /* save left to DE */
                emitExpr(e->right);
                emitT(T_EXDE);  /* left to HL, right to DE */
                emit("or a");
                emit("sbc hl,de");
            }
        }
        /* General comparison: emit conditional jump if in cond context */
        if (e->cond) {
            emitCondJmp(e->op, e->aux2);
            indent -= 2;
            comment("]");
            return;
        }
    }
cmpresult:
    if (!e->cond && e->special != SP_SIGN && e->special != SP_SIGNREG) {
        switch (e->op) {
        case EQ:  /* == */
            emitT(T_LDA0);
            emit("jr nz,$+3");
            emitT(T_INCA);
            break;
        case NEQ:  /* != */
            emitT(T_LDA0);
            emit("jr z,$+3");
            emitT(T_INCA);
            break;
        case LT:  /* < */
            emitT(T_LDA0);
            emit("jr nc,$+3");
            emitT(T_INCA);
            break;
        }
    }
    indent -= 2;
    comment("]");
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
