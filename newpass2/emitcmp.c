/*
 * emitcmp.c - Comparison code emission
 */
#include <stdio.h>
#include "cc2.h"

/*
 * Emit conditional jump for comparison result
 * op: comparison operator (<, >, Q, n, L, g)
 * aux2: label info (negative = TRUE jump to ht, positive = FALSE jump to no)
 */
void
emitCondJmp(char op, int aux2)
{
    if (aux2 < 0) {
        /* TRUE jump to ht{target} */
        int target = -aux2;
        switch (op) {
        case 'Q': emit("jp z,ht%d_%d", target, fnIndex); break;
        case 'n': emit("jp nz,ht%d_%d", target, fnIndex); break;
        case '<': emit("jp c,ht%d_%d", target, fnIndex); break;
        case '>': emit("jp z,$+5"); emit("jp nc,ht%d_%d", target, fnIndex); break;
        case 'L': emit("jp c,ht%d_%d", target, fnIndex); emit("jp z,ht%d_%d", target, fnIndex); break;
        case 'g': emit("jp nc,ht%d_%d", target, fnIndex); break;
        }
    } else {
        /* FALSE jump to no{aux2} */
        switch (op) {
        case 'Q': emit("jp nz,no%d_%d", aux2, fnIndex); break;
        case 'n': emit("jp z,no%d_%d", aux2, fnIndex); break;
        case '<': emit("jp nc,no%d_%d", aux2, fnIndex); break;
        case '>': emit("jp c,no%d_%d", aux2, fnIndex); emit("jp z,no%d_%d", aux2, fnIndex); break;
        case 'L': emit("jp z,$+5"); emit("jp nc,no%d_%d", aux2, fnIndex); break;
        case 'g': emit("jp c,no%d_%d", aux2, fnIndex); break;
        }
    }
}

/*
 * Emit comparison operators: <, >, Q(==), n(!=), L(<=), g(>=)
 */
void
emitCompare(struct expr *e)
{
    comment("%c%c d=%d %s%s [", e->op, e->type, e->demand,
        regnames[e->dest] ? regnames[e->dest] : "-", e->cond ? " C" : "");
    indent += 2;
    if (e->special == SP_CMPHL) {
        /* byte cmp with (hl): left is complex (addr in hl), right is simple */
        struct expr *simple = e->right;
        struct expr *complex = e->left;
        comment("CMPHL");
        emitExpr(complex->left);  /* address to HL */
        emit("ld a,(hl)");        /* load left operand */
        if (simple->op == '#') {
            emit("cp %d", simple->v.c & 0xff);
        } else if (simple->op == 'R') {
            emit("cp %s", regnames[simple->aux]);
        } else if (simple->op == 'V') {
            char *rn = (simple->aux == R_IX) ? "ix" : "iy";
            emit("cp (%s%o)", rn, simple->offset);
        } else if (simple->op == 'M' && simple->left->op == '$') {
            emit("ld hl,%s", simple->left->sym);
            emit("cp (hl)");
        } else if (simple->op == 'M' && simple->left->op == 'V') {
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
        emit("bit 7,(hl)");
        if (!e->cond) {
            int lbl = labelCnt++;
            emit("ld a,0");
            emit("jp nz,sg%d_%d", lbl, fnIndex);
            emit("inc a");
            emit("sg%d_%d:", lbl, fnIndex);
        }
    } else if (e->special == SP_SIGNREG) {
        /* sign test regvar BC: bit 7,b */
        comment("SIGNREG bc");
        emit("bit 7,b");
        if (!e->cond) {
            int lbl = labelCnt++;
            emit("ld a,0");
            emit("jp nz,sg%d_%d", lbl, fnIndex);
            emit("inc a");
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
            /* HL==0 means equal (true for Q, false for n) */
            int lbl = labelCnt++;
            emit("ld a,h");
            emit("or l");
            if (e->op == 'Q') {
                /* == : result is 1 if HL==0, else 0 */
                emit("ld a,1");
                emit("jr z,eq%d_%d", lbl, fnIndex);
                emit("xor a");
            } else {
                /* != : result is 0 if HL==0, else 1 */
                emit("ld a,0");
                emit("jr z,eq%d_%d", lbl, fnIndex);
                emit("inc a");
            }
            emit("eq%d_%d:", lbl, fnIndex);
        }
        /* In conditional context, IF handler will test HL */
    } else {
        /* General comparison - use operand type, not result type */
        unsigned char ctype = e->left->type;
        unsigned char leftDeeper = treeDepth(e->left) >= treeDepth(e->right);
        if (ISLONG(ctype)) {
            /* Long comparison: emit both, call _lcmp */
            if (leftDeeper) {
                emitExpr(e->left);   /* dest=R_DE → _lL */
                emitExpr(e->right);  /* dest=R_HL → _lR */
            } else {
                emitExpr(e->right);  /* dest=R_HL → _lR */
                emitExpr(e->left);   /* dest=R_DE → _lL */
            }
            emit("call _lcmp");
        } else if (leftDeeper) {
            emitExpr(e->left);
            if (ISBYTE(ctype)) {
                if (e->left->size == 2)
                    emit("ld a,l");
            } else if (ISWORD(ctype)) {
                emit("ex de,hl");
            }
            if (ISBYTE(ctype) && e->right->op == '#') {
                /* Byte compare with constant: use cp immediate */
                emit("cp %d", e->right->v.c & 0xff);
            } else {
                emitExpr(e->right);
            }
            if (ISBYTE(ctype)) {
                if (e->right->op != '#')
                    emit("cp l");
            } else if (ISWORD(ctype)) {
                emit("ex de,hl");
                emit("or a");
                emit("sbc hl,de");
            }
        } else {
            emitExpr(e->right);
            emit("ex de,hl");
            emitExpr(e->left);
            if (ISBYTE(ctype)) {
                if (e->left->size == 2)
                    emit("ld a,l");
                emit("cp e");
            } else if (ISWORD(ctype)) {
                emit("or a");
                emit("sbc hl,de");
            }
        }
    }
cmpresult:
    if (!e->cond && e->special != SP_SIGN && e->special != SP_SIGNREG) {
        switch (e->op) {
        case 'Q':  /* == */
            emit("ld a,0");
            emit("jr nz,$+3");
            emit("inc a");
            break;
        case 'n':  /* != */
            emit("ld a,0");
            emit("jr z,$+3");
            emit("inc a");
            break;
        case '<':  /* < */
            emit("ld a,0");
            emit("jr nc,$+3");
            emit("inc a");
            break;
        case '>':  /* > */
            emit("jr z,$+5");
            emit("ld a,0");
            emit("jr c,$+3");
            emit("inc a");
            emit("jr $+3");
            emit("ld a,0");
            break;
        case 'L':  /* <= */
            emit("ld a,1");
            emit("jr z,$+4");
            emit("jr c,$+3");
            emit("dec a");
            break;
        case 'g':  /* >= */
            emit("ld a,0");
            emit("jr c,$+3");
            emit("inc a");
            break;
        default:
            emit("XXXXXXXXX cmpres");
        }
    }
    indent -= 2;
    comment("]");
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
