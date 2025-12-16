/*
 * emitops.c - Compound assignment code emission
 */
#include <stdio.h>
#include "cc2.h"

/*
 * Emit compound assignment: +=, -=, |=, &=, ^=, %=
 */
void
emitCmpArith(struct expr *e)
{
    comment("%c%c d=%d %s%s [", e->op, e->type, e->demand,
        regnames[e->dest] ? regnames[e->dest] : "-", e->unused ? " U" : "");
    indent += 2;
    if (e->left->op == 'R' && e->size == 1) {
        char *rname = (e->left->aux == R_B) ? "b" : "c";
        comment("R%c %s %s", e->left->type, e->left->sym ? e->left->sym : "?", rname);
        emitExpr(e->right);
        if (e->right->op == '#') {
            unsigned char val = e->right->v.c;
            emit("ld a,%s", rname);
            switch (e->op) {
            case 'P': emit("add a,%d", val); break;
            case 'o': emit("sub %d", val); break;
            case '1': emit("or %d", val); break;
            case 'a': emit("and %d", val); break;
            case 'X': emit("xor %d", val); break;
            }
        } else {
            emit("ld e,a");
            emit("ld a,%s", rname);
            switch (e->op) {
            case 'P': emit("add a,e"); break;
            case 'o': emit("sub e"); break;
            case '1': emit("or e"); break;
            case 'a': emit("and e"); break;
            case 'X': emit("xor e"); break;
            }
        }
        emit("ld %s,a", rname);
    } else if (e->left->op == 'R' && e->size == 2) {
        unsigned char r = e->left->aux;
        comment("R%c %s %s", e->left->type, e->left->sym ? e->left->sym : "?", regnames[r]);
        emitExpr(e->right);
        emit("ex de,hl");
        if (r == R_BC) {
            emit("ld h,b");
            emit("ld l,c");
        } else if (r == R_IX) {
            emit("push ix");
            emit("pop hl");
        }
        switch (e->op) {
        case 'P': emit("add hl,de"); break;
        case 'o': emit("or a"); emit("sbc hl,de"); break;
        case '1': emit("ld a,l"); emit("or e"); emit("ld l,a");
                  emit("ld a,h"); emit("or d"); emit("ld h,a"); break;
        case 'a': emit("ld a,l"); emit("and e"); emit("ld l,a");
                  emit("ld a,h"); emit("and d"); emit("ld h,a"); break;
        case 'X': emit("ld a,l"); emit("xor e"); emit("ld l,a");
                  emit("ld a,h"); emit("xor d"); emit("ld h,a"); break;
        }
        if (r == R_BC) {
            emit("ld b,h");
            emit("ld c,l");
        } else if (r == R_IX) {
            emit("push hl");
            emit("pop ix");
        }
    } else if (e->left->op == 'V' && e->size == 1) {
        char ofs = e->left->offset;
        char *rn = (e->left->aux == R_IX) ? "ix" : "iy";
        comment("V%c %s%+d", e->left->type, rn, ofs);
        emit("ld a,(%s%o)", rn, ofs);
        if (e->right->op == '#') {
            unsigned char val = e->right->v.c;
            switch (e->op) {
            case 'P': emit("add a,%d", val); break;
            case 'o': emit("sub %d", val); break;
            case '1': emit("or %d", val); break;
            case 'a': emit("and %d", val); break;
            case 'X': emit("xor %d", val); break;
            }
        } else {
            emitExpr(e->right);
            emit("ld a,(%s%o)", rn, ofs);
            switch (e->op) {
            case 'P': emit("add a,e"); break;
            case 'o': emit("sub e"); break;
            case '1': emit("or e"); break;
            case 'a': emit("and e"); break;
            case 'X': emit("xor e"); break;
            }
        }
        emit("ld (%s%o),a", rn, ofs);
    } else if (e->size == 1 && e->right->op == '#') {
        unsigned char val = e->right->v.c;
        emitExpr(e->left);
        emit("ld a,(hl)");
        switch (e->op) {
        case 'P': emit("add a,%d", val); break;
        case 'o': emit("sub %d", val); break;
        case '1': emit("or %d", val); break;
        case 'a': emit("and %d", val); break;
        case 'X': emit("xor %d", val); break;
        }
        emit("ld (hl),a");
    } else if (e->size == 1) {
        /* byte compound assign to computed address with non-const operand */
        emitExpr(e->right);  /* operand to A (via L if word) */
        if (e->right->size == 2)
            emit("ld a,l");
        emit("ld e,a");      /* save operand in E */
        emitExpr(e->left);   /* address to HL */
        emit("ld a,(hl)");   /* current value to A */
        switch (e->op) {
        case 'P': emit("add a,e"); break;
        case 'o': emit("sub e"); break;
        case '1': emit("or e"); break;
        case 'a': emit("and e"); break;
        case 'X': emit("xor e"); break;
        }
        emit("ld (hl),a");
    } else if (e->left->op == 'V' && e->size == 2) {
        char ofs = e->left->offset;
        char *rn = (e->left->aux == R_IX) ? "ix" : "iy";
        comment("V%c %s%+d", e->left->type, rn, ofs);
        emitExpr(e->right);
        emit("ex de,hl");
        emit("ld l,(%s%o)", rn, ofs);
        emit("ld h,(%s%o)", rn, ofs + 1);
        switch (e->op) {
        case 'P': emit("add hl,de"); break;
        case 'o': emit("or a"); emit("sbc hl,de"); break;
        case '1': emit("ld a,l"); emit("or e"); emit("ld l,a");
                  emit("ld a,h"); emit("or d"); emit("ld h,a"); break;
        case 'a': emit("ld a,l"); emit("and e"); emit("ld l,a");
                  emit("ld a,h"); emit("and d"); emit("ld h,a"); break;
        case 'X': emit("ld a,l"); emit("xor e"); emit("ld l,a");
                  emit("ld a,h"); emit("xor d"); emit("ld h,a"); break;
        }
        emit("ld (%s%o),l", rn, ofs);
        emit("ld (%s%o),h", rn, ofs + 1);
    } else if (e->left->op == 'V' && e->size == 4) {
        /* long compound arith on V node - use store helper */
        char ofs = e->left->offset;
        char *rn = (e->left->aux == R_IX) ? "ix" : "iy";
        char *sth = (e->left->aux == R_IX) ? "sLxindex" : "sLyindex";
        comment("V%c %s%+d", e->left->type, rn, ofs);
        emitExpr(e->right);
        /* right operand in HLHL', load left into DEDE' */
        emit("ld e,(%s%o)", rn, ofs);
        emit("ld d,(%s%o)", rn, ofs + 1);
        emit("exx");
        emit("ld e,(%s%o)", rn, ofs + 2);
        emit("ld d,(%s%o)", rn, ofs + 3);
        emit("exx");
        switch (e->op) {
        case 'P': emit("call __ladd"); break;
        case 'o': emit("call __lsub"); break;
        case '1': emit("call __lor"); break;
        case 'a': emit("call __land"); break;
        case 'X': emit("call __lxor"); break;
        }
        /* store via helper */
        emit("ld a,%d", ofs);
        emit("call %s", sth);
    } else if (e->size == 2 && e->right->op == '#') {
        unsigned val = e->right->v.s & 0xffff;
        emitExpr(e->left);
        emit("ld e,(hl)");
        emit("inc hl");
        emit("ld d,(hl)");
        emit("dec hl");
        emit("ex de,hl");
        switch (e->op) {
        case 'P': emit("ld bc,%d", val); emit("add hl,bc"); break;
        case 'o': emit("ld bc,%d", val); emit("or a"); emit("sbc hl,bc"); break;
        case '1': emit("ld a,l"); emit("or %d", val & 0xff); emit("ld l,a");
                  emit("ld a,h"); emit("or %d", (val >> 8) & 0xff); emit("ld h,a"); break;
        case 'a': emit("ld a,l"); emit("and %d", val & 0xff); emit("ld l,a");
                  emit("ld a,h"); emit("and %d", (val >> 8) & 0xff); emit("ld h,a"); break;
        case 'X': emit("ld a,l"); emit("xor %d", val & 0xff); emit("ld l,a");
                  emit("ld a,h"); emit("xor %d", (val >> 8) & 0xff); emit("ld h,a"); break;
        }
        emit("ex de,hl");
        emit("ld (hl),e");
        emit("inc hl");
        emit("ld (hl),d");
    } else if (e->size == 2) {
        emitExpr(e->right);
        emit("push hl");
        emitExpr(e->left);
        emit("ld e,(hl)");
        emit("inc hl");
        emit("ld d,(hl)");
        emit("dec hl");
        emit("push hl");
        emit("ex de,hl");
        emit("pop de");
        emit("pop bc");
        switch (e->op) {
        case 'P': emit("add hl,bc"); break;
        case 'o': emit("or a"); emit("sbc hl,bc"); break;
        case '1': emit("ld a,l"); emit("or c"); emit("ld l,a");
                  emit("ld a,h"); emit("or b"); emit("ld h,a"); break;
        case 'a': emit("ld a,l"); emit("and c"); emit("ld l,a");
                  emit("ld a,h"); emit("and b"); emit("ld h,a"); break;
        case 'X': emit("ld a,l"); emit("xor c"); emit("ld l,a");
                  emit("ld a,h"); emit("xor b"); emit("ld h,a"); break;
        }
        emit("ex de,hl");
        emit("ld (hl),e");
        emit("inc hl");
        emit("ld (hl),d");
    } else {
        emitExpr(e->left);
        emitExpr(e->right);
        emit("XXXXXXXXX %c", e->op);
    }
    indent -= 2;
    comment("]");
}

/*
 * Emit compound shift: <<=, >>=
 */
void
emitCmpShift(struct expr *e)
{
    comment("%c%c d=%d %s%s [", e->op, e->type, e->demand,
        regnames[e->dest] ? regnames[e->dest] : "-", e->unused ? " U" : "");
    indent += 2;
    if (e->left->op == 'R' && e->size == 2 && e->right->op == '#') {
        /* word regvar <<= or >>= constant */
        unsigned char cnt = e->right->v.c & 0xf;
        comment("R%c %s bc", e->left->type, e->left->sym ? e->left->sym : "?");
        comment("#%c %d d=%d -", e->right->type, cnt, e->right->demand);
        emit("ld h,b");
        emit("ld l,c");
        if (e->op == '0') {
            /* left shift: add hl,hl */
            while (cnt--)
                emit("add hl,hl");
        } else {
            /* right shift: srl h; rr l */
            while (cnt--) {
                emit("srl h");
                emit("rr l");
            }
        }
        emit("ld b,h");
        emit("ld c,l");
    } else if (e->left->op == 'V' && e->size == 2 && e->right->op == '#') {
        /* word local <<= or >>= constant */
        char ofs = e->left->offset;
        char *rn = (e->left->aux == R_IX) ? "ix" : "iy";
        unsigned char cnt = e->right->v.c & 0xf;
        comment("V%c %s%+d", e->left->type, rn, ofs);
        emit("ld l,(%s%o)", rn, ofs);
        emit("ld h,(%s%o)", rn, ofs + 1);
        if (e->op == '0') {
            while (cnt--)
                emit("add hl,hl");
        } else {
            while (cnt--) {
                emit("srl h");
                emit("rr l");
            }
        }
        emit("ld (%s%o),l", rn, ofs);
        emit("ld (%s%o),h", rn, ofs + 1);
    } else if (e->left->op == 'V' && e->size == 4 && e->right->op == '#') {
        char ofs = e->left->offset;
        char *ldh = (e->left->aux == R_IX) ? "lLxindex" : "lLyindex";
        char *sth = (e->left->aux == R_IX) ? "sLxindex" : "sLyindex";
        unsigned char cnt = e->right->v.c & 0x1f;
        comment("Vl %s%+d", (e->left->aux == R_IX) ? "ix" : "iy", ofs);
        /* load via helper */
        emit("ld a,%d", ofs);
        emit("call %s", ldh);
        emit("ld a,%d", cnt);
        if (e->op == '0')
            emit("call __llshl");
        else
            emit("call __lashr");
        /* store via helper */
        emit("ld a,%d", ofs);
        emit("call %s", sth);
    } else {
        emitExpr(e->left);
        emitExpr(e->right);
        emit("XXXXXXXXX %c", e->op);
    }
    indent -= 2;
    comment("]");
}

/*
 * Emit compound multiply/divide: *=, /=
 */
void
emitCmpMulDiv(struct expr *e)
{
    comment("%c%c d=%d %s%s [", e->op, e->type, e->demand,
        regnames[e->dest] ? regnames[e->dest] : "-", e->unused ? " U" : "");
    indent += 2;
    if (e->left->op == 'R' && e->left->size == 1) {
        char *rname = (e->left->aux == R_B) ? "b" : "c";
        comment("R%c %s %s", e->left->type, e->left->sym ? e->left->sym : "?", rname);
        emitExpr(e->right);
        emit("ld e,a");
        emit("ld a,%s", rname);
        if (e->op == 'T')
            emit("call __imulb");
        else
            emit("call __idivb");
        emit("ld %s,l", rname);
    } else if (e->left->op == 'R' && e->left->size == 2) {
        unsigned char r = e->left->aux;
        comment("R%c %s %s", e->left->type, e->left->sym ? e->left->sym : "?", regnames[r]);
        emitExpr(e->right);
        emit("ex de,hl");
        if (r == R_BC) {
            emit("ld h,b");
            emit("ld l,c");
        } else if (r == R_IX) {
            emit("push ix");
            emit("pop hl");
        }
        if (e->op == 'T')
            emit("call __imul");
        else
            emit("call __idiv");
        if (r == R_BC) {
            emit("ld b,h");
            emit("ld c,l");
        } else if (r == R_IX) {
            emit("push hl");
            emit("pop ix");
        }
    } else if (e->left->op == 'V' && e->size == 1) {
        char ofs = e->left->offset;
        char *rn = (e->left->aux == R_IX) ? "ix" : "iy";
        comment("V%c %s%+d", e->left->type, rn, ofs);
        emitExpr(e->right);
        emit("ld e,a");
        emit("ld a,(%s%o)", rn, ofs);
        if (e->op == 'T')
            emit("call __imulb");
        else
            emit("call __idivb");
        emit("ld a,l");
        emit("ld (%s%o),a", rn, ofs);
    } else if (e->left->op == 'V' && e->size == 2) {
        char ofs = e->left->offset;
        char *rn = (e->left->aux == R_IX) ? "ix" : "iy";
        comment("V%c %s%+d", e->left->type, rn, ofs);
        emitExpr(e->right);
        emit("ex de,hl");
        emit("ld l,(%s%o)", rn, ofs);
        emit("ld h,(%s%o)", rn, ofs + 1);
        if (e->op == 'T')
            emit("call __imul");
        else
            emit("call __idiv");
        emit("ld (%s%o),l", rn, ofs);
        emit("ld (%s%o),h", rn, ofs + 1);
    } else {
        emitExpr(e->left);
        emitExpr(e->right);
        emit("XXXXXXXXX %c", e->op);
    }
    indent -= 2;
    comment("]");
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
