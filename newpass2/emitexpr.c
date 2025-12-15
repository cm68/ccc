/*
 * emitexpr.c - Expression code emission
 */
#include <stdio.h>
#include "cc2.h"

/* Forward declaration */
void emitExpr(struct expr *e);

/*
 * Emit comparison operators: <, >, Q(==), n(!=), L(<=), g(>=)
 */
static void
emitCompare(struct expr *e)
{
    comment("%c%c d=%d %s%s [", e->op, e->type, e->demand,
        regnames[e->dest] ? regnames[e->dest] : "-", e->cond ? " C" : "");
    indent += 2;
    if (e->special == SP_CMPHL) {
        /* byte cmp with (hl) */
        struct expr *simple = e->aux2 == 0 ? e->left : e->right;
        struct expr *complex = e->aux2 == 0 ? e->right : e->left;
        comment("CMPHL side=%d", e->aux2);
        emitExpr(complex->left);
        if (simple->op == '#') {
            emit("ld a,%d", simple->v.c & 0xff);
        } else if (simple->op == 'R') {
            emit("ld a,%s", regnames[simple->aux]);
        } else if (simple->op == 'M' && simple->left->op == '$') {
            emit("ld a,(%s)", simple->left->sym);
        } else if (simple->op == 'M' && simple->left->op == 'V') {
            char *rn = (simple->left->aux == R_IX) ? "ix" : "iy";
            emit("ld a,(%s%o)", rn, simple->left->offset);
        } else {
            emit("XXXXXXXXX simple");
        }
        emit("cp (hl)");
        if (!e->cond)
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
    } else {
        /* General comparison */
        unsigned char ctype = e->type;
        unsigned char leftDeeper = treeDepth(e->left) >= treeDepth(e->right);
        if (leftDeeper) {
            emitExpr(e->left);
            if (ISBYTE(ctype)) {
                if (e->left->size == 2)
                    emit("ld a,l");
            } else if (ISWORD(ctype)) {
                emit("ex de,hl");
            } else if (ISLONG(ctype)) {
                emit("ex de,hl");
                emit("exx");
                emit("ex de,hl");
                emit("exx");
            }
            emitExpr(e->right);
            if (ISBYTE(ctype)) {
                emit("cp l");
            } else if (ISWORD(ctype)) {
                emit("ex de,hl");
                emit("or a");
                emit("sbc hl,de");
            } else if (ISLONG(ctype)) {
                emit("ex de,hl");
                emit("exx");
                emit("ex de,hl");
                emit("exx");
                emit("or a");
                emit("sbc hl,de");
                emit("exx");
                emit("sbc hl,de");
                emit("exx");
            }
        } else {
            emitExpr(e->right);
            if (ISLONG(ctype)) {
                emit("ex de,hl");
                emit("exx");
                emit("ex de,hl");
                emit("exx");
            } else
                emit("ex de,hl");
            emitExpr(e->left);
            if (ISBYTE(ctype)) {
                if (e->left->size == 2)
                    emit("ld a,l");
                emit("cp e");
            } else if (ISWORD(ctype)) {
                emit("or a");
                emit("sbc hl,de");
            } else if (ISLONG(ctype)) {
                emit("or a");
                emit("sbc hl,de");
                emit("exx");
                emit("sbc hl,de");
                emit("exx");
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
 * Emit compound assignment: +=, -=, |=, &=, ^=, %=
 */
static void
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
        comment("V%c %s%o", e->left->type, rn, ofs);
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
        comment("V%c %s%o", e->left->type, rn, ofs);
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
    } else if (e->left->op == 'V' && e->size == 4 && e->op == 'P') {
        char ofs = e->left->offset;
        char *rn = (e->left->aux == R_IX) ? "ix" : "iy";
        comment("V%c %s%o", e->left->type, rn, ofs);
        emitExpr(e->right);
        emit("push hl");
        emit("exx");
        emit("push hl");
        emit("exx");
        emit("ld l,(%s%o)", rn, ofs);
        emit("ld h,(%s%o)", rn, ofs + 1);
        emit("exx");
        emit("ld l,(%s%o)", rn, ofs + 2);
        emit("ld h,(%s%o)", rn, ofs + 3);
        emit("exx");
        emit("pop bc");
        emit("add hl,bc");
        emit("ld (%s%o),l", rn, ofs);
        emit("ld (%s%o),h", rn, ofs + 1);
        emit("exx");
        emit("pop bc");
        emit("adc hl,bc");
        emit("ld (%s%o),l", rn, ofs + 2);
        emit("ld (%s%o),h", rn, ofs + 3);
        emit("exx");
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
static void
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
    } else if (e->left->op == 'V' && e->size == 4 && e->right->op == '#') {
        char ofs = e->left->offset;
        char *rn = (e->left->aux == R_IX) ? "ix" : "iy";
        unsigned char cnt = e->right->v.c & 0x1f;
        comment("V%c %s%o", e->left->type, rn, ofs);
        emit("ld l,(%s%o)", rn, ofs);
        emit("ld h,(%s%o)", rn, ofs + 1);
        emit("exx");
        emit("ld l,(%s%o)", rn, ofs + 2);
        emit("ld h,(%s%o)", rn, ofs + 3);
        emit("exx");
        emit("ld a,%d", cnt);
        if (e->op == '0')
            emit("call __llshl");
        else
            emit("call __lashr");
        emit("ld (%s%o),l", rn, ofs);
        emit("ld (%s%o),h", rn, ofs + 1);
        emit("exx");
        emit("ld (%s%o),l", rn, ofs + 2);
        emit("ld (%s%o),h", rn, ofs + 3);
        emit("exx");
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
static void
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
        comment("V%c %s%o", e->left->type, rn, ofs);
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
        comment("V%c %s%o", e->left->type, rn, ofs);
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
 * Emit pre-increment/decrement: (, {
 */
static void
emitPreIncDec(struct expr *e)
{
    if (e->special == SP_INCR || e->special == SP_DECR) {
        char *ins = (e->special == SP_INCR) ? "inc" : "dec";
        unsigned char i;
        for (i = 0; i < e->incr; i++)
            emit("\t%s %r", ins, regnames[e->dest], e->offset);
    } else if (e->special == SP_INCGLOB) {
        char *ins = e->aux2 ? "inc" : "dec";
        unsigned char i;
        emit("ld hl,(%s)", e->sym);
        for (i = 0; i < e->incr; i++)
            emit("%s hl", ins);
        emit("ld (%s),hl", e->sym);
    } else {
        comment("%c%c d=%d %s%s [", e->op, e->type, e->demand,
            regnames[e->dest] ? regnames[e->dest] : "-", e->unused ? " U" : "");
        indent += 2;
        emitExpr(e->left);
        comment("incr=%d", e->aux2);
        if (e->unused && e->size == 1 && e->aux2 <= 4) {
            char *ins = (e->op == '(') ? "inc" : "dec";
            unsigned char i;
            for (i = 0; i < e->aux2; i++)
                emit("%s (hl)", ins);
        } else if (e->left->op == 'V' && e->size == 2 && e->aux2 <= 4) {
            char off = e->left->offset;
            char *rn = (e->left->aux == R_IX) ? "ix" : "iy";
            char *ins = (e->op == '(') ? "inc" : "dec";
            unsigned char i;
            for (i = 0; i < e->aux2; i++)
                emit("%s hl", ins);
            emit("ld (%s%o),l", rn, off);
            emit("ld (%s%o),h", rn, off + 1);
            if (e->dest == R_TOS)
                emit("push hl");
        } else if (e->left->op == 'V' && e->size == 4 && e->aux2 == 1) {
            char off = e->left->offset;
            char *rn = (e->left->aux == R_IX) ? "ix" : "iy";
            if (e->op == '(') {
                emit("inc hl");
                emit("ld a,h");
                emit("or l");
                emit("jp nz,ni%d_%d", labelCnt, fnIndex);
                emit("exx");
                emit("inc hl");
                emit("exx");
                emit("ni%d_%d:", labelCnt++, fnIndex);
            }
            emit("ld (%s%o),l", rn, off);
            emit("ld (%s%o),h", rn, off + 1);
            emit("exx");
            emit("ld (%s%o),l", rn, off + 2);
            emit("ld (%s%o),h", rn, off + 3);
            emit("exx");
            if (e->dest == R_TOS) {
                emit("push hl");
                emit("exx");
                emit("push hl");
                emit("exx");
            }
        } else if (e->left->op == 'R' && e->left->aux == R_BC && e->size == 2) {
            /* pre-inc/dec BC register variable */
            comment("Rs %s bc", e->left->sym ? e->left->sym : "?");
            comment("incr=%d", e->aux2);
            if (e->aux2 <= 4) {
                char *ins = (e->op == '(') ? "inc" : "dec";
                unsigned char i;
                for (i = 0; i < e->aux2; i++)
                    emit("%s bc", ins);
            } else {
                emit("ld hl,%d", e->aux2);
                if (e->op == '(')
                    emit("add hl,bc");
                else {
                    emit("ex de,hl");
                    emit("ld h,b");
                    emit("ld l,c");
                    emit("or a");
                    emit("sbc hl,de");
                }
                emit("ld b,h");
                emit("ld c,l");
            }
            if (e->dest == R_HL) {
                emit("ld h,b");
                emit("ld l,c");
            } else if (e->dest == R_TOS) {
                emit("push bc");
            }
        } else if (e->size == 2 && e->aux2 <= 4) {
            char *ins = (e->op == '(') ? "inc" : "dec";
            unsigned char i;
            emit("ld e,(hl)");
            emit("inc hl");
            emit("ld d,(hl)");
            emit("dec hl");
            emit("push hl");
            emit("ex de,hl");
            for (i = 0; i < e->aux2; i++)
                emit("%s hl", ins);
            emit("pop de");
            emit("ex de,hl");
            emit("ld (hl),e");
            emit("inc hl");
            emit("ld (hl),d");
            emit("ex de,hl");
        } else {
            emit("XXXXXXXXX %c", e->op);
        }
        indent -= 2;
        comment("]");
    }
}

/*
 * Emit post-increment/decrement: ), }
 */
static void
emitPostInc(struct expr *e)
{
    comment("%c%c d=%d %s%s [", e->op, e->type, e->demand,
        regnames[e->dest] ? regnames[e->dest] : "-", e->unused ? " U" : "");
    indent += 2;
    if (e->unused && e->left->op == 'R' && e->aux2 <= 4) {
        char *ins = (e->op == ')') ? "inc" : "dec";
        unsigned char i, r = e->left->aux;
        comment("Rp %s incr=%d", regnames[r], e->aux2);
        for (i = 0; i < e->aux2; i++)
            emit("%s %s", ins, regnames[r]);
    } else if (!e->unused && e->left->op == 'R' && e->aux2 <= 4) {
        char *ins = (e->op == ')') ? "inc" : "dec";
        unsigned char i, r = e->left->aux;
        comment("Rp %s incr=%d", regnames[r], e->aux2);
        if (r == R_BC) {
            emit("ld h,b");
            emit("ld l,c");
        } else if (r == R_IX) {
            emit("push ix");
            emit("pop hl");
        }
        for (i = 0; i < e->aux2; i++)
            emit("%s %s", ins, regnames[r]);
    } else if (e->left->op == 'V' && e->size == 2 && e->aux2 <= 4) {
        char off = e->left->offset;
        char *rn = (e->left->aux == R_IX) ? "ix" : "iy";
        char *ins = (e->op == ')') ? "inc" : "dec";
        unsigned char i;
        emitExpr(e->left);
        comment("incr=%d", e->aux2);
        if (!e->unused)
            emit("push hl");
        for (i = 0; i < e->aux2; i++)
            emit("%s hl", ins);
        emit("ld (%s%o),l", rn, off);
        emit("ld (%s%o),h", rn, off + 1);
        if (!e->unused)
            emit("pop hl");
    } else if (e->left->op == 'V' && e->size == 1 && e->aux2 <= 4) {
        char off = e->left->offset;
        char *rn = (e->left->aux == R_IX) ? "ix" : "iy";
        char *ins = (e->op == ')') ? "inc" : "dec";
        unsigned char i;
        emit("ld a,(%s%o)", rn, off);
        comment("incr=%d", e->aux2);
        if (!e->unused)
            emit("ld e,a");
        for (i = 0; i < e->aux2; i++)
            emit("%s a", ins);
        emit("ld (%s%o),a", rn, off);
        if (!e->unused)
            emit("ld a,e");
    } else if (e->left->op == '$' && e->size == 2 && e->aux2 <= 4) {
        char *ins = (e->op == ')') ? "inc" : "dec";
        unsigned char i;
        emitExpr(e->left);
        comment("incr=%d", e->aux2);
        emit("ld e,(hl)");
        emit("inc hl");
        emit("ld d,(hl)");
        emit("dec hl");
        emit("ex de,hl");
        if (!e->unused)
            emit("push hl");
        for (i = 0; i < e->aux2; i++)
            emit("%s hl", ins);
        emit("ex de,hl");
        emit("ld (hl),e");
        emit("inc hl");
        emit("ld (hl),d");
        if (!e->unused)
            emit("pop hl");
        if (e->dest == R_TOS && !e->unused)
            emit("push hl");
    } else if (e->left->op == '$' && e->size == 1 && e->aux2 <= 4) {
        char *ins = (e->op == ')') ? "inc" : "dec";
        unsigned char i;
        emitExpr(e->left);
        comment("incr=%d", e->aux2);
        emit("ld a,(hl)");
        if (!e->unused)
            emit("ld e,a");
        for (i = 0; i < e->aux2; i++)
            emit("%s a", ins);
        emit("ld (hl),a");
        if (!e->unused)
            emit("ld a,e");
        if (e->dest == R_TOS && !e->unused)
            emit("push af");
    } else if (e->size == 2 && e->aux2 <= 4) {
        char *ins = (e->op == ')') ? "inc" : "dec";
        unsigned char i;
        emitExpr(e->left);
        comment("incr=%d", e->aux2);
        emit("ld e,(hl)");
        emit("inc hl");
        emit("ld d,(hl)");
        emit("dec hl");
        if (!e->unused)
            emit("push de");
        emit("ex de,hl");
        for (i = 0; i < e->aux2; i++)
            emit("%s hl", ins);
        emit("ex de,hl");
        emit("ld (hl),e");
        emit("inc hl");
        emit("ld (hl),d");
        if (!e->unused)
            emit("pop hl");
    } else if (e->size == 1 && e->aux2 <= 4) {
        char *ins = (e->op == ')') ? "inc" : "dec";
        unsigned char i;
        emitExpr(e->left);
        comment("incr=%d", e->aux2);
        emit("ld a,(hl)");
        if (!e->unused)
            emit("ld e,a");
        for (i = 0; i < e->aux2; i++)
            emit("%s a", ins);
        emit("ld (hl),a");
        if (!e->unused)
            emit("ld a,e");
    } else {
        emitExpr(e->left);
        comment("incr=%d", e->aux2);
        emit("XXXXXXXXX %c", e->op);
    }
    indent -= 2;
    comment("]");
}

/*
 * Emit memory dereference: M (load from address)
 */
static void
emitDeref(struct expr *e)
{
    comment("M%c d=%d %s [", e->type, e->demand, regnames[e->dest] ? regnames[e->dest] : "-");
    indent += 2;
    if (e->special == SP_MSYM) {
        /* M $sym -> ld reg,(sym) */
        comment("$%s", e->sym);
        if (e->dest == R_TOS) {
            emit("ld hl,(%s)", e->sym);
            emit("push hl");
        } else
            emit("ld %s,(%s)", regnames[e->dest] ? regnames[e->dest] : "hl", e->sym);
    } else if (e->special == SP_SYMOFD) {
        /* M[+p $sym #ofs] -> ld reg,(sym+ofs) */
        comment("+p $%s #%d", e->sym, e->offset);
        if (e->dest == R_TOS) {
            emit("ld hl,(%s+%d)", e->sym, e->offset);
            emit("push hl");
        } else
            emit("ld %s,(%s+%d)", regnames[e->dest] ? regnames[e->dest] : "hl", e->sym, e->offset);
    } else if (e->left->op == 'R' && e->left->aux == R_IX) {
        /* Mp [Rp IX] */
        comment("Rp ix");
        if (e->dest == R_TOS) {
            emit("push ix");
        } else {
            emit("push ix");
            emit("pop hl");
        }
    } else if (e->left->op == 'R' && e->left->aux == R_BC) {
        /* Mp [Rp BC] or Ms [Rs BC] */
        comment("R%c bc", e->left->type);
        if (e->dest == R_TOS) {
            emit("push bc");
        } else {
            emit("ld h,b");
            emit("ld l,c");
        }
    } else {
        /* indirect: emit address, then deref */
        emitExpr(e->left);
        if (e->size == 1) {
            emit("ld a,(hl)");
            if (e->dest == R_TOS)
                emit("push af");
        } else if (e->size == 2) {
            if (e->dest == R_DE) {
                emit("ld e,(hl)");
                emit("inc hl");
                emit("ld d,(hl)");
            } else {
                emit("ld a,(hl)");
                emit("inc hl");
                emit("ld h,(hl)");
                emit("ld l,a");
                if (e->dest == R_TOS)
                    emit("push hl");
            }
        } else if (e->size == 4) {
            emit("ld e,(hl)");
            emit("inc hl");
            emit("ld d,(hl)");
            emit("inc hl");
            emit("ld a,(hl)");
            emit("inc hl");
            emit("ld h,(hl)");
            emit("ld l,a");
            emit("ex de,hl");
            emit("exx");
            emit("ex de,hl");
            emit("exx");
        } else if (e->demand > 0) {
            emit("XXXXXXXXX M");
        }
    }
    indent -= 2;
    comment("]");
}

/*
 * Emit primary expressions: constants, symbols, regvars, locals
 */
static void
emitPrimary(struct expr *e)
{
    switch (e->op) {
    case '#':
        comment("#%c %ld d=%d %s", e->type, e->v.l, e->demand, regnames[e->dest] ? regnames[e->dest] : "-");
        if (e->demand == 0)
            break;
        if (e->dest == R_TOS) {
            if (e->size == 1) {
                emit("ld a,%d", e->v.c & 0xff);
                emit("push af");
            } else if (e->size == 2) {
                emit("ld hl,%d", e->v.s & 0xffff);
                emit("push hl");
            } else {
                emit("XXXXXXXXX #tos");
            }
        } else if (e->dest == R_DE) {
            if (e->size == 1)
                emit("ld e,%d", e->v.c & 0xff);
            else
                emit("ld de,%d", e->v.s & 0xffff);
        } else if (e->size == 1) {
            emit("ld %s,%d", regnames[e->dest] ? regnames[e->dest] : "a", e->v.c & 0xff);
        } else if (e->size == 2) {
            emit("ld %s,%d", regnames[e->dest] ? regnames[e->dest] : "hl", e->v.s & 0xffff);
        } else if (e->size == 4) {
            emit("ld hl,%d", e->v.s & 0xffff);
            emit("exx");
            emit("ld hl,%d", (e->v.l >> 16) & 0xffff);
            emit("exx");
        } else if (e->type == T_VOID) {
            /* void - no value */
        } else {
            emit("XXXXXXXXX #?");
        }
        break;
    case '$':
        comment("$%s d=%d %s", e->sym ? e->sym : "?", e->demand, regnames[e->dest] ? regnames[e->dest] : "-");
        if (e->dest == R_TOS) {
            emit("ld hl,%s", e->sym);
            emit("push hl");
        } else if (e->demand > 0)
            emit("ld %s,%s", regnames[e->dest] ? regnames[e->dest] : "hl", e->sym);
        break;
    case 'R':  /* register var */
        {
            static char *rn[] = { "?", "b", "c", "bc", "ix" };
            comment("R%c %s %s d=%d %s", e->type, e->sym ? e->sym : "?",
                rn[e->aux < 5 ? e->aux : 0], e->demand, regnames[e->dest] ? regnames[e->dest] : "-");
            if (e->demand == 0)
                break;
            if (e->size == 1) {
                if (e->aux == R_B)
                    emit("ld a,b");
                else if (e->aux == R_C)
                    emit("ld a,c");
                if (e->dest == R_TOS)
                    emit("push af");
            } else if (e->size == 2) {
                if (e->aux == R_BC) {
                    emit("ld h,b");
                    emit("ld l,c");
                } else if (e->aux == R_IX) {
                    emit("push ix");
                    emit("pop hl");
                }
                if (e->dest == R_TOS)
                    emit("push hl");
            }
        }
        break;
    case 'V':  /* local/struct var via IY or IX */
        {
            char off = e->offset;
            char *rn = (e->aux == R_IX) ? "ix" : "iy";
            comment("V%c %s %s%o d=%d %s", e->type, e->sym ? e->sym : "?", rn, off, e->demand, regnames[e->dest] ? regnames[e->dest] : "-");
            if (e->demand == 0)
                break;
            if (e->size == 1) {
                emit("ld a,(%s%o)", rn, off);
                if (e->dest == R_TOS)
                    emit("push af");
            } else if (e->size == 2) {
                emit("ld l,(%s%o)", rn, off);
                emit("ld h,(%s%o)", rn, off + 1);
                if (e->dest == R_TOS)
                    emit("push hl");
            } else if (e->size == 4) {
                emit("ld l,(%s%o)", rn, off);
                emit("ld h,(%s%o)", rn, off + 1);
                emit("exx");
                emit("ld l,(%s%o)", rn, off + 2);
                emit("ld h,(%s%o)", rn, off + 3);
                emit("exx");
                if (e->dest == R_TOS) {
                    emit("exx");
                    emit("push hl");
                    emit("exx");
                    emit("push hl");
                }
            } else {
                emit("XXXXXXXXX V");
            }
        }
        break;
    }
}

/*
 * Emit expression tree
 */
void
emitExpr(struct expr *e)
{
    if (!e || !e->op) return;

    if (e->special)
        comment("SPECIAL");

    switch (e->op) {
    case '#':
    case '$':
    case 'R':
    case 'V':
        emitPrimary(e);
        break;
    case 'M':
        emitDeref(e);
        break;
    case '=':
        comment("=%c d=%d %s%s [", e->type, e->demand, regnames[e->dest] ? regnames[e->dest] : "-", e->unused ? " U" : "");
        indent += 2;
        /* assign to IX or BC register var */
        if (e->left->op == 'R' && e->left->aux == R_IX) {
            comment("Rp %s ix d=0 -", e->left->sym ? e->left->sym : "?");
            emitExpr(e->right);
            emit("push hl");
            emit("pop ix");
        } else if (e->left->op == 'R' && e->left->aux == R_BC) {
            comment("R%c %s bc d=0 -", e->left->type, e->left->sym ? e->left->sym : "?");
            if (e->right->op == '#') {
                emit("ld bc,%d", e->right->v.s & 0xffff);
            } else if (e->right->op == '$') {
                emit("ld bc,%s", e->right->sym);
            } else {
                emitExpr(e->right);
                emit("ld b,h");
                emit("ld c,l");
            }
        } else if (e->left->op == 'R' && e->left->aux == R_B) {
            comment("Rb %s b", e->left->sym ? e->left->sym : "?");
            if (e->special == SP_STCONST && e->right->op == '#') {
                emit("ld b,%d", e->right->v.c & 0xff);
            } else {
                emitExpr(e->right);
                emit("ld b,e");
            }
        } else if (e->left->op == 'R' && e->left->aux == R_C) {
            comment("Rb %s c", e->left->sym ? e->left->sym : "?");
            if (e->special == SP_STCONST && e->right->op == '#') {
                emit("ld c,%d", e->right->v.c & 0xff);
            } else {
                emitExpr(e->right);
                emit("ld c,e");
            }
        } else if (e->left->op == '$' && e->left->sym) {
            /* assign to global symbol: emit value, then ld (sym),hl or ld (sym),a */
            comment("$%s", e->left->sym);
            emitExpr(e->right);
            if (e->size == 1) {
                emit("ld (%s),a", e->left->sym);
            } else if (e->size == 2) {
                emit("ld (%s),hl", e->left->sym);
            } else if (e->size == 4) {
                /* long: HLHL' - store 4 bytes to symbol */
                emit("ld (%s),hl", e->left->sym);
                emit("exx");
                emit("ld (%s+2),hl", e->left->sym);
                emit("exx");
            }
        } else if (e->left->op == 'V') {
            /* assign to local/struct var via IY or IX */
            char ofs = e->left->offset;
            char *rn = (e->left->aux == R_IX) ? "ix" : "iy";
            comment("V%c %s%o", e->left->type, rn, ofs);
            if (e->special == SP_STCONST && e->right->op == '#') {
                /* store constant directly */
                long val = e->right->v.l;
                if (e->size == 1) {
                    emit("ld (%s%o),%d", rn, ofs, (int)(val & 0xff));
                } else if (e->size == 2) {
                    if (val == 0) {
                        emit("xor a");
                        emit("ld (%s%o),a", rn, ofs);
                        emit("ld (%s%o),a", rn, ofs + 1);
                    } else {
                        emit("ld (%s%o),%d", rn, ofs, (int)(val & 0xff));
                        emit("ld (%s%o),%d", rn, ofs + 1, (int)((val >> 8) & 0xff));
                    }
                } else if (e->size == 4) {
                    emit("ld (%s%o),%d", rn, ofs, (int)(val & 0xff));
                    emit("ld (%s%o),%d", rn, ofs + 1, (int)((val >> 8) & 0xff));
                    emit("ld (%s%o),%d", rn, ofs + 2, (int)((val >> 16) & 0xff));
                    emit("ld (%s%o),%d", rn, ofs + 3, (int)((val >> 24) & 0xff));
                }
            } else {
                emitExpr(e->right);
                if (e->size == 1) {
                    emit("ld (%s%o),a", rn, ofs);
                } else if (e->size == 2) {
                    emit("ld (%s%o),l", rn, ofs);
                    emit("ld (%s%o),h", rn, ofs + 1);
                } else if (e->size == 4) {
                    /* long: HLHL' - store 4 bytes */
                    emit("ld (%s%o),l", rn, ofs);
                    emit("ld (%s%o),h", rn, ofs + 1);
                    emit("exx");
                    emit("ld (%s%o),l", rn, ofs + 2);
                    emit("ld (%s%o),h", rn, ofs + 3);
                    emit("exx");
                }
            }
        } else if (e->special == SP_STCONST) {
            /* store constant through HL: ld (hl),n; inc hl; ld (hl),n */
            long val = e->right->v.l;
            comment("STCONST %ld", val);
            /* e->left is M[addr], $global, or +s - get storage address in HL */
            if (e->left->op == 'M') {
                /* Need to get storage address: emit child of M, then deref if global ptr */
                emitExpr(e->left->left);
                /* For global ptr ($), emitExpr gives address of ptr, need to load ptr value.
                 * For local var (V), emitExpr already loads the value (the ptr itself). */
                if (e->left->size == 2 && e->left->left->op == '$') {
                    /* M[$global] pointer: child gave address of ptr, need ptr value */
                    emit("ld a,(hl)");
                    emit("inc hl");
                    emit("ld h,(hl)");
                    emit("ld l,a");
                }
            } else if (e->left->op == '$') {
                /* global symbol: load address directly */
                emit("ld hl,%s", e->left->sym);
            } else {
                /* left is address expression like +p, emit directly */
                emitExpr(e->left);
            }
            if (e->size == 1) {
                emit("ld (hl),%d", (int)(val & 0xff));
            } else if (e->size == 2) {
                emit("ld (hl),%d", (int)(val & 0xff));
                emit("inc hl");
                emit("ld (hl),%d", (int)((val >> 8) & 0xff));
            } else if (e->size == 4) {
                emit("ld (hl),%d", (int)(val & 0xff));
                emit("inc hl");
                emit("ld (hl),%d", (int)((val >> 8) & 0xff));
                emit("inc hl");
                emit("ld (hl),%d", (int)((val >> 16) & 0xff));
                emit("inc hl");
                emit("ld (hl),%d", (int)((val >> 24) & 0xff));
            }
        } else if (e->left->op == ')' || e->left->op == '}') {
            /* assign through post-inc/dec address: emit addr (old value), store, inc happens in emitExpr */
            emitExpr(e->left);  /* address to HL, regvar incremented */
            emit("ex de,hl");   /* save address to DE */
            emitExpr(e->right); /* value to HL (or A for byte) */
            if (e->size == 1) {
                emit("ld (de),a");
            } else if (e->size == 2) {
                emit("ex de,hl");
                emit("ld (hl),e");
                emit("inc hl");
                emit("ld (hl),d");
            } else if (e->size == 4) {
                emit("call __stl");  /* store long from HLHL' to (DE) */
            }
        } else if (e->left->op == '+' || e->left->op == 'M') {
            /* assign through computed address: emit addr, save, emit value, store */
            emitExpr(e->left);  /* address to HL */
            if (e->right->demand >= 2) {
                /* value needs DE - save address to stack */
                emit("push hl");
                emitExpr(e->right); /* value to HL (or A for byte) */
                emit("pop de");  /* restore address to DE */
            } else {
                emit("ex de,hl");   /* save address to DE */
                emitExpr(e->right); /* value to HL (or A for byte) */
            }
            if (e->size == 1) {
                /* byte value is already in A from emitExpr */
                emit("ld (de),a");
            } else if (e->size == 2) {
                emit("ex de,hl");  /* addr back to HL, value to DE */
                emit("ld (hl),e");
                emit("inc hl");
                emit("ld (hl),d");
            } else if (e->size == 4) {
                /* long: addr in DE, value in HLHL' */
                /* Save value, get addr to HL, store 4 bytes */
                emit("push hl");     /* save low word */
                emit("exx");
                emit("push hl");     /* save high word */
                emit("exx");
                emit("ex de,hl");    /* address to HL */
                emit("pop de");      /* high word to DE */
                emit("inc hl");
                emit("inc hl");      /* point to bytes 2-3 */
                emit("ld (hl),e");
                emit("inc hl");
                emit("ld (hl),d");
                emit("dec hl");
                emit("dec hl");
                emit("dec hl");      /* back to byte 0 */
                emit("pop de");      /* low word to DE */
                emit("ld (hl),e");
                emit("inc hl");
                emit("ld (hl),d");
            }
        } else {
            emitExpr(e->left);
            emitExpr(e->right);
            emit("XXXXXXXXX =");
        }
        indent -= 2;
        comment("]");
        break;
    case '@':
        comment("@%c nargs=%d d=%d %s%s [", e->type, e->aux, e->demand, regnames[e->dest] ? regnames[e->dest] : "-", e->unused ? " U" : "");
        indent += 2;
        /* Args already in reverse order from parser - just traverse and emit */
        {
            struct expr *a = e->right;
            while (a && a->op == 'A') {
                emitExpr(a->left);
                a = a->right;
            }
        }
        /* call */
        if (e->left->op == '$') {
            comment("$%s d=%d", e->left->sym, e->left->demand);
            emit("call _%s", e->left->sym);
        } else {
            emitExpr(e->left);
            emit("call _callhl");
        }
        indent -= 2;
        comment("]");
        break;
    case ':':  /* ternary else part - handled by '?' case, should not reach here */
        /* If we somehow get here, just emit both children */
        comment(":%c d=%d %s [", e->type, e->demand, regnames[e->dest] ? regnames[e->dest] : "-");
        indent += 2;
        if (e->left) emitExpr(e->left);
        if (e->right) emitExpr(e->right);
        indent -= 2;
        comment("]");
        break;
    case '?':  /* ternary: cond ? then : else */
        {
            int lbl = labelCnt++;
            comment("?%c d=%d %s [", e->type, e->demand, regnames[e->dest] ? regnames[e->dest] : "-");
            indent += 2;
            /* emit condition */
            emitExpr(e->left);
            /* test condition */
            if (e->left->size == 2) {
                emit("ld a,h");
                emit("or l");
            } else {
                emit("or a");
            }
            emit("jp z,te%d_%d", lbl, fnIndex);  /* if false, go to else */
            /* emit then branch */
            if (e->right) {
                emitExpr(e->right->left);
            }
            emit("jp tn%d_%d", lbl, fnIndex);    /* skip else */
            emit("te%d_%d:", lbl, fnIndex);      /* else label */
            /* emit else branch */
            if (e->right) {
                emitExpr(e->right->right);
            }
            emit("tn%d_%d:", lbl, fnIndex);      /* end label */
            indent -= 2;
            comment("]");
        }
        break;
    case '(':  /* pre-inc */
    case '{':  /* pre-dec */
        emitPreIncDec(e);
        break;
    case ')':  /* post-inc */
    case '}':  /* post-dec */
        emitPostInc(e);
        break;
    case 'F':  /* bitfield */
        comment("F%c off=%d wid=%d d=%d %s [", e->type, e->aux, e->aux2, e->demand, regnames[e->dest] ? regnames[e->dest] : "-");
        indent += 2;
        emitExpr(e->left);
        emit("XXXXXXXXX F");
        indent -= 2;
        comment("]");
        break;
    case 'Y':  /* memory copy: left=dest, right=src, aux=len */
        {
            unsigned char len = e->aux;
            comment("Y len=%d d=%d %s [", len, e->demand, regnames[e->dest] ? regnames[e->dest] : "-");
            indent += 2;
            emitExpr(e->right);  /* source address to HL */
            emit("push hl");
            emitExpr(e->left);   /* dest address to HL */
            emit("ex de,hl");    /* DE = dest */
            emit("pop hl");      /* HL = source */
            if (len <= 4) {
                /* inline small copy */
                unsigned char i;
                for (i = 0; i < len; i++) {
                    emit("ld a,(hl)");
                    emit("ld (de),a");
                    if (i < len - 1) {
                        emit("inc hl");
                        emit("inc de");
                    }
                }
            } else {
                /* use LDIR for larger copies */
                emit("ld bc,%d", len);
                emit("ldir");
            }
            indent -= 2;
            comment("]");
        }
        break;
    case '+':  /* add */
        comment("+%c d=%d %s [", e->type, e->demand, regnames[e->dest] ? regnames[e->dest] : "-");
        indent += 2;
        if (e->special == SP_SYMOFS) {
            /* +p $sym #const -> ld hl,sym+ofs */
            comment("$%s #%d", e->sym ? e->sym : "?", e->offset);
            if (e->offset > 0)
                emit("ld hl,%s+%d", e->sym, e->offset);
            else if (e->offset < 0)
                emit("ld hl,%s%d", e->sym, e->offset);
            else
                emit("ld hl,%s", e->sym);
            if (e->dest == R_TOS)
                emit("push hl");
        } else if (e->special == SP_ADDBC) {
            /* +p Mp[Rp bc] #const -> ld hl,const; add hl,bc */
            comment("bc+%d", e->offset);
            emit("ld hl,%d", e->offset);
            emit("add hl,bc");
            if (e->dest == R_TOS)
                emit("push hl");
        } else {
            /* Emit higher-demand child first to avoid spilling */
            unsigned char ld = e->left->demand, rd = e->right->demand;
            if (ld >= rd) {
                emitExpr(e->left);
                emitExpr(e->right);
            } else {
                emitExpr(e->right);
                emitExpr(e->left);
            }
            if (e->size == 2) {
                emit("add hl,de");
                if (e->dest == R_TOS)
                    emit("push hl");
            } else if (e->size == 1) {
                emit("add a,e");
                if (e->dest == R_TOS)
                    emit("push af");
            } else if (e->size == 4) {
                /* 32-bit add: HLHL' += DEDE' */
                emit("call __ladd");
            }
        }
        indent -= 2;
        comment("]");
        break;
    case '-':  /* subtract */
        comment("-%c d=%d %s [", e->type, e->demand, regnames[e->dest] ? regnames[e->dest] : "-");
        indent += 2;
        emitExpr(e->left);
        emitExpr(e->right);
        if (e->size == 2) {
            /* Need HL = left - right. Check where left ended up */
            if (e->left->dest == R_DE) {
                emit("ex de,hl");  /* swap: left to HL, right to DE */
            }
            emit("or a");
            emit("sbc hl,de");
        } else if (e->size == 1) {
            /* For byte: one is in A, other in E. Need A - E or E - A */
            if (e->left->dest == R_A) {
                /* A has left, E has right. A = A - E. Good. */
                emit("sub e");
            } else {
                /* A has right, E has left. Need left - right = E - A */
                /* ld b,a; ld a,e; sub b */
                emit("ld b,a");
                emit("ld a,e");
                emit("sub b");
            }
        } else if (e->size == 4) {
            /* 32-bit sub: HLHL' = DEDE' - HLHL' */
            emit("call __lsub");
        }
        indent -= 2;
        comment("]");
        break;
    case '&':  /* bitwise AND */
        comment("&%c d=%d %s [", e->type, e->demand, regnames[e->dest] ? regnames[e->dest] : "-");
        indent += 2;
        if (e->special == SP_BITTEST) {
            /* bit n,(ix+ofs) - no children emitted */
            emit("bit %d,(ix%o)", e->incr, e->offset);
        } else {
            emitExpr(e->left);
            emitExpr(e->right);
            if (e->size == 1) {
                emit("and e");  /* A = A & E */
            } else if (e->size == 2) {
                emit("ld a,l");
                emit("and e");
                emit("ld l,a");
                emit("ld a,h");
                emit("and d");
                emit("ld h,a");
            } else if (e->size == 4) {
                /* 32-bit AND: HLHL' &= DEDE' */
                emit("call __land");
            }
        }
        indent -= 2;
        comment("]");
        break;
    case '|':  /* bitwise OR */
        comment("|%c d=%d %s [", e->type, e->demand, regnames[e->dest] ? regnames[e->dest] : "-");
        indent += 2;
        emitExpr(e->left);
        emitExpr(e->right);
        if (e->size == 1) {
            emit("or e");  /* A = A | E */
        } else if (e->size == 2) {
            emit("ld a,l");
            emit("or e");
            emit("ld l,a");
            emit("ld a,h");
            emit("or d");
            emit("ld h,a");
        } else if (e->size == 4) {
            /* 32-bit OR: HLHL' |= DEDE' */
            emit("call __lor");
        }
        indent -= 2;
        comment("]");
        break;
    case '^':  /* bitwise XOR */
        comment("^%c d=%d %s [", e->type, e->demand, regnames[e->dest] ? regnames[e->dest] : "-");
        indent += 2;
        emitExpr(e->left);
        emitExpr(e->right);
        if (e->size == 1) {
            emit("xor e");  /* A = A ^ E */
        } else if (e->size == 2) {
            emit("ld a,l");
            emit("xor e");
            emit("ld l,a");
            emit("ld a,h");
            emit("xor d");
            emit("ld h,a");
        } else if (e->size == 4) {
            /* 32-bit XOR: HLHL' ^= DEDE' */
            emit("call __lxor");
        }
        indent -= 2;
        comment("]");
        break;
    case '*':  /* multiply */
        comment("*%c d=%d %s [", e->type, e->demand, regnames[e->dest] ? regnames[e->dest] : "-");
        indent += 2;
        if (e->special == SP_MUL2) {
            unsigned char i;
            emitExpr(e->left);
            comment("#%c %ld d=%d -", e->right->type, e->right->v.l, e->right->demand);
            for (i = 0; i < e->incr; i++)
                emit("add hl,hl");
        } else {
            /* non-power-of-2: call helper */
            /* Treat small constants (0-255) as bytes for efficiency */
            unsigned char lbyte = e->left->size == 1 ||
                        (e->left->op == '#' && (e->left->v.l & ~0xff) == 0);
            unsigned char rbyte = e->right->size == 1 ||
                        (e->right->op == '#' && (e->right->v.l & ~0xff) == 0);
            emitExpr(e->left);
            emitExpr(e->right);
            if (lbyte && rbyte) {
                /* byte × byte -> word: A has left, E has right */
                emit("call __imulb");
            } else if (lbyte) {
                /* byte × word: A has left (byte), HL has right (word) */
                emit("call __imula");
            } else if (rbyte) {
                /* word × byte: HL has left (word), A has right (byte) */
                emit("call __imula");
            } else {
                /* word × word: DE has left, HL has right */
                emit("call __imul");
            }
        }
        indent -= 2;
        comment("]");
        break;
    case '/':  /* divide */
    case '%':  /* modulo */
        comment("%c%c d=%d %s [", e->op, e->type, e->demand, regnames[e->dest] ? regnames[e->dest] : "-");
        indent += 2;
        {
            unsigned char lbyte = e->left->size == 1 ||
                        (e->left->op == '#' && (e->left->v.l & ~0xff) == 0);
            unsigned char rbyte = e->right->size == 1 ||
                        (e->right->op == '#' && (e->right->v.l & ~0xff) == 0);
            emitExpr(e->left);
            emitExpr(e->right);
            if (lbyte && rbyte) {
                /* byte op byte */
                if (e->op == '/')
                    emit("call __idivb");
                else
                    emit("call __imodb");
            } else {
                /* word op word (or mixed) */
                if (e->op == '/')
                    emit("call __idiv");
                else
                    emit("call __imod");
            }
        }
        indent -= 2;
        comment("]");
        break;
    case 'y':  /* left shift << */
        comment("y%c d=%d %s [", e->type, e->demand, regnames[e->dest] ? regnames[e->dest] : "-");
        indent += 2;
        emitExpr(e->left);
        if (e->right->op == '#') {
            /* constant shift count */
            unsigned char cnt = e->right->v.c & 0x1f;
            comment("#B %d", cnt);
            if (e->size == 1) {
                while (cnt--)
                    emit("add a,a");
            } else if (e->size == 2) {
                while (cnt--)
                    emit("add hl,hl");
            } else if (e->size == 4) {
                emit("ld a,%d", cnt);
                emit("call __llshl");
            }
        } else {
            /* variable shift count: call helper */
            emitExpr(e->right);
            emit("call __lshl");
        }
        indent -= 2;
        comment("]");
        break;
    case 'w':  /* right shift >> (signed) */
        comment("w%c d=%d %s [", e->type, e->demand, regnames[e->dest] ? regnames[e->dest] : "-");
        indent += 2;
        emitExpr(e->left);
        if (e->right->op == '#') {
            /* constant shift count */
            unsigned char cnt = e->right->v.c & 0x1f;
            comment("#B %d", cnt);
            if (e->size == 1) {
                while (cnt--)
                    emit("sra a");
            } else if (e->size == 2) {
                while (cnt--) {
                    emit("sra h");
                    emit("rr l");
                }
            } else if (e->size == 4) {
                emit("ld a,%d", cnt);
                emit("call __lashr");
            }
        } else {
            /* variable shift count: call helper */
            emitExpr(e->right);
            emit("call __ashr");
        }
        indent -= 2;
        comment("]");
        break;
    case 'z':  /* unsigned right shift >>> */
        comment("z%c d=%d %s [", e->type, e->demand, regnames[e->dest] ? regnames[e->dest] : "-");
        indent += 2;
        emitExpr(e->left);
        if (e->right->op == '#') {
            /* constant shift count */
            unsigned char cnt = e->right->v.c & 0x1f;
            comment("#B %d", cnt);
            if (e->size == 1) {
                while (cnt--)
                    emit("srl a");
            } else if (e->size == 2) {
                while (cnt--) {
                    emit("srl h");
                    emit("rr l");
                }
            } else if (e->size == 4) {
                emit("ld a,%d", cnt);
                emit("call __llshr");
            }
        } else {
            /* variable shift count: call helper */
            emitExpr(e->right);
            emit("call __lshr");
        }
        indent -= 2;
        comment("]");
        break;
    case '\\':  /* negation (unary minus) */
        comment("\\%c d=%d %s [", e->type, e->demand, regnames[e->dest] ? regnames[e->dest] : "-");
        indent += 2;
        emitExpr(e->left);
        if (e->size == 1) {
            emit("neg");
        } else if (e->size == 2) {
            /* HL = -HL = 0 - HL */
            emit("xor a");
            emit("sub l");
            emit("ld l,a");
            emit("sbc a,a");
            emit("sub h");
            emit("ld h,a");
        } else if (e->size == 4) {
            /* 32-bit negation: 0 - HLHL' */
            emit("call __lneg");
        }
        indent -= 2;
        comment("]");
        break;
    case '~':  /* bitwise complement */
        comment("~%c d=%d %s [", e->type, e->demand, regnames[e->dest] ? regnames[e->dest] : "-");
        indent += 2;
        emitExpr(e->left);
        if (e->size == 1) {
            emit("cpl");
        } else if (e->size == 2) {
            emit("ld a,l");
            emit("cpl");
            emit("ld l,a");
            emit("ld a,h");
            emit("cpl");
            emit("ld h,a");
        } else if (e->size == 4) {
            emit("call __lcom");
        }
        indent -= 2;
        comment("]");
        break;
    case '<': case '>': case 'Q': case 'n': case 'L': case 'g':
        emitCompare(e);
        break;
    case '!':  /* logical not */
        comment("!%c d=%d %s%s [", e->type, e->demand, regnames[e->dest] ? regnames[e->dest] : "-", e->cond ? " C" : "");
        indent += 2;
        if (e->cond) {
            /* used as condition: just emit child, IF handler flips sense */
            e->left->cond = 1;  /* propagate condition flag */
            emitExpr(e->left);
        } else {
            /* need actual 0/1 value: 1 if input is 0, else 0 */
            int lbl = labelCnt++;
            emitExpr(e->left);
            if (e->left->size == 2) {
                emit("ld a,h");
                emit("or l");
            } else {
                emit("or a");
            }
            emit("ld hl,0");
            emit("jp nz,ln%d_%d", lbl, fnIndex);
            emit("inc l");
            emit("ln%d_%d:", lbl, fnIndex);
        }
        indent -= 2;
        comment("]");
        break;
    case 'W':  /* widen: byte to word, zero extend */
        comment("W%c d=%d %s [", e->type, e->demand, regnames[e->dest] ? regnames[e->dest] : "-");
        indent += 2;
        if (e->left->op == '#') {
            /* constant: widen at compile time */
            unsigned char val = e->left->v.c;
            comment("#B %d d=%d", val, e->left->demand);
            emit("ld hl,%d", val);
            if (e->dest == R_TOS)
                emit("push hl");
        } else {
            emitExpr(e->left);  /* byte value in A */
            emit("ld l,a");
            emit("ld h,0");
            if (e->dest == R_TOS)
                emit("push hl");
        }
        indent -= 2;
        comment("]");
        break;
    case 'N':  /* narrow: word to byte */
        comment("N%c d=%d %s [", e->type, e->demand, regnames[e->dest] ? regnames[e->dest] : "-");
        indent += 2;
        if (e->left->op == '#') {
            /* constant was converted to byte in calcDemand */
            emitExpr(e->left);
        } else {
            emitExpr(e->left);  /* word value in HL */
            emit("ld a,l");
            if (e->dest == R_TOS)
                emit("push af");
        }
        indent -= 2;
        comment("]");
        break;
    case 'x':  /* sign extend: byte to word */
        comment("x%c d=%d %s [", e->type, e->demand, regnames[e->dest] ? regnames[e->dest] : "-");
        indent += 2;
        if (e->left->op == '#') {
            /* constant: sign extend at compile time */
            int val = (char)e->left->v.c;  /* sign extend to int */
            comment("#B %d d=%d", e->left->v.c & 0xff, e->left->demand);
            emit("ld hl,%d", val & 0xffff);
            if (e->dest == R_TOS)
                emit("push hl");
        } else {
            emitExpr(e->left);  /* byte value in A */
            emit("ld l,a");
            emit("rlca");       /* sign bit to carry */
            emit("sbc a,a");    /* 0 or 0xff based on carry */
            emit("ld h,a");
            if (e->dest == R_TOS)
                emit("push hl");
        }
        indent -= 2;
        comment("]");
        break;
    case 'j':  /* logical and (&&) with short-circuit */
        comment("j%c d=%d %s%s [", e->type, e->demand, regnames[e->dest] ? regnames[e->dest] : "-", e->cond ? " C" : "");
        indent += 2;
        if (e->cond) {
            /* conditional context: emit left, test, emit right */
            /* if left is false, skip right (HL will be 0) */
            /* Don't propagate cond to children - we need 0/1 values to test */
            int lbl = labelCnt++;
            emitExpr(e->left);
            /* test left and short-circuit if false */
            if (e->left->size == 2) {
                emit("ld a,h");
                emit("or l");
            } else {
                emit("or a");
            }
            emit("jp z,ja%d_%d", lbl, fnIndex);  /* left false -> skip right */
            emitExpr(e->right);
            emit("ja%d_%d:", lbl, fnIndex);
            /* if we jumped, HL=0 (left was 0); otherwise HL has right result */
        } else {
            /* need 0/1 value */
            int lbl = labelCnt++;
            emitExpr(e->left);
            /* test left */
            if (e->left->size == 2) {
                emit("ld a,h");
                emit("or l");
            } else {
                emit("or a");
            }
            emit("jp z,lj%d_%d", lbl, fnIndex);  /* left false -> result 0 */
            emitExpr(e->right);
            /* test right */
            if (e->right->size == 2) {
                emit("ld a,h");
                emit("or l");
            } else {
                emit("or a");
            }
            emit("lj%d_%d:", lbl, fnIndex);
            /* A is zero if we jumped or if right is 0 */
            emit("ld hl,0");
            emit("jp z,lk%d_%d", lbl, fnIndex);
            emit("inc l");
            emit("lk%d_%d:", lbl, fnIndex);
        }
        indent -= 2;
        comment("]");
        break;
    case 'h':  /* logical or (||) with short-circuit */
        comment("h%c d=%d %s%s [", e->type, e->demand, regnames[e->dest] ? regnames[e->dest] : "-", e->cond ? " C" : "");
        indent += 2;
        if (e->cond) {
            /* conditional context: emit left, if true skip right */
            /* Don't propagate cond to children - we need 0/1 values to test */
            int lbl = labelCnt++;
            emitExpr(e->left);
            /* test left and short-circuit if true */
            if (e->left->size == 2) {
                emit("ld a,h");
                emit("or l");
            } else {
                emit("or a");
            }
            emit("jp nz,hb%d_%d", lbl, fnIndex);  /* left true -> skip right */
            emitExpr(e->right);
            emit("hb%d_%d:", lbl, fnIndex);
            /* if we jumped, HL is nonzero; otherwise HL has right result */
        } else {
            /* need 0/1 value */
            int lbl = labelCnt++;
            emitExpr(e->left);
            /* test left */
            if (e->left->size == 2) {
                emit("ld a,h");
                emit("or l");
            } else {
                emit("or a");
            }
            emit("jp nz,lh%d_%d", lbl, fnIndex);  /* left true -> result 1 */
            emitExpr(e->right);
            /* test right */
            if (e->right->size == 2) {
                emit("ld a,h");
                emit("or l");
            } else {
                emit("or a");
            }
            emit("lh%d_%d:", lbl, fnIndex);
            /* A is nonzero if we jumped or if right is nonzero */
            emit("ld hl,1");
            emit("jp nz,li%d_%d", lbl, fnIndex);
            emit("dec l");
            emit("li%d_%d:", lbl, fnIndex);
        }
        indent -= 2;
        comment("]");
        break;
    case ',':  /* comma: left is value, right is side effect (AST format) */
        comment(",%c d=%d %s [", e->type, e->demand, regnames[e->dest] ? regnames[e->dest] : "-");
        indent += 2;
        emitExpr(e->right);  /* side effect first */
        emitExpr(e->left);   /* then value */
        indent -= 2;
        comment("]");
        break;
    case 'P': case 'o': case '1': case 'a': case 'X': case 'm':
        emitCmpArith(e);
        break;
    case '0':  /* <<= */
    case '6':  /* >>= */
        emitCmpShift(e);
        break;
    case 'T':  /* *= compound multiply */
    case '2':  /* /= compound divide */
        emitCmpMulDiv(e);
        break;
    default:
        if (e->left && e->right) {
            comment("%c%c d=%d %s%s%s [", e->op, e->type, e->demand, regnames[e->dest] ? regnames[e->dest] : "-",
                e->unused ? " U" : "", e->cond ? " C" : "");
            indent += 2;
            emitExpr(e->left);
            emitExpr(e->right);
            emit("XXXXXXXXX %c", e->op);
            indent -= 2;
            comment("]");
        } else if (e->left) {
            comment("%c%c d=%d %s%s%s [", e->op, e->type, e->demand, regnames[e->dest] ? regnames[e->dest] : "-",
                e->unused ? " U" : "", e->cond ? " C" : "");
            indent += 2;
            emitExpr(e->left);
            emit("XXXXXXXXX %c", e->op);
            indent -= 2;
            comment("]");
        } else {
            comment("%c%c d=%d %s%s%s", e->op, e->type, e->demand, regnames[e->dest] ? regnames[e->dest] : "-",
                e->unused ? " U" : "", e->cond ? " C" : "");
            emit("XXXXXXXXX %c", e->op);
        }
        break;
    }
}
/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
