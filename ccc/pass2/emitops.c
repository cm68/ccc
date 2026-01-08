/*
 * emitops.c - Compound assignment code emission
 */
#include <stdio.h>
#include "cc2.h"
#include "templates.h"
#include "../cpp/lexeme.h"

/*
 * Emit compound assignment: +=, -=, |=, &=, ^=, %=
 */
void
emitCmpArith(struct expr *e)
{
    comment("%c%c d=%d %s%s [", e->op, e->type, e->demand,
        regnames[e->dest] ? regnames[e->dest] : "-", e->unused ? " U" : "");
    indent += 2;
    if (e->left->op == REGVAR && e->size == 1) {
        char *rname = (e->left->aux == R_B) ? "b" : "c";
        comment("R%c %s %s", e->left->type, e->left->sym ? e->left->sym : "?", rname);
        emitExpr(e->right);
        if (e->right->op == AST_CONST) {
            unsigned char val = e->right->v.c;
            emit("ld a,%s", rname);
            switch (e->op) {
            case PLUSEQ: emit("add a,%d", val); break;
            case SUBEQ: emit("sub %d", val); break;
            case OREQ: emit("or %d", val); break;
            case ANDEQ: emit("and %d", val); break;
            case XOREQ: emit("xor %d", val); break;
            }
        } else {
            emitT(T_LDEA);
            emit("ld a,%s", rname);
            emitBOp(e->op);
        }
        emit("ld %s,a", rname);
    } else if (e->left->op == REGVAR && e->size == 2) {
        unsigned char r = e->left->aux;
        comment("R%c %s %s", e->left->type, e->left->sym ? e->left->sym : "?", regnames[r]);
        emitExpr(e->right);
        emitT(T_EXDE);
        if (r == R_BC) {
            emitT(T_LDHB);
            emitT(T_LDLC);
        } else if (r == R_IX) {
            emit("push ix");
            emit("pop hl");
        }
        switch (e->op) {
        case PLUSEQ: emit("add hl,de"); break;
        case SUBEQ: emit("or a"); emit("sbc hl,de"); break;
        case OREQ: case ANDEQ: case XOREQ: emitWBit(e->op); break;
        }
        if (r == R_BC) {
            emit("ld b,h");
            emit("ld c,l");
        } else if (r == R_IX) {
            emit("push hl");
            emit("pop ix");
        }
    } else if (e->left->op == LOCALVAR && e->size == 1) {
        char ofs = e->left->offset;
        char *rn = (e->left->aux == R_IX) ? "ix" : "iy";
        comment("V%c %s%+d", e->left->type, rn, ofs);
        emit("ld a,(%s%o)", rn, ofs);
        if (e->right->op == AST_CONST) {
            unsigned char val = e->right->v.c;
            switch (e->op) {
            case PLUSEQ: emit("add a,%d", val); break;
            case SUBEQ: emit("sub %d", val); break;
            case OREQ: emit("or %d", val); break;
            case ANDEQ: emit("and %d", val); break;
            case XOREQ: emit("xor %d", val); break;
            }
        } else {
            emitExpr(e->right);
            emit("ld a,(%s%o)", rn, ofs);
            emitBOp(e->op);
        }
        emit("ld (%s%o),a", rn, ofs);
    } else if (e->size == 1 && e->right->op == AST_CONST) {
        unsigned char val = e->right->v.c;
        emitExpr(e->left);
        emitT(T_LDAHL);
        switch (e->op) {
        case PLUSEQ: emit("add a,%d", val); break;
        case SUBEQ: emit("sub %d", val); break;
        case OREQ: emit("or %d", val); break;
        case ANDEQ: emit("and %d", val); break;
        case XOREQ: emit("xor %d", val); break;
        }
        emitT(T_STHLA);
    } else if (e->size == 1) {
        /* byte compound assign to computed address with non-const operand */
        emitExpr(e->right);  /* operand to A (via L if word) */
        if (e->right->size == 2)
            emit("ld a,l");
        emitT(T_LDEA);      /* save operand in E */
        emitExpr(e->left);   /* address to HL */
        emitT(T_LDAHL);   /* current value to A */
        emitBOp(e->op);
        emitT(T_STHLA);
    } else if (e->left->op == LOCALVAR && e->size == 2) {
        char ofs = e->left->offset;
        char *rn = (e->left->aux == R_IX) ? "ix" : "iy";
        comment("V%c %s%+d", e->left->type, rn, ofs);
        emitExpr(e->right);
        emitT(T_EXDE);
        emit("ld l,(%s%o)", rn, ofs);
        emit("ld h,(%s%o)", rn, ofs + 1);
        switch (e->op) {
        case PLUSEQ: emit("add hl,de"); break;
        case SUBEQ: emit("or a"); emit("sbc hl,de"); break;
        case OREQ: case ANDEQ: case XOREQ: emitWBit(e->op); break;
        }
        emit("ld (%s%o),l", rn, ofs);
        emit("ld (%s%o),h", rn, ofs + 1);
    } else if (e->left->op == LOCALVAR && e->size == 4) {
        /* long compound arith on V node */
        char ofs = e->left->offset;
        char *rn = (e->left->aux == R_IX) ? "ix" : "iy";
        comment("V%c %s%+d", e->left->type, rn, ofs);
        emitExpr(e->right);  /* right to _lR */
        /* load left operand into _lL */
        emit("push %s", rn);
        emit("pop hl");
        if (ofs != 0) {
            emit("ld de,%d", (int)(char)ofs);
            emitT(T_ADDHLDE);
        }
        emit("call lldHL");  /* load (HL) to _lL */
        switch (e->op) {
        case PLUSEQ: emit("call ladd"); break;
        case SUBEQ: emit("call lsub"); break;
        case OREQ: emit("call lor"); break;
        case ANDEQ: emit("call land"); break;
        case XOREQ: emit("call lxor"); break;
        }
        /* store from _lR to local */
        emit("push %s", rn);
        emit("pop hl");
        if (ofs != 0) {
            emit("ld de,%d", (int)(char)ofs);
            emitT(T_ADDHLDE);
        }
        emit("call lstHLR");
    } else if (e->size == 2 && e->right->op == AST_CONST) {
        unsigned val = e->right->v.s & 0xffff;
        emitExpr(e->left);
        emit("ld e,(hl)");
        emit("inc hl");
        emit("ld d,(hl)");
        emit("dec hl");
        emitT(T_EXDE);
        switch (e->op) {
        case PLUSEQ: case SUBEQ: emitWSubBC(e->op, val); break;
        case OREQ: case ANDEQ: case XOREQ: emitWBitImm(e->op, val); break;
        }
        emitT(T_EXDE);
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
        emitT(T_EXDE);
        emit("pop de");
        emit("pop bc");
        switch (e->op) {
        case PLUSEQ: emit("add hl,bc"); break;
        case SUBEQ: emit("or a"); emit("sbc hl,bc"); break;
        case OREQ: case ANDEQ: case XOREQ: emitWBitBC(e->op); break;
        }
        emitT(T_EXDE);
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
    if (e->left->op == REGVAR && e->size == 2 && e->right->op == AST_CONST) {
        /* word regvar <<= or >>= constant */
        unsigned char cnt = e->right->v.c & 0xf;
        comment("R%c %s bc", e->left->type, e->left->sym ? e->left->sym : "?");
        comment("#%c %d d=%d -", e->right->type, cnt, e->right->demand);
        emitT(T_LDHB);
        emitT(T_LDLC);
        if (e->op == LSHIFTEQ) {
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
    } else if (e->left->op == LOCALVAR && e->size == 2 && e->right->op == AST_CONST) {
        /* word local <<= or >>= constant */
        char ofs = e->left->offset;
        char *rn = (e->left->aux == R_IX) ? "ix" : "iy";
        unsigned char cnt = e->right->v.c & 0xf;
        comment("V%c %s%+d", e->left->type, rn, ofs);
        emit("ld l,(%s%o)", rn, ofs);
        emit("ld h,(%s%o)", rn, ofs + 1);
        if (e->op == LSHIFTEQ) {
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
    } else if (e->left->op == LOCALVAR && e->size == 4 && e->right->op == AST_CONST) {
        char ofs = e->left->offset;
        char *rn = (e->left->aux == R_IX) ? "ix" : "iy";
        unsigned char cnt = e->right->v.c & 0x1f;
        comment("Vl %s%+d", rn, ofs);
        /* compute address and load to _lR */
        emit("push %s", rn);
        emit("pop hl");
        if (ofs != 0) {
            emit("ld de,%d", (int)(char)ofs);
            emitT(T_ADDHLDE);
        }
        emit("call lldHLR");  /* load (HL) to _lR */
        emit("ld a,%d", cnt);
        if (e->op == LSHIFTEQ)
            emit("call lshl");
        else
            emit("call lashr");
        /* store from _lR */
        emit("push %s", rn);
        emit("pop hl");
        if (ofs != 0) {
            emit("ld de,%d", (int)(char)ofs);
            emitT(T_ADDHLDE);
        }
        emit("call lstHLR");
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
    if (e->left->op == REGVAR && e->left->size == 1) {
        char *rname = (e->left->aux == R_B) ? "b" : "c";
        comment("R%c %s %s", e->left->type, e->left->sym ? e->left->sym : "?", rname);
        emitExpr(e->right);
        emitT(T_LDEA);
        emit("ld a,%s", rname);
        if (e->op == MULTEQ)
            emit("call imulb");
        else
            emit("call idivb");
        emit("ld %s,l", rname);
    } else if (e->left->op == REGVAR && e->left->size == 2) {
        unsigned char r = e->left->aux;
        comment("R%c %s %s", e->left->type, e->left->sym ? e->left->sym : "?", regnames[r]);
        emitExpr(e->right);
        emitT(T_EXDE);
        if (r == R_BC) {
            emitT(T_LDHB);
            emitT(T_LDLC);
        } else if (r == R_IX) {
            emit("push ix");
            emit("pop hl");
        }
        if (e->op == MULTEQ)
            emit("call imul");
        else
            emit("call idiv");
        if (r == R_BC) {
            emit("ld b,h");
            emit("ld c,l");
        } else if (r == R_IX) {
            emit("push hl");
            emit("pop ix");
        }
    } else if (e->left->op == LOCALVAR && e->size == 1) {
        char ofs = e->left->offset;
        char *rn = (e->left->aux == R_IX) ? "ix" : "iy";
        comment("V%c %s%+d", e->left->type, rn, ofs);
        emitExpr(e->right);
        emitT(T_LDEA);
        emit("ld a,(%s%o)", rn, ofs);
        if (e->op == MULTEQ)
            emit("call imulb");
        else
            emit("call idivb");
        emit("ld a,l");
        emit("ld (%s%o),a", rn, ofs);
    } else if (e->left->op == LOCALVAR && e->size == 2) {
        char ofs = e->left->offset;
        char *rn = (e->left->aux == R_IX) ? "ix" : "iy";
        comment("V%c %s%+d", e->left->type, rn, ofs);
        emitExpr(e->right);
        emitT(T_EXDE);
        emit("ld l,(%s%o)", rn, ofs);
        emit("ld h,(%s%o)", rn, ofs + 1);
        if (e->op == MULTEQ)
            emit("call imul");
        else
            emit("call idiv");
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
