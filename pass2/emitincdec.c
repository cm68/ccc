/*
 * emitincdec.c - Increment/decrement code emission
 */
#include <stdio.h>
#include "cc2.h"

/*
 * Emit pre-increment/decrement: (, {
 */
void
emitPreIncDec(struct expr *e)
{
    if (e->special == SP_INCR || e->special == SP_DECR) {
        char *ins = (e->special == SP_INCR) ? "inc" : "dec";
        unsigned char i;
        if (e->dest == R_IYO) {
            /* byte local via (iy+offset) */
            char *rn = (e->left->aux == R_IX) ? "ix" : "iy";
            for (i = 0; i < e->incr; i++)
                emit("%s (%s%o)", ins, rn, e->offset);
        } else {
            /* regvar: b, c, bc, or ix */
            for (i = 0; i < e->incr; i++)
                emit("%s %s", ins, regnames[e->dest]);
        }
    } else if (e->special == SP_INCGLOB) {
        char *ins = e->aux2 ? "inc" : "dec";
        unsigned char i;
        emit("ld hl,(%s)", e->sym);
        for (i = 0; i < e->incr; i++)
            emit("%s hl", ins);
        emit("ld (%s),hl", e->sym);
    } else {
        comment("%c%c%s [", e->op, e->type, e->unused ? " U" : "");
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
            /* Result in BC, copy to HL */
            emit("ld h,b");
            emit("ld l,c");
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
        } else if (e->size == 2) {
            /* Large increment for 16-bit pre-inc/dec */
            emit("ld e,(hl)");
            emit("inc hl");
            emit("ld d,(hl)");
            emit("dec hl");
            emit("push hl");
            emit("ex de,hl");
            if (e->op == '(')
                emit("ld de,%d", e->aux2);
            else
                emit("ld de,-%d", e->aux2);
            emit("add hl,de");
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
void
emitPostInc(struct expr *e)
{
    comment("%c%c%s [", e->op, e->type, e->unused ? " U" : "");
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
    } else if (e->size == 2) {
        /* Large increment for 16-bit */
        emitExpr(e->left);
        comment("incr=%d", e->aux2);
        emit("ld e,(hl)");
        emit("inc hl");
        emit("ld d,(hl)");
        emit("dec hl");
        if (!e->unused)
            emit("push de");
        emit("ex de,hl");
        if (e->op == ')')
            emit("ld de,%d", e->aux2);
        else
            emit("ld de,-%d", e->aux2);
        emit("add hl,de");
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
    } else if (e->size == 1) {
        /* Large increment for 8-bit */
        emitExpr(e->left);
        comment("incr=%d", e->aux2);
        emit("ld a,(hl)");
        if (!e->unused)
            emit("ld e,a");
        if (e->op == ')')
            emit("add a,%d", e->aux2);
        else
            emit("sub %d", e->aux2);
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
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
