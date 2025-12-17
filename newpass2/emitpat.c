/*
 * emitpat.c - Pattern-based instruction emission
 *
 * Reduces code size by abstracting repeated emit sequences
 * into table-driven pattern expansion.
 */
#include <stdio.h>
#include "cc2.h"

/*
 * Operation table for compound arithmetic/bitwise ops.
 * Maps operator codes to instruction strings for byte/word operations.
 *
 * Operators: P(+=), o(-=), 1(|=), a(&=), X(^=), m(%=)
 */
struct opEntry {
    char op;            /* operator character */
    char *byteImm;      /* byte: A op imm */
    char *byteReg;      /* byte: A op E */
    char *wordOp;       /* word: for emitWBit */
};

static struct opEntry opTab[] = {
    { 'P', "add a,%d", "add a,e", 0 },
    { 'o', "sub %d",   "sub e",   0 },
    { '1', "or %d",    "or e",    "or" },
    { 'a', "and %d",   "and e",   "and" },
    { 'X', "xor %d",   "xor e",   "xor" },
    { '&', 0,          "and e",   "and" },
    { '|', 0,          "or e",    "or" },
    { '^', 0,          "xor e",   "xor" },
    { 0 }
};

/*
 * Find operation entry by operator character
 */
struct opEntry *
findOp(char op)
{
    struct opEntry *p;
    for (p = opTab; p->op; p++)
        if (p->op == op)
            return p;
    return 0;
}

/*
 * Emit byte operation: A = A op E
 */
void
emitBOp(char op)
{
    struct opEntry *p = findOp(op);
    if (p && p->byteReg)
        emit(p->byteReg);
    else
        emit("XXXXXXXXX byteop %c", op);
}

/*
 * Emit byte operation with immediate: A = A op imm
 */
void
emitBOpImm(char op, int val)
{
    struct opEntry *p = findOp(op);
    if (p && p->byteImm)
        emit(p->byteImm, val & 0xff);
    else
        emit("XXXXXXXXX byteimm %c", op);
}

/*
 * Emit 16-bit bitwise operation: HL = HL op DE
 * Pattern: ld a,l; op e; ld l,a; ld a,h; op d; ld h,a
 */
void
emitWBit(char op)
{
    struct opEntry *p = findOp(op);
    if (p && p->wordOp) {
        emit("ld a,l");
        emit("%s e", p->wordOp);
        emit("ld l,a");
        emit("ld a,h");
        emit("%s d", p->wordOp);
        emit("ld h,a");
    } else {
        emit("XXXXXXXXX wordbit %c", op);
    }
}

/*
 * Emit 16-bit bitwise with BC: HL = HL op BC
 * Pattern: ld a,l; op c; ld l,a; ld a,h; op b; ld h,a
 */
void
emitWBitBC(char op)
{
    struct opEntry *p = findOp(op);
    if (p && p->wordOp) {
        emit("ld a,l");
        emit("%s c", p->wordOp);
        emit("ld l,a");
        emit("ld a,h");
        emit("%s b", p->wordOp);
        emit("ld h,a");
    } else {
        emit("XXXXXXXXX wordbitbc %c", op);
    }
}

/*
 * Emit 16-bit bitwise with immediate: HL = HL op imm
 * Pattern: ld a,l; op lo; ld l,a; ld a,h; op hi; ld h,a
 */
void
emitWBitImm(char op, int val)
{
    struct opEntry *p = findOp(op);
    if (p && p->wordOp) {
        emit("ld a,l");
        emit("%s %d", p->wordOp, val & 0xff);
        emit("ld l,a");
        emit("ld a,h");
        emit("%s %d", p->wordOp, (val >> 8) & 0xff);
        emit("ld h,a");
    } else {
        emit("XXXXXXXXX wordbitimm %c", op);
    }
}

/*
 * Emit 16-bit complement: HL = ~HL
 * Pattern: ld a,l; cpl; ld l,a; ld a,h; cpl; ld h,a
 */
void
emitWCpl(void)
{
    emit("ld a,l");
    emit("cpl");
    emit("ld l,a");
    emit("ld a,h");
    emit("cpl");
    emit("ld h,a");
}

/*
 * Emit test for zero based on size/register
 * Sets Z flag if value is zero
 * reg: 0 for HL, R_BC for BC regvar
 */
void
emitTestZero(unsigned char size, unsigned char reg)
{
    if (reg == R_BC) {
        emit("ld a,b");
        emit("or c");
    } else if (size == 2) {
        emit("ld a,h");
        emit("or l");
    } else {
        emit("or a");
    }
}

/*
 * Emit test for zero for an expression result
 * Handles regvar BC as special case
 */
void
emitTestExpr(struct expr *e)
{
    if (e->op == 'R' && e->demand == 0 && e->aux == R_BC)
        emitTestZero(2, R_BC);
    else
        emitTestZero(e->size, 0);
}

/*
 * Emit word add/sub: HL = HL +/- DE
 */
void
emitWAddSub(char op)
{
    if (op == 'P' || op == '+') {
        emit("add hl,de");
    } else {
        emit("or a");
        emit("sbc hl,de");
    }
}

/*
 * Emit word add/sub with BC: HL = HL +/- BC
 */
void
emitWSubBC(char op, int val)
{
    emit("ld bc,%d", val);
    if (op == 'P' || op == '+') {
        emit("add hl,bc");
    } else {
        emit("or a");
        emit("sbc hl,bc");
    }
}

/*
 * Emit long load from (HL) to temp
 * dest: R_DE -> _lL (left), R_HL -> _lR (right)
 */
void
emitLLoad(unsigned char dest)
{
    if (dest == R_DE)
        emit("call _lldHL");
    else
        emit("call _lldHLR");
}

/*
 * Emit long store from _lL to (HL)
 */
void
emitLStore(void)
{
    emit("call _lstHL");
}

/*
 * Emit long store from _lR to (HL)
 */
void
emitLStoreR(void)
{
    emit("call _lstHLR");
}

/*
 * Emit long immediate to _lL
 */
void
emitLImm(long val)
{
    emit("ld hl,%d", (int)(val & 0xffff));
    emit("ld (_lL),hl");
    emit("ld hl,%d", (int)((val >> 16) & 0xffff));
    emit("ld (_lL+2),hl");
}

/*
 * Emit long immediate to _lR
 */
void
emitLImmR(long val)
{
    emit("ld hl,%d", (int)(val & 0xffff));
    emit("ld (_lR),hl");
    emit("ld hl,%d", (int)((val >> 16) & 0xffff));
    emit("ld (_lR+2),hl");
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
