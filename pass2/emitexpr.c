/*
 * emitexpr.c - Expression code emission
 */
#include <stdio.h>
#include "cc2.h"

/*
 * Emit memory dereference: M (load from address)
 */
void
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
            emitLLoad(e->dest);
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
void
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
            } else if (e->size == 4) {
                /* Long to stack: push high word first, then low */
                emit("ld hl,%d", (e->v.l >> 16) & 0xffff);
                emit("push hl");
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
            if (e->dest == R_DE)
                emitLImm(e->v.l);
            else
                emitLImmR(e->v.l);
        } else if (e->type == T_VOID) {
            /* void - no value */
        } else {
            emit("XXXXXXXXX #?");
        }
        break;
    case '$':
        if (e->aux == R_IY || e->aux == R_IX) {
            /* Indexed address: compute IX/IY + offset */
            char off = e->offset;
            char *rn = (e->aux == R_IX) ? "ix" : "iy";
            comment("$%s %s%+d d=%d %s", e->sym ? e->sym : "?", rn, off,
                    e->demand, regnames[e->dest] ? regnames[e->dest] : "-");
            if (e->demand == 0 && e->dest != R_TOS)
                break;
            emit("push %s", rn);
            emit("pop hl");
            if (off != 0) {
                if (off >= -4 && off <= 4) {
                    /* Small offset: use inc/dec hl */
                    while (off > 0) { emit("inc hl"); off--; }
                    while (off < 0) { emit("dec hl"); off++; }
                } else {
                    emit("ld de,%d", off);
                    emit("add hl,de");
                }
            }
            if (e->dest == R_TOS)
                emit("push hl");
        } else {
            comment("$%s d=%d %s", e->sym ? e->sym : "?", e->demand, regnames[e->dest] ? regnames[e->dest] : "-");
            if (e->dest == R_TOS) {
                emit("ld hl,%s", e->sym);
                emit("push hl");
            } else if (e->demand > 0)
                emit("ld %s,%s", regnames[e->dest] ? regnames[e->dest] : "hl", e->sym);
        }
        break;
    case 'R':  /* register var */
        {
            static char *rn[] = { "?", "b", "c", "bc", "ix" };
            comment("R%c %s %s d=%d %s", e->type, e->sym ? e->sym : "?",
                rn[e->aux < 5 ? e->aux : 0], e->demand, regnames[e->dest] ? regnames[e->dest] : "-");
            /* Even with demand=0, TOS dest needs push */
            if (e->demand == 0 && e->dest != R_TOS)
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
                    if (e->dest == R_TOS) {
                        emit("push bc");
                    } else {
                        emit("ld h,b");
                        emit("ld l,c");
                    }
                } else if (e->aux == R_IX) {
                    if (e->dest == R_TOS) {
                        emit("push ix");
                    } else {
                        emit("push ix");
                        emit("pop hl");
                    }
                }
                if (e->dest == R_TOS && e->aux != R_BC && e->aux != R_IX)
                    emit("push hl");
            }
        }
        break;
    case 'V':  /* local/struct var via IY or IX */
        {
            char off = e->offset;
            char *rn = (e->aux == R_IX) ? "ix" : "iy";
            comment("V%c %s %s%+d d=%d %s", e->type, e->sym ? e->sym : "?", rn, off, e->demand, regnames[e->dest] ? regnames[e->dest] : "-");
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
                /* compute address: push ix/iy, pop hl, add offset */
                emit("push %s", rn);
                emit("pop hl");
                if (off != 0) {
                    emit("ld de,%d", (int)(char)off);
                    emit("add hl,de");
                }
                if (e->dest == R_TOS) {
                    emit("call lldHL");
                    emit("ld hl,(lL+2)");
                    emit("push hl");
                    emit("ld hl,(lL)");
                    emit("push hl");
                } else {
                    emitLLoad(e->dest);
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
                /* long: store from lR to symbol */
                emit("ld hl,%s", e->left->sym);
                emitLStoreR();
            }
        } else if (e->left->op == 'V') {
            /* assign to local/struct var via IY or IX */
            char ofs = e->left->offset;
            char *rn = (e->left->aux == R_IX) ? "ix" : "iy";
            comment("V%c %s%+d", e->left->type, rn, ofs);
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
                    /* long: compute addr, store from lR */
                    emit("push %s", rn);
                    emit("pop hl");
                    if (ofs != 0) {
                        emit("ld de,%d", (int)(char)ofs);
                        emit("add hl,de");
                    }
                    emitLStoreR();
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
                /* long: addr in DE, value in lR */
                emit("ex de,hl");
                emitLStoreR();
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
                /* long: addr in DE, value in lR */
                emit("ex de,hl");
                emitLStoreR();
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
            emit("call %s", e->left->sym);
        } else {
            emitExpr(e->left);
            emit("call callhl");
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
            emitTestZero(e->left->size, 0);
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
                /* 32-bit add: lR = lL + lR */
                emit("call ladd");
            }
        }
        indent -= 2;
        comment("]");
        break;
    case '-':  /* subtract */
        comment("-%c d=%d %s [", e->type, e->demand, regnames[e->dest] ? regnames[e->dest] : "-");
        indent += 2;
        emitExpr(e->left);
        /* Skip emitting right if byte immediate - use sub n directly */
        if (!(e->size == 1 && e->right->op == '#'))
            emitExpr(e->right);
        if (e->size == 2) {
            /* Need HL = left - right. Check where left ended up */
            if (e->left->dest == R_DE) {
                emit("ex de,hl");  /* swap: left to HL, right to DE */
            }
            emit("or a");
            emit("sbc hl,de");
        } else if (e->size == 1) {
            /* For byte: check if right is immediate constant */
            if (e->right->op == '#') {
                /* sub immediate: A = A - const */
                emit("sub %d", e->right->v.c & 0xff);
            } else if (e->left->dest == R_A) {
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
            /* 32-bit sub: lR = lL - lR */
            emit("call lsub");
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
                emitBOp('&');
            } else if (e->size == 2) {
                emitWBit('&');
            } else if (e->size == 4) {
                emit("call land");
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
            emitBOp('|');
        } else if (e->size == 2) {
            emitWBit('|');
        } else if (e->size == 4) {
            emit("call lor");
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
            emitBOp('^');
        } else if (e->size == 2) {
            emitWBit('^');
        } else if (e->size == 4) {
            emit("call lxor");
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
                emit("call imulb");
            } else if (lbyte) {
                /* byte × word: A has left (byte), HL has right (word) */
                emit("call imula");
            } else if (rbyte) {
                /* word × byte: HL has left (word), A has right (byte) */
                emit("call imula");
            } else {
                /* word × word: DE has left, HL has right */
                emit("call imul");
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
                    emit("call idivb");
                else
                    emit("call imodb");
            } else {
                /* word op word (or mixed) */
                if (e->op == '/')
                    emit("call idiv");
                else
                    emit("call imod");
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
                emit("call lshl");
            }
        } else {
            /* variable shift count: call helper */
            emitExpr(e->right);
            if (e->size == 4)
                emit("call lshl");
            else
                emit("call lshl");
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
                emit("call lashr");
            }
        } else {
            /* variable shift count: call helper */
            emitExpr(e->right);
            if (e->size == 4)
                emit("call lashr");
            else
                emit("call ashr");
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
                emit("call lshr");
            }
        } else {
            /* variable shift count: call helper */
            emitExpr(e->right);
            if (e->size == 4)
                emit("call lshr");
            else
                emit("call lshr");
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
            /* 32-bit negation: lR = -lR */
            emit("call lneg");
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
            emitWCpl();
        } else if (e->size == 4) {
            /* 32-bit complement: lR = ~lR */
            emit("call lcom");
        }
        indent -= 2;
        comment("]");
        break;
    case '<': case 'Q': case 'n':  /* pass1 normalizes >, <=, >= to these */
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
            emitTestZero(e->left->size, 0);
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
        } else if (e->left->op == 'V') {
            /* V node: just load the low byte directly */
            char *rn = (e->left->aux == R_IX) ? "ix" : "iy";
            comment("V%c %s%+d", e->left->type, rn, e->left->offset);
            emit("ld a,(%s%o)", rn, e->left->offset);
            if (e->dest == R_TOS)
                emit("push af");
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
            /* aux=1 means inside ||: use local label, fall through with Z for FALSE */
            /* aux=0 means not inside ||: jump to no{aux2} on FALSE */
            char lop = e->left->op, rop = e->right->op;
            /* Check if children are comparisons that emit their own jumps */
            int leftIsCmp = (lop == '<' || lop == 'Q' || lop == 'n') && e->left->cond;
            int rightIsCmp = (rop == '<' || rop == 'Q' || rop == 'n') && e->right->cond;
            if (e->aux) {
                /* Inside ||: use unique local label */
                int localLbl = labelCnt++;
                emitExpr(e->left);
                if (!leftIsCmp) {
                    emitTestExpr(e->left);
                    emit("jp z,ja%d_%d", localLbl, fnIndex);
                }
                emitExpr(e->right);
                if (!rightIsCmp) {
                    emitTestExpr(e->right);
                    emit("jp z,ja%d_%d", localLbl, fnIndex);
                }
                emit("ja%d_%d:", localLbl, fnIndex);
            } else {
                /* Not inside ||: jump to no{aux2} on FALSE */
                emitExpr(e->left);
                if (!leftIsCmp) {
                    emitTestExpr(e->left);
                    emit("jp z,no%d_%d", e->aux2, fnIndex);
                }
                emitExpr(e->right);
                if (!rightIsCmp) {
                    emitTestExpr(e->right);
                    emit("jp z,no%d_%d", e->aux2, fnIndex);
                }
            }
        } else {
            /* Non-conditional: need 0/1 value in HL */
            int lbl = labelCnt++, lbl2;
            emitExpr(e->left);
            emitTestZero(e->left->size, 0);
            emit("jp z,ja%d_%d", lbl, fnIndex);
            emitExpr(e->right);
            emitTestZero(e->right->size, 0);
            emit("ja%d_%d:", lbl, fnIndex);
            /* A is zero if we jumped OR if right is zero */
            lbl2 = labelCnt++;
            emit("ld hl,0");
            emit("jp z,jz%d_%d", lbl2, fnIndex);
            emit("inc l");
            emit("jz%d_%d:", lbl2, fnIndex);
        }
        indent -= 2;
        comment("]");
        break;
    case 'h':  /* logical or (||) with short-circuit */
        comment("h%c d=%d %s%s [", e->type, e->demand, regnames[e->dest] ? regnames[e->dest] : "-", e->cond ? " C" : "");
        indent += 2;
        if (e->cond) {
            /* For ||: if any child is true (nz), skip to ht (take then) */
            /* aux=1 means nested inside another ||: fall through, parent emits ht */
            /* aux=0 means not nested: emit ht label here */
            int htLabel = e->aux2 + 1;
            char lop = e->left->op, rop = e->right->op;
            /* Check if children are comparisons that emit their own jumps */
            int leftIsCmp = (lop == '<' || lop == 'Q' || lop == 'n') && e->left->cond;
            int rightIsCmp = (rop == '<' || rop == 'Q' || rop == 'n') && e->right->cond;
            emitExpr(e->left);
            if (!leftIsCmp) {
                emitTestExpr(e->left);
                emit("jp nz,ht%d_%d", htLabel, fnIndex);
            }
            emitExpr(e->right);
            if (!rightIsCmp) {
                emitTestExpr(e->right);
            }
            if (e->aux) {
                /* Nested: fall through with Z flag for FALSE, NZ already jumped to ht */
            } else {
                /* Not nested: FALSE jumps to no, emit ht label */
                if (!rightIsCmp)
                    emit("jp z,no%d_%d", e->aux2, fnIndex);
                emit("ht%d_%d:", htLabel, fnIndex);
            }
        } else {
            /* Non-conditional: need 0/1 value */
            int lbl = labelCnt++;
            emitExpr(e->left);
            emitTestZero(e->left->size, 0);
            emit("jp nz,ht%d_%d", lbl, fnIndex);
            emitExpr(e->right);
            emitTestZero(e->right->size, 0);
            emit("ht%d_%d:", lbl, fnIndex);
            {
                int lbl2 = labelCnt++;
                emit("ld hl,0");
                emit("jp z,hz%d_%d", lbl2, fnIndex);
                emit("inc l");
                emit("hz%d_%d:", lbl2, fnIndex);
            }
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
