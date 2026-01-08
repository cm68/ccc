/*
 * emitexpr.c - Expression code emission
 */
#include <stdio.h>
#include "cc2.h"
#include "templates.h"
#include "../cpp/lexeme.h"

/*
 * Emit primary expressions: constants, symbols, regvars, locals
 * Result: word in HL, byte in A
 */
void
emitPrimary(struct expr *e)
{
    switch (e->op) {
    case AST_CONST:
        comment("#%c %ld", e->type, e->v.l);
        if (e->size == 1) {
            emit("ld a,%d", e->v.c & 0xff);
        } else if (e->size == 2) {
            emit("ld hl,%d", e->v.s & 0xffff);
        } else if (e->size == 4) {
            emitLImmR(e->v.l);
        } else if (e->type == T_VOID) {
            /* void - no value */
        } else {
            emit("XXXXXXXXX #?");
        }
        break;
    case SYM:
        if (e->aux == R_IY || e->aux == R_IX) {
            /* Indexed address: compute IX/IY + offset */
            char off = e->offset;
            char *rn = (e->aux == R_IX) ? "ix" : "iy";
            comment("$%s %s%+d", e->sym ? e->sym : "?", rn, off);
            emit("push %s", rn);
            emitT(T_POPHL);
            if (off != 0) {
                if (off >= -4 && off <= 4) {
                    /* Small offset: use inc/dec hl */
                    while (off > 0) { emit("inc hl"); off--; }
                    while (off < 0) { emit("dec hl"); off++; }
                } else {
                    emit("ld de,%d", off);
                    emitT(T_ADDHLDE);
                }
            }
        } else {
            comment("$%s", e->sym ? e->sym : "?");
            emit("ld hl,%s", e->sym);
        }
        break;
    case REGVAR:  /* register var */
        {
            static char *rn[] = { "?", "b", "c", "bc", "ix" };
            comment("R%c %s %s", e->type, e->sym ? e->sym : "?",
                rn[e->aux < 5 ? e->aux : 0]);
            if (e->size == 1) {
                if (e->aux == R_B)
                    emit("ld a,b");
                else if (e->aux == R_C)
                    emit("ld a,c");
            } else if (e->size == 2) {
                if (e->aux == R_BC) {
                    emitT(T_LDHB);
                    emitT(T_LDLC);
                } else if (e->aux == R_IX) {
                    emit("push ix");
                    emitT(T_POPHL);
                }
            }
        }
        break;
    case LOCALVAR:  /* local/struct var via IY or IX */
        {
            char off = e->offset;
            char *rn = (e->aux == R_IX) ? "ix" : "iy";
            comment("V%c %s %s%+d", e->type, e->sym ? e->sym : "?", rn, off);
            if (e->size == 1) {
                emit("ld a,(%s%o)", rn, off);
            } else if (e->size == 2) {
                emit("ld l,(%s%o)", rn, off);
                emit("ld h,(%s%o)", rn, off + 1);
            } else if (e->size == 4) {
                /* compute address: push ix/iy, pop hl, add offset */
                emit("push %s", rn);
                emitT(T_POPHL);
                if (off != 0) {
                    emit("ld de,%d", (int)(char)off);
                    emitT(T_ADDHLDE);
                }
                emitLLoad(R_HL);
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
    case AST_CONST:
    case SYM:
    case REGVAR:
    case LOCALVAR:
    case DEREF:
    case ASSIGN:
        pemitExpr(e);
        break;
    case CALL:
        comment("@%c nargs=%d%s [", e->type, e->aux, e->unused ? " U" : "");
        indent += 2;
        /* Args already in reverse order from parser - emit and push each */
        {
            struct expr *a = e->right;
            while (a && a->op == ARGNODE) {
                emitExpr(a->left);
                /* Push result to stack */
                if (a->left->size == 1) {
                    emit("push af");
                } else if (a->left->size == 2) {
                    emitT(T_PUSHHL);
                } else if (a->left->size == 4) {
                    /* Long: push high word first, then low */
                    emit("ld hl,(lR+2)");
                    emitT(T_PUSHHL);
                    emit("ld hl,(lR)");
                    emitT(T_PUSHHL);
                }
                a = a->right;
            }
        }
        /* call */
        if (e->left->op == SYM) {
            comment("$%s", e->left->sym);
            emit("call %s", e->left->sym);
        } else {
            emitExpr(e->left);
            emit("call callhl");
        }
        indent -= 2;
        comment("]");
        break;
    case TERNBRANCH:  /* ternary else part - handled by QUES case, should not reach here */
        /* If we somehow get here, just emit both children */
        comment(":%c [", e->type);
        indent += 2;
        if (e->left) emitExpr(e->left);
        if (e->right) emitExpr(e->right);
        indent -= 2;
        comment("]");
        break;
    case QUES:  /* ternary: cond ? then : else */
        {
            int lbl = labelCnt++;
            comment("?%c [", e->type);
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
    case PREINC:
    case PREDEC:
    case POSTINC:
    case POSTDEC:
        pemitExpr(e);
        break;
    case BFEXTRACT:  /* bitfield */
        comment("F%c off=%d wid=%d [", e->type, e->aux, e->aux2);
        indent += 2;
        emitExpr(e->left);
        emit("XXXXXXXXX F");
        indent -= 2;
        comment("]");
        break;
    case PLUS:
    case MINUS:
    case STAR:
    case DIV:
    case MOD:
    case AND:
    case OR:
    case XOR:
    case LSHIFT:
    case RSHIFT:
        /* Pattern-driven binary operators */
        pemitExpr(e);
        break;
    case URSHIFT:  /* unsigned right shift >>> */
        comment("z%c [", e->type);
        indent += 2;
        emitExpr(e->left);
        if (e->right->op == AST_CONST) {
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
            emitT(T_EXDE);
            emitExpr(e->right);
            emitT(T_LDAL);
            emitT(T_EXDE);
            emit("call lshr");
        }
        indent -= 2;
        comment("]");
        break;
    case NEG:  /* negation (unary minus) */
        comment("\\%c [", e->type);
        indent += 2;
        emitExpr(e->left);
        if (e->size == 1) {
            emitT(T_NEG);
        } else if (e->size == 2) {
            /* HL = -HL = 0 - HL */
            emitT(T_XORA);
            emit("sub l");
            emitT(T_LDLA);
            emit("sbc a,a");
            emit("sub h");
            emitT(T_LDHA);
        } else if (e->size == 4) {
            /* 32-bit negation: lR = -lR */
            emit("call lneg");
        }
        indent -= 2;
        comment("]");
        break;
    case TWIDDLE:  /* bitwise complement */
        comment("~%c [", e->type);
        indent += 2;
        emitExpr(e->left);
        if (e->size == 1) {
            emitT(T_CPL);
        } else if (e->size == 2) {
            emitWCpl();
        } else if (e->size == 4) {
            /* 32-bit complement: lR = ~lR */
            emit("call lcom");
        }
        indent -= 2;
        comment("]");
        break;
    case LT: case EQ: case NEQ:  /* pass1 normalizes >, <=, >= to these */
        pemitExpr(e);
        break;
    case BANG:  /* logical not */
        comment("!%c%s [", e->type, e->cond ? " C" : "");
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
    case WIDEN:  /* widen: byte to word, zero extend */
        comment("W%c [", e->type);
        indent += 2;
        if (e->left->op == AST_CONST) {
            /* constant: widen at compile time */
            unsigned char val = e->left->v.c;
            comment("#B %d", val);
            emit("ld hl,%d", val);
        } else {
            emitExpr(e->left);  /* byte value in A */
            emitT(T_LDLA);
            emit("ld h,0");
        }
        indent -= 2;
        comment("]");
        break;
    case NARROW:  /* narrow: word to byte */
        comment("N%c [", e->type);
        indent += 2;
        if (e->left->op == AST_CONST) {
            /* constant: just load byte value */
            emit("ld a,%d", e->left->v.c & 0xff);
        } else if (e->left->op == LOCALVAR) {
            /* V node: just load the low byte directly */
            char *rn = (e->left->aux == R_IX) ? "ix" : "iy";
            comment("V%c %s%+d", e->left->type, rn, e->left->offset);
            emit("ld a,(%s%o)", rn, e->left->offset);
        } else {
            emitExpr(e->left);  /* word value in HL */
            emitT(T_LDAL);
        }
        indent -= 2;
        comment("]");
        break;
    case SEXT:  /* sign extend: byte to word */
        comment("x%c [", e->type);
        indent += 2;
        if (e->left->op == AST_CONST) {
            /* constant: sign extend at compile time */
            int val = (char)e->left->v.c;  /* sign extend to int */
            comment("#B %d", e->left->v.c & 0xff);
            emit("ld hl,%d", val & 0xffff);
        } else {
            emitExpr(e->left);  /* byte value in A */
            emitT(T_LDLA);
            emit("rlca");       /* sign bit to carry */
            emit("sbc a,a");    /* 0 or 0xff based on carry */
            emitT(T_LDHA);
        }
        indent -= 2;
        comment("]");
        break;
    case LAND:  /* logical and (&&) with short-circuit */
        comment("j%c%s [", e->type, e->cond ? " C" : "");
        indent += 2;
        if (e->cond) {
            /* aux=1 means inside ||: use local label, fall through with Z for FALSE */
            /* aux=0 means not inside ||: jump to no{aux2} on FALSE */
            unsigned char lop = e->left->op, rop = e->right->op;
            /* Check if children are comparisons that emit their own jumps */
            int leftIsCmp = (lop == LT || lop == EQ || lop == NEQ) && e->left->cond;
            int rightIsCmp = (rop == LT || rop == EQ || rop == NEQ) && e->right->cond;
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
    case LOR:  /* logical or (||) with short-circuit */
        comment("h%c%s [", e->type, e->cond ? " C" : "");
        indent += 2;
        if (e->cond) {
            /* For ||: if any child is true (nz), skip to ht (take then) */
            /* aux=1 means nested inside another ||: fall through, parent emits ht */
            /* aux=0 means not nested: emit ht label here */
            int htLabel = e->aux2 + 1;
            unsigned char lop = e->left->op, rop = e->right->op;
            /* Check if children are comparisons that emit their own jumps */
            int leftIsCmp = (lop == LT || lop == EQ || lop == NEQ) && e->left->cond;
            int rightIsCmp = (rop == LT || rop == EQ || rop == NEQ) && e->right->cond;
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
    case COMMA:  /* comma operator: left is value, right is side effect */
        comment(",%c [", e->type);
        indent += 2;
        emitExpr(e->right);  /* side effect first */
        emitExpr(e->left);   /* then value */
        indent -= 2;
        comment("]");
        break;
    case PLUSEQ: case SUBEQ: case OREQ: case ANDEQ: case XOREQ: case MODEQ:
    case LSHIFTEQ: case RSHIFTEQ:
    case MULTEQ: case DIVEQ:
        pemitExpr(e);
        break;
    default:
        if (e->left && e->right) {
            comment("%c%c%s%s [", e->op, e->type, e->unused ? " U" : "", e->cond ? " C" : "");
            indent += 2;
            emitExpr(e->left);
            emitExpr(e->right);
            emit("XXXXXXXXX %c", e->op);
            indent -= 2;
            comment("]");
        } else if (e->left) {
            comment("%c%c%s%s [", e->op, e->type, e->unused ? " U" : "", e->cond ? " C" : "");
            indent += 2;
            emitExpr(e->left);
            emit("XXXXXXXXX %c", e->op);
            indent -= 2;
            comment("]");
        } else {
            comment("%c%c%s%s", e->op, e->type, e->unused ? " U" : "", e->cond ? " C" : "");
            emit("XXXXXXXXX %c", e->op);
        }
        break;
    }
}
/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
