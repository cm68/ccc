/*
 * codegen.c - Code generation scheduling
 */
#include <stdio.h>
#include "cc2.h"
#include "../../cpp/lexeme.h"

/*
 * Check if byte expr can load to A without using HL
 * (constants, byte globals, byte regvars, stack bytes via (iy+d))
 */
unsigned char
isSimpleByte(struct expr *e)
{
    unsigned char op;
    if (!e) return 0;
    op = e->op;

    if (e->size != 1)
        return 0;

    if (op == AST_CONST) return 1;  /* constant */
    if (op == REGVAR) return 1;  /* regvar */

    /* Mb $sym - direct global byte */
    if (op == DEREF && e->left->op == SYM)
        return 1;

    /* Mb Vb - stack byte via (iy+d) */
    if (op == DEREF && e->left->op == LOCALVAR)
        return 1;

    return 0;
}

/*
 * Calculate tree depth
 */
unsigned char
treeDepth(struct expr *e)
{
    unsigned char ld, rd;
    if (!e || !e->op) return 0;
    ld = treeDepth(e->left);
    rd = treeDepth(e->right);
    return 1 + (ld > rd ? ld : rd);
}

/*
 * Annotate expression tree with special patterns
 * No demand calculation - just pattern detection for optimization
 */
void
annotate(struct expr *e)
{
    unsigned char op;
    unsigned char size;
    unsigned char count;

    if (!e) return;
    op = e->op;
    if (!op) return;

    size = e->size;
    count = e->aux2;

    /* Recursively annotate children first */
    annotate(e->left);
    annotate(e->right);

    /* special: =[BS] $var #const - store constant to global */
    if (op == ASSIGN && size <= 2 && e->left->op == SYM && e->right->op == AST_CONST) {
        e->special = SP_STCONST;
        return;
    }

    /* =b Rb #const -> ld b,n or ld c,n; direct byte regvar assign */
    if (op == ASSIGN && size == 1 && e->left->op == REGVAR &&
        (e->left->aux == R_B || e->left->aux == R_C) && e->right->op == AST_CONST) {
        e->special = SP_STCONST;
        return;
    }

    /* = V #const -> ld (iy+n),lo; ld (iy+n+1),hi; store const to local */
    if (op == ASSIGN && e->left->op == LOCALVAR && e->right->op == AST_CONST) {
        e->special = SP_STCONST;
        return;
    }

    /* (s Rs reg -> inc reg; pre-inc/dec of regvar, incr <= 4 */
    if ((op == PREINC || op == PREDEC) && e->left->op == REGVAR && count <= 4) {
        e->special = (op == PREINC) ? SP_INCR : SP_DECR;
        e->incr = e->aux2;
        e->dest = e->left->aux;     /* R_B, R_C, R_BC, or R_IX */
        return;
    }

    /* (b Vb ofs -> inc (iy+ofs); pre-inc/dec of byte local, incr <= 4 */
    if ((op == PREINC || op == PREDEC) && e->left->op == LOCALVAR && size == 1 && count <= 4) {
        e->special = (op == PREINC) ? SP_INCR : SP_DECR;
        e->incr = count;
        e->offset = e->left->aux2;  /* IY offset from V node */
        e->dest = R_IYO;            /* (iy+ofs) addressing */
        return;
    }

    /* (s $sym -> ld hl,(_sym); inc hl; ld (_sym),hl; inc/dec global word */
    if ((op == PREINC || op == PREDEC) && e->left->op == SYM && size == 2 && count <= 4) {
        e->special = SP_INCGLOB;
        e->incr = count;
        e->aux2 = (op == PREINC) ? 1 : 0;  /* 1=inc, 0=dec */
        e->sym = e->left->sym;          /* steal symbol */
        e->left->sym = 0;
        return;
    }

    /* +s $sym #const -> ld hl,sym+offset */
    if (op == PLUS && size == 2 &&
        e->left->op == SYM && e->right->op == AST_CONST && e->left->sym) {
        e->special = SP_SYMOFS;
        e->sym = e->left->sym;          /* steal symbol from child */
        e->left->sym = 0;               /* prevent double-free */
        e->offset = e->right->v.s;
        return;
    }

    /* +s Ms[Rs bc] #const -> ld hl,const; add hl,bc */
    if (op == PLUS && size == 2 &&
        e->left->op == DEREF && e->left->left->op == REGVAR &&
        e->left->left->aux == R_BC && e->right->op == AST_CONST) {
        e->special = SP_ADDBC;
        e->offset = e->right->v.s;
        return;
    }

    /* M[+s $sym #const] -> ld hl,(sym+offset) */
    if (op == DEREF && e->left->op == PLUS && e->left->size == 2 &&
        e->left->left->op == SYM && e->left->right->op == AST_CONST &&
        e->left->left->sym) {
        e->special = SP_SYMOFD;
        e->sym = e->left->left->sym;    /* steal symbol from grandchild */
        e->left->left->sym = 0;         /* prevent double-free */
        e->offset = e->left->right->v.s;
        return;
    }

    /* Ms $sym -> ld hl,(sym); direct deref of global */
    if (op == DEREF && e->left->op == SYM && e->left->sym) {
        e->special = SP_MSYM;
        e->sym = e->left->sym;          /* steal symbol */
        e->left->sym = 0;
        return;
    }

    /* *s expr #s 4 -> add hl,hl; multiply by power of 2 */
    if (op == STAR && e->right->op == AST_CONST) {
        long n = e->right->v.l;
        if (n > 0 && (n & (n - 1)) == 0) {
            unsigned char shifts = 0;
            while (n > 1) { shifts++; n >>= 1; }
            e->special = SP_MUL2;
            e->incr = shifts;
            return;
        }
    }

    /* &B M[(ix+ofs)] #pow2 -> bit n,(ix+ofs); bit test */
    if (op == AND && size == 1 && e->right->op == AST_CONST) {
        long n = e->right->v.l & 0xff;
        /* check power of 2 (single bit) */
        if (n > 0 && (n & (n - 1)) == 0) {
            struct expr *l = e->left;
            /* check (ix+ofs) pattern on left */
            if (l->op == DEREF && l->left->op == PLUS && l->left->size == 2 &&
                l->left->left->op == DEREF && l->left->left->size == 2 &&
                l->left->left->left->op == REGVAR && l->left->left->left->aux == R_IX &&
                l->left->right->op == AST_CONST) {
                unsigned char bit = 0;
                while (n > 1) { bit++; n >>= 1; }
                e->special = SP_BITTEST;
                e->offset = l->left->right->v.s;  /* ix offset */
                e->incr = bit;                     /* bit number 0-7 */
                return;
            }
        }
    }

    /* Word equality with small constant: == 0, == 1, == -1 */
    if ((op == EQ || op == NEQ) && ISWORD(e->left->type) &&
        e->right->op == AST_CONST) {
        long val = e->right->v.l;
        if (val == 0 || val == 1 || val == -1 || val == 0xffff) {
            e->special = SP_CMPEQ;
            e->incr = val;
            return;
        }
    }

    /* cmpB Vb #const -> ld a,const; cp (iy/ix+off) */
    /* cmpB Rb #const -> ld a,reg; cp const */
    if ((op == LT || op == EQ || op == NEQ) && size == 1) {
        struct expr *l = e->left, *r = e->right;
        if (l->op == LOCALVAR && r->op == AST_CONST) {
            e->special = SP_CMPV;
            e->aux = l->aux;        /* R_IX or R_IY */
            e->offset = l->offset;  /* stack/struct offset */
            e->incr = r->v.c & 0xff; /* constant value */
            return;
        }
        if (l->op == REGVAR && r->op == AST_CONST) {
            e->special = SP_CMPR;
            e->aux = l->aux;        /* R_B or R_C */
            e->incr = r->v.c & 0xff; /* constant value */
            return;
        }
    }

    /* cmpB with (hl): left is Mb[addr], right is simple (normalized) */
    /* Exclude Mb[Rp] - register pointer needs copy to HL first */
    if ((op == LT || op == EQ || op == NEQ) && size == 1) {
        struct expr *l = e->left, *r = e->right;
        if (l->op == DEREF && l->size == 1 && isSimpleByte(r) &&
            l->left->op != REGVAR) {
            e->special = SP_CMPHL;
            return;
        }
    }

    /* =type [M addr] [#const] -> ld (hl),n; store constant through HL */
    if (op == ASSIGN && e->left->op == DEREF && e->right->op == AST_CONST) {
        e->special = SP_STCONST;
        return;
    }

    /* =type [+s addr] [#const] -> ld (hl),n; store constant through computed ptr */
    if (op == ASSIGN && e->left->op == PLUS && e->left->size == 2 &&
        e->right->op == AST_CONST) {
        e->special = SP_STCONST;
        return;
    }
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
