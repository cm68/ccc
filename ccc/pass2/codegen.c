/*
 * codegen.c - Code generation scheduling
 */
#include <stdio.h>
#include "cc2.h"

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

    if (op == '#') return 1;  /* constant */
    if (op == 'R') return 1;  /* regvar */

    /* Mb $sym - direct global byte */
    if (op == 'M' && e->left->op == '$')
        return 1;

    /* Mb Vb - stack byte via (iy+d) */
    if (op == 'M' && e->left->op == 'V')
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
    if (op == '=' && size <= 2 && e->left->op == '$' && e->right->op == '#') {
        e->special = SP_STCONST;
        return;
    }

    /* =b Rb #const -> ld b,n or ld c,n; direct byte regvar assign */
    if (op == '=' && size == 1 && e->left->op == 'R' &&
        (e->left->aux == R_B || e->left->aux == R_C) && e->right->op == '#') {
        e->special = SP_STCONST;
        return;
    }

    /* = V #const -> ld (iy+n),lo; ld (iy+n+1),hi; store const to local */
    if (op == '=' && e->left->op == 'V' && e->right->op == '#') {
        e->special = SP_STCONST;
        return;
    }

    /* (s Rs reg -> inc reg; pre-inc/dec of regvar, incr <= 4 */
    if ((op == '(' || op == '{') && e->left->op == 'R' && count <= 4) {
        e->special = (op == '(') ? SP_INCR : SP_DECR;
        e->incr = e->aux2;
        e->dest = e->left->aux;     /* R_B, R_C, R_BC, or R_IX */
        return;
    }

    /* (b Vb ofs -> inc (iy+ofs); pre-inc/dec of byte local, incr <= 4 */
    if ((op == '(' || op == '{') && e->left->op == 'V' && size == 1 && count <= 4) {
        e->special = (op == '(') ? SP_INCR : SP_DECR;
        e->incr = count;
        e->offset = e->left->aux2;  /* IY offset from V node */
        e->dest = R_IYO;            /* (iy+ofs) addressing */
        return;
    }

    /* (s $sym -> ld hl,(_sym); inc hl; ld (_sym),hl; inc/dec global word */
    if ((op == '(' || op == '{') && e->left->op == '$' && size == 2 && count <= 4) {
        e->special = SP_INCGLOB;
        e->incr = count;
        e->aux2 = (op == '(') ? 1 : 0;  /* 1=inc, 0=dec */
        e->sym = e->left->sym;          /* steal symbol */
        e->left->sym = 0;
        return;
    }

    /* +s $sym #const -> ld hl,sym+offset */
    if (op == '+' && size == 2 &&
        e->left->op == '$' && e->right->op == '#' && e->left->sym) {
        e->special = SP_SYMOFS;
        e->sym = e->left->sym;          /* steal symbol from child */
        e->left->sym = 0;               /* prevent double-free */
        e->offset = e->right->v.s;
        return;
    }

    /* +s Ms[Rs bc] #const -> ld hl,const; add hl,bc */
    if (op == '+' && size == 2 &&
        e->left->op == 'M' && e->left->left->op == 'R' &&
        e->left->left->aux == R_BC && e->right->op == '#') {
        e->special = SP_ADDBC;
        e->offset = e->right->v.s;
        return;
    }

    /* M[+s $sym #const] -> ld hl,(sym+offset) */
    if (op == 'M' && e->left->op == '+' && e->left->size == 2 &&
        e->left->left->op == '$' && e->left->right->op == '#' &&
        e->left->left->sym) {
        e->special = SP_SYMOFD;
        e->sym = e->left->left->sym;    /* steal symbol from grandchild */
        e->left->left->sym = 0;         /* prevent double-free */
        e->offset = e->left->right->v.s;
        return;
    }

    /* Ms $sym -> ld hl,(sym); direct deref of global */
    if (op == 'M' && e->left->op == '$' && e->left->sym) {
        e->special = SP_MSYM;
        e->sym = e->left->sym;          /* steal symbol */
        e->left->sym = 0;
        return;
    }

    /* *s expr #s 4 -> add hl,hl; multiply by power of 2 */
    if (op == '*' && e->right->op == '#') {
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
    if (op == '&' && size == 1 && e->right->op == '#') {
        long n = e->right->v.l & 0xff;
        /* check power of 2 (single bit) */
        if (n > 0 && (n & (n - 1)) == 0) {
            struct expr *l = e->left;
            /* check (ix+ofs) pattern on left */
            if (l->op == 'M' && l->left->op == '+' && l->left->size == 2 &&
                l->left->left->op == 'M' && l->left->left->size == 2 &&
                l->left->left->left->op == 'R' && l->left->left->left->aux == R_IX &&
                l->left->right->op == '#') {
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
    if ((op == 'Q' || op == 'n') && ISWORD(e->left->type) &&
        e->right->op == '#') {
        long val = e->right->v.l;
        if (val == 0 || val == 1 || val == -1 || val == 0xffff) {
            e->special = SP_CMPEQ;
            e->incr = val;
            return;
        }
    }

    /* cmpB Vb #const -> ld a,const; cp (iy/ix+off) */
    /* cmpB Rb #const -> ld a,reg; cp const */
    if ((op == '<' || op == 'Q' || op == 'n') && size == 1) {
        struct expr *l = e->left, *r = e->right;
        if (l->op == 'V' && r->op == '#') {
            e->special = SP_CMPV;
            e->aux = l->aux;        /* R_IX or R_IY */
            e->offset = l->offset;  /* stack/struct offset */
            e->incr = r->v.c & 0xff; /* constant value */
            return;
        }
        if (l->op == 'R' && r->op == '#') {
            e->special = SP_CMPR;
            e->aux = l->aux;        /* R_B or R_C */
            e->incr = r->v.c & 0xff; /* constant value */
            return;
        }
    }

    /* cmpB with (hl): left is Mb[addr], right is simple (normalized) */
    /* Exclude Mb[Rp] - register pointer needs copy to HL first */
    if ((op == '<' || op == 'Q' || op == 'n') && size == 1) {
        struct expr *l = e->left, *r = e->right;
        if (l->op == 'M' && l->size == 1 && isSimpleByte(r) &&
            l->left->op != 'R') {
            e->special = SP_CMPHL;
            return;
        }
    }

    /* =type [M addr] [#const] -> ld (hl),n; store constant through HL */
    if (op == '=' && e->left->op == 'M' && e->right->op == '#') {
        e->special = SP_STCONST;
        return;
    }

    /* =type [+s addr] [#const] -> ld (hl),n; store constant through computed ptr */
    if (op == '=' && e->left->op == '+' && e->left->size == 2 &&
        e->right->op == '#') {
        e->special = SP_STCONST;
        return;
    }
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
