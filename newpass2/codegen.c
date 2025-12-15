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
    if (!e) return 0;
    if (e->op == '#' && ISBYTE(e->type)) return 1;  /* constant */
    if (e->op == 'R' && ISBYTE(e->type)) return 1;  /* regvar */
    /* Mb $sym - direct global byte */
    if (e->op == 'M' && ISBYTE(e->type) && e->left->op == '$')
        return 1;
    /* Mb Vb - stack byte via (iy+d) */
    if (e->op == 'M' && ISBYTE(e->type) && e->left->op == 'V')
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
 * Calculate temporary demand for expression tree
 * Binop: sum of children, Unop/primary: 1
 * Returns demand, also stores in e->demand
 */
unsigned char
calcDemand(struct expr *e)
{
    unsigned char demand;
    unsigned char op = e->op;

    unsigned char size = TSIZE(e->type);
    unsigned char count = e->aux2;

    if (!op) return 0;  /* sentinel */

    /* special: =[BS] $var - assign to byte/short var is demand 1 */
    /* if right is constant, use SP_STCONST for efficient store */
    if (op == '=' && size <= 2 && e->left->op == '$') {
        if (e->right->op == '#') {
            e->special = SP_STCONST;
        }
        calcDemand(e->right);  /* still calc children for display */
        demand = 1;
        goto done;
    }

    /* =b Rb #const -> ld b,n or ld c,n; direct byte regvar assign */
    if (op == '=' && ISBYTE(e->type) && e->left->op == 'R' &&
        (e->left->aux == R_B || e->left->aux == R_C) && e->right->op == '#') {
        e->special = SP_STCONST;
        demand = 0;
        goto done;
    }

    /* (s Rs reg -> inc reg; pre-inc/dec of regvar, incr <= 4 */
    if ((op == '(' || op == '{') && e->left->op == 'R' && count <= 4) {
        e->special = (op == '(') ? SP_INCR : SP_DECR;
        e->incr = e->aux2;
        e->dest = e->left->aux;     /* R_B, R_C, R_BC, or R_IX */
        demand = 0;
        goto done;
    }

    /* )s Rs reg -> dec reg; post-inc/dec of regvar when unused, incr <= 4 */
    if ((op == ')' || op == '}') && e->left->op == 'R' && count <= 4 && e->unused) {
        e->incr = e->aux2;
        e->dest = e->left->aux;     /* R_B, R_C, R_BC, or R_IX */
        demand = 0;
        goto done;
    }

    /* )s Vs ofs -> post-inc/dec of word local, incr <= 4 */
    if ((op == ')' || op == '}') && e->left->op == 'V' && size == 2 && count <= 4) {
        calcDemand(e->left);  /* calc child demand */
        demand = e->unused ? 1 : 2;  /* need 2 if returning old value */
        goto done;
    }

    /* (b Vb ofs -> inc (iy+ofs); pre-inc/dec of byte local, incr <= 4 */
    if ((op == '(' || op == '{') && e->left->op == 'V' && size == 1 && count <= 4) {
        e->special = (op == '(') ? SP_INCR : SP_DECR;
        e->incr = count;
        e->offset = e->left->aux2;  /* IY offset from V node */
        e->dest = R_IYO;            /* (iy+ofs) addressing */
        demand = 0;
        goto done;
    }

    /* (s $sym -> ld hl,(_sym); inc hl; ld (_sym),hl; inc/dec global word */
    if ((op == '(' || op == '{') && e->left->op == '$' && size == 2 && count <= 4) {
        e->special = SP_INCGLOB;
        e->incr = count;
        e->aux2 = (op == '(') ? 1 : 0;  /* 1=inc, 0=dec */
        e->sym = e->left->sym;          /* steal symbol */
        e->left->sym = 0;
        demand = 0;
        goto done;
    }

    /* +p $sym #const -> ld hl,sym+offset */
    if (op == '+' && e->type == T_PTR &&
        e->left->op == '$' && e->right->op == '#' && e->left->sym) {
        e->special = SP_SYMOFS;
        e->sym = e->left->sym;          /* steal symbol from child */
        e->left->sym = 0;               /* prevent double-free */
        e->offset = e->right->v.s;
        demand = 1;
        goto done;
    }

    /* +p Mp[Rp bc] #const -> ld hl,const; add hl,bc */
    if (op == '+' && e->type == T_PTR &&
        e->left->op == 'M' && e->left->left->op == 'R' &&
        e->left->left->aux == R_BC && e->right->op == '#') {
        e->special = SP_ADDBC;
        e->offset = e->right->v.s;
        demand = 1;
        goto done;
    }

    /* M[+p Mp[Rp(ix)] #ofs] -> (ix+ofs); IX-relative deref */
    if (op == 'M' && e->left->op == '+' && e->left->type == T_PTR &&
        e->left->left->op == 'M' && e->left->left->type == T_PTR &&
        e->left->left->left->op == 'R' && e->left->left->left->aux == R_IX &&
        e->left->right->op == '#') {
        e->special = SP_IXOD;
        e->offset = e->left->right->v.s;
        e->dest = R_IXO;
        demand = 0;
        goto done;
    }

    /* M[+p $sym #const] -> ld hl,(sym+offset) */
    if (op == 'M' && e->left->op == '+' && e->left->type == T_PTR &&
        e->left->left->op == '$' && e->left->right->op == '#' &&
        e->left->left->sym) {
        e->special = SP_SYMOFD;
        e->sym = e->left->left->sym;    /* steal symbol from grandchild */
        e->left->left->sym = 0;         /* prevent double-free */
        e->offset = e->left->right->v.s;
        demand = 1;
        goto done;
    }

    /* Ms $sym -> ld hl,(sym); direct deref of global */
    if (op == 'M' && e->left->op == '$' && e->left->sym) {
        e->special = SP_MSYM;
        e->sym = e->left->sym;          /* steal symbol */
        e->left->sym = 0;
        demand = 1;
        goto done;
    }

    /* N #const -> just use constant directly as byte */
    if (op == 'N' && e->left->op == '#') {
        e->left->type = e->type;  /* change child to byte type */
        e->left->v.c = e->left->v.l & 0xff;
        e->left->demand = 1;      /* child needs to emit */
        demand = 1;
        goto done;
    }

    /* General M: need at least 1 for result in HL */
    if (op == 'M') {
        int d = calcDemand(e->left);
        demand = d > 0 ? d : 1;
        goto done;
    }

    /* *s expr #s 4 -> add hl,hl; multiply by power of 2 */
    if (op == '*' && e->right->op == '#') {
        long n = e->right->v.l;
        if (n > 0 && (n & (n - 1)) == 0) {
            unsigned char shifts = 0;
            while (n > 1) { shifts++; n >>= 1; }
            e->special = SP_MUL2;
            e->incr = shifts;
            demand = calcDemand(e->left);
            goto done;
        }
    }

    /* &B M[(ix+ofs)] #pow2 -> bit n,(ix+ofs); bit test */
    if (op == '&' && ISBYTE(e->type) && e->right->op == '#') {
        long n = e->right->v.l & 0xff;
        /* check power of 2 (single bit) */
        if (n > 0 && (n & (n - 1)) == 0) {
            struct expr *l = e->left;
            /* check (ix+ofs) pattern on left */
            if (l->op == 'M' && l->left->op == '+' && l->left->type == T_PTR &&
                l->left->left->op == 'M' && l->left->left->type == T_PTR &&
                l->left->left->left->op == 'R' && l->left->left->left->aux == R_IX &&
                l->left->right->op == '#') {
                unsigned char bit = 0;
                while (n > 1) { bit++; n >>= 1; }
                e->special = SP_BITTEST;
                e->offset = l->left->right->v.s;  /* ix offset */
                e->incr = bit;                     /* bit number 0-7 */
                demand = 0;
                goto done;
            }
        }
    }

    /* gB Ms $sym #B 0 -> bit 7,(sym+ofs); sign test x >= 0 */
    if (op == 'g' && e->left->op == 'M' && e->left->left->op == '$' &&
        e->right->op == '#' && e->right->v.l == 0 && e->left->left->sym) {
        e->special = SP_SIGN;
        e->sym = e->left->left->sym;    /* steal symbol */
        e->left->left->sym = 0;
        e->offset = TSIZE(e->left->type) - 1;  /* offset to high byte */
        demand = 1;
        goto done;
    }

    /* gB Ms[Rs bc] #0 -> bit 7,b; sign test regvar >= 0 */
    if (op == 'g' && e->left->op == 'M' && e->left->left->op == 'R' &&
        e->left->left->aux == R_BC && e->right->op == '#' && e->right->v.l == 0) {
        e->special = SP_SIGNREG;
        e->dest = e->left->left->aux;  /* R_BC */
        demand = 0;
        goto done;
    }

    /* cmpB left M[+p Mp[Rp(ix)] #ofs] -> cp (ix+d); byte cmp with (ix+d) */
    if ((op == '<' || op == '>' || op == 'Q' || op == 'n' ||
         op == 'L' || op == 'g') && ISBYTE(e->type)) {
        struct expr *l = e->left, *r = e->right;
        /* check if right is the (ix+d) pattern */
        if (r->op == 'M' && r->left->op == '+' && r->left->type == T_PTR &&
            r->left->left->op == 'M' && r->left->left->type == T_PTR &&
            r->left->left->left->op == 'R' && r->left->left->left->aux == R_IX &&
            r->left->right->op == '#') {
            e->special = SP_CMPIX;
            e->offset = r->left->right->v.s;
            e->aux2 = 0;  /* right is (ix+d), normal sense */
            demand = calcDemand(l);  /* only need to emit left */
            goto done;
        }
        /* check if left is the (ix+d) pattern */
        if (l->op == 'M' && l->left->op == '+' && l->left->type == T_PTR &&
            l->left->left->op == 'M' && l->left->left->type == T_PTR &&
            l->left->left->left->op == 'R' && l->left->left->left->aux == R_IX &&
            l->left->right->op == '#') {
            e->special = SP_CMPIX;
            e->offset = l->left->right->v.s;
            e->aux2 = 1;  /* left is (ix+d), flipped sense */
            demand = calcDemand(r);  /* only need to emit right */
            goto done;
        }
        /* check if right is (iy+d) pattern */
        if (r->op == 'M' && r->left->op == '+' && r->left->type == T_PTR &&
            r->left->left->op == 'M' && r->left->left->type == T_PTR &&
            r->left->left->left->op == 'R' && r->left->left->left->aux == R_IY &&
            r->left->right->op == '#') {
            e->special = SP_CMPIY;
            e->offset = r->left->right->v.s;
            e->aux2 = 0;
            demand = calcDemand(l);
            goto done;
        }
        /* check if left is (iy+d) pattern */
        if (l->op == 'M' && l->left->op == '+' && l->left->type == T_PTR &&
            l->left->left->op == 'M' && l->left->left->type == T_PTR &&
            l->left->left->left->op == 'R' && l->left->left->left->aux == R_IY &&
            l->left->right->op == '#') {
            e->special = SP_CMPIY;
            e->offset = l->left->right->v.s;
            e->aux2 = 1;
            demand = calcDemand(r);
            goto done;
        }
        /* cmpB with (hl): one operand is Mb[addr], other is simple */
        /* Exclude Mb[Rp] - register pointer needs copy to HL first */
        if (r->op == 'M' && ISBYTE(r->type) && isSimpleByte(l) &&
            r->left->op != 'R') {
            e->special = SP_CMPHL;
            e->aux2 = 0;  /* right needs (hl) */
            demand = calcDemand(r->left) + 1;  /* addr to HL, simple to A */
            goto done;
        }
        if (l->op == 'M' && ISBYTE(l->type) && isSimpleByte(r) &&
            l->left->op != 'R') {
            e->special = SP_CMPHL;
            e->aux2 = 1;  /* left needs (hl) */
            demand = calcDemand(l->left) + 1;
            goto done;
        }
    }

    /* R in IX (reg=4) or BC (reg=3) is demand 0 - already in register */
    if (op == 'R' && (e->aux == 3 || e->aux == 4)) {
        demand = 0;
        goto done;
    }

    if (op == '#' || op == '$' || op == 'R' || op == 'V') {
        demand = 1;
        goto done;
    }

    /* function call: max of all arg demands */
    /* direct call ($funcname) has demand 0 for the func ref */
    /* Args are wrapped in 'A' nodes; actual arg is in wrapper->left */
    if (op == '@') {
        struct expr *wrapper;
        unsigned char d;
        demand = (e->left->op == '$') ? 0 : calcDemand(e->left);
        for (wrapper = e->right; wrapper && wrapper->op == 'A'; wrapper = wrapper->right) {
            d = calcDemand(wrapper->left);  /* unwrap actual arg */
            if (d > demand) demand = d;
        }
        goto done;
    }

    /* =type [M addr] [#const] -> ld (hl),n; store constant through HL */
    if (op == '=' && e->left->op == 'M' && e->right->op == '#') {
        e->special = SP_STCONST;
        demand = calcDemand(e->left->left);  /* only need to compute address */
        goto done;
    }

    /* = [+p #const Rp(ix/iy)] expr -> ld (ix+ofs),src or ld (iy+ofs),src */
    if (op == '=' && (ISBYTE(e->type) || ISWORD(e->type)) && e->left->op == '+' && e->left->type == T_PTR) {
        struct expr *l = e->left->left, *r = e->left->right;
        int ofs = 0, reg = 0;
        if (l->op == '#' && r->op == 'R' && (r->aux == R_IX || r->aux == R_IY)) {
            ofs = l->v.s; reg = r->aux;
        } else if (r->op == '#' && l->op == 'R' && (l->aux == R_IX || l->aux == R_IY)) {
            ofs = r->v.s; reg = l->aux;
        }
        if (reg) {
            e->special = SP_STIX;
            e->offset = ofs;
            e->aux = reg;  /* R_IX or R_IY */
            demand = calcDemand(e->right);
            if (demand < 1 && e->right->op != '#') demand = 1;
            goto done;
        }
    }

    /* =type [+p addr] [#const] -> ld (hl),n; store constant through computed ptr */
    if (op == '=' && e->left->op == '+' && e->left->type == T_PTR &&
        e->right->op == '#') {
        e->special = SP_STCONST;
        demand = calcDemand(e->left);  /* compute pointer address */
        goto done;
    }

    /* comma: type indicates value child; ensure it has demand >= 1 */
    if (op == ',') {
        int ld = calcDemand(e->left);
        int rd = calcDemand(e->right);
        if (e->type == e->left->type) {
            /* left is value */
            if (ld < 1) ld = 1;
            e->left->demand = ld;
        } else {
            /* right is value */
            if (rd < 1) rd = 1;
            e->right->demand = rd;
        }
        demand = ld > rd + 1 ? ld : rd + 1;
        goto done;
    }

    /* Left-to-right: need left's temps, then 1 to hold result + right's temps */
    {
        int ld = calcDemand(e->left);
        int rd = calcDemand(e->right);
        demand = ld > rd + 1 ? ld : rd + 1;
        /* Set spill if right child needs DE (demand > 1) */
        if (rd > 1)
            e->spill = 1;
    }

done:
    e->demand  = demand;
    return demand;
}

/*
 * Assign destination registers to expression nodes
 * dest: 'H'=HL, 'D'=DE, 'A'=A, 'E'=E
 * Only nodes with demand > 0 get a destination
 */
void
assignDest(struct expr *e, char dest)
{
    char op;

    if (!e || !e->op) return;

    op = e->op;

    /* no destination for demand 0 nodes, except TOS still needs push */
    /* Also don't return early for calls - args still need TOS assignment */
    /* SP_INCR/SP_DECR have demand 0 but dest was set by calcDemand */
    if (e->demand == 0 && dest != R_TOS && op != '@') {
        if (e->special != SP_INCR && e->special != SP_DECR)
            e->dest = 0;
        return;
    }

    /* unused nodes don't need a destination, but children still do */
    if (e->unused)
        e->dest = 0;
    else
        e->dest = dest;

    /* SP_CMPIX/IY: only non-indexed operand needs A */
    if (e->special == SP_CMPIX || e->special == SP_CMPIY) {
        if (e->aux2 == 0)
            assignDest(e->left, R_A);   /* right is indexed, emit left */
        else
            assignDest(e->right, R_A);  /* left is indexed, emit right */
        return;
    }

    /* SP_STIX: value to A (byte) or HL (word), no address computation needed */
    if (e->special == SP_STIX) {
        if (e->right->op != '#')
            assignDest(e->right, ISBYTE(e->type) ? R_A : R_HL);
        return;
    }

    /* SP_STCONST: address to HL, no dest for constant */
    if (e->special == SP_STCONST) {
        if (e->left->op == 'M')
            assignDest(e->left->left, R_HL);  /* addr of ptr to HL */
        else if (e->left->op == '$')
            assignDest(e->left, R_HL);  /* global addr to HL */
        else
            assignDest(e->left, R_HL);  /* computed address to HL */
        return;
    }

    /* SP_CMPHL: complex addr to HL, simple operand to A */
    if (e->special == SP_CMPHL) {
        if (e->aux2 == 0) {
            /* right needs (hl), left is simple */
            assignDest(e->right->left, R_HL);  /* addr of right to HL */
            assignDest(e->left, R_A);          /* left to A */
        } else {
            /* left needs (hl), right is simple */
            assignDest(e->left->left, R_HL);   /* addr of left to HL */
            assignDest(e->right, R_A);         /* right to A */
        }
        return;
    }

    /* determine child destinations based on operator */
    if (op == 'Q' || op == 'n' || op == '<' || op == '>' ||
        op == 'L' || op == 'g') {
        /* comparison: for byte-byte, left->A, right->HL (compare A with L) */
        /* for word-word: both to HL (deeper first, save to DE) */
        unsigned char ltype = e->left->type;
        unsigned char ldest = ISBYTE(ltype) ? R_A : R_HL;
        /* right always goes to HL - for byte, we use L; for word, we save left to DE first */
        if (treeDepth(e->left) >= treeDepth(e->right)) {
            assignDest(e->left, ldest);
            assignDest(e->right, R_HL);
        } else {
            assignDest(e->right, R_HL);
            assignDest(e->left, ldest);
        }
    } else if (op == '-' || op == '/' || op == '%') {
        /* non-commutative: left must be in HL for sbc/div/mod */
        unsigned char hlDest = ISBYTE(e->type) ? R_A : R_HL;
        assignDest(e->left, hlDest);
        assignDest(e->right, R_DE);
    } else if (op == '*') {
        /* multiply register assignment */
        /* Treat small constants (0-255) as bytes for efficiency */
        unsigned char lbyte = ISBYTE(e->left->type) ||
                    (e->left->op == '#' && (e->left->v.l & ~0xff) == 0);
        unsigned char rbyte = ISBYTE(e->right->type) ||
                    (e->right->op == '#' && (e->right->v.l & ~0xff) == 0);
        if (lbyte && rbyte) {
            /* byte × byte -> word: left to A, right to E, result in HL */
            assignDest(e->left, R_A);
            assignDest(e->right, R_DE);  /* will use E */
        } else if (lbyte) {
            /* byte × word: left (byte) to A, right (word) to HL */
            assignDest(e->left, R_A);
            assignDest(e->right, R_HL);
        } else if (rbyte) {
            /* word × byte: left (word) to HL, right (byte) to A */
            assignDest(e->left, R_HL);
            assignDest(e->right, R_A);
        } else {
            /* word × word: use _imul, left->DE, right->HL */
            assignDest(e->left, R_DE);
            assignDest(e->right, R_HL);
        }
    } else if (op == '+' || op == '&' || op == '|' || op == '^') {
        /* commutative binop: higher demand child gets HL, emitted first */
        unsigned char hlDest = ISBYTE(e->type) ? R_A : R_HL;
        unsigned char ld = e->left->demand, rd = e->right->demand;
        if (ld >= rd) {
            assignDest(e->left, hlDest);
            assignDest(e->right, R_DE);
        } else {
            assignDest(e->right, hlDest);
            assignDest(e->left, R_DE);
        }
    } else if (op == '=' || op == 'y' || op == 'w' || op == 'z') {  /* assign, shifts */
        /* non-commutative: left gets HL, right gets DE */
        unsigned char hlDest = ISBYTE(e->type) ? R_A : R_HL;
        assignDest(e->left, hlDest);
        assignDest(e->right, R_DE);
    } else if (op == 'R') {
        /* regvar: byte regvar result goes to A */
        if (ISBYTE(e->type))
            e->dest = R_A;
    } else if (op == 'M') {
        /* deref: result goes to A for byte, HL for word - override parent's dest */
        if (ISBYTE(e->type))
            e->dest = R_A;
        /* child produces address in HL */
        /* For special cases (SP_IXOD, SP_MSYM, SP_SYMOFD), child isn't emitted */
        if (e->special == SP_IXOD || e->special == SP_MSYM || e->special == SP_SYMOFD)
            ;  /* no child assignment for specials */
        else
            assignDest(e->left, R_HL);  /* child computes address to HL */
    } else if (op == 'N') {
        /* narrow: if child is constant, pass dest through; else child to HL */
        if (e->left->op == '#')
            assignDest(e->left, dest);
        else
            assignDest(e->left, R_HL);
    } else if (op == 'W' || op == 'x') {  /* widen/sext: byte to word */
        /* child produces byte in A; widen puts word in HL (and pushes if TOS) */
        assignDest(e->left, R_A);
    } else if (op == '~' || op == '!' || op == '_' || op == '\\') {  /* unary ops */
        /* unop: child to same dest */
        assignDest(e->left, dest);
    } else if (op == '(' || op == '{' || op == ')' || op == '}') {  /* inc/dec */
        /* inc/dec: child is lvalue (address), always needs HL */
        assignDest(e->left, R_HL);
    } else if (op == '@') {
        /* call: args go to TOS (stack) */
        /* Args are wrapped in 'A' nodes; actual arg is in wrapper->left */
        struct expr *wrapper;
        for (wrapper = e->right; wrapper && wrapper->op == 'A'; wrapper = wrapper->right) {
            assignDest(wrapper->left, R_TOS);  /* unwrap actual arg */
        }
        /* indirect call needs func ptr in HL */
        if (e->left->op != '$') {
            assignDest(e->left, R_HL);
        }
    } else if (op == '?') {
        /* ternary: both branches to same dest */
        assignDest(e->left, dest);  /* condition */
        if (e->right) {
            assignDest(e->right->left, dest);   /* then */
            assignDest(e->right->right, dest);  /* else */
        }
    } else if (op == ',') {
        /* comma: left is value, right is side effect (AST format) */
        assignDest(e->right, 0);   /* right is side effect */
        assignDest(e->left, dest);
    } else if (op == 'j' || op == 'h') {
        /* logical and/or: each child based on its type */
        unsigned char ldest = ISBYTE(e->left->type) ? R_A : R_HL;
        unsigned char rdest = ISBYTE(e->right->type) ? R_A : R_HL;
        assignDest(e->left, ldest);
        assignDest(e->right, rdest);
    }
}
