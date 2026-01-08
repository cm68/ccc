/*
 * codegen.c - Code generation scheduling
 */
#include <stdio.h>
#include "cc2.h"
#include "match.h"
#include "../cpp/lexeme.h"

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

    /* DEREF[SYM] - direct global byte */
    if (op == DEREF && e->left->op == SYM)
        return 1;

    /* DEREF[LOCALVAR] - stack byte via (iy+d) */
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
 * Uses bytecode pattern matcher for structural matching,
 * with post-match handlers for complex cases.
 */
void
annotate(struct expr *e)
{
    unsigned char op;
    int result;

    if (!e) return;
    op = e->op;
    if (!op) return;

    /* Recursively annotate children first */
    annotate(e->left);
    annotate(e->right);

    /* Try bytecode pattern matcher */
    result = exprMatch(e, exprPatterns);
    if (result) {
        /* Apply common field copying from action table */
        applyAction(e, result);

        /* Handle result-specific post-processing */
        switch (result) {
        case SP_STCONST:
            /* No extra processing needed */
            break;

        case SP_INCR:
            /* Pattern matches both PREINC and PREDEC */
            if (op == PREDEC)
                e->special = SP_DECR;
            /* LOCALVAR variant needs different dest handling */
            if (e->left->op == LOCALVAR) {
                e->offset = e->left->aux2;  /* IY offset */
                e->dest = R_IYO;
            }
            break;

        case SP_INCGLOB:
            /* Need to set aux2 for inc/dec flag */
            e->aux2 = (op == PREINC) ? 1 : 0;
            break;

        case SP_MUL2:
            /* Compute shift count from power of 2 */
            {
                long n = e->right->v.l;
                unsigned char shifts = 0;
                while (n > 1) { shifts++; n >>= 1; }
                e->incr = shifts;
            }
            break;

        case SP_CMPEQ:
            /* Verify value is 0, 1, or -1, else reject match */
            {
                long val = e->right->v.l;
                if (val == 0 || val == 1 || val == -1 || val == 0xffff) {
                    e->incr = val;
                } else {
                    e->special = SP_NONE;  /* reject */
                }
            }
            break;

        case SP_CMPV:
            /* Copy fields from LOCALVAR child */
            e->aux = e->left->aux;          /* R_IX or R_IY */
            e->offset = e->left->offset;    /* stack offset */
            e->incr = e->right->v.c & 0xff; /* constant */
            break;

        case SP_CMPR:
            /* Copy fields from REGVAR child */
            e->aux = e->left->aux;          /* R_B or R_C */
            e->incr = e->right->v.c & 0xff; /* constant */
            break;

        case SP_CMPHL:
            /* Verify isSimpleByte(right), left->left->op != REGVAR, and
             * left DEREF doesn't have its own special (SP_MSYM/SP_SYMOFD) */
            if (!isSimpleByte(e->right) || e->left->left->op == REGVAR ||
                e->left->special)
                e->special = SP_NONE;  /* reject */
            break;
        }
        if (e->special)
            return;
    }

    /* Complex patterns not handled by bytecode matcher */

    /* AND DEREF[PLUS DEREF[REGVAR(IX)] AST_CONST] AST_CONST -> bit test */
    if (op == AND && e->size == 1 && e->right->op == AST_CONST) {
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
}

/*
 * vim: tabstop=4 shiftwidth=4 noexpandtab:
 */
