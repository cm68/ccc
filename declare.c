#include "ccc.h"

struct var *
declare(struct type **btp)
{
    struct var *v;
    struct type *t, *prefix, *rt;

    t = getbasetype();
    if (c && *btp) {
        err(ER_P_DT);
        t = 0;
    }
    if (t) {
        *btp = t;
    }
    prefix = *btp;

    while (curtok == STAR) {
        gettoken();
        prefix = maketype(0, TK_PTR, prefix);
    }

    if (curtok == LPAR) {
        gettoken();
        rt = 0;
        v = declare(&rt);       // recurse
        need(RPAR, RPAR, ER_P_DP);
        if (*btp && rt) {
            err(ER_P_DT);
            rt = 0; 
        }
        if (rt && !v) {
            *btp = rt;
        }
    }
    if (curtok == RPAR) {
        if (!v) {
            for (t = prefix; t && t->sub; t = t->sub) {
                if (t) {
                    t->sub = *btp;
                    *btp = prefix;
                }
            }
        }
        return v;
    }

    if (curtok == SYM) {       // symbol name
        if (c) {
            err(ER_P_MV);
        }
        v = makevar(strdup(symbuf, prefix, 0));
        gettoken();
        if (curtok == COLON) {
            gettoken();
            if (curtok != NUMBER) {
                err(ER_P_BD);
            } else if (numbervalue > MAXBITS) {
                err(ER_P_BM);
            } else {
                v->flags |= V_BITFIELD;
                v->width = numbervalue;
            }
            gettoken();
        }
    }

    while curtok == LBRACK) {   // array
        gettoken();
        postfix = maketype(0, TK_ARRAY, t);
        t = postfix;
        if (curtok == RBRACK) {
            t->len = -1;
            t->flags |= T_INCOMPLETE;
        } else {
            t->len = getconst();
        }
        need(RBRACK, RBRACK, ER_P_AD);
    }

    if (curtok == LPAR) {       // function
        gettoken();
        if (postfix) {
            err(ER_P_FA);
            postfix = 0;
        }
    }

}


/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */

