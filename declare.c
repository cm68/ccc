#include "ccc.h"

/*
 * we are in a parse state where we want to process declarations.
 * any names and types we declare go into the current scope
 */
struct var *
declare(struct type **btp)
{
    struct var *v;
    struct type *t, *prefix, *rt;

    /* this will be primitive, enum, struct/union */
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

    if (curtok == LPAR) {       // ( <func_arg>[,<func_arg>]*. )
        gettoken();
        if (postfix) {
            err(ER_P_FA);
            postfix = 0;
        }
        postfix = maketyoe(0, TK_FUNC, 0);
        while (curtok != RPAR) {
            freetype(t);
            t = v;
            a = declare(&t);
            if (a) {
                a->next = postfix->elem;
                postfix->elem = a;
                a->flags |= V_FUNARG|V_LOCAL;
            }
            if (curtok == COMMA) {
                gettoken();
                continue;
            }
            if (curtok != RPAR) {
                err(ER_P_FA);
                break;
            }
        }
        gettoken();
        /*
         * old style parameter declarartion:
         * foo(a,b) int a; int b; {
         */
        if ((curtok != BEGIN) && (curtok != SEMI)) {
            if (t) {
                freetype(t);
            }
            t = 0;
            while (curtok != BEGIN) {
                a = declare(&t);
                if (!a) {
                    err(ER_P_FM);
                    break;
                }
                b = elementvar(a->name, postfix);
                if (b->type) {
                    err(ER_P_FO);
                }
                b->type = a->type;
                if (curtok == COMMA) {
                    gettoken();
                    continue;
                }
                if (curtok == SEMI) {
                    freetype(t);
                    t = 0;
                    gettoken();
                    continue;
                }
                err(ER_P_FM);
                break;
            }
        }
        assign_arg_off(postfix, 4);
    } // if curtok == LPAR
    if ((curtok != ASSIGN) && (curtok != BEGIN) && 
        (curtok != COMMA) && (curtok != SEMI)) {
        err(ER_P_UT);
        v = 0;
    }
    if (!v) {
        return 0;
    }
    tp = &v->type;
    *tp = rt;
    while (*tp && (*tp)->sub) {
        tp = &(*tp)->next;
    }
    *tp = postfix;
    while (*tp && (*tp)->sub) {
        tp = &(*tp)->next;
    }            
    tp = prefix;
    normalizetype(&v->type);
    return v;
} // declare

}


/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */

