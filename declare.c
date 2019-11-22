#include "ccc.h"

/*
 * we are in a parse state where we want to process declarations.
 * any names and types we declare go into the current scope
 */
struct name *
declare(struct type **btp)
{
    struct name *v;
    struct type *t, *prefix, *postfix, *rt;

    /*
     * this will be primitive, enum, struct/union 
     */
    t = getbasetype();
    if (t && *btp) {
        err(ER_T_DT);
        t = 0;
    }
    if (t) {
        *btp = t;
    }
    prefix = *btp;

    while (cur.type == STAR) {
        gettoken();
        prefix = new_type(0, TYPE_DEF, prefix);
        prefix->flags = TF_POINTER;
    }

    if (cur.type == LPAR) {
        gettoken();
        rt = 0;
        v = declare(&rt);       // recurse
        need(RPAR, RPAR, ER_D_DP);
        if (*btp && rt) {
            err(ER_T_DT);
            rt = 0;
        }
        if (rt && !v) {
            *btp = rt;
        }
    }
    if (cur.type == RPAR) {
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

    if (cur.type == SYM) {      // symbol name
        if (v) {
            err(ER_D_MV);
        }
        v = new_name(strdup(strbuf), prefix, SYMBOL);
        gettoken();
        if (cur.type == COLON) {
            gettoken();
            if (cur.type != NUMBER) {
                err(ER_D_BD);
            } else if (cur.v.numeric > MAXBITS) {
                err(ER_D_BM);
            } else {
                v->flags |= V_BITFIELD;
                v->width = cur.v.numeric;
            }
            gettoken();
        }
    }

    while (cur.type == LBRACK) {        // array
        gettoken();
        t = new_type(0, TYPE_DEF, t);
        t->flags = TF_ARRAY;
        if (cur.type == RBRACK) {
            t->count = -1;
            t->flags |= TF_INCOMPLETE;
        } else {
            t->count = parse_const();
        }
        need(RBRACK, RBRACK, ER_D_AD);
    }

#ifdef notdef
    if (cur.type == LPAR) {     // ( <func_arg>[,<func_arg>]*. )
        gettoken();
        if (postfix) {
            err(ER_P_FA);
            postfix = 0;
        }
        postfix = new_type(0, SYMBOL, 0);
        while (cur.type != RPAR) {
            freetype(t);
            t = v;
            a = declare(&t);
            if (a) {
                a->next = postfix->elem;
                postfix->elem = a;
                a->flags |= V_FUNARG | V_LOCAL;
            }
            if (cur.type == COMMA) {
                gettoken();
                continue;
            }
            if (cur.type != RPAR) {
                err(ER_P_FA);
                break;
            }
        }
        gettoken();
        /*
         * old style parameter declarartion:
         * foo(a,b) int a; int b;
         */
        if ((cur.type != BEGIN) && (cur.type != SEMI)) {
            if (t) {
                freetype(t);
            }
            t = 0;
            while (cur.type != BEGIN) {
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
                if (cur.type == COMMA) {
                    gettoken();
                    continue;
                }
                if (cur.type == SEMI) {
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
    }                           // if cur.type == LPAR
#endif

    if ((cur.type != ASSIGN) && (cur.type != BEGIN) &&
        (cur.type != COMMA) && (cur.type != SEMI)) {
        printf("token: %d 0x%x '%c'\n", cur.type, cur.type, cur.type);
        err(ER_D_UT);
        v = 0;
    }
    if (!v) {
        return 0;
    }

    /*
     * prepend the prefix and append the postfix
     */
    t = v->type;
    t = rt;
    while (t && t->sub) {
        t = t->sub;
    }
    t = postfix;
    while (t && t->sub) {
        t = t->sub;
    }
    t = prefix;
    return v;
}                               // declare

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
