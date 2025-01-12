#include "ccc.h"

/*
 * we are in a parse state where we want to process declarations.
 * any names and types we declare go into the current scope
 */
struct name *
declare(struct type **btp)
{
    struct name *nm, *arg;
    struct type *t, *prefix, *suffix, *rt;
    int i;

    nm = 0;

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
        prefix = get_type(TF_POINTER, prefix, 0, 0);
    }

    // parenthesed type definition does precedence
    if (cur.type == LPAR) {
        gettoken();
        rt = 0;
        nm = declare(&rt);       // recurse
        need(RPAR, RPAR, ER_D_DP);
        if (*btp && rt) {
            err(ER_T_DT);
            rt = 0;
        }
        if (rt && !nm) {
            *btp = rt;
        }
    }
    if (cur.type == RPAR) {
        if (!nm) {
            for (t = prefix; t && t->sub; t = t->sub) {
                if (t) {
                    t->sub = *btp;
                    *btp = prefix;
                }
            }
        }
        return nm;
    }
    if (cur.type == SYM) {      // symbol name
        if (nm) {
            err(ER_D_MV);
        }
        nm = new_name(strdup(strbuf), var, prefix, 0);
        gettoken();
        
        if (cur.type == COLON) {    // check for bitfield
            gettoken();
            if (cur.type != NUMBER) {
                err(ER_D_BD);
            } else if (cur.v.numeric > MAXBITS) {
                err(ER_D_BM);
            } else {
                nm->flags |= V_BITFIELD;
                nm->width = cur.v.numeric;
            }
            gettoken();
        }
    }

    while (cur.type == LBRACK) {        // array
        gettoken();
        if (cur.type == RBRACK) {
            i = -1;
        } else {
            i = parse_const();
        }
        t = get_type(TF_ARRAY, t, 0, i);
        need(RBRACK, RBRACK, ER_D_AD);
    }

    if (cur.type == LPAR) {     // ( <func_arg>[,<func_arg>]*. )
        gettoken();
        if (suffix) {
            err(ER_D_FA);
            suffix = 0;
        }

        suffix = get_type(0, func, 0);
        while (cur.type != RPAR) {
#ifdef notdef
            freetype(t);
            t = v;
#endif
            a = declare(&t);
            if (a) {
                a->next = suffix->elem;
                suffix->elem = a;
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
#ifdef notdef
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
                b = elementvar(a->name, suffix);
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
        assign_arg_off(suffix, 4);
#endif
    }                           // if cur.type == LPAR

    if ((cur.type != ASSIGN) && (cur.type != BEGIN) &&
        (cur.type != COMMA) && (cur.type != SEMI)) {
        printf("token: %d 0x%x '%c'\n", cur.type, cur.type, cur.type);
        err(ER_D_UT);
        nm = 0;
    }
    if (!v) {
        return 0;
    }

    /*
     * prepend the prefix and append the suffix
     */
    t = v->type;
    t = rt;
    while (t && t->sub) {
        t = t->sub;
    }
    t = suffix;
    while (t && t->sub) {
        t = t->sub;
    }
    t = prefix;
    return v;
}                               // declare

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
