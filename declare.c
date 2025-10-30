#include "ccc.h"

/*
 * we are in a parse state where we want to process declarations.
 * any names and types we declare go into the current scope
 *
 * struct_elem: if true, this is a struct member and should NOT be added
 *              to the global names[] array (to avoid namespace pollution)
 */
struct name *
declare_internal(struct type **btp, boolean struct_elem)
{
    struct name *nm, *arg;
    struct type *t, *prefix, *suffix, *rt;
    int i;

    suffix = 0;

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
        nm = declare_internal(&rt, struct_elem);       // recurse
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

        if (struct_elem) {
            /* struct members: create name but DON'T add to global names[] array */
            nm = malloc(sizeof(*nm));
            nm->name = strdup(strbuf);
            nm->type = prefix;
            nm->level = lexlevel;
            nm->is_tag = 0;
            nm->kind = elem;  /* will be struct/union member */
            nm->flags = 0;
            nm->offset = 0;
            nm->width = 0;
            nm->bitoff = 0;
            nm->next = 0;
            nm->init = 0;
            nm->body = 0;
            printf("struct_elem: %s (not added to names[])\n", nm->name);
        } else {
            /* normal variable: add to global names[] array */
            nm = new_name(strdup(strbuf), var, prefix, 0);
        }
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
            i = parse_const(RBRACK);
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
            arg = declare_internal(&t, 0);  /* function args are normal names, not struct elems */
            if (arg) {
                arg->next = suffix->elem;
                suffix->elem = arg;
                arg->flags |= V_FUNARG | V_LOCAL;
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
         * old style parameter declaration:
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
    if (!nm) {
        return 0;
    }

    /*
     * prepend the prefix and append the suffix
     */
    t = nm->type;
    t = rt;
    while (t && t->sub) {
        t = t->sub;
    }
    t = suffix;
    while (t && t->sub) {
        t = t->sub;
    }
    t = prefix;
    return nm;
}                               // declare_internal

/*
 * Public wrapper for declare_internal.
 * Normal variables are added to the global names[] array.
 */
struct name *
declare(struct type **btp)
{
    return declare_internal(btp, 0);
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
