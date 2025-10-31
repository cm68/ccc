#include "ccc.h"

/*
 * Check if current token is a type keyword
 */
static int
is_type_token(char t)
{
    return (t == CHAR || t == SHORT || t == INT || t == LONG ||
            t == FLOAT || t == DOUBLE || t == VOID || t == UNSIGNED ||
            t == STRUCT || t == UNION || t == ENUM ||
            t == CONST || t == VOLATILE || t == TYPEDEF);
}

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
            nm = calloc(1, sizeof(*nm));  // Zero-initialize all fields
            nm->name = strdup(cur.v.name);
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
            if (VERBOSE(V_SYM)) {
                printf("struct_elem: %s (not added to names[])\n", nm->name);
            }
        } else {
            /* normal variable: add to global names[] array */
            nm = new_name(cur.v.name, var, prefix, 0);
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

        suffix = get_type(TF_FUNC, prefix, 0, 0);

        // Check if this is K&R style (identifier without type) or ANSI style
        int kr_style = 0;
        if (cur.type == SYM) {
            // K&R style: collect parameter names only
            kr_style = 1;
            while (cur.type == SYM && cur.type != E_O_F) {
                // Create parameter with no type yet (will be filled in later)
                arg = calloc(1, sizeof(*arg));  // Zero-initialize all fields
                arg->name = strdup(cur.v.name);
                arg->type = 0;  // Type will be set later from declarations
                arg->level = lexlevel + 1;
                arg->is_tag = 0;
                arg->kind = var;
                arg->flags = V_FUNARG | V_LOCAL;
                arg->next = suffix->elem;
                suffix->elem = arg;

                gettoken();

                if (cur.type == COMMA) {
                    gettoken();
                    continue;
                }
                if (cur.type == RPAR) {
                    break;
                }
            }
        } else {
            // ANSI style: parse typed parameters
            while (cur.type != RPAR && cur.type != E_O_F) {
                struct type *param_type = 0;  // Local variable for each parameter type
                arg = declare_internal(&param_type, 0);  /* function args are normal names, not struct elems */
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
                    err(ER_D_FA);
                    break;
                }
            }
        }

        need(RPAR, RPAR, ER_D_FA);

        // If K&R style, parse the parameter type declarations after )
        // Stop when we hit the function body { or run out of declarations
        if (kr_style && is_type_token(cur.type)) {
            while (is_type_token(cur.type) && cur.type != E_O_F && cur.type != BEGIN) {
                struct type *paramtype = 0;
                struct name *paramdecl = declare_internal(&paramtype, 0);

                if (paramdecl) {
                    // Find the matching parameter in suffix->elem and update its type
                    struct name *p;
                    for (p = suffix->elem; p; p = p->next) {
                        if (strcmp(p->name, paramdecl->name) == 0) {
                            p->type = paramdecl->type;
                            break;
                        }
                    }
                    if (!p) {
                        err(ER_D_FM);  // old style arg defs error - parameter declared but not in list
                    }
                    // paramdecl is in names[] array and will be cleaned up by pop_scope
                    // Don't free it here
                }

                // Check if we should continue or stop
                if (cur.type == SEMI) {
                    gettoken();
                    // Stop if next token is BEGIN (function body) or not a type keyword
                    if (cur.type == BEGIN || !is_type_token(cur.type)) {
                        break;
                    }
                } else {
                    break;
                }
            }

            // Default any undeclared K&R parameters to int
            struct name *p;
            for (p = suffix->elem; p; p = p->next) {
                if (p->type == 0) {
                    p->type = inttype;
                }
            }
        }
    }                           // if cur.type == LPAR

    if ((cur.type != ASSIGN) && (cur.type != BEGIN) &&
        (cur.type != COMMA) && (cur.type != SEMI) && (cur.type != RPAR)) {
        err(ER_D_UT);
        nm = 0;
    }
    if (!nm) {
        return 0;
    }

    /*
     * Handle function types: connect suffix (function type) to nm
     * Note: The original type assembly code here was corrupted during
     * retyping from paper printout and caused infinite loops.
     */
    if (suffix) {
        // For functions, suffix contains the function type
        // Connect suffix->sub to the return type (prefix or nm->type)
        if (nm->type && !suffix->sub) {
            suffix->sub = nm->type;
            nm->type = suffix;
        }
    }

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
