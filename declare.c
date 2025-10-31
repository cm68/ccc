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

        // Create a new function type (don't use get_type() which caches types)
        // Function types need unique instances because we modify elem list
        suffix = calloc(1, sizeof(*suffix));
        suffix->flags = TF_FUNC;
        suffix->sub = prefix;  // Return type

        // Detect style: K&R if starts with SYM (not a type keyword), ANSI otherwise
        boolean kr_style = (cur.type == SYM && !is_type_token(cur.type));

        // Parse parameter list
        while (cur.type != RPAR && cur.type != E_O_F) {
            struct type *param_type = NULL;
            char *param_name = NULL;

            if (kr_style) {
                // K&R style: just collect names (types come later)
                if (cur.type == SYM) {
                    param_name = strdup(cur.v.name);
                    gettoken();
                } else {
                    err(ER_D_FA);
                    break;
                }
            } else {
                // ANSI style: parse full type + declarator
                struct type *basetype = getbasetype();
                if (!basetype) {
                    err(ER_D_FA);
                    break;
                }

                // Parse pointer prefix
                param_type = basetype;
                while (cur.type == STAR) {
                    gettoken();
                    param_type = get_type(TF_POINTER, param_type, 0, 0);
                }

                // Get parameter name (optional for ANSI declarations)
                if (cur.type == SYM) {
                    param_name = strdup(cur.v.name);
                    gettoken();
                } else {
                    param_name = strdup("");  // Anonymous parameter
                }

                // Handle array suffix (converts to pointer)
                if (cur.type == LBRACK) {
                    gettoken();
                    if (cur.type != RBRACK) {
                        parse_expr(0, NULL);  // Array size (ignored for parameters)
                    }
                    need(RBRACK, RBRACK, ER_D_FA);
                    param_type = get_type(TF_POINTER, param_type->sub ? param_type->sub : param_type, 0, 0);
                }
            }

            // Create parameter entry
            arg = calloc(1, sizeof(*arg));
            arg->name = param_name;
            arg->type = param_type;  // NULL for K&R (filled in later)
            arg->level = lexlevel + 1;
            arg->is_tag = 0;
            arg->kind = var;
            arg->flags = V_FUNARG | V_LOCAL;
            arg->next = suffix->elem;
            suffix->elem = arg;

            // Handle comma or end of list
            if (cur.type == COMMA) {
                gettoken();
                continue;
            }
            if (cur.type != RPAR) {
                err(ER_D_FA);
                break;
            }
        }

        need(RPAR, RPAR, ER_D_FA);

        // K&R style: parse type declarations after )
        if (kr_style && is_type_token(cur.type)) {
            while (is_type_token(cur.type) && cur.type != E_O_F && cur.type != BEGIN) {
                struct type *basetype = getbasetype();
                if (!basetype) {
                    break;
                }

                // Parse declarator
                struct type *param_type = basetype;
                while (cur.type == STAR) {
                    gettoken();
                    param_type = get_type(TF_POINTER, param_type, 0, 0);
                }

                // Get parameter name
                char *param_name = NULL;
                if (cur.type == SYM) {
                    param_name = strdup(cur.v.name);
                    gettoken();
                } else {
                    err(ER_D_FM);
                    break;
                }

                // Find matching parameter and update its type
                struct name *p;
                for (p = suffix->elem; p; p = p->next) {
                    if (strcmp(p->name, param_name) == 0) {
                        p->type = param_type;
                        break;
                    }
                }
                if (!p) {
                    err(ER_D_FM);  // Parameter declared but not in list
                }
                free(param_name);

                // Continue or stop
                if (cur.type == SEMI) {
                    gettoken();
                    if (cur.type == BEGIN || !is_type_token(cur.type)) {
                        break;
                    }
                } else {
                    break;
                }
            }

            // Default undeclared K&R parameters to int
            for (arg = suffix->elem; arg; arg = arg->next) {
                if (arg->type == NULL) {
                    arg->type = inttype;
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
        // For functions/pointers/arrays, suffix contains the derived type
        // suffix->sub should already point to the return/base type from get_type()
        // Just need to make suffix the final type
        nm->type = suffix;
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
