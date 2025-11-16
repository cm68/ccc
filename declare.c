#include "cc1.h"

/*
 * Check if current token is a type keyword
 */
static int
is_type_token(unsigned char t)
{
    return (t == CHAR || t == SHORT || t == INT || t == LONG ||
            t == FLOAT || t == DOUBLE || t == VOID || t == UNSIGNED ||
            t == STRUCT || t == UNION || t == ENUM ||
            t == CONST || t == VOLATILE || t == TYPEDEF);
}

/*
 * Parse pointer prefix (zero or more '*' tokens)
 * Returns a pointer type wrapping the given base type
 */
static struct type *
parse_pointer_prefix(struct type *basetype)
{
    struct type *t = basetype;
    while (cur.type == STAR) {
        gettoken();
        // Skip const/volatile qualifiers after * (e.g., char *const)
        while (cur.type == CONST || cur.type == VOLATILE) {
            gettoken();
        }
        t = get_type(TF_POINTER, t, 0);
    }
    return t;
}

/*
 * Parse optional parameter name into provided buffer
 * Returns pointer to buffer (or "" for anonymous) or NULL if error
 */
static char *
parse_param_name(boolean allow_anonymous, char *buf, int bufsize)
{
    if (cur.type == SYM) {
        strncpy(buf, cur.v.name, bufsize - 1);
        buf[bufsize - 1] = '\0';
        gettoken();
        return buf;
    }
    if (allow_anonymous) {
        buf[0] = '\0';
        return buf;
    }
    return NULL;
}

/*
 * Create a new function parameter name entry
 */
static struct name *
create_param_entry(char *name, struct type *type)
{
    struct name *arg = calloc(1, sizeof(*arg));
    // Always strdup the name to avoid dangling pointers
    arg->name = name ? strdup(name) : strdup("");
    arg->type = type;
    arg->level = lexlevel + 1;
    arg->is_tag = 0;
    arg->kind = funarg;
    return arg;
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
        gripe(ER_T_DT);
        t = 0;
    }
    if (t) {
        *btp = t;
    }
    prefix = *btp;

    prefix = parse_pointer_prefix(prefix);

    // parenthesed type definition does precedence
    if (cur.type == LPAR) {
        gettoken();
        rt = 0;
        nm = declare_internal(&rt, struct_elem);       // recurse
        expect(RPAR, ER_D_DP);
        if (*btp && rt) {
            gripe(ER_T_DT);
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
            gripe(ER_D_MV);
        }

        if (struct_elem) {
            /*
             * struct members: create name but DON'T add to
             * global names[] array
             */
            nm = calloc(1, sizeof(*nm));  // Zero-initialize all fields
            nm->name = strdup(cur.v.name);
            nm->type = prefix;
            nm->level = lexlevel;
            nm->is_tag = 0;
            nm->kind = elem;  /* will be struct/union member */
            nm->offset = 0;
            nm->width = 0;
            nm->bitoff = 0;
            nm->next = 0;
            nm->u.init = 0;
            nm->u.body = 0;
#ifdef DEBUG
            if (VERBOSE(V_SYM)) {
                fdprintf(2, "struct_elem: %s (not added to names[])\n",
                         nm->name);
            }
#endif
        } else {
            /* normal variable: add to global names[] array */
            /* Check if this name already exists at this scope */
            struct name *existing = lookup_name(cur.v.name, 0);
            if (existing && existing->level == lexlevel) {
                /*
                 * Name exists at current scope - check if it's a
                 * function prototype
                 */
                if (existing->type && (existing->type->flags & TF_FUNC) &&
                    !existing->u.body) {
                    /* Reuse existing function declaration (prototype) */
                    nm = existing;
                    /*
                     * Update type to the new one (definition may have
                     * full param list)
                     */
                    /* But keep the existing name structure */
                } else {
                    /* Not a function prototype - error on redeclaration */
                    nm = new_name(cur.v.name, var, prefix, 0);
                }
            } else {
                /* New name - create it */
                nm = new_name(cur.v.name, var, prefix, 0);
            }
        }
        gettoken();

        if (cur.type == COLON) {    // check for bitfield
            gettoken();
            if (cur.type != NUMBER) {
                gripe(ER_D_BD);
            } else if (cur.v.numeric > MAXBITS) {
                gripe(ER_D_BM);
            } else {
                nm->kind = bitfield;
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
        /*
         * Arrays have both TF_ARRAY and TF_POINTER flags for array decay
         * semantics
         */
        prefix = get_type(TF_ARRAY|TF_POINTER, prefix, i);
        expect(RBRACK, ER_D_AD);
        /* Store array type in suffix so it gets assigned to nm->type */
        suffix = prefix;
    }

    if (cur.type == LPAR) {     // ( <func_arg>[,<func_arg>]*. )
        gettoken();
        if (suffix) {
            gripe(ER_D_FA);
            suffix = 0;
        }

        // Create a new function type (don't use get_type() which caches types)
        // Function types need unique instances because we modify elem list
        suffix = calloc(1, sizeof(*suffix));
        suffix->flags = TF_FUNC;
        suffix->sub = prefix;  // Return type

        /*
         * Detect style: K&R if starts with SYM that's not a typedef,
         * ANSI otherwise
         */
        // Check if current symbol is a typedef name
        boolean is_typedef_name = 0;
        if (cur.type == SYM) {
            struct name *n = lookup_name(cur.v.name, 0);
            if (n && n->kind == tdef) {
                is_typedef_name = 1;
            }
        }
        boolean kr_style = (cur.type == SYM &&
                            !is_type_token(cur.type) &&
                            !is_typedef_name);

        // Parse parameter list
        while (cur.type != RPAR && cur.type != E_O_F) {
            struct type *param_type = NULL;
            char param_name_buf[64];  /* Stack buffer for parameter names */
            char *param_name = NULL;

            // Check for variadic ... (three DOT tokens)
            if (cur.type == DOT && next.type == DOT) {
                gettoken();  // consume first DOT
                if (cur.type == DOT && next.type == DOT) {
                    gettoken();  // consume second DOT
                    if (cur.type == DOT) {
                        gettoken();  // consume third DOT
                        suffix->flags |= TF_VARIADIC;
                        // ... must be last parameter
                        if (cur.type == COMMA) {
                            gripe(ER_D_FA);  // ... must be last
                        }
                        break;  // exit parameter loop
                    }
                }
            }

            if (kr_style) {
                // K&R style: just collect names (types come later)
                param_name = parse_param_name(0, param_name_buf,
                                              sizeof(param_name_buf));
                if (!param_name) {
                    gripe(ER_D_FA);
                    break;
                }
            } else {
                // ANSI style: parse full type + declarator
                struct type *basetype = getbasetype();
                if (!basetype) {
                    gripe(ER_D_FA);
                    break;
                }

                // Parse pointer prefix
                param_type = parse_pointer_prefix(basetype);

                // Get parameter name (optional for ANSI declarations)
                /* Allow anonymous */
                param_name = parse_param_name(1, param_name_buf,
                                              sizeof(param_name_buf));

                // Handle array suffix (converts to pointer)
                if (cur.type == LBRACK) {
                    gettoken();
                    if (cur.type != RBRACK) {
                        /* Array size (ignored for parameters) */
                        parse_expr(0, NULL);
                    }
                    expect(RBRACK, ER_D_FA);
                    param_type = get_type(TF_POINTER,
                        param_type->sub ? param_type->sub : param_type, 0);
                }
            }

            // Create parameter entry for type->elem with actual name
            /*
             * Names are kept for K&R matching and for parsefunc() to
             * add to namespace
             */
            /*
             * Type comparison uses compatible_function_types() which
             * ignores names
             */
            arg = create_param_entry(param_name, param_type);
            arg->next = suffix->elem;
            suffix->elem = arg;

            /* Stack buffer automatically freed */

            // Handle comma or end of list
            if (cur.type == COMMA) {
                gettoken();
                continue;
            }
            if (cur.type != RPAR) {
                gripe(ER_D_FA);
                break;
            }
        }

        expect(RPAR, ER_D_FA);

        // K&R style: parse type declarations after )
        if (kr_style && is_type_token(cur.type)) {
            while (is_type_token(cur.type) && cur.type != E_O_F &&
                   cur.type != BEGIN) {
                char param_name_buf[64];  /* Stack buffer for parameter names */
                struct type *basetype = getbasetype();
                if (!basetype) {
                    break;
                }

                // Parse declarator
                struct type *param_type = parse_pointer_prefix(basetype);

                // Get parameter name (required for K&R declarations)
                char *param_name = parse_param_name(0, param_name_buf,
                                                    sizeof(param_name_buf));
                if (!param_name) {
                    gripe(ER_D_FM);
                    break;
                }

                // Find matching parameter in suffix->elem and update its type
                struct name *p;
                for (p = suffix->elem; p; p = p->next) {
                    if (strcmp(p->name, param_name) == 0) {
                        p->type = param_type;
                        break;
                    }
                }
                if (!p) {
                    gripe(ER_D_FM);  // Parameter declared but not in list
                }
                /* Stack buffer automatically freed */

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
        gripe(ER_D_UT);
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
 * Check if current token could start a type cast
 * Returns 1 if it's a type keyword or typedef name
 */
int
is_cast_start(void)
{
    struct name *n;

    /* Check for type keywords */
    if (is_type_token(cur.type)) {
        return 1;
    }

    /* Check if it's a typedef name */
    if (cur.type == SYM) {
        n = lookup_name(cur.v.name, 0);
        if (n && n->kind == tdef) {
            return 1;
        }
    }

    return 0;
}

/*
 * Parse a type name for a cast: (type)
 * This parses type specifiers and abstract declarator (pointers, arrays)
 * but does NOT require a variable name (unlike normal declarations)
 *
 * Examples:
 *   int
 *   char *
 *   int **
 *   int *[]
 *   int (*)[10]
 */
struct type *
parse_type_name(void)
{
    struct type *base_type, *result_type;

    /* Parse base type (int, char, struct foo, typedef, etc.) */
    base_type = getbasetype();
    if (!base_type) {
        /* No type specified - default to int (K&R style) */
        base_type = inttype;
    }

    /* Parse pointer prefix (*, **, etc.) */
    result_type = parse_pointer_prefix(base_type);

    /* TODO: Parse abstract declarator for arrays/function pointers
     * For now, we handle simple types and pointers
     * Full support would parse things like:
     *   (*)[10]  - pointer to array
     *   (*)()    - pointer to function
     */

    return result_type;
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
