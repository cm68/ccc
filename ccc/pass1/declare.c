#include "cc1.h"

/*
 * Parse pointer prefix and build pointer type chain
 *
 * Processes zero or more '*' tokens to construct a chain of pointer types.
 * Each '*' wraps the previous type in a pointer type, building from right
 * to left in the type hierarchy.
 *
 * Type construction:
 *   - char **      -> pointer to pointer to char
 *   - int ***      -> pointer to pointer to pointer to int
 *
 * Precedence with arrays/functions:
 *   - Pointers bind tighter than arrays: int *arr[10] is array of pointers
 *   - Function pointers need parens: int (*fptr)() is pointer to function
 *
 * Parameters:
 *   basetype - The base type to wrap with pointer(s)
 *
 * Returns:
 *   Type pointer representing the complete pointer chain, or basetype if no '*'
 *
 * Side effects:
 *   - Consumes '*' and qualifier tokens from input stream
 */
static struct type *
parsePtrPfx(struct type *t)
{
    while (cur.type == STAR) {
        gettoken();
        t = getType(TF_POINTER, t, 0);
    }
    return t;
}

/*
 * Create function parameter name entry for type signature
 *
 * Allocates and initializes a name structure to represent a function parameter
 * in the function type's elem list. These entries serve dual purposes:
 *   1. Store parameter types in function type signature
 *   2. Provide names for parsefunc() to create namespace entries
 *
 * Parameter name handling:
 *   - Always duplicates name string to prevent dangling pointers
 *   - Anonymous parameters get empty string "" (not NULL)
 *   - compatFnTyp() ignores names when comparing signatures
 *
 * Level and scope:
 *   - Sets level to lexlevel+1 (function body scope)
 *   - Parameters become visible when parsefunc() processes function body
 *   - Temporary entries in type->elem, real namespace entries created later
 *
 * Parameters:
 *   name - Parameter name (can be NULL for anonymous, will be duplicated)
 *   type - Parameter type
 *
 * Returns:
 *   Newly allocated name entry with funarg kind
 *
 * Side effects:
 *   - Allocates memory for name structure and duplicates name string
 */
static struct name *
createPrmEnt(char *name, struct type *type)
{
    struct name *arg = calloc(1, sizeof(*arg));
    if (name) {
        strncpy(arg->name, name, 15);
        arg->name[15] = 0;
    }
    arg->type = type;
    arg->level = lexlevel + 1;
    arg->is_tag = 0;
    arg->kind = funarg;
    return arg;
}

/*
 * Parse a complete C declarator and create symbol table entry
 *
 * This is the core declaration parser that handles all C declarator syntax:
 * pointers, arrays, functions, and their combinations with proper precedence.
 * It creates name entries in the symbol table (or struct members) with
 * complete type information.
 *
 * Declarator grammar (simplified):
 *   declarator = prefix_declarator suffix_declarator*
 *   prefix_declarator = '*'* ('(' declarator ')' | identifier)
 *   suffix_declarator = '[' const_expr? ']' | '(' params ')'
 *
 * Precedence rules (postfix binds tighter than prefix):
 *   - int *p[10]    -> array of 10 pointers to int
 *   - int (*p)[10]  -> pointer to array of 10 ints
 *   - int *f()      -> function returning pointer to int
 *   - int (*f)()    -> pointer to function returning int
 *
 * Parameter modes:
 *   btp         - Base type pointer (in/out parameter)
 *   struct_elem - If true, creates struct member (not added to names[])
 *
 * Type assembly:
 *   - Base type comes from caller (int, char, struct foo, etc.)
 *   - Prefix modifiers (pointers) wrap base type
 *   - Suffix modifiers (arrays, functions) become new outer type
 *   - Final type attached to name entry
 *
 * Function parameter handling:
 *   - ANSI style only (K&R style is converted by cpp preprocessor)
 *   - Parameters stored in type->elem as linked list
 *   - Variadic functions detected (...) and marked with TF_VARIADIC
 *
 * Array handling:
 *   - Size can be omitted: int arr[] (size -1, completed later)
 *   - Arrays get both TF_ARRAY and TF_POINTER for decay semantics
 *   - Multi-dimensional arrays supported: int m[10][20]
 *
 * Struct member mode:
 *   - struct_elem=1: Creates name but doesn't add to names[] array
 *   - Avoids polluting global namespace with member names
 *   - Members linked via next pointer in struct type's elem list
 *
 * Bitfield support:
 *   - Detected by ':' after identifier: unsigned flags : 3;
 *   - Width stored in name->width field
 *   - Kind changed to bitfield
 *
 * Redeclaration handling:
 *   - Function forward declarations: Reuses existing name entry
 *   - Other redeclarations: Creates new entry (error reported elsewhere)
 *
 * Parameters:
 *   btp         - Pointer to base type (can be NULL, updated if type seen)
 *   struct_elem - 1 for struct members, 0 for normal variables/functions
 *
 * Returns:
 *   Name entry with complete type, or NULL on error
 *
 * Side effects:
 *   - May update *btp if type keywords encountered
 *   - Consumes declarator tokens from input stream
 *   - Creates name entry (added to names[] unless struct_elem=1)
 *   - For functions: allocates parameter name entries
 */
struct name *
declare(struct type **btp, unsigned char struct_elem)
{
    struct name *nm, *arg, *param_tail;
    struct type *t, *prefix, *suffix, *rt;
    unsigned long i;
    struct type *param_type;
    char *param_name;
    static char paramNameBuf[16];

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

    prefix = parsePtrPfx(prefix);

    // parenthesed type definition does precedence
    if (cur.type == LPAR) {
        gettoken();
        rt = 0;
        nm = declare(&rt, struct_elem);       // recurse
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
                t->sub = *btp;
                *btp = prefix;
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
            strncpy(nm->name, cur.v.name, 15);
            nm->name[15] = 0;
            nm->type = prefix;
            nm->level = lexlevel;
            nm->is_tag = 0;
            nm->kind = elem;  /* will be struct/union member */
            nm->offset = 0;
            nm->width = 0;
            nm->bitoff = 0;
            nm->next = 0;
            nm->u.init = 0;
#ifdef DEBUG
            if (VERBOSE(V_SYM)) {
                fdprintf(2, "struct_elem: %s (not added to names[])\n",
                         nm->name);
            }
#endif
        } else {
            /* normal variable: add to global names[] array */
            /* Check if this name already exists at this scope */
            struct name *existing = findName(cur.v.name, 0);
            if (existing && existing->level == lexlevel) {
                /*
                 * Name exists at current scope - check if it's a
                 * function prototype
                 */
                if (existing->type && (existing->type->flags & TF_FUNC) &&
                    !existing->u.locals) {
                    /* Reuse existing function declaration (prototype) */
                    nm = existing;
                    /*
                     * Update type to the new one (definition may have
                     * full param list)
                     */
                    /* But keep the existing name structure */
                } else {
                    /* Not a function prototype - error on redeclaration */
                    nm = newName(cur.v.name, var, prefix, 0);
                }
            } else if (existing && existing->level < lexlevel) {
                /*
                 * Name exists at outer scope - this is shadowing.
                 * Assign static_id so cc2 can distinguish variables.
                 * Emitted as L<id> (not S<id> which is for statics).
                 */
                nm = newName(cur.v.name, var, prefix, 0);
                nm->static_id = ++shadowCtr;
            } else {
                /* New name - create it */
                nm = newName(cur.v.name, var, prefix, 0);
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
            i = parseConst(RBRACK);
        }
        /*
         * Arrays have both TF_ARRAY and TF_POINTER flags for array decay
         * semantics
         */
        prefix = getType(TF_ARRAY|TF_POINTER, prefix, i);
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

        // Create a new function type (don't use getType() which caches types)
        // Function types need unique instances because we modify elem list
        suffix = calloc(1, sizeof(*suffix));
        suffix->flags = TF_FUNC;
        suffix->sub = prefix ? prefix : inttype;

        // Parse parameter list (ANSI style only)
        param_type = NULL;
        param_tail = NULL;
        while (cur.type != RPAR && cur.type != E_O_F) {
            struct type *basetype;
            param_name = paramNameBuf;
            param_type = NULL;
            paramNameBuf[0] = '\0';

            // Check for variadic ...
            if (cur.type == ELLIPSIS) {
                gettoken();
                suffix->flags |= TF_VARIADIC;
                break;  // exit parameter loop
            }

            // ANSI style: parse full type + declarator
            basetype = getbasetype();
            if (!basetype) {
                gripe(ER_D_FA);
                break;
            }

            // Parse pointer prefix
            param_type = parsePtrPfx(basetype);

            // Handle function pointer: type (*)(args) or type (*name)(args)
            if (cur.type == LPAR) {
                gettoken();
                if (cur.type == STAR) {
                    // (*) or (*name) - pointer to function
                    gettoken();
                    // Optional name inside (*)
                    if (cur.type == SYM) {
                        strcpy(paramNameBuf, cur.v.name);
                        gettoken();
                    }
                    expect(RPAR, ER_D_FA);
                    // Now parse function parameter list
                    if (cur.type == LPAR) {
                        struct type *fn_type;
                        struct name *inner_arg, *inner_tail;
                        fn_type = calloc(1, sizeof(*fn_type));
                        fn_type->flags = TF_FUNC;
                        fn_type->sub = param_type;  // return type
                        inner_tail = NULL;
                        gettoken();  // consume (
                        // Parse inner function's parameters
                        while (cur.type != RPAR && cur.type != E_O_F) {
                            struct type *inner_base, *inner_type;
                            inner_base = getbasetype();
                            if (!inner_base) {
                                if (cur.type == COMMA) {
                                    gettoken();
                                    continue;
                                }
                                break;
                            }
                            inner_type = parsePtrPfx(inner_base);
                            // Skip optional inner parameter name
                            if (cur.type == SYM)
                                gettoken();
                            inner_arg = createPrmEnt(NULL, inner_type);
                            inner_arg->next = NULL;
                            if (inner_tail)
                                inner_tail->next = inner_arg;
                            else
                                fn_type->elem = inner_arg;
                            inner_tail = inner_arg;
                            if (cur.type == COMMA) {
                                gettoken();
                                continue;
                            }
                            break;
                        }
                        expect(RPAR, ER_D_FA);
                        // Result: pointer to function type
                        param_type = getType(TF_POINTER, fn_type, 0);
                    } else {
                        // Just (*) without function params - pointer type
                        param_type = getType(TF_POINTER, param_type, 0);
                    }
                } else {
                    // Unexpected token after ( - try to recover
                    gripe(ER_D_FA);
                    // Skip to matching )
                    while (cur.type != RPAR && cur.type != E_O_F)
                        gettoken();
                    if (cur.type == RPAR)
                        gettoken();
                }
            } else {
                /* get param name */
                if (cur.type == SYM) {
                    strcpy(paramNameBuf, cur.v.name);
                    gettoken();
                }
            }

            // Handle array suffix (converts to pointer)
            if (cur.type == LBRACK) {
                struct expr *sz;
                gettoken();
                if (cur.type != RBRACK) {
                    /* Array size (ignored for parameters) */
                    sz = parseExpr(0);
                    if (sz) FreeExpr(sz);
                }
                expect(RBRACK, ER_D_FA);
                param_type = getType(TF_POINTER,
                    param_type->sub ? param_type->sub : param_type, 0);
            }

            // Create parameter entry for type->elem with actual name
            arg = createPrmEnt(param_name, param_type);
            arg->next = NULL;
            if (param_tail) {
                param_tail->next = arg;
            } else {
                suffix->elem = arg;
            }
            param_tail = arg;

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
    }                           // if cur.type == LPAR

    if ((cur.type != ASSIGN) && (cur.type != BEGIN) &&
        (cur.type != COMMA) && (cur.type != SEMI) && (cur.type != RPAR)) {
        fprintf(stderr, "token: %d $%c$\n", cur.type, cur.type);
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
}

/*
 * Detect if current token starts a type cast expression
 *
 * Distinguishes between type casts and parenthesized expressions when
 * parsing '(' token. This is critical for correctly parsing cast expressions
 * vs. grouped expressions:
 *
 * Disambiguation examples:
 *   - (int)x      -> type cast (starts with type keyword)
 *   - (foo)x      -> type cast if foo is typedef, expression otherwise
 *   - (x + y)     -> parenthesized expression (starts with identifier/expression)
 *   - (int*)p     -> type cast (starts with type keyword)
 *
 * Detection strategy:
 *   1. Check for C type keywords (int, char, struct, etc.)
 *   2. Check if SYM token is a typedef name in symbol table
 *
 * Typedef handling:
 *   - Looks up identifier in names[] array
 *   - Checks if kind is tdef (typedef)
 *   - Enables casts with user-defined type names: (foo_t)x
 *
 * Used by:
 *   - Expression parser when encountering '(' to decide parse path
 *   - Cast expression parsing (parseTypeName)
 *
 * Returns:
 *   1 if current token could start a type cast
 *   0 if current token starts a parenthesized expression
 */
char
isCastStart(void)
{
    struct name *n;

    /* Check for type keywords */
    if (isTypeToken(cur.type)) {
        return 1;
    }

    /* Check if it's a typedef name */
    if (cur.type == SYM) {
        n = findName(cur.v.name, 0);
        if (n && n->kind == tdef) {
            return 1;
        }
    }

    return 0;
}

/*
 * Parse type name in cast expression without requiring identifier
 *
 * Parses abstract type names used in cast expressions, sizeof, and other
 * contexts where a type is specified without a variable name. This is
 * distinct from normal declarations which require an identifier.
 *
 * Abstract declarators supported:
 *   - Simple types: int, char, struct foo
 *   - Pointer types: char *, int **, void ***
 *   - Typedef names: size_t, my_type_t
 *
 * Abstract declarators NOT YET supported:
 *   - Arrays: int[], int[10]
 *   - Function pointers: int (*)(void), void (*)(int, int)
 *   - Complex combinations: int (*[])(void)
 *
 * TODO: Full abstract declarator support would require:
 *   - Parsing (*) for pointer-to-array/function
 *   - Parsing [] and () without identifiers
 *   - Proper precedence handling
 *
 * Current limitations:
 *   - Only handles base types and pointer prefixes
 *   - No array dimensions in casts: (int[])x doesn't work
 *   - No function pointer casts: (int(*)(void))x doesn't work
 *
 * Default type:
 *   - If no type specified, defaults to int
 *   - Enables implicit int casts (rare in practice)
 *
 * Examples:
 *   (int)x           -> parses "int"
 *   (char *)p        -> parses "char *" (pointer to char)
 *   (unsigned long)v -> parses "unsigned long"
 *   (foo_t)x         -> parses typedef "foo_t"
 *
 * Parameters:
 *   None (reads from current token stream)
 *
 * Returns:
 *   Type structure representing the parsed type name
 *
 * Side effects:
 *   - Consumes type tokens from input stream
 *   - Does NOT consume closing ')' of cast
 */
struct type *
parseTypeName(void)
{
    struct type *base_type, *result_type;

    /* Parse base type (int, char, struct foo, typedef, etc.) */
    base_type = getbasetype();
    if (!base_type) {
        /* No type specified - default to int */
        base_type = inttype;
    }

    /* Parse pointer prefix (*, **, etc.) */
    result_type = parsePtrPfx(base_type);

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
