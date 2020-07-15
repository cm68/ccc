#include "ccc.h"

/*
 * we are in a parse state where we want to process declarations.
 * any names and types we declare go into the current scope
 *
 * the grammar handled:
 * declaration: basetype decl
 * declarations: declaration , declarations
 * decl: ( decl )
 * decl: * decl
 * decl: decl [ bounds ]
 * decl: decl [ ]
 * decl: name
 * decl: name : fieldwidth
 * decl: decl ( declarations )
 *
 * what we end up with is a name with an attached type inside this scope.
 * an example
 * char **foo[20] ->
 * foo -> array [20] -> pointer to -> pointer to -> char
 */
struct name *
declare(struct type **basetype)
{
    struct name *symbol;
    struct type *prefix = 0;
    struct type *postfix = 0;
    struct type *t;

    /*
     * this will be primitive, enum, struct/union or a typedef
     */
    if (!*basetype) {
        *basetype = getbasetype();
    }
    prefix = *basetype;

    // decl: * decl
    // the pointer denotation binds less tightly than others
    while (match(STAR)) {
        prefix = new_type(0, TYPE_DEF, prefix);
        prefix->flags = TF_POINTER;
    }

    // decl: ( decl )
    if (match(LPAR)) {
        t = 0;
        symbol = declare(&t);       // recurse
        need(RPAR, RPAR, ER_D_DP);
        if (*basetype && t) {
            err(ER_T_DT);
            t = 0;
        }
        if (t && !symbol) {
            *basetype = t;
        }
    }
#ifdef notdef // not sure what this code is for
    if (cur.type == RPAR) {
        if (!symbol) {
            for (t = prefix; t && t->sub; t = t->sub) {
                if (t) {
                    t->sub = *basetype;
                    *basetype = prefix;
                }
            }
        }
        return v;
    }
#endif

    // decl: name
    if (cur.type == SYM) {      // symbol name
        if (symbol) {           // there can only be one
            err(ER_D_MV);
        }
        symbol = new_name(strdup(strbuf), prefix, SYMBOL);
        gettoken();
        // decl: name : fieldwidth
        if (match(COLON)) {
            if (cur.type != NUMBER) {
                err(ER_D_BD);
            } else if (cur.v.numeric > MAXBITS) {
                err(ER_D_BM);
            } else {
                symbol->flags |= V_BITFIELD;
                symbol->width = cur.v.numeric;
            }
            gettoken();
        }
    }

    // decl: decl [ bounds ]
    while (match(LBRACK)) {
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

    /* declaration terminators */
    switch (cur.type) {
    case ASSIGN:
    case BEGIN:
    case COMMA:
    case SEMI:
        break;
    default: 
        printf("token: %d 0x%x '%c'\n", cur.type, cur.type, cur.type);
        err(ER_D_UT);
        symbol = 0;
    }

    if (!symbol) {  // we just had a type specifier, but no symbol
        return 0;
    }

    /*
     * prepend the prefix and append the postfix
     */
    t = symbol->type;
    while (t && t->sub) {
        t = t->sub;
    }
    t = postfix;
    while (t && t->sub) {
        t = t->sub;
    }
    t = prefix;
    return symbol;
}                               // declare

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
