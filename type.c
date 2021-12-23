/*
 * we want a squeaky-clean type system
 * this compiler has an agenda to do operations in as small an integer as 
 * possible. this means that we might even get the wrong answer sometimes.  
 * we don't do the standard thing of doing word arithmetic on bytes just 
 * so we don't get overflows. that's slow and big.  don't be slow and big.
 *
 * there is no redundancy in the type tree, so two variables of the same
 * type have identical type pointers, even if one of them was declared 
 * with a typedef.
 *
 * names go out of scope, but types don't, so we don't need a ref count.
 *
 * we to add the primitive types to the global scope.
 *
 * the purpose of this is to unify basic type and typedef handling
 * example: typedef unsigned char byte 
 *          gets a name entry that points at the primitive
 * 
 * scope is handled by pushing names onto the name stack for open, 
 * and popping for close.  very simple and fast
 *
 * some example types:
 * int f          - short
 * int *f         - pointer -> short
 * int *f[4]      - array(4) -> pointer -> short
 * int f()        - function -> short
 * int *f()       - function -> pointer -> short
 * int (*f)()     - pointer -> function -> short
 * int (*f[4])()  - array(4) -> pointer -> function -> short
 * int *(*f)()    - pointer -> function -> pointer -> short
 *
 * some other interesting cases:
 * int *pi, i;
 * typedef int *pi;  pi *ppi, pi; 
 */


#include "ccc.h"

char *kindname[] = { 
    "prim", "etag", "stag", "utag", "vari", "elem", "tdef"
};
struct type *types;

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
        if (cur.type == COLON) {
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
        suffix = get_type(0, prefix, 0);
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
#define ENUM_TYPE   "_uchar_"

/*
 * basic types = 0
 * global = 1
 * inner blocks > 1
 */
int lexlevel;

int lastname;
int maxnames = 100;
struct name **names;

/*
 * free up all the names at a higher scope than here
 */
void
pop_scope()
{
	struct name *n;

    lexlevel--;

    for (n = names[lastname]; lastname >= 0; lastname--) {
        if (n->level <= lexlevel)
            break;
        names[lastname] = 0;

        printf("pop_scope: delete %s%s", 
            n->is_tag ? "tag:":"", n->name);

        free(n->name);
#ifdef notdef
        destroy_expr(n->init);
        destroy_stmt(n->body);
#endif
        free(n);
    }
}

void
push_scope(char *n)
{
    lexlevel++;
}

/*
 * resolve this name into a name struct
 */
struct name *
lookup_name(char *name, boolean is_tag)
{
	struct name *n;
    int i;

    for (i = lastname; i >= 0; i--) {
        n = names[i];
        if ((n->is_tag == is_tag) && 
            (name[0] == n->name[0]) && 
            (strcmp(name, n->name) == 0)) {
	        return (n);
		}
	}
	return 0;
}

/*
 * a more restrictive name lookup that looks through the elements of a type
 * used for struct, union, and enum tag lookups
 */
struct name *
lookup_element(char *name, struct type *t)
{
	struct name *n;
	for (n = t->elem; n; n = n->next) {
		if (strcmp(name, n->name) == 0) {
			return (n);
		}
	}
	return 0;
}

char *type_bitdefs[] = {
		"AGGREGATE", "INCOMPLETE", "UNSIGNED", 
        "NORMALIZED", "POINTER", "ARRAY", "FLOAT"
};

/*
 * what's in a name
 */
void
dump_name(struct name *n)
{
	char *k;
    extern char *sclass_bitdefs[];

	printf("dump_name: ");
	if (!n) { printf("null\n"); return; }
	printf("%s (%s)\n", n->name, n->is_tag ? "tag" : "decl");
	if (n->type) {
		printf("\ttype: %s\n", n->type->name);
	}
    printf("\toffset: %d bitoff: %d width: %d sclass: %s\n",
        n->offset, n->bitoff, n->width, bitdef(n->sclass, sclass_bitdefs));
}

/*
 * push a name on the symbol list at lexlevel
 */
struct name *
new_name(char *name, kind k, struct type *t, boolean is_tag)
{
	struct name *n;
    int i;
 
    if (!names) {
        names = malloc(sizeof(n) * maxnames); 
        lastname = -1;
    }
    if (lastname == maxnames) {
        err(ER_D_OF);
        return (0);
    }

    // if there already is one of the same space, lose
    for (i = lastname; i >= 0; i--) {
        if (names[i]->level < lexlevel) {
            break;
        }
        n = names[i];
        if ((n->is_tag == is_tag) && 
            (name[0] == n->name[0]) && 
            (strcmp(name, n->name) == 0)) {
            err(ER_D_DN);
	        return (0);
		}
    }
	n = malloc(sizeof(*n));
	n->name = strdup(name);
	n->type = t;	
    n->level = lexlevel;
    n->is_tag = is_tag;
    n->kind = k;
    names[++lastname] = n;
    printf("new_name: level:%d index:%3d %s %s%s\n", 
        lexlevel, lastname, 
        k < sizeof(kindname)/sizeof(kindname[0]) ? kindname[k] : "unkn", 
        is_tag ? "tag:":"", name);

	return (n);
}

static struct {
        char *name;
        short size;
        char flags;
} basictype[] = {
	{ "_char_", 1, 0 },						// 0
	{ "_short_", 2, 0 },					
	{ "_long_", 4, 0 },
	{ "_uchar_", 1, TF_UNSIGNED },			// 3
	{ "_ushort_", 2, TF_UNSIGNED },
	{ "_ulong_", 4, TF_UNSIGNED },
	{ "_void_", 0, 0 },						// 6
	{ "_boolean_", 1, TF_UNSIGNED },
	{ "_float_", 4, TF_FLOAT },
	{ "_double_", 8, TF_FLOAT },
};

/*
 * find this type, or if it does not exist, create it
 * this is a search of all defined types.
 * open question:  how should incomplete types be handled
 *   for structs, it's clear that it gets updated in place.
 *   for arrays, when it becomes concrete, it should be updated
 *   typedefs for arrays can't be incomplete
 */
struct type *
get_type(int flags, struct type *sub, struct type *args, int count)
{
    struct type *t;

    /* 
     * search through types to see if we have a permissive match
     */
    for (types; t; t = t->next) {
        if ((t->flags == flags) && (t->sub == sub) && (t->args == args)) {
            printf("type hit\n");
            return t;
        }
    }

    printf("type miss\n");
    t = malloc(sizeof(*t));
    t->sub = sub;
    t->flags = flags;
    t->count = count;
    if (t->count == -1) {
        t->flags |= TF_INCOMPLETE;
    }
    t->next = types;
    types = t;
    return t;
}

void
initbasictype()
{
    char i;
    struct name *n;
    struct type *t;

    for (i = 0; i < sizeof(basictype)/sizeof(basictype[0]); i++) {
        t = malloc(sizeof(*t));
        t->name = basictype[i].name;
        t->flags = basictype[i].flags;
        t->size = basictype[i].size;
        t->next = types;
        types = t;
        n = new_name(basictype[i].name, prim, t, 0);
    }
}

#define UN_SIGNED   3
#define OTHERS      6
#define BYTES_1     0
#define BYTES_2     1
#define BYTES_4     2
#define	MISC_BASIC	6

/*
 * parse the basic type
 * these are a little bizarre, since the words 'unsigned', 'short' and 'long'
 * can be prefixes or type names, but short and long can't both exist
 */
struct type *
parsebasic()
{
	struct name *n;
    char unsignedness = 0;
    char length = 0;
    char misc = 0;

    while (1) {
		switch (cur.type) {

		case CHAR:
			gettoken();
			length = BYTES_1 + 1;
			goto done;

		case LONG:
			gettoken();
			if (length) {
				err(ER_T_PT);
			}
			length = BYTES_4 + 1;
			continue;

		case SHORT:
			gettoken();
			if (length) {
				err(ER_T_PT);
			}
			length = BYTES_2 + 1;
			continue;

		case INT:
			gettoken();
			if (!length) {
				length = BYTES_2 + 1;
			}
			goto done;

        case UNSIGNED:
            gettoken();
            unsignedness = UN_SIGNED;
            continue;

		case DOUBLE:
			misc++;
		case FLOAT:
			misc++;
		case BOOLEAN:
			misc++;
		case VOID:
			gettoken();
			if (length + unsignedness) {
				err(ER_T_PT);
				length = 0;
			}
			misc += MISC_BASIC;
			goto done;

		default:
            // no type, no prefixes, unrecognized keyword. stop parsing type.
			if ((length + unsignedness) == 0) { 
				return 0;
			}
			break;
		}
	}
done:
    if (unsignedness && (length == 0)) {    // naked unsigned
        length = BYTES_2 + 1;
    }
    if (length) length--;
    n = names[unsignedness + length + misc];
	return n->type;
}

#ifdef notdef
/*
 * typedef <type> <name>
 * like:   int (*foo)() pfi;
 */
void
do_typedef()
{
    struct type *bt, *t;
    struct name *n;
    t = new_type(0, TYPE_DEF, 0);
    n = declare(&bt);
    if (cur.type == SYM) {
        t->name = strdup(strbuf);
        gettoken();
        if (!v) {
            t->sub = bt;
        } else {
            t->sub = v->type;
        }
    } else if (v) {
        t->name = v->symbol;
        t->sub = v->type;
    }
    if (t->name && t->sub) {
        t = normalizetype(t);
    }
    need(';', ';', ER_P_TD);
}
#endif

#ifdef notdef
boolean
sametype(struct type *a, struct type *b)
{
    struct var *va, *vb;

    if (a == b) return 1;
    if (a->kind != b->kind) return 0;
    if (a->kind == TK_PTR || a->kind == TK_ARRAY || a->kind == TK_FUNC) {
        if (!sametype(a->sub, b->sub)) return 0;
    }

    if (a->kind == TK_FUNC) {
        va = a->elem;
        vb = b->elem;
        while (va && vb) {
            if (!sametype(va->type, vb->type)) return 0;
            va = va->next;
            vb = vb->next;
        }
        if (va || vb) return 0;
    }
    if (a->size !- b->size) return 0;
    if ((a->flags ^ b->flags) & T_UNSIGNED) return 0;
    return 1;
}
#endif

/*
 * return a base type - this is either a primitive, struct/union or enum
 */
struct type *
getbasetype()
{
    struct type *t;
    struct name *n;
    char off = 0;
    char i;
    char *s;

    /* a typedef? */
    if (cur.type == SYM) {
        n = lookup_name(strbuf, 0);
        if (n && (n->kind == tdef)) {
            gettoken();
            return n->type;
        }
    }
    t = parsebasic();
    if (t) {
        return t;
    }
    if ((cur.type != ENUM) && (cur.type != STRUCT) && (cur.type != UNION)) {
        return 0;
    }

#ifdef notdef
    /*
     * enum [name] [ { tag [= const], ... } ]
     */
    if (match(ENUM)) {
        if (cur.type == SYM) {    // had better be an enum or undefined
            n = lookup_name(strbuf, ENUMTAG);
            if (n) {
                if (next.type == BEGIN) { // if the enum is defined already
                    recover(ER_P_ED, END);
                    gettoken();
                } 
                gettoken();
                return n->type;
            }
            s = strdup(strbuf);
            gettoken();
        } else {                // anonymous enum
            recover(ER_T_AE, END);
            gettoken();
            return lookup_type(ENUM_TYPE, TYPE_DEF)->type;
        } 

        /* a new enum, which must have a definition */
        t = new_type(s, ENUMTAG, 0);

        if (!match(BEGIN)) {
            err(ER_P_ED);
            return lookup_type(ENUM_TYPE, TYPE_DEF)->type;
        }
        while (!match(END)) {
            if (cur.type != SYM) {
                recover(ER_P_ET, END);
                continue;
            }
            n = alloc_name(strbuf);
            n->type = lookup_type(ENUM_TYPE, TYPE_DEF)->type;
            n->space = ENUMELEMENT;
            n->next = t->elem;
            t->elem = n;
            gettoken();

            // handle tag = const
            if (match(ASSIGN)) {
                n->init = expr(PRI_ASSIGN, 0);
                if (!n->flags & E_CONST) {
                    n->init = 0;
                    err(ER_P_ET);
                } else {
                    off = n->init->v;
                }
            }
            if (!n->init) {
                n->init = makeexpr(CONST, 0, 0, E_CONST);
                n->init->v = off;
            }
            off++;

            /* a trailing comma with a close next is an error */
            if (match(COMMA)) {
                if (cur.type == RBRACK) {
                    err(ER_P_ED);
                }
            }
        } // while
        return t;
    } // ENUM
#endif

#ifdef notdef
    /*
     * define struct or union
     */ 
    if (cur.type == STRUCT || cur.type == UNION) {
        off = cur.type;
        s = 0;
        gettoken();
            
    } // STRUCT || UNION
#endif
    err(ER_T_UT);
    return 0;
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
