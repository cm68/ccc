/*
 * we want a squeaky-clean type system
 * this compiler has an agenda to do operations in as small an integer as possible.
 * this means that we might even get the wrong answer sometimes.  we don't do the standard
 * thing of doing word arithmetic on bytes just so we don't get overflows.
 * that's slow and big.  don't be slow and big.
 */

#include "ccc.h"

#define ENUM_TYPE   "_uchar_"

struct scope *global;
struct scope *local;

/*
 * a scope is a container for names of identifiers and types
 * a declarator like struct foo { int bar; } baz
 * defines 2 names at this scope:  foo and baz.
 */
void
push_scope(char *name)
{
	struct scope *s;
	s = malloc(sizeof(*s));
	s->names = 0;
	s->name = strdup(name);
	s->prev = local;
    local = s;
}

/*
 * destroy a scope.
 * deallocate all the symbols and types we defined
 * freeing the global scope is done for each file, discarding everything
 */
void
pop_scope()
{
	struct name *n;
    struct scope *s = local;

	while ((n = s->names)) {
        s->names = n->next;
        destroy_name(n);
	}
    free(s->name);
    if (local == global) {
    	local = global = 0;
    } else {
		local = s->prev;
    }
    free(s);
}

#ifdef DEBUG
/*
 * print out a scope's names
 */
void
dump_scope(struct scope *s)
{
	struct name *n;

	printf("dump_scope: ");
	if (!s) { printf("null\n"); return; }
	printf("%s\n", s->name);
	for (n = s->names; n; n = n->next) {
		dump_name(n);
	}
}
#endif

/*
 * resolve this name into a name struct, applying scope and namespace rules
 */
struct name *
lookup_name(char *name, namespace_t space)
{
	struct name *n;
	struct scope *s;

    // search from current scope going up
	for (s = local; s; s = s->prev) {
		for (n = s->names; n; n = n->next) {
			if ((n->space == space) && (strcmp(name, n->name)) == 0) {
				return (n);
			}
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
		"AGGREGATE", "INCOMPLETE", "UNSIGNED", "NORMALIZED", "POINTER", "ARRAY", "FLOAT"
};

char *namespace_name[] = {
		"SYMBOL", "TYPEDEF", "ENUMTAG", "ENUMELEMENT", "AGGTAG", "AGGELEMENT"
};

#ifdef DEBUG
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
	printf("%s (%s)\n", n->name, namespace_name[n->space]);
	if (n->type) {
		printf("\ttype: %s\n", n->type->name->name);
	}
    printf("\toffset: %d bitoff: %d width: %d sclass: %s\n",
        n->offset, n->bitoff, n->width, bitdef(n->sclass, sclass_bitdefs));
}
#endif

/*
 * add an enum element or a structure element to a type
 * just allocate. the caller needs to fill out struct
 */
struct name *
alloc_name(char *name)
{
	struct name *n;
	n = malloc(sizeof(*n));
	n->name = strdup(name);
	return (n);
}

/*
 * add a name to the current scope, tagged with attributes
 */
struct name *
new_name(char *name, struct type *t, namespace_t space)
{
	struct name *n;
    n = alloc_name(name);
	n->space = space;
	n->type = t;	
	n->next = local->names;
	local->names = n;
	return (n);
}

/*
 * destructors go deep.
 */
void
destroy_name(struct name *s)
{
	if (!s)
		return;
	free(s->name);
#ifdef notdef
	destroy_expr(s->init);
	destroy_stmt(s->body);
#endif
}

/*
 * synthetic name creator - return a static buffer that we need to strdup
 * to actually use
 * we'll either pass s or i, never both.
 */
char *
makename(char *parent, char *s, int i)
{
	static char namebuf[100];

	if (s) {
		sprintf(namebuf, "%s.%s", parent, s);
	} else if (i >= 0) {
		sprintf(namebuf, "%s.%d", parent, i);
	} else {
		strcpy(namebuf, "bogus");
	}
	return (namebuf);
}

/*
 * define a new type.
 */
struct type *
new_type(char *name, namespace_t space, struct type *sub)
{
    struct type *t;
    struct name *n;

    if (name) {
		n = lookup_name(name, space);
		if (n) {
			t = n->type;
			if (t) {
				// could be fleshing out an incomplete structure
				if (t->flags & TF_AGGREGATE) {
					return t;
				}
				err(ER_T_RT);
			}
			// we asked for a type, and got a naked name with no type struct. lose.
			err(ER_T_PA);
			return 0;
		}
		n = new_name(name, 0, space);
    }
    t = malloc(sizeof(*t));
    t->sub = sub;
    t->flags = 0;

    switch (space) {
    case AGGTAG:
        t->flags = TF_AGGREGATE | TF_INCOMPLETE;
        break;
    case ENUMTAG:
        t->flags = TF_UNSIGNED;
        t->size = 1;
        break;
    default:
        break;
    }
    return t;
}

/*
 * initially, we don't have any types defined at all.
 * we to add the primitive types to the global scope
 * this is only callable at global scope
 * the bogus name is used so that typedef won't get confused
 */
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

void
initbasictype()
{
    char i;
    struct type *t;

    for (i = 0; i < sizeof(basictype)/sizeof(basictype[0]); i++) {
        t = new_type(basictype[i].name, TYPE_DEF, 0);
        t->flags = basictype[i].flags;
        t->size = basictype[i].size;
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
	n = lookup_name(basictype[unsignedness + length + misc].name, TYPE_DEF);
	if (!n) {
		err(ER_T_PT);
	}
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
        n = lookup_name(strbuf, TYPE_DEF);
        if (n) {
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
