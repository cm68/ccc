/*
 * we want a squeaky-clean type system
 *
 * this file contains constructors, destructors, lookups and dumpers 
 * for all the compiler types. the pattern is something like:
 * new_<thing>(args)
 * lookup_<thing>(args)
 * dump_thing(args)
 * destroy_<thing>(thing)
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "ccc.h"

/*
 * at our current scope, look up the name.   this means that we need to
 * search back through all the scopes, to the global scope.
 * namespace should be one of:
 * NK_SYMBOL
 * NK_TYPE | (TK_STRUCT | TK_UNION | TK_ENUM | TK_FUNC)
 * NK_TDEF
 */
struct name *
lookup_name(char *name, kind_t namespace)
{
	struct name *n;
	struct scope *s;

    /* start at the top scope and go to higher levels until we hit */
	for (s = scope; s; s = s->next) {
		for (n = s->names; n; n = n->next) {
			if ((n->kind == namespace) && (strcmp(name, n->name)) == 0) {
				return (n);
			}
		}
	}
	return 0;
}

/*
 * a more restrictive name lookup.
 * used for struct, union, and enum tag lookups
 */
struct name *
lookup_tag(char *name, struct type *t)
{
	struct name *n;
	for (n = t->elem; n; n = n->next) {
		if (strcmp(name, n->name) == 0) {
			return (n);
		}
	}
	return 0;
}

/*
 * what's in a name
 */
void
dump_name(struct name *n)
{
	char *k;

	printf("dump_name: ");
	if (!n) { printf("null\n"); return; }
    if (n->kind & NK_TYPE) {
        switch (n->kind & TK_TMASK) {
        case TK_STRUCT:
            k = "struct";
            break;
        case TK_UNION:
            k = "union";
            break;
        case TK_ENUM:
            k = "enum";
            break;
        case TK_FUNC:
            k = "func";
            break;
        default:
            k = "wtftype";
            break;
        }
    } else if (n->kind & NK_SYMBOL) {
        k = "symbol";
    } else if (n->kind & NK_TDEF) {
        k = "typedef";
    } else {
        k = "wtfname";
    }
	printf("%s is %s inside %s type %s\n", 
		n->name, k, 
		n->scope->scopename, 
		n->type->name->name);
}

/*
 * add a name to the current scope, tagged with attributes
 */
struct name *
new_name(char *name, struct type *t, kind_t k)
{
	struct name *n;
	n = malloc(sizeof(*n));
	n->name = strdup(name);
	n->kind = k;
	n->type = t;	
	n->next = scope->names;
	scope->names = n;
	return (n);
}

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
	s->scopename = strdup(name);
	s->next =scope;
    scope = s;
}

/*
 * when we destroy the scope, both names and types go away
 */
void
pop_scope()
{
	struct name *n;
    struct scope *s = scope;

	while ((n = s->names)) {
        s->names = n->next;
		free(n->name);
        free(n);
	}
    free(s->scopename);
    scope = s->next;
    free(s);
}

/*
 * print out a scope's names
 */
void
dump_scope(struct scope *s)
{
	struct name *n;

	printf("dump_scope: ");
	for (n = s->names; n; n = n->next) {
		dump_name(n);
	}
}

/*
 * synthetic name creator - return a static buffer that we need to strdup
 * to actually use
 * we'll either pass s or i, never both.
 */
char *
newname(char *parent, char *s, int i)
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

struct type *
maketype(char *name, char kind, struct type *sub)
{
    struct type *t;

    t = findtype(name, kind);
    if (t && ((t->flags & T_AGGMASK) == T_AGGMASK)) {
        return t;
    }
    if (t) return 0;

    t = malloc(sizeof(*t));
    t->kind = kind;
    t->sub = sub;
    t->flags = 0;

    switch (kind) {
    case TK_STRUCT:
    case TK_UNION:
        t->flags = T_AGGREGATE | T_INCOMPLETE;
        break;
    case TK_ARRAY:
        t->flags = T_INCOMPLETE;
        t->size = -1;
        break;
    case TK_PTR:
        t->flags = T_UNSIGNED;
        t->size = TS_PTR;
        break;
    case TK_ENUM:
        t->flags = T_UNSIGNED;
        t->size = 1;
        break;
    }
    return t;
}

#define UN_SIGNED   4
#define OTHERS      6
#define BYTES_1     0
#define BYTES_2     1
#define BYTES_4     2

/*
 * initially, we don't have any types defined at all.
 * we to add the primitive types to the global scope
 * we normalize types so we don't have to do unneeded type conversions
 */
void
initptype()
{
    char i;
    static struct {
        char *name;
        short size;
        char flags;
    } pi[] = {
        { "char", 1, 0 },
        { "short", 2, 0 },
        { "long", 4, 0 },
        { "uchar", 1, T_UNSIGNED },
        { "ushort", 2, T_UNSIGNED },
        { "ulong", 4, T_UNSIGNED },
        { "void", 0, 0 },
        { "boolean", 1, T_UNSIGNED },
        { "float", 4, 0 },
        { "double", 8, 0 },
    };
    struct type *t;

    for (i = 0; i < sizeof(pi/sizeof(pi[0]); i++) {
        t = maketype(pi->name, TK_SCALAR, 0);
        t->flags = pi[i].flags;
        t->size = pi[i].size;
        regtype(t);
    }
}

/*
 * parse the primitive type
 * these are a little bizarre, since the words 'unsigned', 'short' and 'long'
 * can be prefixes or type names, but short and long can't both exist
 */
struct type *
gettype()
{
    char s = 0;     // signed offset
    char p = 0;     // size prefix
    char o = 0;     // type offset

    if (match(UNSIGNED)) {
        s = 4;
    }

    if (match(SHORT)) {
        p = BYTES_2;
    } else if (match(LONG)) {
        p = BYTES_4;
    }

    switch (curtok) {
    case CHAR:
        if (p) {        // can't have (short|long) char
            err(ER_P_PT);
            p = 0;
        }
        gettoken();
        break;
    case LONG:  // o = BYTES_4;
        o++;
    case SHORT: // o = BYTES_2;
        o++;
        if (p) {
            err(ER_P_PT);
            p = 0;
        }
        gettoken();
        break;
    case INT:   // o = 0
        if (!p) {
            o = BYTES_2;
        }
        gettoken();
        break;
    case DOUBLE:
        o++;
    case FLOAT:
        o++;
    case BOOLEAN:
        o++;
    case VOID:
        gettoken();
        o += OTHERS;
        if (s + p) {
            err(ER_P_PT);
        }
    default:
        if ((p + s) == 0) { // no type, no prefixes
            return 0;
        }
        break;
    }
    return ptype[s + p + o];
}

/*
 * typedef <type> <name>
 * like:   int (*foo)() pfi;
 */
void
do_typedef()
{
    struct type *bt, *t;
    struct var *v;
    t = maketype(0, TYPEDEF, 0);
    v = declare(&bt);
    if (curtok == SYM) {
        t->name = strdup(symbuf);
        gettoken();
        if (!v) {
            t->sub = bt;
        } else {
            t->sub = v->type;
        }
    } else if (v) {
        t->name = v->name;
        t->sub = v->type;
    }
    if (t->name && t->sub) {
        t = normalizetype(t);
    }
    need(';', ';', ER_P_TD);
}

void
regtype(struct type *t)
{
    if (t->flags & T_NORMALIZED) return;
    if (!alltypes) {
        alltypes = t;
    } else {
        typetail->next = t;
    }
    typetail = t;
    t->flags |= T_NORMALIZED;
}

struct type *
findtype(char *name, char kind)
{
    struct type *t;

    for (t = alltypes; t; t = t->next) {
        if (t->kind == kind) && *t->name == name && !strcmp(t->name, name)) {
            return t;
        }
    }
    return 0;
}

void freetype(struct type *t)
{
    if (!t || t->flags & T_NORMALIZED) return;
    freetype(t->sub);
    if (t->name)
        free(t->name)
    free(t);
}

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

struct type *
normalizetype(struct type *tc)
{
    struct type *t;
    struct var *v;
    if (!tc) return 0;
    if (tb->flags & T_NORMALIZED) return tc;

    if (tc->flags & T_AGGREGATE) {
        regtype(tc);
        for (v = tc->elem; v ; v = v->next) {
            v->type = normalizetype(v->type);
        }
        return (tc);
    }

    for (t = alltypes ; t = t->next) {
        if (sametype(t, tc)) {
            freetype(tc);
            return (t);
        }
    }

    regtype(tc);
    return (tc);
}

/*
 * return a base type - this is either a primitive, struct/union or enum
 */
struct type *
getbasetype()
{
    struct type *bt, *t;
    char *s;                // name of type definition
    char off = 0;
    char i;
    struct var *lastp, *tg;

    /* a typedef? */
    if (curtok == SYM) {
        t = findtype(symbuf, 't');
        if (t) {
            gettoken();
            return t->sub;
        }
    }
    t = getptype();
    if (t) {
        return t;
    }
    if ((curtok != ENUM) && (curtok != STRUCT) && (curtok != UNION)) {
        return 0;
    }
    /*
     * enum [name] [ { tag [= const], ... } ]
     */
    if (match(ENUM)) {
        if (curtok == SYM) {    // had better be an enum or undefined
            t = findtype(symbuf, 'e');
            if (t) {
                if (nexttok == '{') { // if the enum is defined already
                    recover(ER_P_ED, '}');
                    gettoken();
                } 
                gettoken();
                return t;
            }
            s = strdup(symbuf);
            gettoken();
        } else {
            s = 0;
        } 
        /* a new enum, which must have a definition */
        t = maketype(s, 'e', 0);
        if (!match(LBRACK)) {
            err(ER_P_ED);
            return ptype[PT_INT];
        }
        lastp = &t->elem;
        while (!match(RBRACK)) {
            if (curtok != SYM) {
                recover(ER_P_ET);
                continue;
            }
            tg = makevar(strdup(symbuf, ptype[PT_UCHAR], V_GLOBAL|V_CONST);
            tg->init = makeexpr(CONST, 0, 0, E_CONST);
            tg->init->v = off;
            *lastp = tg;
            lastp = &tg->next;
            gettoken();
            if (match(ASSIGN)) {
                tg->init = expr(PRI_ASSIGN, 0);
                if (!tg_init->flags & E_CONST) {
                    tg->init = 0;
                    err(ER_P_ET);
                } else {
                    off = tg->init->v;
                }
            }
            if (!tg->init) {
                tg->init = makeexpr(CONST, 0, 0, E_CONST);
                tg->init->v = off;
            }
            off++;
            /* a trailing comma with a close next is an error */
            if (match(COMMA)) {
                if (curtok == RBRACK) {
                    err(ER_P_ED);
                }
            }
        } // while
        regtype(t);
        return t;
    } // ENUM

    if (curtok == STRUCT || curtok == UNION) {
        off = curtok;
        s = 0;
        gettoken();
            
    } // STRUCT || UNION
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
