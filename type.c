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
#include "type.h"

/*
 * at our current scope, look up the name.   this means that we need to
 * search back through all the scopes, to the global scope.
 */
struct name *
lookup_name(char *name, struct scope *scope)
{
	struct name *n;
	struct scope *s;
	for (s = scope; s; s = s->parent) {
		for (n = s->names; n; n = n->next) {
			if (strcmp(name, n->name) == 0) {
				return (n);
			}
		}
	}
	return 0;
}

/*
 * a more restrictive name lookup.
 * used for struct, union, and enum lookups
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
	char *name_kind[] = { "type", "symbol" };

	printf("dump_name: ");
	if (n) { printf("null\n"); return; }
	printf("%s is %s inside %s type %s\n", 
		n->name, name_kind[n->kind], 
		n->scope->scopename, 
		n->type->typename->name);
}

/*
 * add a name to a scope, tagged with attributes
 */
struct name *
new_name(char *name, struct scope *scope, struct type *t, enum namekind k)
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
 */
struct scope *
new_scope(struct scope *parent, char *name)
{
	struct scope *s;
	s = malloc(sizeof(*s));
	s->parent = parent;
	s->names = 0;
	s->scopename = strdup(name);
    return (s);
}

/*
 * when we destroy the scope, both names and types go away
 */
void
destroy_scope(struct scope *s)
{
	struct name *n;

	while ((n = s->names)) {
        s->names = n->next;
		free(n->name);
        free(n);
	}
    free(s->scopename);
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

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
