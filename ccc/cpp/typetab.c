/*
 * typetab.c - Typedef name table
 *
 * Tracks typedef names so filters can distinguish type names from
 * regular identifiers. Used by filtdecl and filtknr.
 */

#include "cpp.h"
#include "lexeme.h"
#include <string.h>

/* Simple hash table for typedef names */
#define HASH_SIZE 127

static struct tdef {
	char *name;
	struct tdef *next;
} *table[HASH_SIZE];

static unsigned
hash(char *s)
{
	unsigned h = 0;
	while (*s)
		h = h * 31 + *s++;
	return h % HASH_SIZE;
}

/*
 * Register a typedef name
 */
void
addTypedef(char *name)
{
	unsigned h = hash(name);
	struct tdef *t;

	/* Check if already present */
	for (t = table[h]; t; t = t->next)
		if (strcmp(t->name, name) == 0)
			return;

	t = malloc(sizeof(*t));
	t->name = strdup(name);
	t->next = table[h];
	table[h] = t;
}

/*
 * Check if name is a typedef
 */
int
isTypedef(char *name)
{
	unsigned h = hash(name);
	struct tdef *t;

	for (t = table[h]; t; t = t->next)
		if (strcmp(t->name, name) == 0)
			return 1;
	return 0;
}

/*
 * Reset table (for new compilation unit)
 */
void
typedefReset(void)
{
	int i;
	for (i = 0; i < HASH_SIZE; i++) {
		struct tdef *t = table[i];
		while (t) {
			struct tdef *next = t->next;
			free(t->name);
			free(t);
			t = next;
		}
		table[i] = 0;
	}
}
