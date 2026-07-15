/*
 * typetab.c - Typedef name table
 *
 * Tracks typedef names so filters can distinguish type names from
 * regular identifiers. Used by filtdecl and filtknr.
 */

#include "cpp.h"
#include "lexeme.h"
#include <string.h>

/* Simple hash table for typedef names.  31 buckets keeps BSS small;
 * self-compile rarely defines more than ~20 typedefs so chains stay short. */
#define HASH_SIZE 31

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

#ifdef DEBUG
	extern short verbose;
	extern int fdprintf(int, char *, ...);
	if (verbose & 0x100)
		fdprintf(2, "addTypedef(%s) h=%d\n", name, h);
#endif
	/* Check if already present */
	for (t = table[h]; t; t = t->next)
		if (strcmp(t->name, name) == 0)
			return;

	t = (struct tdef *)permalloc(sizeof(*t));
	t->name = permdup(name);
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

#ifdef DEBUG
	extern short verbose;
	extern int fdprintf(int, char *, ...);
	if (verbose & 0x100)
		fdprintf(2, "isTypedef(%s) h=%d\n", name, h);
#endif
	for (t = table[h]; t; t = t->next)
		if (strcmp(t->name, name) == 0) {
#ifdef DEBUG
			if (verbose & 0x100)
				fdprintf(2, "  -> found\n");
#endif
			return 1;
		}
#ifdef DEBUG
	if (verbose & 0x100)
		fdprintf(2, "  -> NOT found\n");
#endif
	return 0;
}

/*
 * Reset table (for new compilation unit).  Entries live in the
 * permanent arena; abandoning them is fine - cpp handles one
 * compilation unit per run.
 */
void
typedefReset(void)
{
	int i;
	for (i = 0; i < HASH_SIZE; i++)
		table[i] = 0;
}
