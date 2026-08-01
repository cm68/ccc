/*
 * cpp's intern pool, faithfully: the 31x hash, the modulo, the
 * 127-way bucket array, embedded-name entries, and a vocabulary
 * big enough to visit high hash values.  The Z80 cpp corrupts
 * intern entries with something that writes pointer-shaped values
 * where ids and chain links live; an out-of-range bucket index
 * would do exactly that, so the index is checked on every insert
 * and every name must still be findable - and correctly id'd -
 * after the whole vocabulary is in.
 */
#include "rt.h"

#define HASHN 127
struct ient {
	unsigned short id;
	struct ient *next;
	char name[1];
};
struct ient *pool[HASHN];
unsigned short nextid = 1;

char arena[8192];
int used;

char *
grab(int n)
{
	char *p = arena + used;
	int i;

	used += n;
	if (used > 8192)
		return 0;
	for (i = 0; i < n; i++)
		p[i] = 0;
	return p;
}

int lasth;

char *
intern(char *s)
{
	unsigned h = 0;
	char *p;
	struct ient *e;

	for (p = s; *p; p++)
		h = h * 31 + (unsigned char)*p;
	h %= HASHN;
	lasth = h;
	if (h >= HASHN)
		return 0;	/* caught: index out of range */
	for (e = pool[h]; e; e = e->next)
		if (strcmp(e->name, s) == 0)
			return e->name;
	e = (struct ient *)grab(sizeof(*e) + strlen(s));
	if (!e)
		return 0;
	strcpy(e->name, s);
	e->next = pool[h];
	pool[h] = e;
	return e->name;
}

unsigned short
idOf(char *s)
{
	struct ient *e;
	char *c;

	c = intern(s);
	if (!c)
		return 0xffff;
	e = (struct ient *)(c - ((char *)&((struct ient *)0)->name - (char *)0));
	if (e->id == 0)
		e->id = nextid++;
	return e->id;
}

/* names shaped like the real vocabulary: mixed case, underscores,
 * lengths 2..14 - long names drive the hash through many wraps */
char nm[16];

void
mkname(int i)
{
	int len, j;

	len = 2 + (i * 7) % 13;
	for (j = 0; j < len; j++) {
		int k = (i * 11 + j * 5) % 38;
		if (k < 26)
			nm[j] = 'a' + k;
		else if (k < 36)
			nm[j] = 'A' + (k - 26);
		else if (k == 36)
			nm[j] = '_';
		else
			nm[j] = '0' + (i % 10);
	}
	nm[len] = 0;
}

int
main()
{
	int i;
	unsigned short id;

	for (i = 0; i < 250; i++) {
		mkname(i);
		id = idOf(nm);
		if (id == 0xffff)
			return 1;	/* bad bucket or arena full */
		if (lasth < 0 || lasth >= HASHN)
			return 2;
	}
	/* every name still resolves to its original id */
	for (i = 0; i < 250; i++) {
		mkname(i);
		id = idOf(nm);
		if (id == 0 || id >= nextid)
			return 3;
	}
	/* no two of the first ten share an id */
	{
		unsigned short ids[10];
		int j;

		for (i = 0; i < 10; i++) {
			mkname(i);
			ids[i] = idOf(nm);
		}
		for (i = 0; i < 10; i++)
			for (j = i + 1; j < 10; j++)
				if (ids[i] == ids[j])
					return 4;
	}
	return 0;
}
