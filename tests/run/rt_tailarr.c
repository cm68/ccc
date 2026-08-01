/*
 * A struct whose last member is char name[1], allocated oversize and
 * used as a variable-length tail - the K&R flexible-member idiom.
 * cpp's intern pool stores names this way; the Z80 build corrupted
 * every string in it while the host build was fine, so the tail
 * member access itself is under test: &e->name must be e + offset,
 * on every path a pointer can reach it.
 */
#include "rt.h"

struct ent {
	unsigned short id;
	struct ent *next;
	char name[1];
};

char pool[64];
char *pp = pool;

char *
grab(int n)
{
	char *p = pp;
	pp += n;
	return p;
}

struct ent *head;

char *
put(char *s)
{
	struct ent *e;

	e = (struct ent *)grab(sizeof(*e) + strlen(s));
	strcpy(e->name, s);
	e->id = 7;
	e->next = head;
	head = e;
	return e->name;
}

int
main()
{
	char *a, *b;
	struct ent *e;

	a = put("alpha");
	b = put("bx");

	/* the returned tails hold what was stored */
	if (strcmp(a, "alpha") != 0)
		return 1;
	if (strcmp(b, "bx") != 0)
		return 2;

	/* walking the chain reads the same strings through members */
	e = head;
	if (strcmp(e->name, "bx") != 0)
		return 3;
	if (e->id != 7)
		return 4;
	e = e->next;
	if (strcmp(e->name, "alpha") != 0)
		return 5;

	/* the tail pointer is the struct plus the member offset */
	if (a != (char *)head->next +
	    ((char *)&((struct ent *)0)->name - (char *)0))
		return 6;

	/* and lookup walks: strcmp through the member, hash of it */
	{
		unsigned short h;	/* explicit width: wraps alike everywhere */
		char *p;

		for (e = head; e; e = e->next)
			if (strcmp(e->name, "alpha") == 0)
				break;
		if (!e || e != head->next)
			return 7;
		h = 0;
		for (p = e->name; *p; p++)
			h = h * 31 + (unsigned char)*p;
		if (h % 127 != (unsigned short)
		    ((((('a' * 31 + 'l') * 31 + 'p') * 31 + 'h') * 31 + 'a')) % 127)
			return 8;
	}

	return 0;
}
