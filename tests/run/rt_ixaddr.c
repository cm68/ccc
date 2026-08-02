/*
 * A register-pointer-plus-constant, stored as a value.
 *
 * "g = a + 2" with a in IX reduces to an INDEX node - the (ix+2)
 * form the addressing rules mint for parents that dereference it.
 * The store to a global was the one context with no rule for the
 * form as a NUMBER: the statement fell to the incomplete-rewrite
 * marker, which is a comment the assembler ignores, and the store
 * silently vanished.  cpp's argument scanner was the first tree
 * code to write the shape - every -I path it registered was lost
 * and no include ever resolved under the simulator.
 *
 * The frame-store, argument, comparison and return uses were
 * already materialized on other paths; they are here to keep them
 * honest too.
 */
#include "rt.h"

char *g;
char buf[8];

int
take(p)
char *p;
{
	return p - buf;
}

int
run(p)
char *p;
{
	register char *a = p;
	char *q;

	g = a + 2;
	q = a + 3;
	if (take(a + 4) != 4)
		return 90;
	if (a + 5 == g)
		return 91;
	if (q - buf != 3)
		return 92;
	return take(a + 6);
}

/*
 * (appended) The value OF a store through a computed slot.  The
 * short store form left HL one past the slot, and a chain that
 * consumed the value - "d = ap->init = p" - filed that address
 * into d; filtdecl's initializer copy then walked the assigns
 * table writing tokens over everything after it.
 */
struct slot {
	char *nm;
	char *init;
	int len;
};
struct slot slots[3];
int nslot;

char *
chainstore(p)
char *p;
{
	struct slot *ap;
	char *d;

	ap = &slots[nslot++];
	ap->nm = p;
	d = ap->init = p + 3;
	return d;
}

char *tab[4];
int ntab;

int
fill(p)
char *p;
{
	register char *a = p;

	/* the staged store: address of the slot waits on the stack
	 * while the value is formed.  An INDEX right emitted nothing
	 * and the slot was filed into itself. */
	tab[ntab++] = a + 2;
	tab[ntab++] = a + 5;
	return 0;
}

int
main()
{
	g = 0;
	CHECK(1, run(buf), 6);
	CHECK(2, g - buf, 2);

	ntab = 0;
	fill(buf);
	CHECK(7, ntab, 2);
	CHECK(8, tab[0] - buf, 2);
	CHECK(9, tab[1] - buf, 5);

	nslot = 0;
	{
		char *d = chainstore(buf);
		CHECK(14, d - buf, 3);
		CHECK(15, slots[0].init - buf, 3);
		CHECK(16, nslot, 1);
	}

	/* both register homes occupied: same shape, other register */
	{
		register char *a = buf;
		register int acc = 0;

		g = a + 7;
		acc = g - buf;
		CHECK(3, acc, 7);
	}
	return 0;
}
