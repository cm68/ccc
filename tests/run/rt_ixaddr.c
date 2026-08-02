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

int
main()
{
	g = 0;
	CHECK(1, run(buf), 6);
	CHECK(2, g - buf, 2);

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
