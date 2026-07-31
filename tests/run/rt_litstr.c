/*
 * A string literal stored where the address has to be worked out.
 *
 * pass1 turns a literal into a SYMREF for the label it emitted, and a
 * SYMREF is left unreduced so the load and store rules can use it as
 * an address.  Where its *value* is wanted - and the value of a
 * literal is its address - it has to be loaded, and three store
 * shapes had no form for that:
 *
 *	arr[i] = "lit";		stored nothing
 *	sp->f  = "lit";		stored nothing
 *
 * A constant subscript folds to a plain symbol store and was always
 * right, and any value that is not a literal is in a register by the
 * time the store happens.  So it took a literal *and* a subscript
 * that had to be computed, which is why it survived: the obvious
 * spellings of both halves are fine.
 *
 * This one did leave XXXXXX markers - it was found by probing around
 * the empty-macro bug rather than by the marker count, because none
 * of the tree's own sources take the shape.
 */
#include "rt.h"

struct ent {
	char *name;
	short v;
};

char *arr[8];
struct ent tab[4];
struct ent *ep;
char *p;
short i;

main()
{
	short n;

	for (n = 0; n < 8; n++)
		arr[n] = 0;

	/* a computed subscript into a global array */
	i = 0;
	arr[i] = "zero";
	i = 3;
	arr[i] = "three";
	CHECK(1, strcmp(arr[0], "zero"), 0);
	CHECK(2, strcmp(arr[3], "three"), 0);
	CHECK(3, arr[1] == 0, 1);

	/* a constant subscript, which always worked */
	arr[5] = "five";
	CHECK(4, strcmp(arr[5], "five"), 0);

	/* through a pointer to a struct */
	ep = &tab[0];
	ep->name = "first";
	CHECK(5, strcmp(tab[0].name, "first"), 0);
	ep = &tab[2];
	ep->name = "third";
	CHECK(6, strcmp(tab[2].name, "third"), 0);

	/* a computed subscript reaching a struct member */
	i = 1;
	tab[i].name = "second";
	CHECK(7, strcmp(tab[1].name, "second"), 0);

	/* a plain variable, and a member of a plain struct */
	p = "plain";
	CHECK(8, strcmp(p, "plain"), 0);
	tab[0].v = 9;
	CHECK(9, tab[0].v, 9);

	/* post-incremented subscript, which is how macdefine fills its
	 * parameter list - the shape this was found under */
	i = 0;
	arr[i++] = "a";
	arr[i++] = "b";
	CHECK(10, i, 2);
	CHECK(11, strcmp(arr[0], "a"), 0);
	CHECK(12, strcmp(arr[1], "b"), 0);

	return 0;
}
