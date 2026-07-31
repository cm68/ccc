/*
 * A static initializer that names an element other than the first.
 *
 * "&arr[0]" folds to the bare symbol and was emitted as one.
 * "&arr[n]" keeps its offset and is a PLUS, which matched neither the
 * constant branch nor the symbol branch of the initializer streamer,
 * so it took the unsupported branch and was written as zero.
 *
 * pass1's own type table is a chain built exactly that way:
 *
 *	{ 2, 0, 0, 0, 0, &basictypes[0] },
 *	{ 4, 0, 0, 0, 0, &basictypes[1] },
 *
 * so every link past the first was null, and inttype, longtype and
 * voidtype - which are &basictypes[1], [2] and [6] - were all zero.
 * The c0 that ccc built could not name any type it had not been handed
 * by a typedef: "extern void f(void *)" came back "fn array".
 *
 * The assembler takes label+offset, which is all this needed to say.
 */
#include "rt.h"

struct ent {
	short v;
	struct ent *next;
};

struct ent tab[4];

/* the shape pass1's basictypes has: a chain through the array */
struct ent *e0 = &tab[0];
struct ent *e1 = &tab[1];
struct ent *e2 = &tab[2];
struct ent *e3 = &tab[3];

short arr[6];
short *a0 = &arr[0];
short *a3 = &arr[3];
short *a5 = &arr[5];

char cbuf[8];
char *c0p = &cbuf[0];
char *c7p = &cbuf[7];

main()
{
	short i;

	for (i = 0; i < 4; i++)
		tab[i].v = i + 10;
	for (i = 0; i < 6; i++)
		arr[i] = i + 100;
	for (i = 0; i < 8; i++)
		cbuf[i] = i + 'a';

	/* each pointer must name its own element */
	CHECK(1, e0->v, 10);
	CHECK(2, e1->v, 11);
	CHECK(3, e2->v, 12);
	CHECK(4, e3->v, 13);

	/* and they must be distinct - the bug made them all zero */
	CHECK(5, e0 != e1, 1);
	CHECK(6, e1 != e2, 1);
	CHECK(7, e0 != 0, 1);
	CHECK(8, e3 != 0, 1);

	/* the offsets have to scale by the element size */
	CHECK(9, e1 - e0, 1);
	CHECK(10, e3 - e0, 3);

	CHECK(11, *a0, 100);
	CHECK(12, *a3, 103);
	CHECK(13, *a5, 105);
	CHECK(14, a3 - a0, 3);

	CHECK(15, *c0p, 'a');
	CHECK(16, *c7p, 'h');
	CHECK(17, c7p - c0p, 7);

	return 0;
}
