/*
 * A long return from a function that saved IX.
 *
 * The epilogue restored IX through DE - "preserves HL", said the
 * comment, which was the whole truth back when every return value
 * came back in HL.  A long comes back in HL:DE, so the low word of
 * every such return was the saved IX's address off the frame.
 *
 * Found by the differential: the ccc-built c0 answered "int a[5]"
 * with .ds -27776, because parseConst returns an unsigned long and
 * uses a struct pointer hard enough to be given IX.
 */
#include "rt.h"

struct pair {
	int x;
	int y;
};

struct pair g;

long
lp(struct pair *p)
{
	/* enough member traffic that the allocator homes p in IX */
	p->x = 3;
	p->y = 4;
	p->x += p->y;
	return 70000L + p->x;		/* low word must survive the restore */
}

int
main()
{
	long v;

	v = lp(&g);
	CHECK(1, g.x, 7);
	CHECK(2, v, 70007L);
	return 0;
}
