/*
 * Both operands of an operator needing HL.
 *
 * A value read through an address needs that address in HL to be read
 * through.  Two such operands cannot both be worked out in place: the
 * first is sitting in HL when the second goes to load through it, and
 * it is gone before the operator ever runs.  Nothing spells -(H,H), so
 * no code came out at all and the answer was whatever the second side
 * had left behind.
 *
 * Sethi-Ullman already works the costlier side out first; it just did
 * not count equal costs as worth reordering.  A comparison function is
 * written exactly this way - "*(int *)a - *(int *)b" - so qsort
 * compared nothing and reversed its input instead of sorting it.
 */
#include "rt.h"

short
subp(p, q)
short *p, *q;
{
	return *p - *q;
}

short
addp(p, q)
short *p, *q;
{
	return *p + *q;
}

short
ltp(p, q)
short *p, *q;
{
	return *p < *q;
}

/* the comparison-function spelling, through char * and a cast */
short
cmps(a, b)
char *a, *b;
{
	return *(short *)a - *(short *)b;
}

/* one side through a pointer, the other a step further out */
short
nested(p, q)
short *p, **q;
{
	return *p - **q;
}

main()
{
	short v[2];
	short *pv;

	v[0] = 3;
	v[1] = 9;
	pv = &v[1];

	CHECK(1, subp(&v[0], &v[1]), -6);
	CHECK(2, subp(&v[1], &v[0]), 6);
	CHECK(3, addp(&v[0], &v[1]), 12);
	CHECK(4, ltp(&v[0], &v[1]), 1);
	CHECK(5, ltp(&v[1], &v[0]), 0);
	CHECK(6, cmps((char *)&v[0], (char *)&v[1]), -6);
	CHECK(7, nested(&v[0], &pv), -6);
	return 0;
}
