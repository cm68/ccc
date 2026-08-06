/*
 * A long-returning call with more than eight argument bytes.  The
 * post-call cleanup used the HL form past eight bytes, and its
 * ex de,hl shuffle - safe for a short result - swapped one half of
 * a long result and replaced the other with the byte count.  The
 * constant folder's capply() was the first such call anywhere.
 */
#include "rt.h"

long
cap3(t, a, b)
char t;
long a;
long b;
{
	if (t == 1)
		return a * b;
	if (t == 2)
		return a + b;
	return a - b;
}

long
cap4(a, b, c)
long a;
long b;
long c;
{
	return a + b + c;
}

long ga, gb;

int
main()
{
	long r;

	ga = 8;
	gb = 256;
	r = cap3(1, ga, gb);
	CHECK(1, r == 2048, 1);
	CHECK(2, cap3(2, ga, gb) == 264, 1);
	CHECK(3, cap3(9, gb, ga) == 248, 1);
#ifndef RT_ZC3
	/* twelve argument bytes; zc3 has its own trouble here, and
	 * the bug this file pins is ccc's */
	r = cap4(ga, gb, 100000);
	CHECK(4, r == 100264, 1);
#endif
	return 0;
}
