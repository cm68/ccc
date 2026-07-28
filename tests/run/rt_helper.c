/*
 * Values that have to survive a helper call.
 *
 * The 16-bit helpers - amul, adiv, amod - take their second operand off
 * the stack with a pop bc and do not put it back, so anything living in
 * BC is gone when they return.  A register variable lives in BC.
 *
 * Nothing said so.  "t = a * a" in a function with a register variable
 * quietly destroyed the variable, and where the variable was the
 * subscript of the loop doing the multiplying, the loop never ended.
 * The long helpers had always saved BC; the short ones never did, and
 * the table cannot tell whether there is anything to save - so the
 * templates ask, with $[ and $], and the answer is worked out where it
 * is known.
 */
#include "rt.h"

short ga, gb;
short arr[6];

/* a register variable held across each of the three */
short
mul(a) short a;
{
	register short r;
	short t;

	r = 5;
	t = a * a;
	return r + t;
}

short
divd(a) short a;
{
	register short r;
	short t;

	r = 7;
	t = a / ga;
	return r + t;
}

short
modu(a) short a;
{
	register short r;
	short t;

	r = 9;
	t = a % ga;
	return r + t;
}

/* the register variable is itself an operand */
short
selfop(a) short a;
{
	register short r;

	r = a;
	return r * ga;
}

short
selfdiv(a) short a;
{
	register short r;

	r = a;
	return r / ga;
}

/* and the shape that found it: the loop subscript does the multiplying */
short
loopmul(n) short n;
{
	register short i;
	short t;

	t = 0;
	for (i = 0; i < n; i++)
		t += i * ga;
	return t;
}

/* the subscript both indexes and multiplies */
short
loopidx(n) short n;
{
	register short i;

	for (i = 0; i < n; i++)
		arr[i] = i * ga;
	return arr[n - 1];
}

main()
{
	ga = 3;

	CHECK(1, mul(4), 21);		/* 5 + 16 */
	CHECK(2, mul(0), 5);
	CHECK(3, divd(9), 10);		/* 7 + 3 */
	CHECK(4, divd(0), 7);
	CHECK(5, modu(10), 10);		/* 9 + 1 */
	CHECK(6, modu(9), 9);

	CHECK(7, selfop(4), 12);
	CHECK(8, selfop(0), 0);
	CHECK(9, selfdiv(9), 3);
	CHECK(10, selfdiv(2), 0);

	CHECK(11, loopmul(0), 0);
	CHECK(12, loopmul(1), 0);
	CHECK(13, loopmul(4), 18);	/* 3*(0+1+2+3) */

	CHECK(14, loopidx(4), 9);	/* arr[3] = 3*3 */
	CHECK(15, arr[0], 0);
	CHECK(16, arr[2], 6);

	/* a helper in the middle of a longer expression */
	ga = 2; gb = 5;
	CHECK(17, gb * ga + gb, 15);
	CHECK(18, gb / ga + gb, 7);
	CHECK(19, gb % ga + gb, 6);

	return 0;
}
