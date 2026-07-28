/*
 * Locals declared in a nested block.
 *
 * A function's frame is one area, and blocks inside it are not live at
 * the same time, so their locals can share it: in
 *
 *	if (a) { int b; ... } else { int c; ... }
 *
 * b and c want the same offset, and a block nested inside another wants
 * to sit above it rather than beside it.
 *
 * None of this used to happen.  Locals declared in a block never
 * reached the function's list at all - capLocals only ever sees the
 * level it is called at, and by then the block has been popped - so no
 * slot was allocated, the variable was addressed at (iy+0), and writing
 * to it overwrote the saved IY.  The function then returned into
 * nowhere.  Two test files in this directory had to declare their
 * locals at the top and say why.
 *
 * Two things a block local still cannot do, both older than the hoist
 * and neither checked here.  Shadowing an outer name renames the
 * declaration but not the references, so the inner variable is the
 * outer one.  And taking its address yields its value instead - "vp =
 * &v" stores what v holds.  Both were wrong before as well, and more
 * destructively, since there was no slot at all to be wrong about.
 */
#include "rt.h"

short g, g2;

/* siblings: one slot between them */
short
either(a) short a;
{
	short r;

	r = 0;
	if (a) {
		short b;
		b = a * 2;
		r = b;
	} else {
		short c;
		c = a - 7;
		r = c;
	}
	return r;
}

/* siblings under the same name, which are still two variables */
short
twice(a) short a;
{
	short r;

	r = 0;
	{ short b; b = 10; r += b; }
	{ short b; b = 20; r += b; }
	return r;
}

/* nested, where the inner one has to sit above the outer */
short
deep(a) short a;
{
	short r;

	r = 0;
	{
		short x;
		x = a;
		{
			short y;
			y = x + 1;
			r = y;
		}
		r += x;			/* x has to have survived the inner block */
	}
	return r;
}

/* a block inside a loop, entered more than once */
short
looped(n) short n;
{
	short i, t;

	t = 0;
	for (i = 0; i < n; i++) {
		short step;
		step = i + 1;
		t += step;
	}
	return t;
}

/* three siblings, all sharing, with something after them */
short
three(a) short a;
{
	short r;

	r = 0;
	{ short p; p = 1; r += p; }
	{ short q; q = 2; r += q; }
	{ short s; s = 4; r += s; }
	r += a;
	return r;
}

/* an array in a block, which is allocated below the save slots */
short
arrayblk(a) short a;
{
	short r;

	r = 0;
	{
		short arr[4];
		short i;

		for (i = 0; i < 4; i++)
			arr[i] = i * a;
		r = arr[3];
	}
	return r;
}

main()
{
	CHECK(1, either(5), 10);
	CHECK(2, either(0), -7);
	CHECK(3, either(1), 2);

	CHECK(4, twice(0), 30);
	CHECK(5, twice(9), 30);

	CHECK(6, deep(3), 7);		/* (3+1) + 3 */
	CHECK(7, deep(0), 1);		/* (0+1) + 0 */
	CHECK(8, deep(-1), -1);		/* (-1+1) + -1 */

	CHECK(9, looped(0), 0);
	CHECK(10, looped(1), 1);
	CHECK(11, looped(4), 10);	/* 1+2+3+4 */

	CHECK(12, three(0), 7);
	CHECK(13, three(10), 17);

	CHECK(14, arrayblk(2), 6);
	CHECK(15, arrayblk(0), 0);

	/* the frame survived all of that, so the caller is still here */
	g = 1234;
	CHECK(16, g, 1234);
	g2 = either(3) + twice(0) + deep(1);
	CHECK(17, g2, 6 + 30 + 3);

	return 0;
}
