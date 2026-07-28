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

/*
 * A block local shadowing an outer name.  Both are called v and both
 * are in the one list of the function's locals, so looking one up by
 * name alone found whichever came first - and put both of them in the
 * outer one's register.  Level and block tell them apart.
 */
short
shadow(a) short a;
{
	short v;

	v = 1;
	{
		short v;

		v = 100;
		g = v;
	}
	return v;
}

/* shadowed twice over, and the middle one read after the inner block */
short
shadow2(a) short a;
{
	short v;

	v = 1;
	{
		short v;

		v = 2;
		{
			short v;

			v = 3;
			g = v;
		}
		g2 = v;			/* the middle one, not the inner */
	}
	return v;			/* the outer one */
}

/* a parameter shadowed by a block local */
short
shadowp(v) short v;
{
	short t;

	{
		short v;

		v = 50;
		t = v;
	}
	return t + v;			/* the block's 50 plus the parameter */
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

	g = 0; g2 = 0;
	CHECK(16, shadow(0), 1);
	CHECK(17, g, 100);
	g = 0; g2 = 0;
	CHECK(18, shadow2(0), 1);
	CHECK(19, g, 3);
	CHECK(20, g2, 2);
	CHECK(21, shadowp(7), 57);
	CHECK(22, shadowp(0), 50);

	/* the frame survived all of that, so the caller is still here */
	g = 1234;
	CHECK(23, g, 1234);
	g2 = either(3) + twice(0) + deep(1);
	CHECK(24, g2, 6 + 30 + 3);

	return 0;
}
