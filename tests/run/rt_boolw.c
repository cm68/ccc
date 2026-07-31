/*
 * The width the two sides of && and || are tested at.
 *
 * Each side is tested against zero, separately, with a short-circuit
 * between - so there is no common width for them to meet at.  The
 * node's own type is uchar, because the answer is 0 or 1, and pass1
 * was converting both operands to that.  Narrowing a value to a byte
 * drops exactly the bytes a zero test needs:
 *
 *	256 && 1	was false
 *	if (p && ...)	was false whenever p was 0x??00
 *
 * "if (p)" on its own was always right, and so was "if (!p)" - only
 * inside a boolean chain did it go wrong, which is what made it look
 * like a pointer problem rather than a width one.
 *
 * Found in cpp.  Its conditional stack is a malloc'd list, and the
 * skip in gettoken reads "cond && !(cond->flags & C_TRUE)".  When the
 * allocation happened to land on an address with a zero low byte,
 * cond tested as null, the skip never ran, and every #if in the file
 * leaked its body - including the ones guarding debug code.  Which
 * allocation you got depended on how much had been allocated before,
 * so it moved with the length of the input and with any change to the
 * binary, including adding a printf to look at it.
 */
#include "rt.h"

struct node {
	unsigned char f;
	struct node *next;
};

int x, y;
struct node *p;
struct node one;

short
both(a, b)
short a; short b;
{
	if (a && b)
		return 1;
	return 0;
}

short
either(a, b)
short a; short b;
{
	if (a || b)
		return 1;
	return 0;
}

short
ptrand()
{
	if (p && p->f)
		return 1;
	return 0;
}

main()
{
	/* a value whose low byte is zero is still true */
	CHECK(1, both(256, 1), 1);
	CHECK(2, both(1, 256), 1);
	CHECK(3, both(256, 512), 1);
	CHECK(4, both(1, 1), 1);
	CHECK(5, both(0, 1), 0);
	CHECK(6, both(1, 0), 0);
	CHECK(7, both(0x100, 0x100), 1);

	CHECK(8, either(256, 0), 1);
	CHECK(9, either(0, 256), 1);
	CHECK(10, either(0, 0), 0);
	CHECK(11, either(0x200, 0), 1);

	/* the same as an expression rather than a condition */
	x = 256; y = 1;
	CHECK(12, x && y, 1);
	CHECK(13, x || 0, 1);
	CHECK(14, !x, 0);

	/* three deep, so the middle one is neither first nor last */
	CHECK(15, (256 && 512 && 1) ? 1 : 0, 1);
	CHECK(16, (1 && 256 && 0) ? 1 : 0, 0);

	/* a pointer in a chain, which is where this was found */
	one.f = 7;
	p = &one;
	CHECK(17, ptrand(), 1);
	p = 0;
	CHECK(18, ptrand(), 0);

	/* and the plain tests, which were always right - keep them so */
	p = &one;
	CHECK(19, p ? 1 : 0, 1);
	CHECK(20, !p, 0);
	p = 0;
	CHECK(21, p ? 1 : 0, 0);
	CHECK(22, !p, 1);

	return 0;
}
