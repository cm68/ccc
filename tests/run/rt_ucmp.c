/*
 * An unsigned comparison against a small constant.
 *
 * "(unsigned int)p < 0x100" is the natural way to ask whether a
 * pointer is in page zero, and it answered YES for 0xfe9c - which is
 * how a diagnostic written to catch a null pointer fired on a
 * perfectly good stack address instead.
 */
#include "rt.h"

char buf[4];

int
low(p)
char *p;
{
	return (unsigned int)p < 0x100;
}

/*
 * The same question asked of a pointer the allocator homes in a
 * register, which is where the cast was lost: the comparison kept
 * the pointer's own signed width and 0xfe9c came out as -356.
 */
struct tok { int a, b; char *c; long d; };

int
lowreg(t)
register struct tok *t;
{
	t->a = 1;			/* enough traffic to earn a home */
	t->b = 2;
	return (unsigned int)t < 0x100;
}

int
main()
{
	unsigned int u;

	CHECK(1, low((char *)0), 1);
	CHECK(2, low((char *)0xff), 1);
	CHECK(3, low((char *)0x100), 0);
	CHECK(4, low((char *)0xfe9c), 0);	/* a stack address */
	CHECK(5, low(buf), 0);

	/* the same shape without the cast, on a plain unsigned */
	u = 0xfe9c;
	CHECK(6, u < 0x100, 0);
	u = 0x00ff;
	CHECK(7, u < 0x100, 1);
	u = 0x8000;
	CHECK(8, u < 0x100, 0);
	CHECK(9, u > 0x100, 1);
	CHECK(10, u < 0x9000, 1);

	{
		struct tok tk;
		CHECK(11, lowreg(&tk), 0);	/* a stack address is not low */
	}
	return 0;
}

/* vim: set tabstop=4 shiftwidth=4 noexpandtab: */
