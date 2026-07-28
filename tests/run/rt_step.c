/*
 * Stepping a byte.
 *
 * There is no inc (nn) on this machine, so a byte at a global gets
 * its address into HL and is stepped in memory.  Which side of the
 * step the load falls on is the whole difference between a prefix and
 * a postfix, and a statement wants neither - so there are three forms
 * of each and the one that is picked depends on what the answer is
 * for.  Getting that wrong is silent: the statement form leaves the
 * address in HL, and whatever wanted a value takes that instead.
 */
#include "rt.h"

char g;
unsigned char ug;
short r;

main()
{
	char l;
	short i;

	/* a global, as statements */
	g = 5;
	g++;
	CHECK(1, g != 6, 0);
	g--;
	CHECK(2, g != 5, 0);
	++g;
	CHECK(3, g != 6, 0);
	--g;
	CHECK(4, g != 5, 0);

	/* a global, for its value - postfix is before, prefix is after */
	g = 5;
	r = g++;
	CHECK(5, r != 5, 0);
	CHECK(6, g != 6, 0);
	r = g--;
	CHECK(7, r != 6, 0);
	CHECK(8, g != 5, 0);
	r = ++g;
	CHECK(9, r != 6, 0);
	r = --g;
	CHECK(10, r != 5, 0);

	/* in a condition, which tests the value not the step */
	g = 1;
	r = 0; if (g--) r = 1;
	CHECK(11, r != 1, 0);
	CHECK(12, g != 0, 0);
	r = 0; if (g--) r = 1;
	CHECK(13, r != 0, 0);

	/* wrapping, where the step touches the whole byte */
	ug = 255;
	ug++;
	CHECK(14, (ug & 0xff) != 0, 0);
	ug--;
	CHECK(15, (ug & 0xff) != 255, 0);

	/* a byte in the frame */
	l = 5;
	i = l++;
	CHECK(16, i != 5, 0);
	CHECK(17, l != 6, 0);
	i = l--;
	CHECK(18, i != 6, 0);
	CHECK(19, l != 5, 0);
	l++;
	CHECK(20, l != 6, 0);

	return 0;
}
