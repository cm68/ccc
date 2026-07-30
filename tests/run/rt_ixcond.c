/*
 * A value in IX used as a condition.
 *
 * HL, DE, BC, A and E each become a typed register node before the
 * rule table is reached, and each has a rule that tests it when a
 * condition is wanted.  IX does not become one - it arrives as a bare
 * CODE node - and there was no rule for it at all.
 *
 * A missing rule normally leaves an XXXXXX marker, which is how these
 * get found.  Not this one: the caller wanted flags and simply took
 * whatever was in them, so nothing was missing from the output and
 * nothing was reported.
 *
 *	while ((top = fstack_top(&fs)) != NULL)
 *
 * with top in IX emitted push hl / pop ix - which sets no flags - and
 * then branched on what the call had left behind.  cpp's filtbrace
 * looped forever on any brace-less if, while or for, which is most of
 * them.  Found by running the cpp that ccc built over cpp's own
 * sources; it hung on the first file.
 *
 * IX is where the allocator puts a pointer that is used for member
 * access, so these functions are written the way that earns it.
 */
#include "rt.h"

struct ent {
	unsigned char a;
	unsigned char b;
};

struct ent pool[4];
int navail;

struct ent *
takeone()
{
	if (navail <= 0)
		return 0;
	navail = navail - 1;
	return &pool[navail];
}

/* the shape that hung: assign a call result to a pointer and test it */
short
drain()
{
	struct ent *top;
	short count;

	count = 0;
	while ((top = takeone()) != 0) {
		count = count + top->a;
		count = count + top->b;
	}
	return count;
}

/* the same test written as an if rather than a while */
short
oneif()
{
	struct ent *top;

	top = takeone();
	if (top)
		return top->a + top->b;
	return -1;
}

/* and negated, which went through a different rule and worked */
short
onenot()
{
	struct ent *top;

	top = takeone();
	if (!top)
		return -1;
	return top->a + top->b;
}

/* a pointer that is null from the start, so the test must say so */
short
neverset()
{
	struct ent *top;
	short n;

	n = 0;
	top = 0;
	while (top != 0) {
		n = n + top->a;
		top = 0;
	}
	return n;
}

main()
{
	short i;

	for (i = 0; i < 4; i++) {
		pool[i].a = 1;
		pool[i].b = 2;
	}

	navail = 4;
	CHECK(1, drain(), 12);		/* four entries at 3 each */

	navail = 0;
	CHECK(2, drain(), 0);		/* the loop must not run at all */

	navail = 1;
	CHECK(3, drain(), 3);

	navail = 1;
	CHECK(4, oneif(), 3);
	navail = 0;
	CHECK(5, oneif(), -1);

	navail = 1;
	CHECK(6, onenot(), 3);
	navail = 0;
	CHECK(7, onenot(), -1);

	CHECK(8, neverset(), 0);

	return 0;
}
