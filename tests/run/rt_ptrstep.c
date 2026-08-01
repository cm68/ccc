/*
 * A byte stepped in place through a pointer - "(*p)++" - with the
 * pointer in a register and on the frame.  The byte forms of the
 * step-through-pointer rules had never existed: cpp's tok_depth
 * counts braces exactly this way, and when the depth counters were
 * narrowed to bytes the ccc-built cpp lost count of its braces and
 * filtknr rewrote call sites as K&R headers.  diffcpp went 13/16
 * differ; this is the shape that did it.
 */
#include "rt.h"

unsigned char d;

void
bump(unsigned char *p, int up)
{
	if (up)
		(*p)++;
	else
		(*p)--;
}

int
main()
{
	unsigned char loc;
	unsigned char *q;

	d = 5;
	bump(&d, 1);
	bump(&d, 1);
	CHECK(1, d, 7);
	bump(&d, 0);
	CHECK(2, d, 6);

	loc = 40;
	q = &loc;
	(*q)++;
	CHECK(3, loc, 41);
	CHECK(4, (*q)++, 41);
	CHECK(5, loc, 42);
	(*q)--;
	CHECK(6, loc, 41);
	return 0;
}
