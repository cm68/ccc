/*
 * Long-valued conditional operator.  Historically suspected of
 * miscompiling (cpp's evaluators still spell theirs if/else); the
 * garbage traced to the >8-byte call cleanup instead, but nothing
 * pinned the long-valued forms themselves until now.
 */
#include "rt.h"

long gv;
short which;

long tern(short c, long a, long b) { return c ? a : b; }
long tconst(short c) { return c ? 55L : 66L; }
long tmix(short c, long a) { return c ? a + 1 : gv - 1; }

long bump(long x) { which++; return x; }

main()
{
	long m, v;
	short c;

	CHECK(1, (int)tern(1, 55L, 66L), 55);
	CHECK(2, (int)tern(0, 55L, 66L), 66);
	CHECK(3, (int)tconst(1), 55);
	CHECK(4, (int)tconst(0), 66);
	gv = 100000L;
	CHECK(5, tmix(0, 5L) == 99999L, 1);
	CHECK(6, tmix(1, 70000L) == 70001L, 1);

	/* the shape cpp's evaluator wanted to write */
	c = 1;
	m = 0x12345L;
	v = c ? m : bump(9L);
	CHECK(7, v == 0x12345L, 1);
	CHECK(8, which, 0);
	c = 0;
	v = c ? m : bump(0x54321L);
	CHECK(9, v == 0x54321L, 1);
	CHECK(10, which, 1);

	/* ternary as a subexpression, both widths */
	v = (c ? 1L : 2L) + 10;
	CHECK(11, (int)v, 12);
	CHECK(12, (int)((gv ? 3L : gv) >> 1), 1);
	return 0;
}
