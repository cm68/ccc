/*
 * A register variable has to survive everything that calls out.
 *
 * The allocator puts locals in B, C, BC and IX, and they are worth
 * having - so anything the compiler emits a call to has to leave them
 * alone or be wrapped in a save.  There are three ways that is true
 * here and each has been wrong at least once:
 *
 *   - the helper does not touch BC at all (lld, swtab, swidx)
 *   - the rule wraps it unconditionally (lainc, always push/pop)
 *   - the rule wraps it with $[ and $], which emit the push and pop
 *     only when a variable actually lives there (amul, adiv, ldiv,
 *     and now lstde)
 *
 * The failures all look the same from C: a variable set before the
 * operation reads back as something belonging to the compiler.  A
 * long store left it holding the return address of the call.  A
 * switch left it holding the leftover count of the dispatch scan.
 * Neither touched the value being computed, so nothing that checked
 * only the result would notice.
 *
 * Each function here keeps 7 in a local across one such operation and
 * reads it back.  7 rather than 0 on purpose: a failed table scan
 * leaves zero in BC, and a test written with 0 passes while the bug
 * is live.
 */
#include "rt.h"

int sink;
long dst;
long *lp;
long lsrc;

/* long store through a global pointer: lstde */
short
stglobal(v)
long v;
{
	short r;

	r = 7;
	*lp = v;
	sink = r;
	return r;
}

/* and through a local one */
short
stlocal(p, v)
long *p; long v;
{
	short r;

	r = 7;
	*p = v;
	sink = r;
	return r;
}

/* integer multiply: amul */
short
bymul(a, b)
short a; short b;
{
	short r;

	r = 7;
	sink = a * b;
	return r;
}

/* integer divide and remainder: adiv, amod */
short
bydiv(a, b)
short a; short b;
{
	short r;

	r = 7;
	sink = a / b;
	sink = a % b;
	return r;
}

/* long increment: lainc */
short
byinc()
{
	short r;

	r = 7;
	lsrc = lsrc + 1;
	return r;
}

/* long divide: ldiv */
short
byldiv(a, b)
long a; long b;
{
	short r;

	r = 7;
	dst = a / b;
	return r;
}

short
other(n)
short n;
{
	return n + 1;
}

/* an ordinary call, which is what docall saves around */
short
bycall()
{
	short r;

	r = 7;
	sink = other(3);
	return r;
}

/* the switch dispatch helpers */
short
bysparse(v)
short v;
{
	short r;

	r = 7;
	switch (v) {			/* sparse: swtab */
	case 0: sink = 1; break;
	case 60: sink = 2; break;
	case 120: sink = 3; break;
	case 200: sink = 4; break;
	}
	return r;
}

short
bydense(v)
short v;
{
	short r;

	r = 7;
	switch (v) {			/* dense: swidx */
	case 10: sink = 1; break;
	case 11: sink = 2; break;
	case 12: sink = 3; break;
	case 13: sink = 4; break;
	case 14: sink = 5; break;
	}
	return r;
}

main()
{
	lp = &dst;

	dst = 0;
	CHECK(1, stglobal(12345L), 7);
	CHECK(2, dst == 12345L, 1);	/* and the store still happened */

	dst = 0;
	CHECK(3, stlocal(&dst, 6789L), 7);
	CHECK(4, dst == 6789L, 1);

	CHECK(5, bymul(6, 7), 7);
	CHECK(6, sink, 42);

	CHECK(7, bydiv(17, 5), 7);
	CHECK(8, sink, 2);		/* 17 % 5 */

	lsrc = 100;
	CHECK(9, byinc(), 7);
	CHECK(10, lsrc == 101L, 1);

	CHECK(11, byldiv(1000L, 8L), 7);
	CHECK(12, dst == 125L, 1);

	CHECK(13, bycall(), 7);
	CHECK(14, sink, 4);

	CHECK(15, bysparse(0), 7);	/* first entry of the scan */
	CHECK(16, bysparse(200), 7);	/* last entry */
	CHECK(17, bysparse(7), 7);	/* scan runs out */
	CHECK(18, bysparse(300), 7);	/* rejected on the high byte */

	CHECK(19, bydense(10), 7);
	CHECK(20, bydense(14), 7);
	CHECK(21, bydense(99), 7);	/* outside the span */

	return 0;
}
