/*
 * Shapes taken from real code that pass2 could not compile.
 *
 * These are not interesting as programs.  Each one is the smallest
 * thing that reproduces a marker found in the tree's own sources, kept
 * because reproducing one of these took longer than writing it: the
 * failing shape is usually a particular combination of storage class,
 * width and register that the obvious test does not reach.  Two
 * attempts at the first one below compiled cleanly before the third
 * matched what lexread.c actually does.
 *
 * Where a case came from is recorded with it, so if it regresses the
 * original is findable.
 */
#include "rt.h"

/*
 * pass1/lexread.c readByte(): a static byte array indexed by a static
 * int that steps.  The array's address goes to HL and the step comes
 * back in HL too, so it added HL to itself.  Stepping a global is
 * costed like a call now, which is what puts it first.
 */
static unsigned char lexBuf[512];
static int lexPos = 0;
static int lexValid = 0;

static unsigned char
readByte()
{
	if (lexPos >= lexValid)
		return 0;
	return lexBuf[lexPos++];
}

/* the same with the step before the subscript, and going down */
static int wpos;
static unsigned char wbuf[8];

static unsigned char
readBack()
{
	return wbuf[--wpos];
}

/* stepping a frame slot instead, which the (iy+d) forms handle in
 * place and which must keep the shorter code it already had */
static unsigned char
localstep(n) short n;
{
	short p;
	unsigned char t;

	p = 0;
	t = 0;
	while (p < n)
		t += wbuf[p++];
	return t;
}

/*
 * libcpm/time.c: the address of a local struct handed to a function.
 * This used to pass the first two bytes of the struct instead.
 */
struct tod { short lo, hi; };

short
sumtod(t) struct tod *t;
{
	return t->lo + t->hi;
}

short
bytod()
{
	struct tod lt;

	lt.lo = 3;
	lt.hi = 4;
	return sumtod(&lt);
}

main()
{
	lexBuf[0] = 10; lexBuf[1] = 20; lexBuf[2] = 30;
	lexPos = 0;
	lexValid = 3;
	CHECK(1, readByte(), 10);
	CHECK(2, readByte(), 20);
	CHECK(3, readByte(), 30);
	CHECK(4, readByte(), 0);
	CHECK(5, lexPos, 3);

	wbuf[0] = 1; wbuf[1] = 2; wbuf[2] = 4; wbuf[3] = 8;
	wpos = 3;
	CHECK(6, readBack(), 4);
	CHECK(7, wpos, 2);
	CHECK(8, readBack(), 2);
	CHECK(9, wpos, 1);

	CHECK(10, localstep(0), 0);
	CHECK(11, localstep(1), 1);
	CHECK(12, localstep(4), 15);

	CHECK(13, bytod(), 7);

	return 0;
}
