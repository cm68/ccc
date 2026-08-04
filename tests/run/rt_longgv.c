/*
 * An initialised long global is four bytes.
 *
 * A byte was emitted for a byte and a word for everything else, so an
 * initialised long global was half a long.  Whatever came next in the
 * data segment sat where its high word belonged, and reading it back
 * gave that variable in the top half:
 *
 *	long base = 500;
 *	int  pos  = 23;
 *
 * made base 0x001701F4.  An uninitialised one was never wrong - it
 * goes to bss, which is sized from the type.
 *
 * The neighbours below are the point of the test: a long on its own
 * would read correctly even at two bytes if what followed happened to
 * be zero.  Each of these has something non-zero after it.
 */
#include "rt.h"

long small = 500;
short after1 = 0x1234;

long big = 0x12345678L;
short after2 = 0x5678;

long neg = -2L;
short after3 = 0x7f7f;

long zero = 0L;
short after4 = 0x4321;

long uninit;			/* bss: sized from the type, always was */

/* a long between two others, so both edges are covered */
long lo = 1L;
long mid = 0x00010000L;		/* high word set, low word zero */
long hi = 2L;

main()
{
	CHECK(1, small, 500L);
	CHECK(2, after1, 0x1234);
	CHECK(3, big, 0x12345678L);
	CHECK(4, after2, 0x5678);
	CHECK(5, neg, -2L);
	CHECK(6, after3, 0x7f7f);
	CHECK(7, zero, 0L);
	CHECK(8, after4, 0x4321);
	CHECK(9, uninit, 0L);
	CHECK(10, lo, 1L);
	CHECK(11, mid, 0x00010000L);
	CHECK(12, hi, 2L);
	return 0;
}
