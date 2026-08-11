/*
 * Comparing two pointers that straddle 0x8000.
 *
 * Pointers have no type of their own by the time the code generator
 * sees them - pass2 knows b B s S l L v and nothing else - so pass1
 * decides their signedness when it writes the AST.  It wrote 's', and
 * every pointer comparison in the compiler's life came out signed.
 * Two addresses either side of 0x8000 therefore compared backwards:
 * 0x8008 is -32760 and 0x7ff6 is +32758.
 *
 * That is not an edge case on a machine whose heap grows up through
 * the middle of the address space.  malloc guards its block search
 * with "p+nw >= p" to catch address wraparound, and once the heap
 * passed 0x8000 the guard refused every free block above it - for
 * good.  The linker could not link the peephole optimiser: one
 * fourteen byte request grew the arena 246 times, 31,734 bytes, and
 * never took the 162 byte block it kept being offered.  See
 * SIGNEDPOINTER.
 *
 * The heap has to be walked up past 0x8000 for this to bite, which is
 * why nothing caught it before: a small program never gets there.
 */
#include "rt.h"

extern char *	malloc();

char lowbuf[8];

main()
{
	char *	lo;
	char *	hi;
	char *	p;
	short	i;

	lo = lowbuf;			/* data: low */
	hi = lowbuf;

	/*
	 * Climb past 0x8000, which on this machine takes about thirty
	 * kilobytes: the heap starts a little above the program, and the
	 * bug only bites once an address has the top bit set.  On the
	 * host it is nothing at all and the checks below still hold.  Compared as numbers rather than as
	 * pointers, so that finding the high address does not depend on
	 * the thing being tested.
	 */
	for (i = 0; i < 70; i++) {
		p = malloc(512);
		if ((unsigned)p > (unsigned)hi)
			hi = p;
	}

	/* the plain facts: an address in data is below one in the heap */
	CHECK(1, lo < hi, 1);
	CHECK(2, hi > lo, 1);
	CHECK(3, lo > hi, 0);
	CHECK(4, hi < lo, 0);
	CHECK(5, lo <= hi, 1);
	CHECK(6, hi >= lo, 1);

	/*
	 * The comparison must agree with the same question asked of the
	 * addresses as numbers.
	 *
	 * No check here compares a pointer with itself: "p >= p" has no
	 * rule in pass2 and is refused outright, for signed shorts as
	 * much as for addresses, so it is a separate gap and not this
	 * one.  Nor does it cast an address to long: (unsigned long)p
	 * sign-extends, which is a third thing again.
	 */
	CHECK(7, lo < hi, (unsigned)lo < (unsigned)hi);
	CHECK(8, hi > lo, (unsigned)hi > (unsigned)lo);
	CHECK(9, lo >= hi, (unsigned)lo >= (unsigned)hi);
	CHECK(10, hi <= lo, (unsigned)hi <= (unsigned)lo);

	/*
	 * malloc's own guard, which is what the bug actually broke: does
	 * p + n run past the end of memory?  It does not, and must not
	 * be said to just because p + n crossed 0x8000.
	 */
	p = hi;
	CHECK(11, (p + 18) >= p, 1);
	CHECK(12, p <= (p + 18), 1);

	return 0;
}

/* vim: set tabstop=4 shiftwidth=4 noexpandtab: */
