/*
 * A conditional operator with a constant test, folded.
 *
 * Nothing folded one, and nothing said so.  A static initializer has
 * to be constant, so an unfolded ?: left the whole expression
 * non-constant and the initializer quietly emitted zero.  cpp's
 * folder hides it for short spellings - it answers them before pass1
 * sees them - so it only surfaced when an expression grew past the
 * folder's save buffer and came back to pass1 to be folded: every
 * packed path byte in pass2's rule table came out nought, and the
 * self-built compiler matched rules that were not there.
 */
#include "rt.h"

#define SUBR 0x40
#define PACK(l, r, d, rlo) \
	((l) | ((r) << 2) | ((d) << 4) | ((rlo) ? SUBR : 0))

unsigned char packed[3];
short chosen[4];
long lsel[2];

short pick(short c) { return c ? 11 : 22; }

main()
{
	short i;

	/* the shape that found it: a ternary inside an or-chain */
	packed[0] = PACK(0, 1, 0, 0);
	packed[1] = PACK(1, 0, 2, 0);
	packed[2] = PACK(3, 2, 1, 234);
	CHECK(1, packed[0], 4);
	CHECK(2, packed[1], 33);
	CHECK(3, packed[2], 91);

	/* a bare constant ternary, and one on each side of an operator */
	chosen[0] = 1 ? 5 : 6;
	chosen[1] = 0 ? 5 : 6;
	chosen[2] = (1 ? 2 : 3) + (0 ? 40 : 50);
	chosen[3] = (2 > 1 ? 8 : 9) * 10;
	CHECK(4, chosen[0], 5);
	CHECK(5, chosen[1], 6);
	CHECK(6, chosen[2], 52);
	CHECK(7, chosen[3], 80);

	/* the arms still evaluate at run time when the test is not
	 * constant - folding must not eat the live form */
	i = 1;
	CHECK(8, pick(i), 11);
	i = 0;
	CHECK(9, pick(i), 22);

	/* long arms, folded */
	lsel[0] = 1 ? 100000L : 7L;
	lsel[1] = 0 ? 100000L : 7L;
	CHECK(10, lsel[0] == 100000L, 1);
	CHECK(11, lsel[1] == 7L, 1);

	return 0;
}
