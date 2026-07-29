/*
 * Shifting by a constant count.
 *
 * The left shifts have always taken any count, repeating "add hl,hl"
 * as many times as asked.  The right shifts had a form for one to four
 * and another for exactly eight, and nothing else - so a shift by
 * five, six, seven, or more than eight matched no rule and emitted no
 * code at all.  Eleven places in this tree were doing it.
 *
 * Signed and unsigned are different instructions - sra brings the sign
 * down, srl brings in zero - so each count is tried on a value with
 * its top bit set.
 */
#include "rt.h"

short a;
unsigned short u;

main()
{
	register short r;
	register unsigned short ur;

	/* right shift by every count, signed */
	a = 0x4000;
	CHECK(1, a >> 1, 0x2000);
	CHECK(2, a >> 4, 0x0400);
	CHECK(3, a >> 5, 0x0200);	/* past M, which stopped at four */
	CHECK(4, a >> 6, 0x0100);
	CHECK(5, a >> 7, 0x0080);
	CHECK(6, a >> 8, 0x0040);
	CHECK(7, a >> 9, 0x0020);
	CHECK(8, a >> 14, 1);
	a = -256;
	CHECK(9, a >> 5, -8);		/* the sign has to come down */
	CHECK(10, a >> 8, -1);
	CHECK(11, a >> 9, -1);

	/* and unsigned, where it must not */
	u = 0x8000;
	CHECK(12, u >> 5, 0x0400);
	CHECK(13, u >> 8, 0x0080);
	CHECK(14, u >> 15, 1);
	u = 0xffff;
	CHECK(15, u >> 5, 0x07ff);
	CHECK(16, u >> 12, 0x000f);

	/* the same out of a register variable */
	r = 0x4000;
	CHECK(17, r >> 5, 0x0200);
	CHECK(18, r >> 6, 0x0100);
	CHECK(19, r >> 9, 0x0020);
	r = -256;
	CHECK(20, r >> 5, -8);
	CHECK(21, r >> 9, -1);
	ur = 0xffff;
	CHECK(22, ur >> 5, 0x07ff);
	CHECK(23, ur >> 12, 0x000f);

	/* left shift, which always took any count */
	a = 1;
	CHECK(24, a << 5, 32);
	CHECK(25, a << 9, 512);
	r = 1;
	CHECK(26, r << 5, 32);
	CHECK(27, r << 9, 512);

	return 0;
}
