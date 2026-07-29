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

/*
 * By a count only known at runtime.  The Z80 has no variable shift, so
 * this is a loop, and the loop is a different path from every rule
 * above - one that nothing in this file used to reach.
 */
short
vsl(v, n) short v; short n;
{
	return v << n;
}

short
vsr(v, n) short v; short n;
{
	return v >> n;			/* sra: the sign comes down */
}

unsigned short
vur(v, n) unsigned short v; short n;
{
	return v >> n;			/* srl: zero comes in */
}

/*
 * Shifting a variable by a variable, twice, so the allocator puts the
 * value in a register.  That case had neither an inline path nor a
 * rule, and emitted nothing.
 */
short
selfsl(n) short n;
{
	short v;

	v = 3;
	v <<= n;
	v <<= n;
	return v;
}

short
selfsr(n) short n;
{
	short v;

	v = 64;
	v >>= n;
	v >>= n;
	return v;
}

main()
{
	register short r;
	register unsigned short ur;
	short n;

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

	/*
	 * By a count only known at runtime.  Everything above shifts by a
	 * constant, so none of it reached the loop that a variable count
	 * needs - which is why a whole register-variable case of it could
	 * be missing and emit nothing without a test noticing.
	 */
	n = 2;
	a = 3;
	CHECK(28, vsl(a, n), 12);
	CHECK(29, vsr(a, n), 0);
	a = -16;
	CHECK(30, vsr(a, n), -4);	/* signed: sign is carried down */
	ur = 0xfff0;
	CHECK(31, vur(ur, 4), 0x0fff);	/* unsigned: zero comes in */

	/* by zero, which the loop has to skip rather than run once */
	n = 0;
	a = 7;
	CHECK(32, vsl(a, n), 7);
	CHECK(33, vsr(a, n), 7);
	CHECK(34, vur(7, n), 7);

	/* to the ends of the word */
	a = 1;
	CHECK(35, vsl(a, 14), 16384);
	a = 16384;
	CHECK(36, vsr(a, 14), 1);
	ur = 0x8000;
	CHECK(37, vur(ur, 15), 1);

	/* and a variable shifting itself, twice, so it lands in a
	 * register and the count is not the only thing that moves */
	CHECK(38, selfsl(2), 48);
	CHECK(39, selfsr(1), 16);

	return 0;
}
