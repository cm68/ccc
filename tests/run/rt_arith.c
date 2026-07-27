/*
 * Arithmetic, shifts and the integer promotions.
 *
 * Covers the widths meeting each other: a char operand under a short
 * operator has to be sign- or zero-extended depending on its own
 * signedness, and a result stored back into a char keeps only the low
 * byte.  Also the shifts, where a count of eight is a register move
 * and a signed right shift has to carry the sign down.
 */
#include "rt.h"

char c1, c2, c3;
unsigned char u1, u2, u3;
short s, s2;
unsigned short us;

main()
{
	/* byte arithmetic, both operands live */
	c1 = 20; c2 = 7;
	CHECK(1, (short)(c1 + c2), 27);
	CHECK(2, (short)(c1 - c2), 13);
	CHECK(3, (short)(c1 & c2), 4);
	CHECK(4, (short)(c1 | c2), 23);
	CHECK(5, (short)(c1 ^ c2), 19);

	/*
	 * Stored back into a char, so only the low byte is observable.
	 * Masked, because whether plain char is signed is up to the
	 * implementation - it is signed for the host and for zc3 and
	 * unsigned for ccc - and the point here is the truncation, not
	 * the signedness.
	 */
	c1 = 100; c2 = 100;
	c3 = c1 + c2;
	CHECK(6, c3 & 0xff, 200);

	/* a char promoted against a short keeps its sign */
	c1 = -1; s = 1;
	CHECK(7, c1 + s, 0);
	CHECK(8, c1 < s, 1);
	CHECK(9, s + c1, 0);

	/* an unsigned char widens with zeroes instead */
	u1 = 255; s = 1;
	CHECK(10, u1 + s, 256);
	CHECK(11, u1 > s, 1);

	/* narrowing on assignment keeps the low part */
	s = 0x1234;
	c1 = s;
	CHECK(12, c1, 0x34);
	s2 = 0x7fff;
	c2 = s2;
	CHECK(13, c2 & 0xff, 0xff);

	/* multiply, divide, remainder */
	s = 7; s2 = 6;
	CHECK(14, s * s2, 42);
	s = 100; s2 = 7;
	CHECK(15, s / s2, 14);
	CHECK(16, s % s2, 2);
	s = -100; s2 = 7;
	CHECK(17, s / s2, -14);
	CHECK(18, s % s2, -2);

	/* strength-reduced multiplies */
	s = 11;
	CHECK(19, s * 2, 22);
	CHECK(20, s * 3, 33);
	CHECK(21, s * 4, 44);
	CHECK(22, s * 5, 55);
	CHECK(23, s * 6, 66);
	CHECK(24, s * 8, 88);
	CHECK(25, s * 10, 110);

	/* shifts by a constant */
	s = 1;
	CHECK(26, s << 1, 2);
	CHECK(27, s << 4, 16);
	CHECK(28, s << 8, 256);
	s = 0x1234;
	CHECK(29, s >> 8, 0x12);
	CHECK(30, s >> 4, 0x123);

	/* a signed right shift carries the sign down */
	s = -256;
	CHECK(31, s >> 8, -1);
	s = -2;
	CHECK(32, s >> 1, -1);

	/* an unsigned one does not */
	us = 0xff00;
	CHECK(33, us >> 8, 0xff);
	us = 0x8000;
	CHECK(34, us >> 1, 0x4000);

	/* shifts by a value only known at run time */
	s = 1; s2 = 5;
	CHECK(35, s << s2, 32);
	s = -256; s2 = 8;
	CHECK(36, s >> s2, -1);
	us = 0xff00; s2 = 8;
	CHECK(37, us >> s2, 0xff);

	/* a count of zero has to leave the value alone */
	s = 1234; s2 = 0;
	CHECK(38, s << s2, 1234);
	CHECK(39, s >> s2, 1234);

	/* unary */
	s = 5;
	CHECK(40, -s, -5);
	CHECK(41, ~s, -6);
	CHECK(42, !s, 0);
	s = 0;
	CHECK(43, !s, 1);

	return 0;
}
