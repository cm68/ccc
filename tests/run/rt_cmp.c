/*
 * Relational comparison, signed and unsigned.
 *
 * The case that matters is operands either side of zero: sbc hl,de
 * sets carry on an unsigned borrow, so using carry alone answers the
 * unsigned question and reports that -1 is not less than 1.  That
 * generated clean code and no marker for the whole life of the
 * compiler until it was checked against an actual answer.
 */
#include "rt.h"

short a, b;
unsigned short ua, ub;

short lt() { return a < b; }
short gt() { return a > b; }
short le() { return a <= b; }
short ge() { return a >= b; }
short eq() { return a == b; }
short ne() { return a != b; }

short ult() { return ua < ub; }
short ugt() { return ua > ub; }
short ule() { return ua <= ub; }
short uge() { return ua >= ub; }

main()
{
	/* straddling zero - the case carry gets wrong */
	a = -1; b = 1;
	CHECK(1, lt(), 1);
	CHECK(2, gt(), 0);
	CHECK(3, le(), 1);
	CHECK(4, ge(), 0);

	/* the other way round */
	a = 1; b = -1;
	CHECK(5, lt(), 0);
	CHECK(6, gt(), 1);
	CHECK(7, le(), 0);
	CHECK(8, ge(), 1);

	/* both negative */
	a = -5; b = -2;
	CHECK(9, lt(), 1);
	CHECK(10, gt(), 0);

	/* both positive */
	a = 2; b = 7;
	CHECK(11, lt(), 1);
	CHECK(12, ge(), 0);

	/* equal */
	a = 3; b = 3;
	CHECK(13, lt(), 0);
	CHECK(14, le(), 1);
	CHECK(15, ge(), 1);
	CHECK(16, eq(), 1);
	CHECK(17, ne(), 0);

	/* the extremes, where a subtraction overflows */
	a = -32768; b = 32767;
	CHECK(18, lt(), 1);
	CHECK(19, gt(), 0);
	a = 32767; b = -32768;
	CHECK(20, lt(), 0);
	CHECK(21, gt(), 1);

	/* unsigned keeps carry, and 0xffff is large rather than -1 */
	ua = 65535; ub = 1;
	CHECK(22, ult(), 0);
	CHECK(23, ugt(), 1);
	CHECK(24, ule(), 0);
	CHECK(25, uge(), 1);

	ua = 1; ub = 65535;
	CHECK(26, ult(), 1);
	CHECK(27, ugt(), 0);

	ua = 7; ub = 7;
	CHECK(28, ule(), 1);
	CHECK(29, uge(), 1);
	CHECK(30, ult(), 0);

	/* against zero, which has its own sign-bit path */
	a = -1;
	CHECK(31, a < 0, 1);
	CHECK(32, a > 0, 0);
	CHECK(33, a <= 0, 1);
	CHECK(34, a >= 0, 0);
	a = 0;
	CHECK(35, a < 0, 0);
	CHECK(36, a > 0, 0);
	CHECK(37, a <= 0, 1);
	CHECK(38, a >= 0, 1);
	a = 1;
	CHECK(39, a < 0, 0);
	CHECK(40, a > 0, 1);
	CHECK(41, a <= 0, 0);
	CHECK(42, a >= 0, 1);

	/* -32768 > 0 is false, and x-1 >= 0 would say otherwise */
	a = -32768;
	CHECK(43, a > 0, 0);
	CHECK(44, a < 0, 1);

	return 0;
}
