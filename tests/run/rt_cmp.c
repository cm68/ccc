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
char ca, cb;
unsigned char uca, ucb;
short r;

/*
 * A comparison used as a branch and the same one used as a value take
 * different paths through the compiler - one wants a flag, the other
 * a number - so both are worth asking.
 */
#define BR(n, cond, want) r = 0; if (cond) r = 1; CHECK(n, r, want)
#define VA(n, cond, want) r = (cond); CHECK(n, r, want)

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
	/* locals, so the compiler is free to keep them in registers */
	short ra, rb;
	unsigned short ura;

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

	/*
	 * The same ground a byte at a time.  A byte comparison goes
	 * through cp rather than sbc, and cp answers the unsigned
	 * question just as squarely: "c < 0" was false for every char in
	 * the language, and none of it left a marker.
	 */
	ca = -1;
	BR(45, ca < 0, 1);
	BR(46, ca >= 0, 0);
	BR(47, ca > 0, 0);
	BR(48, ca <= 0, 1);
	VA(49, ca < 0, 1);
	VA(50, ca >= 0, 0);
	VA(51, ca > 0, 0);
	VA(52, ca <= 0, 1);

	ca = 0;
	BR(53, ca < 0, 0);
	BR(54, ca > 0, 0);
	BR(55, ca <= 0, 1);
	BR(56, ca >= 0, 1);

	ca = 1;
	BR(57, ca < 0, 0);
	BR(58, ca > 0, 1);
	BR(59, ca <= 0, 0);
	BR(60, ca >= 0, 1);

	/* two bytes either side of zero */
	ca = -1; cb = 1;
	BR(61, ca < cb, 1);
	BR(62, ca > cb, 0);
	BR(63, ca <= cb, 1);
	BR(64, ca >= cb, 0);
	VA(65, ca < cb, 1);
	ca = 1; cb = -1;
	BR(66, ca < cb, 0);
	BR(67, ca > cb, 1);

	/* against a constant that is not zero */
	ca = -1;
	BR(68, ca < 1, 1);
	BR(69, ca > 1, 0);
	BR(70, ca >= 1, 0);
	BR(71, ca <= 1, 1);

	/*
	 * The ends of the range, where turning "> n" into ">= n+1" has
	 * nowhere to go: the increment wraps and the answer inverts.
	 */
	ca = 127;
	BR(72, ca > 127, 0);
	BR(73, ca <= 127, 1);
	BR(74, ca >= 127, 1);
	BR(75, ca < 127, 0);
	ca = -128;
	BR(76, ca < -128, 0);
	BR(77, ca <= -128, 1);
	BR(78, ca > -128, 0);
	BR(79, ca >= -128, 1);
	ca = -128; cb = 127;
	BR(80, ca < cb, 1);
	BR(81, ca >= cb, 0);

	/* an unsigned char keeps the unsigned answers */
	uca = 255; ucb = 1;
	BR(82, uca > ucb, 1);
	BR(83, uca < ucb, 0);
	BR(84, uca >= ucb, 1);
	BR(85, uca <= ucb, 0);
	VA(86, uca > ucb, 1);
	uca = 200;
	BR(87, uca > 0, 1);
	BR(88, uca < 0, 0);
	BR(89, uca >= 0, 1);
	BR(90, uca > 128, 1);
	uca = 255;
	BR(91, uca > 255, 0);
	BR(92, uca <= 255, 1);

	/* equality does not care about signedness either way */
	ca = -1; cb = -1;
	BR(93, ca == cb, 1);
	BR(94, ca != cb, 0);

	/*
	 * The same questions of a variable the compiler kept in a
	 * register.  Everything above is a global, and BC had its own set
	 * of comparison rules that the signed fix never reached: a
	 * register variable that went negative compared as though it were
	 * large, and "greater than" and "at or below" had no rule at all,
	 * so they emitted nothing and answered with whatever the flags
	 * happened to hold.
	 */
	ra = -1;
	BR(95, ra < 2, 1);
	BR(96, ra > 2, 0);
	BR(97, ra >= 2, 0);
	BR(98, ra <= 2, 1);
	VA(99, ra < 2, 1);
	VA(100, ra > 2, 0);
	rb = 2;
	BR(101, ra < rb, 1);
	BR(102, ra > rb, 0);
	BR(103, ra <= rb, 1);
	BR(104, ra >= rb, 0);

	ra = 5;
	BR(105, ra > 2, 1);
	BR(106, ra <= 2, 0);
	BR(107, ra >= 5, 1);
	BR(108, ra < 5, 0);

	/* unsigned keeps the top bit a value here too */
	ura = 65535;
	BR(109, ura > 2, 1);
	BR(110, ura < 2, 0);
	BR(111, ura >= 2, 1);
	BR(112, ura <= 2, 0);

	return 0;
}
