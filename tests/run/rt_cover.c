/*
 * Shapes the rest of the suite does not reach.
 *
 * pass2 matches a table of some 476 rules, and a rule that matches
 * nothing is worse than one that is absent: it reads as coverage that
 * is not there.  One sat in the table for a long time emitting
 * bit n,(iy+d) - correct, and unreachable, because an AND reduced its
 * left operand before any rule could see it - and every test passed
 * the whole time, because what ran instead was right, only longer.
 *
 * So c1 counts which rules fire (CCC_RULEHITS, debug build only) and
 * this file exists to drive the count up.  Compiling the whole tree,
 * libc and every other test here leaves a third of the table never
 * matched.  What follows aims at those: the multiply-by-a-constant
 * forms that no source happens to use, comparisons between two live
 * registers, stepping a global or a pointer target, and the unary
 * operators away from their common operand.
 *
 * The answers matter as much as the coverage.  A rule reached for the
 * first time has never had its output run.
 */
#include "rt.h"

short a, b, c;
short arr[8];
short *sp;
long l1, l2;
unsigned short u;
char ch;
unsigned char uc;

short idsh(x) short x; { return x; }

main()
{
	register short r;
	short loc;

	/*
	 * Multiply by a constant, which is done with shifts and adds.
	 * Each multiplier the table names has its own rule and its own
	 * chance to be wrong by one add.
	 */
	a = 3;
	CHECK(1, a * 3, 9);
	CHECK(2, a * 5, 15);
	CHECK(3, a * 6, 18);
	CHECK(4, a * 7, 21);
	CHECK(5, a * 9, 27);
	CHECK(6, a * 10, 30);
	CHECK(7, a * 11, 33);
	CHECK(8, a * 12, 36);
	CHECK(9, a * 14, 42);
	CHECK(10, a * 15, 45);
	CHECK(11, a * 20, 60);
	CHECK(12, a * 24, 72);
	CHECK(13, a * 40, 120);

	/* and with a value that exercises the carries */
	a = 1000;
	CHECK(14, a * 3, 3000);
	CHECK(15, a * 7, 7000);
	CHECK(16, a * 11, 11000);
	CHECK(17, a * 15, 15000);
	CHECK(18, a * 24, 24000);
	a = -3;
	CHECK(19, a * 7, -21);
	CHECK(20, a * 40, -120);
	a = 0;
	CHECK(21, a * 11, 0);

	/* divide and modulo by a variable, not a constant */
	a = 100; b = 7;
	CHECK(22, a / b, 14);
	CHECK(23, a % b, 2);
	a = -100;
	CHECK(24, a / b, -14);
	CHECK(25, a % b, -2);

	/* a register variable divided by a constant */
	r = 100;
	CHECK(26, r / 4, 25);
	CHECK(27, r % 4, 0);
	r = 103;
	CHECK(28, r / 4, 25);
	CHECK(29, r % 4, 3);

	/*
	 * Comparisons with both sides live, which is the form that has
	 * to hold one operand somewhere while it works out the other.
	 */
	a = 3; b = 5;
	CHECK(30, a > b, 0);
	CHECK(31, a >= b, 0);
	CHECK(32, b > a, 1);
	CHECK(33, b >= a, 1);
	CHECK(34, a >= a, 1);
	a = -3; b = 5;
	CHECK(35, a > b, 0);
	CHECK(36, a >= b, 0);
	CHECK(37, b >= a, 1);
	a = -5; b = -3;
	CHECK(38, a > b, 0);
	CHECK(39, b > a, 1);
	CHECK(40, a >= b, 0);

	/* a register variable against a computed value */
	r = 7;
	CHECK(41, r > idsh(3), 1);
	CHECK(42, r >= idsh(7), 1);
	CHECK(43, r > idsh(9), 0);
	CHECK(44, r >= idsh(9), 0);

	/* and against a constant, at the boundary */
	r = 5;
	CHECK(45, r <= 5, 1);
	CHECK(46, r <= 4, 0);
	CHECK(47, r > 4, 1);
	CHECK(48, r >= 6, 0);
	a = 5;
	CHECK(49, a <= 5, 1);
	CHECK(50, a <= 4, 0);

	/*
	 * Stepping something other than a local: a global, and the thing
	 * a pointer points at.  Both as a statement and for the value,
	 * before and after.
	 */
	a = 10;
	a--;
	CHECK(51, a, 9);
	CHECK(52, a--, 9);
	CHECK(53, a, 8);
	CHECK(54, --a, 7);
	CHECK(55, a++, 7);
	CHECK(56, ++a, 9);

	sp = &a;
	*sp = 20;
	(*sp)--;
	CHECK(57, a, 19);
	CHECK(58, (*sp)--, 19);
	CHECK(59, a, 18);
	CHECK(60, --(*sp), 17);
	CHECK(61, (*sp)++, 17);
	CHECK(62, ++(*sp), 19);

	/* a byte through a pointer */
	uc = 5;
	CHECK(63, uc++, 5);
	CHECK(64, uc, 6);
	CHECK(65, --uc, 5);

	/* a long stepped in place */
	l1 = 0x0000ffffL;
	l1++;
	CHECK(66, l1 == 0x00010000L, 1);
	l1--;
	CHECK(67, l1 == 0x0000ffffL, 1);
	++l1;
	CHECK(68, l1 == 0x00010000L, 1);
	--l1;
	CHECK(69, l1 == 0x0000ffffL, 1);

	/*
	 * The unary operators away from their usual operand: not, and
	 * complement, on things that are not already in HL.
	 */
	r = 0;
	CHECK(70, !r, 1);
	r = 5;
	CHECK(71, !r, 0);
	a = 0;
	CHECK(72, !a, 1);
	uc = 0;
	CHECK(73, !uc, 1);
	uc = 3;
	CHECK(74, !uc, 0);
	ch = 0;
	CHECK(75, !ch, 1);

	uc = 0x0f;
	CHECK(76, (unsigned char)~uc, 0xf0);
	a = 0;
	CHECK(77, ~a, -1);
	a = -1;
	CHECK(78, ~a, 0);

	/* negation, of a register variable and a global */
	r = 7;
	CHECK(79, -r, -7);
	a = -9;
	CHECK(80, -a, 9);

	/*
	 * Widening an unsigned value, which carries no sign, from each
	 * of the places one can sit.
	 */
	uc = 200;
	a = uc;
	CHECK(81, a, 200);
	u = 65535;
	l1 = u;
	CHECK(82, l1 == 65535L, 1);
	ch = -1;
	a = ch;
	CHECK(83, a, -1);

	/* a pointer difference and a pointer against a symbol */
	sp = &arr[5];
	CHECK(84, sp - arr, 5);
	sp = arr;
	CHECK(85, sp == arr, 1);
	CHECK(86, sp != arr, 0);

	/* shifts by eight, which move a whole byte */
	a = 0x1234;
	CHECK(87, (a >> 8) & 0xff, 0x12);
	a = 0x0034;
	CHECK(88, a << 8, 0x3400);
	u = 0x8000;
	CHECK(89, u >> 8, 0x80);

	/* an array element stepped, and the index a register variable */
	arr[0] = 1; arr[1] = 2; arr[2] = 3;
	r = 1;
	arr[r]++;
	CHECK(90, arr[1], 3);
	++arr[r];
	CHECK(91, arr[1], 4);
	arr[r]--;
	CHECK(92, arr[1], 3);

	/* a local kept in a register, assigned from everywhere */
	loc = 3;
	r = loc;
	CHECK(93, r, 3);
	r = a;
	CHECK(94, r, 0x0034);
	r = idsh(11);
	CHECK(95, r, 11);
	a = r;
	CHECK(96, a, 11);
	loc = r;
	CHECK(97, loc, 11);

	return 0;
}
