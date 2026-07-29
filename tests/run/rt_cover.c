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
char ba[8];
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

	/*
	 * Any other constant, which the table does not name and which has
	 * to go through the helper.  There was no rule for it at all, so
	 * the multiply simply was not emitted.
	 */
	a = 7;
	CHECK(22, a * 100, 700);
	CHECK(23, a * 13, 91);
	CHECK(24, a * 1000, 7000);
	CHECK(25, a * 1, 7);
	CHECK(26, a * 0, 0);
	CHECK(27, 100 * a, 700);	/* and with the constant on the left */
	a = -3;
	CHECK(28, a * 100, -300);
	CHECK(29, a * 13, -39);
	a = 0;
	CHECK(30, a * 100, 0);
	/* a power of two still becomes a shift, and a named one its adds */
	a = 7;
	CHECK(31, a * 8, 56);
	CHECK(32, a * 16, 112);

	/* divide and modulo by a variable, not a constant */
	a = 100; b = 7;
	CHECK(33, a / b, 14);
	CHECK(34, a % b, 2);
	a = -100;
	CHECK(35, a / b, -14);
	CHECK(36, a % b, -2);

	/* a register variable divided by a constant */
	r = 100;
	CHECK(37, r / 4, 25);
	CHECK(38, r % 4, 0);
	r = 103;
	CHECK(39, r / 4, 25);
	CHECK(40, r % 4, 3);

	/*
	 * Comparisons with both sides live, which is the form that has
	 * to hold one operand somewhere while it works out the other.
	 */
	a = 3; b = 5;
	CHECK(41, a > b, 0);
	CHECK(42, a >= b, 0);
	CHECK(43, b > a, 1);
	CHECK(44, b >= a, 1);
	CHECK(45, a >= a, 1);
	a = -3; b = 5;
	CHECK(46, a > b, 0);
	CHECK(47, a >= b, 0);
	CHECK(48, b >= a, 1);
	a = -5; b = -3;
	CHECK(49, a > b, 0);
	CHECK(50, b > a, 1);
	CHECK(51, a >= b, 0);

	/* a register variable against a computed value */
	r = 7;
	CHECK(52, r > idsh(3), 1);
	CHECK(53, r >= idsh(7), 1);
	CHECK(54, r > idsh(9), 0);
	CHECK(55, r >= idsh(9), 0);

	/* and against a constant, at the boundary */
	r = 5;
	CHECK(56, r <= 5, 1);
	CHECK(57, r <= 4, 0);
	CHECK(58, r > 4, 1);
	CHECK(59, r >= 6, 0);
	a = 5;
	CHECK(60, a <= 5, 1);
	CHECK(61, a <= 4, 0);

	/*
	 * Stepping something other than a local: a global, and the thing
	 * a pointer points at.  Both as a statement and for the value,
	 * before and after.
	 */
	a = 10;
	a--;
	CHECK(62, a, 9);
	CHECK(63, a--, 9);
	CHECK(64, a, 8);
	CHECK(65, --a, 7);
	CHECK(66, a++, 7);
	CHECK(67, ++a, 9);

	sp = &a;
	*sp = 20;
	(*sp)--;
	CHECK(68, a, 19);
	CHECK(69, (*sp)--, 19);
	CHECK(70, a, 18);
	CHECK(71, --(*sp), 17);
	CHECK(72, (*sp)++, 17);
	CHECK(73, ++(*sp), 19);

	/* a byte through a pointer */
	uc = 5;
	CHECK(74, uc++, 5);
	CHECK(75, uc, 6);
	CHECK(76, --uc, 5);

	/* a long stepped in place */
	l1 = 0x0000ffffL;
	l1++;
	CHECK(77, l1 == 0x00010000L, 1);
	l1--;
	CHECK(78, l1 == 0x0000ffffL, 1);
	++l1;
	CHECK(79, l1 == 0x00010000L, 1);
	--l1;
	CHECK(80, l1 == 0x0000ffffL, 1);

	/*
	 * The unary operators away from their usual operand: not, and
	 * complement, on things that are not already in HL.
	 */
	r = 0;
	CHECK(81, !r, 1);
	r = 5;
	CHECK(82, !r, 0);
	a = 0;
	CHECK(83, !a, 1);
	uc = 0;
	CHECK(84, !uc, 1);
	uc = 3;
	CHECK(85, !uc, 0);
	ch = 0;
	CHECK(86, !ch, 1);

	uc = 0x0f;
	CHECK(87, (unsigned char)~uc, 0xf0);
	a = 0;
	CHECK(88, ~a, -1);
	a = -1;
	CHECK(89, ~a, 0);

	/* negation, of a register variable and a global */
	r = 7;
	CHECK(90, -r, -7);
	a = -9;
	CHECK(91, -a, 9);

	/*
	 * Widening an unsigned value, which carries no sign, from each
	 * of the places one can sit.
	 */
	uc = 200;
	a = uc;
	CHECK(92, a, 200);
	u = 65535;
	l1 = u;
	CHECK(93, l1 == 65535L, 1);
	ch = -1;
	a = ch;
	CHECK(94, a, -1);

	/* a pointer difference and a pointer against a symbol */
	sp = &arr[5];
	CHECK(95, sp - arr, 5);
	sp = arr;
	CHECK(96, sp == arr, 1);
	CHECK(97, sp != arr, 0);

	/* shifts by eight, which move a whole byte */
	a = 0x1234;
	CHECK(98, (a >> 8) & 0xff, 0x12);
	a = 0x0034;
	CHECK(99, a << 8, 0x3400);
	u = 0x8000;
	CHECK(100, u >> 8, 0x80);

	/* an array element stepped, and the index a register variable */
	arr[0] = 1; arr[1] = 2; arr[2] = 3;
	r = 1;
	arr[r]++;
	CHECK(101, arr[1], 3);
	++arr[r];
	CHECK(102, arr[1], 4);
	arr[r]--;
	CHECK(103, arr[1], 3);

	/* a local kept in a register, assigned from everywhere */
	loc = 3;
	r = loc;
	CHECK(104, r, 3);
	r = a;
	CHECK(105, r, 0x0034);
	r = idsh(11);
	CHECK(106, r, 11);
	a = r;
	CHECK(107, a, 11);
	loc = r;
	CHECK(108, loc, 11);

	/*
	 * A global array subscripted by a register variable, which puts
	 * the subscript in BC.  There were forms for adding HL and DE to
	 * a symbol's address and none for BC, so the address was never
	 * worked out.
	 */
	arr[0] = 10; arr[1] = 20; arr[2] = 30; arr[3] = 40;
	ba[0] = 1; ba[1] = 2; ba[2] = 3;
	r = 2;
	CHECK(109, arr[r], 30);
	CHECK(110, ba[r], 3);
	r = 0;
	CHECK(111, arr[r], 10);
	CHECK(112, ba[r], 1);
	r = 1;
	arr[r] = 99;
	CHECK(113, arr[1], 99);
	CHECK(114, arr[0], 10);
	CHECK(115, arr[2], 30);
	ba[r] = 7;
	CHECK(116, ba[1], 7);
	CHECK(117, ba[0], 1);

	return 0;
}
