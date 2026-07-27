/*
 * The truth test, "!x".
 *
 * A comparison already leaves a flag, and "!" of one is that flag
 * inverted - which the compiler did.  A value is the other half: the
 * flags say nothing about a register that was merely loaded, so the
 * value has to be tested.  There was no rule for that at any width
 * but long, so "!x" on an ordinary value reduced to nothing at all
 * and the branch went on whatever the flags happened to hold.
 *
 * Asked in both contexts, because a branch and a number come out of
 * the compiler by different paths.
 */
#include "rt.h"

char c;
unsigned char uc;
short s;
unsigned short us;
long l;
char *p;
short r;

#define BR(n, cond, want) r = 0; if (cond) r = 1; CHECK(n, r, want)
#define VA(n, cond, want) r = (cond); CHECK(n, r, want)

main()
{
	short a, b;

	/* a short, zero and not */
	s = 0;
	BR(1, !s, 1);
	VA(2, !s, 1);
	s = 1;
	BR(3, !s, 0);
	VA(4, !s, 0);
	s = -1;
	BR(5, !s, 0);
	VA(6, !s, 0);
	/* only the high half set, which a byte-wide test would miss */
	s = 256;
	BR(7, !s, 0);
	VA(8, !s, 0);

	/* a char */
	c = 0;
	BR(9, !c, 1);
	VA(10, !c, 1);
	c = 1;
	BR(11, !c, 0);
	c = -1;
	BR(12, !c, 0);
	VA(13, !c, 0);

	/* unsigned of both widths */
	uc = 0;
	BR(14, !uc, 1);
	uc = 255;
	BR(15, !uc, 0);
	us = 0;
	BR(16, !us, 1);
	us = 65535;
	BR(17, !us, 0);

	/* a long, where all four bytes have to speak */
	l = 0L;
	BR(18, !l, 1);
	VA(19, !l, 1);
	l = 0x00010000L;
	BR(20, !l, 0);
	VA(21, !l, 0);
	l = 1L;
	BR(22, !l, 0);

	/* a pointer */
	p = 0;
	BR(23, !p, 1);
	p = &c;
	BR(24, !p, 0);

	/* a local, which the compiler may keep in a register */
	a = 0; b = 5;
	BR(25, !a, 1);
	BR(26, !b, 0);
	VA(27, !a, 1);
	VA(28, !b, 0);

	/* doubled, which is the idiom for "make it 0 or 1" */
	s = 7;
	VA(29, !!s, 1);
	s = 0;
	VA(30, !!s, 0);
	BR(31, !!s, 0);

	/* of a comparison, which is the flag-inverting path */
	a = 1; b = 2;
	BR(32, !(a < b), 0);
	BR(33, !(a > b), 1);
	BR(34, !(a == b), 1);
	BR(35, !(a != b), 0);
	VA(36, !(a < b), 0);
	VA(37, !(a > b), 1);

	/* and combined, which is where it usually turns up */
	s = 0;
	BR(38, !s && b == 2, 1);
	BR(39, !s || b == 9, 1);
	s = 3;
	BR(40, !s && b == 2, 0);

	/*
	 * The short-circuit operators themselves.  They used to branch
	 * on the zero flag and claim their answer was in it, which holds
	 * only for operands that happen to answer there.  A signed
	 * comparison answers in the sign flag and an unsigned one in
	 * carry, so these are the interesting operands - and "!" is too,
	 * since it makes zero mean true.
	 */
	a = -1; b = 1;
	us = 65535;
	BR(41, a < b && b > 0, 1);
	BR(42, a > b && b > 0, 0);
	BR(43, a > b || b > 0, 1);
	BR(44, a > b || b < 0, 0);
	BR(45, us > 1 && b == 1, 1);
	BR(46, us < 1 && b == 1, 0);
	BR(47, us < 1 || b == 1, 1);

	/* three deep, mixing the two flags */
	BR(48, a < b && us > 1 && b == 1, 1);
	BR(49, a < b && us > 1 && b == 9, 0);
	BR(50, a > b || us < 1 || b == 1, 1);

	/* and as a value, which had no rule at all */
	VA(51, a < b && b > 0, 1);
	VA(52, a > b || b < 0, 0);
	VA(53, !a && b > 0, 0);

	/* the right side must not be evaluated when the left decides */
	s = 0;
	BR(54, s && (s = 9), 0);
	CHECK(55, s, 0);
	s = 1;
	BR(56, s || (s = 9), 1);
	CHECK(57, s, 1);

	return 0;
}
