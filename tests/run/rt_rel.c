/*
 * Relational operators with both sides live.
 *
 * Most comparisons in ordinary code have a constant on one side, or an
 * operand simple enough to stay where it was put, and those forms are
 * well covered.  The rules for two computed operands - one in HL and
 * the other in DE or BC - were matched by nothing at all in this tree,
 * along with every signed >= and > between registers, and a signed
 * byte against zero.
 *
 * Getting there takes operands the compiler cannot fold or reduce to a
 * leaf, so the values come through calls and array subscripts, and one
 * side is often a register variable to force BC or IX.  The answers
 * are checked against a native build, which is the only reason to
 * trust code no test had ever run.
 *
 * Signed and unsigned are separate rules with separate arithmetic -
 * the signed forms subtract and test sign-exclusive-or-overflow, the
 * unsigned ones subtract and read the carry - so each is tried on both
 * sides of zero and across the point where they disagree.
 */
#include "rt.h"

short sa[6];
unsigned short ua[6];
char ba[6];
unsigned char uba[6];
short g1, g2;

struct s { short v; };
struct s st1, st2;

/* opaque to the folder: the value has to be computed */
short  vs(i) short i; { return sa[i]; }
unsigned short vu(i) short i; { return ua[i]; }
char   vb(i) short i; { return ba[i]; }
unsigned char vub(i) short i; { return uba[i]; }

short bytes();
short ptrs();

main()
{
	register short r;
	register unsigned short ur;

	sa[0] = -30000; sa[1] = -1; sa[2] = 0; sa[3] = 1; sa[4] = 30000;
	ua[0] = 0; ua[1] = 1; ua[2] = 32767; ua[3] = 0x8000; ua[4] = 0xffff;
	ba[0] = -128; ba[1] = -1; ba[2] = 0; ba[3] = 1; ba[4] = 127;
	uba[0] = 0; uba[1] = 1; uba[2] = 127; uba[3] = 128; uba[4] = 255;

	/*
	 * Signed word, both sides computed.  HL against DE.
	 */
	CHECK(1, vs(3) > vs(1), 1);		/* 1 > -1 */
	CHECK(2, vs(1) > vs(3), 0);
	CHECK(3, vs(3) >= vs(3), 1);
	CHECK(4, vs(1) >= vs(3), 0);
	CHECK(5, vs(3) >= vs(1), 1);
	CHECK(6, vs(0) > vs(4), 0);		/* -30000 > 30000 */
	CHECK(7, vs(4) > vs(0), 1);
	CHECK(8, vs(0) >= vs(0), 1);
	CHECK(9, vs(2) > vs(1), 1);		/* 0 > -1 */
	CHECK(10, vs(1) >= vs(2), 0);

	/* the same the other way, so < and <= are not the only ones run */
	CHECK(11, vs(1) < vs(3), 1);
	CHECK(12, vs(3) <= vs(3), 1);
	CHECK(13, vs(4) <= vs(0), 0);

	/*
	 * Unsigned word, where the top half is magnitude and not sign.
	 * 32768 and 65535 are the values a signed comparison gets wrong.
	 */
	CHECK(14, vu(3) > vu(2), 1);		/* 32768 > 32767 */
	CHECK(15, vu(2) > vu(3), 0);
	CHECK(16, vu(4) > vu(0), 1);		/* 65535 > 0 */
	CHECK(17, vu(0) > vu(4), 0);
	CHECK(18, vu(3) >= vu(3), 1);
	CHECK(19, vu(2) >= vu(3), 0);
	CHECK(20, vu(4) >= vu(2), 1);
	CHECK(21, vu(0) < vu(4), 1);
	CHECK(22, vu(4) <= vu(4), 1);

	/*
	 * A register variable on one side, which puts an operand in BC.
	 */
	r = 5;
	CHECK(23, r > vs(3), 1);		/* 5 > 1 */
	CHECK(24, r >= vs(3), 1);
	CHECK(25, vs(3) > r, 0);
	CHECK(26, vs(3) >= r, 0);
	r = -1;
	CHECK(27, r > vs(1), 0);		/* -1 > -1 */
	CHECK(28, r >= vs(1), 1);
	CHECK(29, vs(2) > r, 1);		/* 0 > -1 */
	CHECK(30, vs(0) >= r, 0);		/* -30000 >= -1 */
	r = 30000;
	CHECK(31, r >= vs(4), 1);
	CHECK(32, r > vs(4), 0);

	ur = 0x9c40;			/* 40000 */
	CHECK(33, ur > vu(2), 1);		/* 40000 > 32767 */
	CHECK(34, ur >= vu(2), 1);
	CHECK(35, vu(2) > ur, 0);
	CHECK(36, ur > vu(4), 0);		/* 40000 > 65535 */
	CHECK(37, vu(4) >= ur, 1);

	/* a register variable against a constant, at the boundary */
	r = 5;
	CHECK(38, r >= 5, 1);
	CHECK(39, r >= 6, 0);
	CHECK(40, r <= 5, 1);
	CHECK(41, r <= 4, 0);
	CHECK(42, r > 4, 1);
	r = -1;
	CHECK(43, r >= -1, 1);
	CHECK(44, r <= -1, 1);
	CHECK(45, r >= 0, 0);
	CHECK(46, r <= 0, 1);

	/* and a computed word against a constant, both directions */
	CHECK(47, vs(4) <= 30000, 1);
	CHECK(48, vs(4) <= 29999, 0);
	CHECK(49, vs(0) <= -30000, 1);
	CHECK(50, vs(0) <= -30001, 0);
	CHECK(51, vu(4) <= 0xffff, 1);
	CHECK(52, vu(2) <= 32767, 1);

	return bytes();
}

/*
 * The rest in functions of their own: zc3's peephole optimiser runs
 * out of memory on a body much larger than the one above, which is
 * worth knowing before the next test file gets long.
 */
short
bytes()
{
	/*
	 * A signed byte against zero, where the sign bit is the whole
	 * answer, and against another byte.
	 */
	CHECK(53, vb(1) >= 0, 0);		/* -1 >= 0 */
	CHECK(54, vb(2) >= 0, 1);
	CHECK(55, vb(3) >= 0, 1);
	CHECK(56, vb(0) >= 0, 0);		/* -128 */
	CHECK(57, vb(4) >= 0, 1);		/* 127 */
	CHECK(58, vb(1) <= 0, 1);
	CHECK(59, vb(2) <= 0, 1);
	CHECK(60, vb(3) <= 0, 0);
	CHECK(61, vb(0) <= 0, 1);
	CHECK(62, vb(4) <= 0, 0);

	CHECK(63, vb(3) > vb(1), 1);		/* 1 > -1 */
	CHECK(64, vb(1) > vb(3), 0);
	CHECK(65, vb(4) > vb(0), 1);		/* 127 > -128 */
	CHECK(66, vb(0) > vb(4), 0);
	CHECK(67, vb(3) >= vb(3), 1);
	CHECK(68, vb(1) >= vb(2), 0);
	CHECK(69, vb(2) >= vb(1), 1);
	CHECK(70, vb(0) <= vb(1), 1);
	CHECK(71, vb(4) <= vb(3), 0);
	CHECK(72, vb(2) <= vb(2), 1);

	/* unsigned bytes, where 128 and 255 are large and not negative */
	CHECK(73, vub(3) > vub(2), 1);		/* 128 > 127 */
	CHECK(74, vub(2) > vub(3), 0);
	CHECK(75, vub(4) > vub(0), 1);		/* 255 > 0 */
	CHECK(76, vub(3) >= vub(3), 1);
	CHECK(77, vub(2) >= vub(3), 0);
	CHECK(78, vub(4) >= vub(4), 1);
	CHECK(79, vub(0) <= vub(4), 1);
	CHECK(80, vub(4) <= vub(2), 0);

	/*
	 * A local byte against a constant, which is the byte sitting in
	 * the frame rather than in a register.
	 */
	{
		char lb;
		unsigned char lub;

		lb = -1;
		CHECK(81, lb >= 0, 0);
		CHECK(82, lb < 0, 1);
		CHECK(83, lb > -2, 1);
		CHECK(84, lb <= -1, 1);
		lb = 100;
		CHECK(85, lb >= 100, 1);
		CHECK(86, lb > 100, 0);
		CHECK(87, lb <= 100, 1);
		CHECK(88, lb == 100, 1);
		CHECK(89, lb != 100, 0);

		lub = 200;
		CHECK(90, lub >= 200, 1);
		CHECK(91, lub > 200, 0);
		CHECK(92, lub <= 200, 1);
		CHECK(93, lub < 201, 1);
		CHECK(94, lub == 200, 1);
		CHECK(95, lub != 199, 1);
	}

	return ptrs();
}

short
ptrs()
{
	register struct s *rp;
	struct s *p;
	short i, j;

	/*
	 * Pointers compared for equality, with one in a register.  A
	 * pointer is unsigned and the comparison is of addresses, so only
	 * equality is asked - ordering two unrelated objects is not a
	 * question C answers.
	 */
	rp = &st1;
	p = &st1;
	CHECK(96, rp == p, 1);
	CHECK(97, rp != p, 0);
	p = &st2;
	CHECK(98, rp == p, 0);
	CHECK(99, rp != p, 1);

	/* against a pointer the tree has to work out */
	rp = &st1;
	CHECK(100, rp == &st1, 1);
	CHECK(101, rp != &st1, 0);
	CHECK(102, rp == &st2, 0);

	/* a word against a global, rather than a constant */
	g1 = 1234;
	g2 = 1234;
	CHECK(103, vs(3) == 1, 1);
	CHECK(104, g1 == g2, 1);
	CHECK(105, g1 != g2, 0);
	g2 = 1235;
	CHECK(106, g1 != g2, 1);
	CHECK(107, g1 == g2, 0);

	/*
	 * Subscripts on both sides, which is the shape that has to hold
	 * one address while it works out the other.
	 */
	i = 1; j = 3;
	CHECK(108, sa[i] < sa[j], 1);
	CHECK(109, sa[j] > sa[i], 1);
	CHECK(110, sa[i] >= sa[j], 0);
	CHECK(111, sa[j] >= sa[i], 1);
	CHECK(112, sa[i] >= sa[i], 1);
	i = 0; j = 4;
	CHECK(113, sa[i] > sa[j], 0);
	CHECK(114, sa[j] >= sa[i], 1);
	CHECK(115, ua[2] < ua[3], 1);
	CHECK(116, ua[4] >= ua[3], 1);
	CHECK(117, ba[1] < ba[3], 1);
	CHECK(118, uba[4] > uba[2], 1);

	return 0;
}
