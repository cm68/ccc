/*
 * The conditional operator.
 *
 * Both arms have to leave their value in the same place, and a
 * constant arm has to emit anything at all - a bare constant reduces
 * to itself, so "x ? 1 : 0" used to produce two empty branches and
 * hand back the constant from the first arm whichever way the branch
 * went.  The condition is the other half: a comparison leaves its
 * answer in whichever flag suits it, and loading a register leaves the
 * flags alone entirely, so assuming zero meant false was wrong both
 * ways round.
 */
#include "rt.h"

short s, t, r;
char c;
long l;
short calls;

short bump()
{
	calls++;
	return 7;
}

main()
{
	/* constant arms, which is the shape that emitted nothing */
	s = 5;
	CHECK(1, s ? 1 : 0, 1);
	s = 0;
	CHECK(2, s ? 1 : 0, 0);
	s = 5;
	CHECK(3, s ? 10 : 20, 10);
	s = 0;
	CHECK(4, s ? 10 : 20, 20);

	/* the arms the other way up, in case the branch is inverted */
	s = 1;
	CHECK(5, s ? 0 : 1, 0);
	s = 0;
	CHECK(6, s ? 0 : 1, 1);

	/* variable arms */
	s = 1; t = 42; r = 99;
	CHECK(7, s ? t : r, 42);
	s = 0;
	CHECK(8, s ? t : r, 99);

	/*
	 * A comparison as the condition.  This is the case that cannot
	 * work by accident: the answer is in the sign or the carry, not
	 * in Z, and testing the wrong flag gives the wrong arm.
	 */
	s = -1; t = 1;
	CHECK(9, s < t ? 1 : 0, 1);
	CHECK(10, s > t ? 1 : 0, 0);
	CHECK(11, s <= t ? 1 : 0, 1);
	CHECK(12, s >= t ? 1 : 0, 0);
	CHECK(13, s == t ? 1 : 0, 0);
	CHECK(14, s != t ? 1 : 0, 1);
	s = 3; t = 3;
	CHECK(15, s == t ? 5 : 6, 5);
	CHECK(16, s < t ? 5 : 6, 6);

	/* unsigned, where the answer is in carry */
	s = 2; t = 7;
	CHECK(17, s < t ? 1 : 0, 1);
	CHECK(18, t < s ? 1 : 0, 0);

	/* a byte condition, and a byte compared */
	c = 0;
	CHECK(19, c ? 1 : 0, 0);
	c = 1;
	CHECK(20, c ? 1 : 0, 1);
	c = -1;
	CHECK(21, c < 0 ? 1 : 0, 1);
	CHECK(22, c > 0 ? 1 : 0, 0);

	/* a long condition tests all four bytes */
	l = 0L;
	CHECK(23, l ? 1 : 0, 0);
	l = 0x00010000L;
	CHECK(24, l ? 1 : 0, 1);

	/* the value is usable in an expression, not just on its own */
	s = 1;
	CHECK(25, (s ? 2 : 3) + 10, 12);
	s = 0;
	CHECK(26, (s ? 2 : 3) + 10, 13);
	s = 1;
	CHECK(27, (s ? 2 : 3) * 4, 8);

	/* and assignable */
	s = 0;
	r = s ? 100 : 200;
	CHECK(28, r, 200);
	s = 1;
	r = s ? 100 : 200;
	CHECK(29, r, 100);

	/* nested, both ways */
	s = 1; t = 1;
	CHECK(30, s ? (t ? 1 : 2) : 3, 1);
	t = 0;
	CHECK(31, s ? (t ? 1 : 2) : 3, 2);
	s = 0;
	CHECK(32, s ? (t ? 1 : 2) : 3, 3);
	s = 0; t = 1;
	CHECK(33, (s ? 1 : t) ? 8 : 9, 8);

	/* only the arm that is taken is evaluated */
	calls = 0;
	s = 0;
	r = s ? bump() : 5;
	CHECK(34, r, 5);
	CHECK(35, calls, 0);
	s = 1;
	r = s ? bump() : 5;
	CHECK(36, r, 7);
	CHECK(37, calls, 1);
	s = 0;
	r = s ? 5 : bump();
	CHECK(38, r, 7);
	CHECK(39, calls, 2);

	/* as a condition in its own right */
	s = 1;
	r = 0;
	if (s ? 1 : 0) r = 1;
	CHECK(40, r, 1);
	s = 0;
	r = 0;
	if (s ? 1 : 0) r = 1;
	CHECK(41, r, 0);

	return 0;
}
