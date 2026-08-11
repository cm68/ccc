/*
 * A relational with a step on the right.
 *
 * pass1 canonicalises > and >= by swapping the operands, so "++i >= n"
 * arrives as LE(n, ++i) with the side effect on the right.  The frame
 * slot rule for a prefix step named l and h outright and declared HL,
 * so there was no rule at all when the answer was wanted in DE - which
 * is what a relational asks for its right operand.  pass2 then loaded
 * n into HL, loaded i on top of it, and jumped on whatever flags the
 * inc had left.
 *
 * The count of unreduced expressions made that a failed compile rather
 * than a silent one, but a test that only assembles would still pass
 * the day the rule half works: the failure is a jump on stale flags,
 * so these have to check the answer.  See PREINCBUG.
 *
 * "if (++i >= argc)" after an option that takes an argument is how
 * every program of this vintage steps over the argument and checks it
 * exists; cmd/cc and cmd/s both failed to build on it.
 */
#include "rt.h"

/*
 * Each of these takes the step and the comparison in one expression,
 * and returns the answer.  The variable is passed in so that nothing
 * is constant folded and no assignment precedes the test - an
 * assignment to the same variable just before is enough to change the
 * shape pass1 hands down, which is why this was rare.
 */
int lt(i, n) short i, n; { return ++i <  n; }
int le(i, n) short i, n; { return ++i <= n; }
int gt(i, n) short i, n; { return ++i >  n; }
int ge(i, n) short i, n; { return ++i >= n; }

int rlt(i, n) short i, n; { return n <  ++i; }
int rle(i, n) short i, n; { return n <= ++i; }

int dgt(i, n) short i, n; { return --i >  n; }
int dge(i, n) short i, n; { return --i >= n; }
int rdle(i, n) short i, n; { return n <= --i; }

int pgt(i, n) short i, n; { return i++ >  n; }

/* and the step must actually have happened */
short g;

int
stepped(n)
short n;
{
	g = n;
	return ++g;
}

main()
{
	/* ++i against n: i becomes 4 */
	CHECK(1,  lt(3, 5), 1);
	CHECK(2,  lt(3, 4), 0);		/* 4 < 4 is false */
	CHECK(3,  le(3, 4), 1);
	CHECK(4,  le(3, 3), 0);
	CHECK(5,  gt(3, 3), 1);		/* 4 > 3 */
	CHECK(6,  gt(3, 4), 0);
	CHECK(7,  ge(3, 4), 1);		/* 4 >= 4 - the reported form */
	CHECK(8,  ge(3, 5), 0);

	/* the same with the step written on the right to begin with */
	CHECK(9,  rlt(3, 3), 1);	/* 3 < 4 */
	CHECK(10, rlt(3, 4), 0);
	CHECK(11, rle(3, 4), 1);	/* 4 <= 4 */
	CHECK(12, rle(3, 5), 0);

	/* decrement: i becomes 2 */
	CHECK(13, dgt(3, 1), 1);
	CHECK(14, dgt(3, 2), 0);
	CHECK(15, dge(3, 2), 1);
	CHECK(16, dge(3, 3), 0);
	CHECK(17, rdle(3, 2), 1);	/* 2 <= 2 */
	CHECK(18, rdle(3, 3), 0);

	/* postfix compares the value from before the step */
	CHECK(19, pgt(3, 3), 0);	/* 3 > 3 is false */
	CHECK(20, pgt(4, 3), 1);

	/* and the variable really was stepped */
	CHECK(21, stepped(7), 8);
	CHECK(22, g, 8);

	/* negative values, where a signed compare is the whole point */
	CHECK(23, ge(-3, -2), 1);	/* -2 >= -2 */
	CHECK(24, ge(-3, -1), 0);
	CHECK(25, dge(-3, -4), 1);	/* -4 >= -4 */

	return 0;
}

/* vim: set tabstop=4 shiftwidth=4 noexpandtab: */
