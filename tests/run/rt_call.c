/*
 * Function calls: argument count, order and nesting.
 *
 * Arguments go on the stack right to left and the caller takes them
 * off again, so a miscount is silent - the values simply arrive in the
 * wrong places and the stack drifts.  Checking each argument's value
 * separately is what catches that.
 */
#include "rt.h"

short calls;

short one(a) short a; { return a; }
short two(a, b) short a, b; { return a * 10 + b; }
short four(a, b, c, d) short a, b, c, d;
{
	return ((a * 10 + b) * 10 + c) * 10 + d;
}

short bump()
{
	calls++;
	return calls;
}

short addup(a, b) short a, b; { return a + b; }

short *pident(p) short *p; { return p; }

short g;

main()
{
	short i;

	CHECK(1, one(7), 7);
	CHECK(2, two(1, 2), 12);
	CHECK(3, four(1, 2, 3, 4), 1234);

	/* the order arguments arrive in */
	CHECK(4, two(9, 1), 91);
	CHECK(5, four(4, 3, 2, 1), 4321);

	/* negative and wide values survive the trip */
	CHECK(6, one(-1), -1);
	CHECK(7, one(32767), 32767);
	CHECK(8, one(-32768), -32768);

	/* a call as an argument */
	calls = 0;
	CHECK(9, one(bump()), 1);
	CHECK(10, calls, 1);

	/* two calls as arguments: each happens once */
	calls = 0;
	CHECK(11, two(bump(), bump()), 21);
	CHECK(12, calls, 2);

	/* two calls added: the first result must survive the second */
	calls = 0;
	CHECK(13, addup(bump(), bump()), 3);

	/* nested calls */
	CHECK(14, one(one(one(5))), 5);
	CHECK(15, addup(one(3), two(1, 2)), 15);

	/* a compound assignment and a postfix as arguments */
	i = 1;
	CHECK(16, one(i += 4), 5);
	CHECK(17, i, 5);
	/*
	 * Only one argument may carry a side effect.  C leaves the order
	 * arguments are evaluated in unspecified, so "two(i++, i)" is
	 * allowed to give either 11 or 12 and is no use as a check.
	 */
	i = 1;
	CHECK(18, two(i++, 5), 15);
	CHECK(19, i, 2);

	/* a pointer through a call */
	g = 77;
	CHECK(20, *pident(&g), 77);

	return 0;
}
