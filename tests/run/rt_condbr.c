/*
 * The Z80 calls and returns on a condition, so a hop over a call or a
 * return is the whole cost of the hop.
 *
 *	jp z,L		3	    call nz,_foo	3
 *	call _foo	3  ->
 * L:			    L:
 *
 *	jp z,L		3	    ret nz		1
 *	ret		1  ->
 * L:			    L:
 *
 * Both are peephole rewrites, so what has to be checked is not that
 * they fire but that the program still does the same thing: the call
 * happens on exactly the conditions it did, and no more.  Each of
 * these counts its calls, so a call made when it should not have been
 * is caught as surely as one that did not happen.
 *
 * The conditional return only exists in a function with no frame -
 * with one, a return is a jump to the unwind and never a bare ret - so
 * the returning cases here take no parameters and keep no locals.
 *
 * The shapes that must NOT be rewritten are here too: a call with
 * arguments has the pushes and the caller's cleanup between the branch
 * and the label, and an if/else has a jump over the else in the way.
 * Both are checked by result, since a wrong rewrite there would call
 * with a stack the callee does not expect.
 */
#include "rt.h"

int calls;
int g;

nocount() { calls++; }
witharg(a) int a; { calls++; g = a; }

/* the plain shape: no arguments, nothing after the call */
plain(c) int c;
{
	if (c)
		nocount();
	return calls;
}

/* two conditions: only the second hop is adjacent to the call */
andand(a, b) int a, b;
{
	if (a && b)
		nocount();
	return calls;
}

/* arguments: must not become a conditional call */
args(c) int c;
{
	if (c)
		witharg(7);
	return calls;
}

/* if/else: must not either */
either(c) int c;
{
	if (c)
		nocount();
	else
		witharg(9);
	return calls;
}

/* frameless, so the return is a bare ret and the hop can fold */
early()
{
	if (g)
		return;
	calls++;
}

main()
{
	calls = 0;
	CHECK(1, plain(0), 0);
	CHECK(2, plain(1), 1);
	CHECK(3, plain(0), 1);		/* still one: not called again */

	calls = 0;
	CHECK(4, andand(0, 0), 0);
	CHECK(5, andand(1, 0), 0);
	CHECK(6, andand(0, 1), 0);
	CHECK(7, andand(1, 1), 1);

	calls = 0;
	g = 0;
	CHECK(8, args(0), 0);
	CHECK(9, args(1), 1);
	CHECK(10, g, 7);

	calls = 0;
	g = 0;
	CHECK(11, either(1), 1);
	CHECK(12, g, 0);		/* the else did not run */
	CHECK(13, either(0), 2);
	CHECK(14, g, 9);		/* this time it did */

	/* the conditional return */
	calls = 0;
	g = 1;
	early();
	CHECK(15, calls, 0);		/* returned early */
	g = 0;
	early();
	CHECK(16, calls, 1);		/* fell through */
	early();
	CHECK(17, calls, 2);

	return 0;
}
