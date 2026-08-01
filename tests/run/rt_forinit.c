/*
 * Parens in a for-loop's init clause.
 *
 * cpp lowers for-loops itself, and its init-clause scanner counted
 * open parens but never close ones: any call, cast or parenthesized
 * subexpression in the init left the depth stuck, the first
 * semicolon was never seen at clause level, and the filter swallowed
 * the rest of the function as "init" - two unmatched braces at EOF
 * and no object.  asz's listing code was the first tree source to
 * write one.
 */
#include "rt.h"

int
bump(x)
int x;
{
	return x + 1;
}

int
main()
{
	int i, s;

	s = 0;
	for (i = (s ? 5 : 2); i < 5; i++)
		s++;
	CHECK(1, s, 3);

	for (i = bump(0); i < 4; i++)
		s++;
	CHECK(2, s, 6);

	for (i = bump(bump(0)) + (s ? 1 : 9); i < 9; i++)
		s++;
	CHECK(3, s, 12);
	return 0;
}
