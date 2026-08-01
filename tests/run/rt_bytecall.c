/*
 * A byte accumulator fed by a call - "cnt += rec(n)" - which is how
 * c0's cntCondLbls counts short-circuit labels.  The call result
 * comes back in HL and is moved to DE; the byte operators knew the
 * count-in-E shape (K) but not the word-in-DE one (E), so the add
 * had no rule and the self-build's c0 emitted every IF with zero
 * labels.  Found by the full self-build differential, first divergent
 * byte in pass1/util.s.
 */
#include "rt.h"

unsigned char
rec(int n)
{
	unsigned char cnt = 0;

	if (n > 0)
		cnt += rec(n - 1);
	if (n == 1)
		cnt++;
	return cnt;
}

int
main()
{
	CHECK(1, rec(3), 1);
	CHECK(2, rec(0), 0);
	CHECK(3, rec(1), 1);
	return 0;
}
