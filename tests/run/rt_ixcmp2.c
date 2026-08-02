/*
 * Ordering comparisons on a register-homed pointer, in the three
 * shapes that had no form.
 *
 * 1. against a symbol WITH AN OFFSET.  The staging that loads a
 *    symbol into HL asked whether the operand was one before the
 *    children were reduced, where "&tab[0]" is a bare SYM but
 *    "&tab[N-1]" is still a PLUS - it becomes a symbol reference
 *    only when the rules fold the offset in, which is later.  So
 *    the first was staged and matched and the second was not, and
 *    pass1's
 *
 *	return t >= &basictypes[0] && t <= &basictypes[N_BASIC-1];
 *
 *    emitted the first comparison, nothing at all for the second,
 *    and jumped on the flags the first had left.  Every type in the
 *    table answered whatever the low end answered.
 *
 * 2. against a bound worked out into DE - "p < def + 32" in cpp's
 *    filtenum.  Only the two equalities existed.
 *
 * 3. the difference against DE, "p - def", which is how the span is
 *    read back.
 *
 * The pointers are unsigned by allocation policy, so the interesting
 * boundaries are the ends of the array, where an off-by-one shows.
 */
#include "rt.h"

#define N 10

char tab[N];

/* shape 1: both ends of a table, the second with the offset */
int
inrange(t)
register char *t;
{
	return t >= &tab[0] && t <= &tab[N - 1];
}

/* shape 2 and 3: walk to a computed bound, report the span */
int
span(s, lim)
char *s;
int lim;
{
	register char *p;

	p = s;
	while (*p && p < s + lim)
		p++;
	return p - s;			/* shape 3 */
}

/* the orderings against DE, each one on its own */
int
rel(p, q, which)
register char *p;
char *q;
int which;
{
	switch (which) {
	case 0: return p < q;
	case 1: return p <= q;
	case 2: return p > q;
	case 3: return p >= q;
	}
	return -1;
}

int
main()
{
	int i;

	for (i = 0; i < N; i++)
		tab[i] = 'a';

	CHECK(1, inrange(&tab[0]), 1);
	CHECK(2, inrange(&tab[N - 1]), 1);
	CHECK(3, inrange(&tab[0] - 1), 0);
	CHECK(4, inrange(&tab[N - 1] + 1), 0);	/* the end the fold hid */

	tab[4] = 0;
	CHECK(5, span(tab, 100), 4);		/* stops at the NUL */
	tab[4] = 'a';
	CHECK(6, span(tab, 3), 3);		/* stops at the bound */

	CHECK(7, rel(&tab[1], &tab[2], 0), 1);
	CHECK(8, rel(&tab[2], &tab[2], 0), 0);
	CHECK(9, rel(&tab[2], &tab[2], 1), 1);
	CHECK(10, rel(&tab[3], &tab[2], 1), 0);
	CHECK(11, rel(&tab[3], &tab[2], 2), 1);
	CHECK(12, rel(&tab[2], &tab[2], 2), 0);
	CHECK(13, rel(&tab[2], &tab[2], 3), 1);
	CHECK(14, rel(&tab[1], &tab[2], 3), 0);

	return 0;
}

/* vim: set tabstop=4 shiftwidth=4 noexpandtab: */
