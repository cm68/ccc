/*
 * A long stored THROUGH a register pointer, which had no rule.
 *
 * pass1's addCase keeps a register pointer into the case array and
 * files each label with "cp->value = value" - a long store through
 * IX.  There was no form for it, so the store emitted nothing at all
 * and a self-hosted c0 gave every case label whatever the freshly
 * grown array happened to be holding.  The constant form, "cp->value
 * = 0", was missing the same way.
 *
 * Both halves matter: a long is stored HL:DE with the high word in
 * HL, and the address has to reach lstde as the stacked operand
 * without the value being disturbed on the way.
 */
#include "rt.h"

struct ent {
	long		value;
	unsigned char	tag;
};

struct ent tab[3];

/* enough member traffic through p that the allocator homes it in IX */
void
fill(p, v)
register struct ent *p;
long v;
{
	p->value = v;			/* long through a register pointer */
	p->tag = 1;
	p[1].value = 0;			/* long constant, same route */
	p[1].tag = 2;
	p[2].value = v + 1;
	p[2].tag = 3;
}

/* the walk that reads them back, also register-homed */
long
total(n)
int n;
{
	register struct ent *p;
	long sum;

	sum = 0;
	p = tab;
	while (n--) {
		sum += p->value;
		p++;
	}
	return sum;
}

int
main()
{
	fill(tab, 70000L);

	CHECK(1, tab[0].value, 70000L);
	CHECK(2, tab[0].tag, 1);
	CHECK(3, tab[1].value, 0L);
	CHECK(4, tab[1].tag, 2);
	CHECK(5, tab[2].value, 70001L);
	CHECK(6, tab[2].tag, 3);

	/* the low word is the half that vanished when the store was
	 * done through a clobbered pointer, so check a value whose
	 * halves are both non-zero and different */
	CHECK(7, total(3), 140001L);

	fill(tab, -1L);
	CHECK(8, tab[0].value, -1L);
	CHECK(9, tab[2].value, 0L);

	return 0;
}

/* vim: set tabstop=4 shiftwidth=4 noexpandtab: */
