/*
 * A pointer compared against the address of a LOCAL array.
 *
 * A local array's name is its address, and it reduces to a frame
 * descriptor - (iy+d) - which no comparison rule has a form for.  The
 * table compares against DE, HL and the register homes, because a
 * frame slot in a comparison normally means the value in it, not
 * where it is.  So "p != local" and "p < def + 32" emitted no
 * comparison at all and branched on whatever flags were lying about.
 *
 * qsort, doscan and cpp's filtenum all walk a pointer to the end of a
 * local buffer, and all three had it.
 *
 * Both operand positions are exercised: against a register home,
 * where HL is free, and against a value already in HL, where it has
 * to be kept across working the address out.
 */
#include "rt.h"

/* against a register-homed pointer: HL is free here */
int
atend(p, n)
register char *p;
int n;
{
	char local[16];
	register char *q;
	int steps;

	q = local;
	steps = 0;
	while (q != local + n) {	/* the shape with no rule */
		*q++ = 'x';
		steps++;
	}
	/* p is unrelated; keep it live so it stays in a register */
	return steps + (p != local);
}

/* against a value that has to be loaded into HL first */
int
points_at(pp, sel)
char **pp;
int sel;
{
	char local[8];

	if (sel)
		*pp = local;
	return *pp == local;		/* left in HL, right a frame address */
}

/* the bound written as array-plus-offset, the filtenum shape */
int
span(s)
char *s;
{
	char def[40];
	register char *p;
	int i;

	for (i = 0; i < 40; i++)
		def[i] = 'a';
	p = def;
	while (*p && p < def + 32)
		p++;
	return p - def;
}

int
main()
{
	char *other;

	other = "z";

	CHECK(1, atend(other, 0), 1);	/* loop never runs, p != local */
	CHECK(2, atend(other, 5), 6);	/* five steps plus the one */
	CHECK(3, atend(other, 16), 17);

	CHECK(4, points_at(&other, 0), 0);
	CHECK(5, points_at(&other, 1), 1);

	CHECK(6, span("unused"), 32);	/* stops at the bound, not the NUL */

	return 0;
}

/* vim: set tabstop=4 shiftwidth=4 noexpandtab: */
