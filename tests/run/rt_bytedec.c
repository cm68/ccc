/*
 * Byte counters stepped in place.
 *
 * A byte step on a frame slot is one read-modify-write: inc/dec
 * (iy+d), which sets Z itself.  The rules for the flag and statement
 * contexts are new, so pin the semantics they must preserve:
 *
 *   while (--n)  tests the value AFTER the step (n+1 trips for n),
 *   while (n--)  tests the value BEFORE it and still steps on the
 *                exit test - n leaves the loop wrapped to 255,
 *   ++n to 256   wraps to 0 and answers false in flag context.
 *
 * The register locals here are not decoration: they hold BC and IX
 * so the counters land in the frame, where the new rules fire.  The
 * same shapes on a register byte were already covered.
 */
#include "rt.h"

int
sum8(base, count)
char *base;
unsigned char count;
{
	unsigned char n;
	register char *p = base;
	register int acc = 0;

	n = count + 1;
	while (--n)
		acc += *p++;
	return acc;
}

int
sum8post(base, count)
char *base;
unsigned char count;
{
	unsigned char n;
	register char *p = base;
	register int acc = 0;

	n = count;
	while (n--)
		acc += *p++;
	return acc;
}

char tab[5];

int
main()
{
	unsigned char n;
	register char *p = tab;
	register int acc = 0;

	tab[0] = 1; tab[1] = 2; tab[2] = 4; tab[3] = 8; tab[4] = 16;

	CHECK(1, sum8(tab, 5), 31);
	CHECK(2, sum8(tab, 1), 1);
	CHECK(3, sum8(tab, 0), 0);
	CHECK(4, sum8post(tab, 5), 31);
	CHECK(5, sum8post(tab, 0), 0);

	/* statement-context steps collapse to one instruction; the
	 * stored value must still be exact */
	n = 7;
	--n;
	n++;
	n++;
	CHECK(6, n, 8);

	/* n-- steps even on the exit test: 0 leaves 255 behind */
	n = 0;
	if (n--)
		return 7;
	CHECK(8, n, 255);

	/* ++n wrapping to zero answers false in flag context */
	n = 255;
	if (++n)
		return 9;
	CHECK(10, n, 0);

	/* postfix in flag context answers with the old value */
	n = 1;
	if (n--)
		acc = 100;
	else
		return 11;
	CHECK(12, n, 0);
	CHECK(13, acc + *p, 101);
	return 0;
}
