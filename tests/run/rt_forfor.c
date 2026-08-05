/*
 * Nested loops where the outer for has an empty increment.
 *
 * cpp lowers a for to labels and gotos, saving the increment tokens
 * while the body streams through.  The save was skipped when the
 * enclosing for's increment was empty - a for(;;) - so the inner
 * loop's increment leaked into the outer loop's trailer: one stray
 * i++ ran every outer iteration, and when i lived in an inner block
 * the reference to it did not even compile.  Both shapes are here.
 */
#include "rt.h"

short g;

/* the silent shape: the counters are function locals, so the stray
 * increment compiled clean and just corrupted them at runtime */
short
spin(n) short n;
{
	short total = 0;
	short i;
	short outer = 0;

	for (;;) {
		for (i = 0; i < n; i++)
			total++;
		outer++;
		if (outer >= 3)
			break;
	}
	return total + outer;		/* 3*n + 3, nothing extra */
}

/* the loud shape: the inner counter lives in a block, where the
 * leaked increment referenced a name no longer in scope */
short
blocked(n) short n;
{
	short total = 0;

	for (;;) {
		if (n) {
			short i;

			for (i = 0; i < n; i++)
				total++;
		}
		break;
	}
	return total;
}

/* increments must also come back after a nested for, not just stay
 * out of an empty trailer */
short
restore(n) short n;
{
	short i, j;
	short total = 0;

	for (i = 0; i < n; i++) {
		for (j = 0; j < n; j++)
			total++;
		total++;		/* runs n times, once per outer pass */
	}
	return total;			/* n*n + n */
}

int
main()
{
	CHECK(1, spin(4), 15);
	CHECK(2, spin(1), 6);
	CHECK(3, blocked(5), 5);
	CHECK(4, blocked(0), 0);
	CHECK(5, restore(3), 12);
	g = 0;
	return 0;
}
