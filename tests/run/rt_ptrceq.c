/*
 * Pointer arithmetic in its compound form.
 *
 * "p += n" is "p = p + n" and counts in elements, but only the plain
 * operators were scaled - so a walk over anything wider than a char
 * advanced one byte per step.  doprnt walks its varargs that way,
 * "a += len" over an int *, so printf("%d %d") read its second number
 * one byte into the first.
 *
 * The scaling cannot be decided from the left operand here: a
 * compound assignment has had its DEREF taken off to leave an
 * address, so every one of them looks like a pointer.  It is the type
 * being ASSIGNED that says whether there is an element size, which is
 * why "arr[2] += 50" must add fifty and not fifty elements.
 */
#include "rt.h"

struct pair {
	short l, r;
};

main()
{
	short v[8];
	short *p;
	long lv[4];
	long *lp;
	struct pair sv[4];
	struct pair *sp;
	char cv[8];
	char *cp;
	short arr[4];
	short i;
	unsigned char uc;

	for (i = 0; i < 8; i++)
		v[i] = i * 10;
	for (i = 0; i < 4; i++)
		lv[i] = (long)i * 1000L;
	for (i = 0; i < 4; i++) {
		sv[i].l = i;
		sv[i].r = i + 100;
	}
	for (i = 0; i < 8; i++)
		cv[i] = (char)i;

	/* a word pointer, by a variable and by a literal */
	p = v; i = 1; p += i;
	CHECK(1, *p, 10);
	p = v; p += 3;
	CHECK(2, *p, 30);
	p = &v[7]; p -= 2;
	CHECK(3, *p, 50);

	/* the count narrower than an int, which is how doprnt holds it */
	p = v; uc = 2; p += uc;
	CHECK(4, *p, 20);

	/* four bytes an element */
	lp = lv; lp += 2;
	CHECK(5, *lp, 2000L);
	lp = &lv[3]; lp -= 1;
	CHECK(6, *lp, 2000L);

	/* a struct element, whose size is neither one nor a power to guess */
	sp = sv; sp += 2;
	CHECK(7, sp->l, 2);
	CHECK(8, sp->r, 102);

	/* a char pointer, which needs no scaling and never did */
	cp = cv; cp += 3;
	CHECK(9, *cp, 3);

	/* and the case the scaling must NOT reach: a value, not a pointer */
	for (i = 0; i < 4; i++)
		arr[i] = 100;
	arr[2] += 50;
	CHECK(10, arr[2], 150);
	arr[1] -= 40;
	CHECK(11, arr[1], 60);

	/* the same for a long value */
	lv[0] = 5L;
	lv[0] += 7L;
	CHECK(12, lv[0], 12L);

	/* stepping one pointer to another and measuring the span */
	p = v;
	p += 4;
	CHECK(13, p - v, 4);
	return 0;
}
