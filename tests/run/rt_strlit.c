/*
 * Indexing a string literal.
 *
 * A string literal is flagged E_CONST in pass1 because it is a constant
 * address, but the address is a label emitted later - strN - and kept
 * beside the node, not in its value, which is zero.  Constant folding
 * took the value at its word and folded the arithmetic into a number,
 * so the label was thrown away and what came out was the offset as an
 * absolute address:
 *
 *	"MWDL"[1]	ld a,(1)
 *	"MWDL"[0]	ld a,(str1)	right, and only by accident - x + 0
 *					returns x before the fold is reached
 *
 * Indexing at a variable, or through a pointer, does not fold and was
 * always right.  So the broken form is the one that looks most like a
 * constant, and the two spellings of the same thing disagreed.
 *
 * Found in the hard disk boot loader, comparing the label it read off
 * the disk against DL_MAGIC - four constant indices of a literal, three
 * of them wrong, so a correct label never matched.  See micronix
 * stand/mwio.c.
 */
#include "rt.h"

char *p;
short i;

main()
{
	/* the constant indices, which were the broken ones */
	CHECK(1, "MWDL"[0], 'M');
	CHECK(2, "MWDL"[1], 'W');
	CHECK(3, "MWDL"[2], 'D');
	CHECK(4, "MWDL"[3], 'L');
	CHECK(5, "MWDL"[4], 0);		/* the terminator is there too */

	/* a variable index, which never folded */
	for (i = 0; i < 4; i++)
		CHECK(6, "MWDL"[i], "MWDL"[i]);
	i = 2;
	CHECK(7, "MWDL"[i], 'D');

	/* and through a pointer, which is the same string again */
	p = "MWDL";
	CHECK(8, p[0], 'M');
	CHECK(9, p[3], 'L');

	/* the two spellings have to agree, which is the whole point */
	i = 1;
	CHECK(10, "MWDL"[1], "MWDL"[i]);

	/* arithmetic on the address rather than an index */
	p = "MWDL" + 2;
	CHECK(11, *p, 'D');
	CHECK(12, *("MWDL" + 3), 'L');

	/* a longer one, so the offset is more than a digit */
	CHECK(13, "0123456789abcdef"[10], 'a');
	CHECK(14, "0123456789abcdef"[15], 'f');

	return 0;
}

/* vim: set tabstop=4 shiftwidth=4 noexpandtab: */
