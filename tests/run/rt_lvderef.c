/*
 * A store through a pointer that is itself fetched from memory.
 *
 * "*q->p = c" has two dereferences on the left: fetch the pointer p
 * out of the struct, then store through it.  Two different collapses
 * each lost one:
 *
 * With q in a register, pass1 spelled the fetched pointer the same
 * way "*q = c" spells the register itself, and pass2 - which cannot
 * tell the two apart from that spelling - stored the character INTO
 * the pointer member.  With q in memory, pass2's rewrite kept the
 * lvalue's DEREF while reducing only the address arithmetic under
 * it, so the store went to the member's address instead of through
 * the member's value.
 *
 * stdio's _flsbuf does exactly this - *f->_base = c - so every 513th
 * byte written through a buffered stream landed in the buffer
 * pointer's low byte instead of the buffer, and peep's output files
 * came out truncated and shifted.
 *
 * Both variants below, at byte and word widths, with the pointer at
 * offset zero (where the register spelling collided) and offset two
 * (where the memory rewrite collapsed).
 */
#include "rt.h"

struct at0 { char *p; int n; };		/* pointer first: offset 0 */
struct at2 { int n; char *p; };		/* pointer second: offset 2 */
struct wat0 { int *w; int n; };

char cells[6];
int wcell;

/* register-resident struct pointer */
short
regbyte(q, c)
register struct at0 *q;
char c;
{
	*q->p = c;
	return *q->p == c ? 0 : 1;
}

short
regoff2(q, c)
register struct at2 *q;
char c;
{
	*q->p = c;
	return *q->p == c ? 0 : 1;
}

short
regword(q, v)
register struct wat0 *q;
int v;
{
	*q->w = v;
	return *q->w == v ? 0 : 1;
}

/*
 * The frame-resident variant - _flsbuf's own shape, a stack FILE
 * pointer and "*f->_base = c" - cannot be pinned down here: the
 * allocator promotes any busy aggregate-pointer parameter into IX on
 * its own, register keyword or not, and what stays in the frame is
 * decided by pressure, not by the source.  It is covered end to end
 * instead: the stdio seam test writes 520 distinct bytes through a
 * buffered stream, which crosses _flsbuf exactly where the collapse
 * used to overwrite the buffer pointer.
 *
 * The global variants are a shape of their own.  A global's value is
 * an explicit load in the tree - a frame slot's is implicit in the
 * operand that names it - so the same source arrives in pass2 one
 * DEREF deeper through a global, and the rewrite used to take the
 * member fetch for the store: "*g->p = c" stored the character at
 * the struct's first byte.  The double-indirect "**h = c" is the
 * same chain without the member dressing.
 */
struct at0 *gq0;
struct at2 *gq2;
char **gh;

short
glbbyte(c)
char c;
{
	*gq0->p = c;
	return *gq0->p == c ? 0 : 1;
}

short
glboff2(c)
char c;
{
	*gq2->p = c;
	return *gq2->p == c ? 0 : 1;
}

short
glbdbl(c)
char c;
{
	**gh = c;
	return **gh == c ? 0 : 1;
}

/*
 * Double-indirect through a register-resident pointer-to-pointer:
 * three fetches deep.  The lvalue flag used to ride only one level
 * of the spine, so this spelled itself identically to the two-deep
 * "*q->p" and stored one indirection short, at the struct base.
 */
short
dblind(q, c)
struct at0 *q;
char c;
{
	struct at0 **h = &q;

	*(*h)->p = c;
	return (*q->p == c) ? 0 : 1;
}

main()
{
	struct at0 s0;
	struct at2 s2;
	struct wat0 w0;

	s0.p = &cells[1];
	s0.n = 0x5aa5;
	s2.p = &cells[3];
	s2.n = 0x5aa5;
	w0.w = &wcell;
	w0.n = 0x5aa5;

	CHECK(1, regbyte(&s0, 'X'), 0);
	CHECK(2, cells[1], 'X');
	CHECK(3, s0.p, &cells[1]);	/* the pointer survived the store */
	CHECK(4, s0.n, 0x5aa5);

	CHECK(5, regoff2(&s2, 'Y'), 0);
	CHECK(6, cells[3], 'Y');
	CHECK(7, s2.p, &cells[3]);

	CHECK(8, regword(&w0, 0x1234), 0);
	CHECK(9, wcell, 0x1234);
	CHECK(10, w0.w, &wcell);

	gq0 = &s0;
	gq2 = &s2;
	CHECK(11, glbbyte('Z'), 0);
	CHECK(12, cells[1], 'Z');
	CHECK(13, s0.p, &cells[1]);

	CHECK(14, glboff2('Q'), 0);
	CHECK(15, cells[3], 'Q');
	CHECK(16, s2.p, &cells[3]);

	gh = &s0.p;
	CHECK(17, glbdbl('W'), 0);
	CHECK(18, cells[1], 'W');
	CHECK(19, s0.p, &cells[1]);

	CHECK(20, dblind(&s0, 'V'), 0);
	CHECK(21, cells[1], 'V');
	CHECK(22, s0.p, &cells[1]);
	CHECK(23, s0.n, 0x5aa5);

	return 0;
}
