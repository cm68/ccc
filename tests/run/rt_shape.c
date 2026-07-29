/*
 * Shapes taken from real code that pass2 could not compile.
 *
 * These are not interesting as programs.  Each one is the smallest
 * thing that reproduces a marker found in the tree's own sources, kept
 * because reproducing one of these took longer than writing it: the
 * failing shape is usually a particular combination of storage class,
 * width and register that the obvious test does not reach.  Two
 * attempts at the first one below compiled cleanly before the third
 * matched what lexread.c actually does.
 *
 * Where a case came from is recorded with it, so if it regresses the
 * original is findable.
 */
#include "rt.h"

/*
 * pass1/lexread.c readByte(): a static byte array indexed by a static
 * int that steps.  The array's address goes to HL and the step comes
 * back in HL too, so it added HL to itself.  Stepping a global is
 * costed like a call now, which is what puts it first.
 */
static unsigned char lexBuf[512];
static int lexPos = 0;
static int lexValid = 0;

static unsigned char
readByte()
{
	if (lexPos >= lexValid)
		return 0;
	return lexBuf[lexPos++];
}

/* the same with the step before the subscript, and going down */
static int wpos;
static unsigned char wbuf[8];

static unsigned char
readBack()
{
	return wbuf[--wpos];
}

/* stepping a frame slot instead, which the (iy+d) forms handle in
 * place and which must keep the shorter code it already had */
static unsigned char
localstep(n) short n;
{
	short p;
	unsigned char t;

	p = 0;
	t = 0;
	while (p < n)
		t += wbuf[p++];
	return t;
}

/*
 * libcpm/time.c: the address of a local struct handed to a function.
 * This used to pass the first two bytes of the struct instead.
 */
struct tod { short lo, hi; };

short
sumtod(t) struct tod *t;
{
	return t->lo + t->hi;
}

short
bytod()
{
	struct tod lt;

	lt.lo = 3;
	lt.hi = 4;
	return sumtod(&lt);
}


/*
 * tools/wssize.c byname(): a byte array subscripted by a long.  The
 * sum of a pointer and a long is a pointer, so only the low word can
 * reach the address - but the long operand was emitted at its own
 * width and pass2 has no rule for adding the two together, so nothing
 * came out.  Eight places in the tools did this.
 */
static unsigned char lbuf[32];
static short li;

short
bylong(size) long size;
{
	long pos;
	short n;

	pos = 2;
	n = 0;
	for (li = 0; li < 4 && lbuf[pos + li]; li++)
		n++;
	return n;
}

short
atlong(p) long p;
{
	return lbuf[p];
}

/* a long narrowed into short arithmetic, which must keep the low word */
short
mixlong(a) short a;
{
	long l;

	l = 100000L;			/* 0x186a0 */
	return a + (short)l;
}


/*
 * tools/wslib.c: a pointer register variable subscripted by a
 * variable.  A constant offset folds into an INDEX and never reaches
 * the add, so the only rules for the index register plus something
 * were for a constant and for a symbol - "p[i]" with i worked out had
 * none, and emitted nothing.  Eleven places, counting the ones that
 * then sign-extend what they read.
 */
struct ent { short a, b; };
struct ent etab[4];
char cbuf[8];
short ei;

short
byidx(p, i) register struct ent *p; short i;
{
	return p[i].a;
}

short
byidxb(p, i) register struct ent *p; short i;
{
	return p[i].b;
}

short
bychar(p, i) register char *p; short i;
{
	return p[i] + 1;		/* sign-extended, which is the 7-site form */
}

void
setidx(p, i, v) register struct ent *p; short i, v;
{
	p[i].a = v;
}


/*
 * tools/asm.c and wslib.c: a local array subscripted by a register
 * variable.  The frame slot's address plus the subscript had forms for
 * the subscript in HL and in DE and none for BC, so storing through it
 * emitted nothing.  Six places, all of them clearing a buffer.
 */
short
locidx(n) short n;
{
	char b[8];
	register short i;
	short t;

	for (i = 0; i < 8; i++)
		b[i] = 0;		/* a constant stored through it */
	for (i = 0; i < n; i++)
		b[i] = i + 1;
	t = 0;
	for (i = 0; i < 8; i++)
		t += b[i];
	return t;
}

short
locidxw(n) short n;
{
	short w[4];
	register short i;
	short t;

	for (i = 0; i < 4; i++)
		w[i] = 0;
	w[n] = 100;
	t = 0;
	for (i = 0; i < 4; i++)
		t += w[i];
	return t;
}

main()
{
	lexBuf[0] = 10; lexBuf[1] = 20; lexBuf[2] = 30;
	lexPos = 0;
	lexValid = 3;
	CHECK(1, readByte(), 10);
	CHECK(2, readByte(), 20);
	CHECK(3, readByte(), 30);
	CHECK(4, readByte(), 0);
	CHECK(5, lexPos, 3);

	wbuf[0] = 1; wbuf[1] = 2; wbuf[2] = 4; wbuf[3] = 8;
	wpos = 3;
	CHECK(6, readBack(), 4);
	CHECK(7, wpos, 2);
	CHECK(8, readBack(), 2);
	CHECK(9, wpos, 1);

	CHECK(10, localstep(0), 0);
	CHECK(11, localstep(1), 1);
	CHECK(12, localstep(4), 15);

	CHECK(13, bytod(), 7);

	for (li = 0; li < 32; li++)
		lbuf[li] = li;
	lbuf[5] = 0;
	CHECK(14, bylong(0L), 3);	/* [2],[3],[4] set, [5] clear */
	CHECK(15, atlong(7L), 7);
	CHECK(16, atlong(0L), 0);
	CHECK(17, atlong(31L), 31);
	CHECK(18, mixlong(0), (short)0x86a0);
	CHECK(19, mixlong(1), (short)0x86a1);

	etab[0].a = 10; etab[0].b = 11;
	etab[1].a = 20; etab[1].b = 21;
	etab[2].a = 30; etab[2].b = 31;
	ei = 1;
	CHECK(20, byidx(etab, ei), 20);
	CHECK(21, byidxb(etab, ei), 21);
	ei = 0;
	CHECK(22, byidx(etab, ei), 10);
	ei = 2;
	CHECK(23, byidx(etab, ei), 30);
	CHECK(24, byidxb(etab, ei), 31);

	cbuf[0] = 5; cbuf[1] = -3; cbuf[2] = 100;
	ei = 0;
	CHECK(25, bychar(cbuf, ei), 6);
	ei = 1;
	CHECK(26, bychar(cbuf, ei), -2);
	ei = 2;
	CHECK(27, bychar(cbuf, ei), 101);

	setidx(etab, 1, 99);
	CHECK(28, etab[1].a, 99);
	CHECK(29, etab[0].a, 10);
	CHECK(30, etab[2].a, 30);

	CHECK(31, locidx(0), 0);
	CHECK(32, locidx(1), 1);
	CHECK(33, locidx(3), 6);	/* 1+2+3 */
	CHECK(34, locidx(8), 36);	/* 1..8 */
	CHECK(35, locidxw(0), 100);
	CHECK(36, locidxw(3), 100);

	return 0;
}
