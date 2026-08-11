/*
 * An aggregate initialised with a scalar is sized from its TYPE.
 *
 *	char image[512] = 0;
 *
 * is not C - a scalar initialiser for an array or a struct is a
 * constraint violation in every edition - but Whitesmiths read it as
 * "zero fill the object" and source of that period is full of it.  It
 * was accepted here and given the size of the VALUE: two bytes.  The
 * object linked, nothing was said, and whatever was declared next was
 * laid down inside it, so the first write through the buffer landed on
 * another variable.
 *
 * The Micronix boot loader found it.  "union diskbuf disk0 = 0;" had
 * two bytes of storage, the disk spec sat nineteen bytes into it, and
 * reading one inode block wrote two zero bytes over spec.limit - using
 * the limit it was about to destroy for its own range check.  The next
 * read said "Block out of range" for a block that was in range.
 *
 * Sizes are checked by SPACING, not by sizeof: sizeof reads the type,
 * and the type was never what was wrong - the storage was.  A scalar
 * is planted between each pair so the gap is unambiguous, and the
 * objects are written through end to end so that a short one corrupts
 * its neighbour and the check catches it.
 *
 * Unions are absent on purpose: initialising one is refused outright
 * now, in any spelling - tests/union_init.c is the expected-failure
 * case, and the gripe is in doInitlzr.  A union declared and not
 * initialised goes to bss at its full size, which is what "= 0" was
 * reaching for.
 */
#include "rt.h"

/*
 * "= 0" on an aggregate is what is being tested and is not C, so the
 * host compiler - which is this suite's reference - will not take it.
 * It is spelled "= { 0 }" there, which is valid and means the same
 * thing, so both legs really do check the same sizing and the host is
 * still the authority on the answers.
 */
#ifdef z80
#define ZINIT	= 0
#else
#define ZINIT	= { 0 }
#endif

struct s { int x; char p[14]; };

/* the shapes from the report, each followed by a witness */
char   a[512] ZINIT;
int    w1 = 0x1111;
struct s b ZINIT;
int    w2 = 0x2222;
char   c[512] = { 0 };
int    w3 = 0x3333;
struct s d = { 0 };
int    w4 = 0x4444;
char   e[64] ZINIT;
long   w5 = 0x55556666L;
int    f[100] ZINIT;
int    w6 = 0x6666;

/* uninitialised, which always worked and must keep working */
char   g[512];
int    w7;

main()
{
	int i;

	/* every witness still holds what it was given: nothing was
	 * laid down on top of it */
	CHECK(1, w1, 0x1111);
	CHECK(2, w2, 0x2222);
	CHECK(3, w3, 0x3333);
	CHECK(4, w4, 0x4444);
	CHECK(5, w5 == 0x55556666L, 1);
	CHECK(6, w6, 0x6666);

	/* the objects are zero, which is what "= 0" asked for */
	CHECK(7, a[0] | a[255] | a[511], 0);
	CHECK(8, b.x, 0);
	CHECK(9, b.p[0] | b.p[13], 0);
	CHECK(10, c[0] | c[511], 0);
	CHECK(11, d.x | d.p[13], 0);
	CHECK(12, f[0] | f[99], 0);

	/* write through each one end to end, then read the witnesses
	 * back: a short object writes over its neighbour */
	for (i = 0; i < 512; i++)
		a[i] = 0x5a;
	for (i = 0; i < 512; i++)
		c[i] = 0x5a;
	for (i = 0; i < 64; i++)
		e[i] = 0x5a;
	for (i = 0; i < 100; i++)
		f[i] = 0x1234;
	for (i = 0; i < 512; i++)
		g[i] = 0x5a;
	b.x = 0x7fff;
	for (i = 0; i < 14; i++)
		b.p[i] = 0x5a;
	d.x = 0x7fff;
	for (i = 0; i < 14; i++)
		d.p[i] = 0x5a;

	CHECK(13, w1, 0x1111);
	CHECK(14, w2, 0x2222);
	CHECK(15, w3, 0x3333);
	CHECK(16, w4, 0x4444);
	CHECK(17, w5 == 0x55556666L, 1);
	CHECK(18, w6, 0x6666);

	/* and the objects themselves kept every byte they were given */
	CHECK(19, a[0] & 0xff, 0x5a);
	CHECK(20, a[511] & 0xff, 0x5a);
	CHECK(21, c[511] & 0xff, 0x5a);
	CHECK(22, e[63] & 0xff, 0x5a);
	CHECK(23, f[99], 0x1234);
	CHECK(24, g[511] & 0xff, 0x5a);
	CHECK(25, b.p[13] & 0xff, 0x5a);
	CHECK(26, d.p[13] & 0xff, 0x5a);

	return 0;
}
