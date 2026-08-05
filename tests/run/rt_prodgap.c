/*
 * The last three missing productions, found by sweeping every source
 * in the tree for XXXXXX markers - the comment pass2 leaves when no
 * rule names a shape, which means code was silently NOT emitted.
 *
 * 1. A store through a worked-out address, of a worked-out value,
 *    when the lvalue arrives already wrapped in DEREF: both sides
 *    want HL, and the address has to wait on the stack while the
 *    value is computed.  doscan's "**args++ = val" - scanf's %d and
 *    %ld conversions stored NOTHING before this.
 *
 * 2. Byte arithmetic whose left operand is a word in HL - a short
 *    loaded through a pointer, meeting a byte at byte width.  The
 *    compiler's own expr.c dropped an |= through this hole.
 *
 * 3. A long assignment used as a value in a condition:
 *    "if ((pos = a - f()) == 0)".  longable() declined the ASSIGN,
 *    nothing else can compare a long, and the branch went on stray
 *    flags.  libcpm's fseek.
 */
#include "rt.h"

/* --- 1: the doscan shape, word and long, through stepped pointers */
short scell[3];
long lcell[3];
short *sargs[3];
long *largs[3];
short **sap;
long **lap;

short
scanword(v)
short v;
{
	**sap++ = v;		/* the exact doscan store */
	return 0;
}

short
scanlong(v)
long v;
{
	**lap++ = v;
	return 0;
}

/* --- 2: byte op with a word-in-HL left operand */
struct fl { short flags; };
struct fl fset;
unsigned char gmask;

short
orflags(p, m)
struct fl *p;
unsigned char m;
{
	unsigned char r;

	/* p->flags loads a word into HL; (m & 4) lands in E */
	r = (unsigned char)(p->flags | (m & 4));
	return r;
}

/* --- 3: long assignment as a condition operand */
long gl;
long
lget()
{
	return 70000L;
}

short
lassigncond()
{
	long pos;

	if ((pos = gl - lget()) == 0)
		return 1;
	if (pos != 30000L)
		return 2;
	if ((pos = gl - 100000L) != 0)
		return 3;
	return 0;
}

main()
{
	short i;

	for (i = 0; i < 3; i++) {
		sargs[i] = &scell[i];
		largs[i] = &lcell[i];
	}
	sap = sargs;
	lap = largs;

	CHECK(1, scanword(111), 0);
	CHECK(2, scanword(222), 0);
	CHECK(3, scell[0], 111);
	CHECK(4, scell[1], 222);
	CHECK(5, sap, &sargs[2]);	/* the pointer stepped twice */

	CHECK(6, scanlong(70001L), 0);
	CHECK(7, scanlong(-5L), 0);
	CHECK(8, lcell[0], 70001L);
	CHECK(9, lcell[1], -5L);
	CHECK(10, lap, &largs[2]);

	fset.flags = 0x0121;
	CHECK(11, orflags(&fset, 0xff), 0x25);	/* 0x21 | 4 */
	CHECK(12, orflags(&fset, 0xfb), 0x21);	/* masked bit clear */

	gl = 100000L;
	CHECK(13, lassigncond(), 0);
	return 0;
}
