/*
 * An argument goes out at the width the prototype declares, not the
 * width the expression happens to have.
 *
 * Everything is pushed in whole slots, so a long occupies two of them
 * and everything narrower occupies one.  Get the count wrong in either
 * direction and the callee reads its arguments from slots the caller
 * did not write, and every argument after the offending one is off by
 * one:
 *
 *	fseek(fp, 0, SEEK_SET);
 *
 * pushed the 0 as a word.  fseek reads four bytes, so its offset was
 * the caller's 0 with the caller's whence pasted on above it, and its
 * own whence came from the stack beyond the arguments - not 0, 1 or 2,
 * so lseek fell to default, set EINVAL and returned without seeking.
 * Nothing traps and hardly any caller checks fseek, so the first sign
 * of it was asz writing an object with a header and no contents.
 *
 * The other direction is the same mistake read backwards: a long
 * handed to an int parameter puts four bytes where the callee reads
 * two, and the callee takes the high word as the next argument.
 *
 * Only a prototype can fix either one - without one the caller has to
 * write the L, which is the ordinary K&R rule - so the callees here
 * are defined with their parameter types, which is what puts a
 * prototype in scope at the calls below.
 */
#include "rt.h"

short g1, g2, g3;
long gl;
short gs;

/* what the callee got, split so a 16-bit CHECK can see all of it */
void
sawlong(long v, short w, short x)
{
	g1 = (short)(v & 0xffff);
	g2 = (short)((v >> 16) & 0xffff);
	g3 = w + x;
}

short
takelong(long v, short w)
{
	sawlong(v, w, 0);
	return 0;
}

short
takelong2(short a, long v, short w)
{
	sawlong(v, a, w);
	return 0;
}

short
takeint(short v, short w)
{
	g1 = v;
	g2 = 0;
	g3 = w;
	return 0;
}

short
takeint2(short a, short v, short w)
{
	g1 = v;
	g2 = 0;
	g3 = a + w;
	return 0;
}

short
takechar(char c, short w)
{
	g1 = c;
	g2 = 0;
	g3 = w;
	return 0;
}

main()
{
	/* an int constant where the prototype says long */
	takelong(0, 7);
	CHECK(1, g1, 0);
	CHECK(2, g2, 0);
	CHECK(3, g3, 7);

	takelong(-2, 7);
	CHECK(4, g1, -2);
	CHECK(5, g2, -1);		/* sign extended, not padded */
	CHECK(6, g3, 7);

	/* an int variable, which is a real SEXT rather than a relabel */
	gs = 300;
	takelong(gs, 9);
	CHECK(7, g1, 300);
	CHECK(8, g2, 0);
	CHECK(9, g3, 9);

	gs = -300;
	takelong(gs, 9);
	CHECK(10, g1, -300);
	CHECK(11, g2, -1);
	CHECK(12, g3, 9);

	/* with arguments on both sides of the long one */
	takelong2(3, 5, 4);
	CHECK(13, g1, 5);
	CHECK(14, g2, 0);
	CHECK(15, g3, 7);

	/* a long where the prototype says int: the high word must go */
	gl = 0x00010005L;
	takeint(gl, 6);
	CHECK(16, g1, 5);
	CHECK(17, g3, 6);

	takeint2(2, gl, 6);
	CHECK(18, g1, 5);
	CHECK(19, g3, 8);

	/* a long constant, truncated where it stands rather than wrapped */
	takeint(0x00020003L, 6);
	CHECK(20, g1, 3);
	CHECK(21, g3, 6);

	/* and a char parameter, which reads one byte of the one slot */
	gl = 0x00040041L;
	takechar(gl, 6);
	CHECK(22, g1, 0x41);
	CHECK(23, g3, 6);

	return 0;
}
