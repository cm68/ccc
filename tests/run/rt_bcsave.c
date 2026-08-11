/*
 * BC survives everything a compiled function can call.
 *
 * BC is callee-save: a function that keeps a register variable there
 * expects it back across any call it makes.  Two helpers did not
 * honour that - amul/lmul kept the byte counter in B and the right
 * operand's high half in C, and adiv/ldiv (and amod/lmod through them)
 * counted the quotient bits in B and used BC in negat.  Neither saved.
 *
 * Nothing noticed, because every compiled function used to save BC in
 * its own prologue whether it needed to or not.  That blanket save is
 * what the two helpers were really costing: ten bytes of frame in all
 * 864 functions of this tree so that two arithmetic routines could use
 * a register they had not asked for.  With them fixed the prologue asks
 * the function header instead - see savesbc() - and a function with
 * nothing in BC carries no frame at all.
 *
 * So these checks are load-bearing in a way that is easy to miss: they
 * are what says the frame may safely be left out.  Each helper is
 * called from a function that is holding a known value in a register
 * variable, and the value is checked afterwards.
 *
 * The variable SHIFTS are here for the same reason from the other
 * side.  Those are not a helper call - the code generator writes the
 * djnz loop inline - so they clobber B in the function itself, and the
 * $[ $] guard around them is now unconditional rather than asking
 * whether a variable lives there.  No function in the tree currently
 * pairs a variable shift with an empty BC, which is exactly why this
 * has to be pinned rather than left to luck.
 */
#include "rt.h"

int mul(a, b) int a, b;
{
	register int r;
	r = 0x5a5a;
	a = a * b;
	return (r == 0x5a5a) ? a : -1;
}

int dvd(a, b) int a, b;
{
	register int r;
	r = 0x1234;
	a = a / b;
	return (r == 0x1234) ? a : -1;
}

int rem(a, b) int a, b;
{
	register int r;
	r = 0x7ffe;
	a = a % b;
	return (r == 0x7ffe) ? a : -1;
}

unsigned udvd(a, b) unsigned a, b;
{
	register int r;
	r = 0x2b2b;
	a = a / b;
	return (r == 0x2b2b) ? a : 0xffff;
}

unsigned urem(a, b) unsigned a, b;
{
	register int r;
	r = 0x3c3c;
	a = a % b;
	return (r == 0x3c3c) ? a : 0xffff;
}

/* the inline djnz loops, not a call */
int shl(a, n) int a, n;
{
	register int r;
	r = 0x0f0f;
	a = a << n;
	return (r == 0x0f0f) ? a : -1;
}

int shr(a, n) int a, n;
{
	register int r;
	r = 0x1e1e;
	a = a >> n;
	return (r == 0x1e1e) ? a : -1;
}

main()
{
	CHECK(1, mul(7, 6), 42);
	CHECK(2, mul(-7, 6), -42);
	CHECK(3, mul(1000, 30), 30000);
	CHECK(4, mul(0, 12345), 0);

	CHECK(5, dvd(42, 6), 7);
	CHECK(6, dvd(-42, 6), -7);
	CHECK(7, dvd(42, -6), -7);
	CHECK(8, dvd(-42, -6), 7);
	CHECK(9, dvd(0, 7), 0);
	CHECK(10, dvd(1, 2), 0);
	CHECK(11, dvd(30000, 30), 1000);

	CHECK(12, rem(42, 5), 2);
	CHECK(13, rem(-42, 5), -2);
	CHECK(14, rem(0, 5), 0);
	CHECK(15, rem(30000, 7), 30000 % 7);

	CHECK(16, udvd(40000, 4) == 10000, 1);
	CHECK(17, udvd(65535, 255) == 257, 1);
	CHECK(18, udvd(7, 9) == 0, 1);
	CHECK(19, urem(40000, 7) == 40000 % 7, 1);
	CHECK(20, urem(65535, 256) == 255, 1);

	CHECK(21, shl(1, 4), 16);
	CHECK(22, shl(3, 0), 3);
	CHECK(23, shl(0x101, 4), 0x1010);
	CHECK(24, shr(256, 4), 16);
	CHECK(25, shr(-256, 4), -16);
	CHECK(26, shr(7, 0), 7);

	return 0;
}
