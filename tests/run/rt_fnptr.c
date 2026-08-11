/*
 * Calling through a function pointer.
 *
 * The Z80 can call an address fixed at assembly time, and it can jump
 * to the one in HL, but it cannot call that one.  The difference is
 * only the return address, so one is borrowed: a call to a trampoline
 * that is nothing but "jp (hl)" pushes it, the jump hands over, and
 * the function's own ret comes back to the caller.
 *
 * The pointer has to be loaded after the arguments are pushed, not
 * before, or the pushes trample it - which is the whole reason the
 * callee is left alone until the end.
 *
 * pass1 had its own share: it looked for the return type on the thing
 * being called without going through the pointer first, so a call
 * through one had no type and was rejected the moment its result was
 * used.  Calling one and throwing the answer away was accepted, which
 * is why this asks for the answer every time.
 */
#include "rt.h"

short add2(a, b) short a, b; { return a + b; }
short neg(a) short a; { return -a; }
short zero() { return 99; }
long lret(a) short a; { return a * 1000L; }

/* both spellings of the declarator, and both scopes: a local is
 * built afresh in pass1's second phase and used to lose the "()" */
typedef short (*SFP)();

short (*gfp)();
short (*gtab[2])();
long (*glp)();
short calls;

short bump(a) short a; { calls++; return a + 1; }

main()
{
	short (*fp)();
	short (*ltab[2])();
	SFP tfp;
	short r;
	long l;
	short i;

	/* through a global */
	gfp = add2;
	CHECK(1, gfp(3, 4), 7);
	gfp = neg;
	CHECK(2, gfp(9), -9);
	gfp = zero;
	CHECK(3, gfp(), 99);

	/* through a local, which may live in a register */
	fp = add2;
	CHECK(4, fp(10, 5), 15);
	fp = neg;
	CHECK(5, fp(3), -3);

	/* the explicit form, which means the same thing */
	fp = add2;
	CHECK(6, (*fp)(2, 3), 5);
	gfp = neg;
	CHECK(7, (*gfp)(4), -4);

	/* the pointer reassigned between calls */
	fp = add2;
	r = fp(1, 1);
	fp = neg;
	r = r + fp(1);
	CHECK(8, r, 1);

	/* arguments that are themselves work, so the pushes are not
	 * trivial and would trample a pointer loaded too early */
	fp = add2;
	i = 3;
	CHECK(9, fp(i + 1, i * 2), 10);
	CHECK(10, fp(fp(1, 2), fp(3, 4)), 10);

	/* the answer used, not just taken */
	fp = add2;
	CHECK(11, fp(2, 3) + 1, 6);
	CHECK(12, fp(2, 3) == 5, 1);
	r = fp(6, 6);
	CHECK(13, r, 12);

	/* returning something wider than a word */
	glp = lret;
	l = glp(3);
	CHECK(14, l == 3000L, 1);

	/* called once, not twice */
	calls = 0;
	fp = bump;
	CHECK(15, fp(5), 6);
	CHECK(16, calls, 1);

	/*
	 * An ARRAY of them, which is the usual reason for having any -
	 * and which this said it was testing while testing two scalars.
	 * That gap is why "int (*tab[2])();" went wrong unnoticed: as a
	 * local it is rebuilt in pass1's second phase, the "()" was
	 * dropped, and the type read "array of pointer to short".  The
	 * call then derefed the function's address as though it were a
	 * pointer variable and jumped through the first two bytes of the
	 * function's own code.  It compiled clean; the simulated leg
	 * simply never came back.
	 */
	fp = add2;
	gfp = neg;
	CHECK(17, fp(20, 22), 42);
	CHECK(18, gfp(42), -42);

	ltab[0] = add2;
	ltab[1] = neg;
	CHECK(22, (*ltab[0])(20, 22), 42);
	CHECK(23, (*ltab[1])(42), -42);
	CHECK(24, ltab[0](5, 6), 11);		/* same call, no star */
	CHECK(25, ltab[1](6), -6);

	gtab[0] = neg;
	gtab[1] = add2;
	CHECK(26, (*gtab[0])(11), -11);
	CHECK(27, (*gtab[1])(11, 4), 15);
	CHECK(28, gtab[1](2, 2), 4);

	/* subscripted by a variable, so the index is not folded away */
	i = 1;
	CHECK(29, ltab[i](8), -8);
	CHECK(30, (*gtab[i])(8, 1), 9);

	/* the explicit spelling through a local, and a local typedef */
	fp = add2;
	CHECK(19, (*fp)(8, 9), 17);
	tfp = neg;
	CHECK(20, tfp(6), -6);
	CHECK(21, (*tfp)(7), -7);

	return 0;
}
