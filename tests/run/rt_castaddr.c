/*
 * Storing and loading through a cast constant.
 *
 *	*(int *)0x50 = v;
 *
 * This is how a driver or a bootstrap talks to fixed hardware, and it
 * is the shape the hard disk's second level boot used to hand the
 * controller its command address.  It compiled to NOTHING - not to
 * wrong code, to no code - because pass2 had no rule for it, and a
 * missing rule leaves an XXXXXX comment, which the assembler never
 * sees.  See CASTBUG.
 *
 * What pass1 hands pass2 is
 *
 *	(ASSIGN:short 80:short $_cmd)
 *
 * where a store to a global is (ASSIGN $_cmd 5) and one through a
 * pointer variable is (ASSIGN (DEREF $_cca) ...).  Both of those name
 * an ADDRESS on the left.  The cast constant arrives as a bare
 * integer: the cast to "int *" was folded away and the constant kept
 * its integer type, so nothing downstream can tell the address 80
 * from the value 80.
 *
 * z80 ONLY.  This suite runs everything twice, natively and under the
 * simulator, and compares - but 0x50 is a perfectly good address on a
 * Z80 and a null-page fault in a Linux process.  There is no literal
 * that means "writable" on both, so the native leg checks nothing and
 * says so.  The point of the test is the code the compiler emits, and
 * only one of the two legs can run it.
 *
 * Each store is read back through a pointer VARIABLE, a shape that
 * has always worked.  That isolates the store, so this fails for the
 * right reason while only the store side is broken, and keeps testing
 * the store once the load side works too.
 */
#include "rt.h"

#ifdef z80

int *wp;
char *bp;
long *lp;

/*
 * Never called, and it still has to compile and ASSEMBLE.  A literal
 * address above 0x7fff is the ordinary case - it is where memory
 * mapped hardware lives - and pass1 hands the constant down narrowed
 * to a short, so 0xf000 arrived as -4096 and asz said "invalid
 * operand".  Nothing here can be run: writing to 0xf000 under the
 * simulator lands on the operating system.  Compiling it is the test.
 */
hiaddr()
{
	*(int *)0xF000 = 1;
	*(char *)0xFFFE = 2;
	*(long *)0xF100 = 0x11223344L;
	return *(int *)0xF000;
}

main()
{
	wp = (int *)0x50;
	bp = (char *)0x60;
	lp = (long *)0x70;

	/* a word through a literal address */
	*(int *)0x50 = 0x1234;
	CHECK(1, *wp, 0x1234);

	/* a byte */
	*(char *)0x60 = 0x5a;
	CHECK(2, *bp & 0xff, 0x5a);

	/* the address of something, which is what the boot loader did */
	*(int *)0x50 = (int)&wp;
	CHECK(3, *wp, (int)&wp);

	/* and reading one back the same way */
	*wp = 0x7788;
	CHECK(4, *(int *)0x50, 0x7788);

	*bp = 0x39;
	CHECK(5, *(char *)0x60 & 0xff, 0x39);

	/*
	 * A long, which is four bytes through one literal address and
	 * was the width nobody had written.  There is no ld (nn),n on
	 * this machine, so the constant form has to point HL at the
	 * address and walk it - the word and byte forms above get away
	 * with ld (nn),hl and ld (nn),a and the long cannot.
	 */
	*(long *)0x70 = 0x11223344L;
	CHECK(6, *lp == 0x11223344L, 1);

	*lp = 0x55667788L;
	CHECK(7, *(long *)0x70 == 0x55667788L, 1);

	/* the halves land in the right order, low word first */
	CHECK(8, *(int *)0x70 & 0xffff, 0x7788);
	CHECK(9, *(int *)0x72 & 0xffff, 0x5566);

	/*
	 * Storing a VALUE THE TREE HAS TO WORK OUT, which every store
	 * above avoids: each of them stores a constant or an address,
	 * and a constant right operand takes a different path through
	 * pass2 than a variable does.  That is why this file passed
	 * while the shape the bug report opens with
	 *
	 *	*(int *)0x54 = v;
	 *
	 * was still emitting a marker.  A constant fails the gate that
	 * sends address and value to separate registers and never
	 * reaches the code that dropped the store; anything computed
	 * goes straight into it.
	 */
	{
		register int r;
		int v;
		char c;
		long l;

		v = 0x2468;
		*(int *)0x50 = v;
		CHECK(10, *wp, 0x2468);

		c = 0x6b;
		*(char *)0x60 = c;
		CHECK(11, *bp & 0xff, 0x6b);

		l = 0x0a0b0c0dL;
		*(long *)0x70 = l;
		CHECK(12, *lp == 0x0a0b0c0dL, 1);

		/* a computed value, not just a copy of one */
		*(int *)0x50 = v + 1;
		CHECK(13, *wp, 0x2469);

		/*
		 * A REGISTER variable, which lives in BC rather than HL.
		 * The store rules named HL and A and nothing else, so
		 * this one shape still had no rule after the others were
		 * written - and a register variable is exactly what a
		 * loop counter in a driver is.
		 */
		r = 0x1357;
		*(int *)0x50 = r;
		CHECK(14, *wp, 0x1357);

		/* the same register narrowed to a byte */
		*(char *)0x60 = r;
		CHECK(15, *bp & 0xff, 0x57);

		/* literal address to literal address, both ends */
		*(int *)0x52 = *(int *)0x50;
		CHECK(16, *(int *)0x52, 0x1357);

		/* an address that folds, with a value that does not */
		*(int *)(0x40 + 0x10) = v;
		CHECK(17, *wp, 0x2468);
	}

	return 0;
}

#else

main()
{
	return 0;		/* no literal address is writable here */
}

#endif
