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

	return 0;
}

#else

main()
{
	return 0;		/* no literal address is writable here */
}

#endif
