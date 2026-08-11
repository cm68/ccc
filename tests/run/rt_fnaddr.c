/*
 * The address of a function, taken with an explicit &.
 *
 *	sort (curbad, &order, &ex);
 *
 * A function name is already its own address, so &f and f are the same
 * value - and a BARE name always worked here.  With the & it did not:
 * pfxAddr handled &(DEREF x) and &array and had no case for a function
 * designator, so it fell through to a general "wrap it in AND" that
 * built a node with a LEFT AND NO RIGHT.
 *
 * AND is binary in astops.h, the one table pass1 writes by and pass2
 * reads by, so that node desynchronised the stream where it stood.
 * pass2 read whatever came next as the missing operand, walked off the
 * end and died - c1 SIGSEGV, no diagnostic from any pass, and the
 * driver reporting only "c1 failed".  Morrow's formatmw.c hit it in
 * report(), 95K of assembler in.
 *
 * Both legs run this.  It is ordinary C and the host is the reference;
 * what is being checked is that the call actually reaches the function
 * the & named, which a truncated tree cannot do.
 */
#include "rt.h"

static int add(a, b)
int a, b;
{
	return a + b;
}

static int sub(a, b)
int a, b;
{
	return a - b;
}

/* the shape formatmw uses: function addresses passed to a worker */
static int apply(f, a, b)
int (*f)();
int a, b;
{
	return (*f)(a, b);
}

/*
 * Two of them, as sort(n, &order, &ex) does.  The two pointers are
 * declared on separate lines: "int (*f)(), (*g)();" - two
 * function-pointer declarators sharing one base type - is a gap of its
 * own here, and this test is about the & rather than about that.
 */
static int apply2(f, g, a, b)
int (*f)();
int (*g)();
int a, b;
{
	return (*f)(a, b) * 10 + (*g)(a, b);
}

int gv;

main()
{
	int (*fp)();

	/* & on a function, assigned */
	fp = &add;
	CHECK(1, (*fp)(7, 3), 10);

	fp = &sub;
	CHECK(2, (*fp)(7, 3), 4);

	/* the bare name, which always worked - same value */
	fp = add;
	CHECK(3, (*fp)(7, 3), 10);
	CHECK(4, add == &add, 1);

	/* & on a function as a call argument - the formatmw shape */
	CHECK(5, apply(&add, 7, 3), 10);
	CHECK(6, apply(&sub, 7, 3), 4);
	CHECK(7, apply2(&add, &sub, 7, 3), 104);

	/* mixed with an ordinary argument, as sort(curbad, &order, &ex) */
	CHECK(8, apply2(&sub, &add, 9, 4), 63);

	/*
	 * An ARRAY of function pointers belongs here and is not
	 * here.  It compiles and then runs away: the
	 * native leg of this suite gets the right answers and the
	 * simulated one never returns.  That is a separate fault, in
	 * the array of pointers rather than in the &, and putting it in
	 * this file would only stop this file from running.
	 */

	/* calling straight through the address */
	CHECK(9, (*(&add))(2, 2), 4);

	/* & on data still works, and is a different thing entirely */
	gv = 42;
	CHECK(10, *(&gv), 42);

	return 0;
}
