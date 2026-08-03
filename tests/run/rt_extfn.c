/*
 * A function DECLARED inside a body.
 *
 * "extern short _pnum(), _fnum();" is K&R's way of saying a routine
 * returns something other than int, and it is how doprnt names the
 * two it calls.  pass1 builds a body's names afresh in its second
 * phase, and that phase never attached the "()" - so the name came
 * back carrying only the return type.  A short does not decay, so the
 * reference became a load rather than an address and the call went
 * indirect through the first two bytes of the routine's own code.
 *
 * printf("%d") was the visible end of it: doprnt jumped into the
 * weeds, came back to the top of the format, and printed the literal
 * text over and over.  qsort declares "extern char *malloc();" the
 * same way and called whatever malloc's first two bytes pointed at.
 */
#include "rt.h"

char pool[8];

short
adds(a, b)
short a, b;
{
	return a + b;
}

char *
getp()
{
	return pool;
}

long
getl()
{
	return 100000L;
}

/* declared in a body, returning short */
short
useshort()
{
	extern short adds();

	return adds(3, 4);
}

/* declared in a body, returning a pointer - qsort's malloc case */
char *
useptr()
{
	extern char *getp();

	return getp();
}

/* declared in a body, returning long */
long
uselong()
{
	extern long getl();

	return getl();
}

/* two names in one declaration, the way doprnt writes it */
short
usetwo()
{
	extern short adds();
	extern long getl();

	if (getl() != 100000L)
		return 0;
	return adds(10, 11);
}

main()
{
	CHECK(1, useshort(), 7);
	CHECK(2, useptr(), pool);
	CHECK(3, uselong(), 100000L);
	CHECK(4, usetwo(), 21);
	return 0;
}
