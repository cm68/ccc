/*
 * extern with the type left out.
 *
 *	extern _exit();		extern foo;		static bar;
 *
 * K&R spells an int this way, and the Bell Labs headers are full of
 * it.  Our own stdio.h declares _exit and exit like that, so every
 * program that included it failed to compile - and failed silently,
 * because the parse gave up before anything could report why.  The
 * bisect that found it was cutting the header in half by hand.
 *
 * Without a storage class the same spelling never took that path: a
 * leading name is dispatched as an expression statement, so "foo();"
 * parses as a call and always worked.  Only the storage class brought
 * it into declaration() with no type to work from.
 *
 * What must not regress is everything the storage class can be
 * followed by that IS a type - a keyword, unsigned, a struct, a
 * typedef name - because the fix turns on what comes next rather than
 * on what is missing.
 */

#include "rt.h"

/*
 * The shapes that used to fail.  Each extern is a declaration only, so
 * the definition that gives it storage follows - written the ordinary
 * way, which is also what proves the two agree that the type is int.
 */
extern	implicitfn();
extern	implicitvar;
extern	multi_a, multi_b;

int	implicitvar;
int	multi_a;
int	multi_b;

static	staticvar;              /* static is its own definition */

/* and the ones that must go on working */
extern int	explicitfn();
extern char	*charptr();
extern unsigned	uns;
struct thing { int x; };
extern struct thing *thingp;
typedef int MYT;
extern MYT	tdefvar;

/*
 * The declarations above are the test: reaching main at all means they
 * were taken as declarations of int and not as something else.  This
 * one is defined here so there is something to call, and it is
 * declared the same way to prove the declaration and the definition
 * agree about the type.
 */
extern	implicitfn();

implicitfn()
{
	return 42;
}

/*
 * Both flavours, because the fix turns on the token after the storage
 * class and an ansi prototype puts a different one there.  Each is
 * declared with the return type left out and defined to match, so a
 * wrong return type would show up as a wrong value rather than as a
 * diagnostic.
 */
extern	knrfn();
extern	ansifn(int a);
extern	ansivoid(void);

knrfn(a, b)
	int a;
	int b;
{
	return a + b;
}

ansifn(int a)
{
	return a + 2;
}

ansivoid(void)
{
	return 5;
}

main()
{
	CHECK(1, implicitfn(), 42);

	implicitvar = 7;
	CHECK(2, implicitvar, 7);

	staticvar = 9;
	CHECK(3, staticvar, 9);

	multi_a = 1;
	multi_b = 2;
	CHECK(4, multi_a + multi_b, 3);

	/* an int, so it must be two bytes wide and signed */
	implicitvar = -1;
	CHECK(5, implicitvar, -1);

	/* both flavours return what they computed, so both were typed int */
	CHECK(6, knrfn(20, 3), 23);
	CHECK(7, ansifn(40), 42);
	CHECK(8, ansivoid(), 5);

	return 0;
}
