/*
 * A static function called from both sides of its definition.
 *
 * A name called before it is declared is extern int - K&R's rule, and
 * pass1 follows it.  When the definition further down says static,
 * the calls already emitted are not revisited: phase 1 and phase 2
 * run per function, so everything above the definition is written out
 * before the definition is read.  The calls above went to _g and the
 * ones below to the local label - one name, one file, two functions,
 * and no complaint.  That is diagnosed now (tests/static_latedef.c
 * checks the diagnosis), and the source wants what K&R always
 * required: a forward declaration.
 *
 * This is the other half - that with the declaration present, both
 * sides reach the same function.  It has to RUN: two functions with
 * the same name would link only if something else supplied the
 * global, and here nothing does, so getting it wrong is a link error
 * rather than a wrong answer.  What can be checked at runtime is that
 * the answer comes from the static one and that state it keeps is
 * seen by both callers.  See STATICLATEDEF.
 */
#include "rt.h"

static int	bump();
static char *	label();

/* called before the definition */
int
early()
{
	return bump(1);
}

static int	count;

static int
bump(n)
int n;
{
	count = count + n;
	return count;
}

static char *
label()
{
	static char buf[4];

	buf[0] = 'L';
	buf[1] = 0;
	return buf;
}

/* called after it */
int
late()
{
	return bump(2);
}

/*
 * An ordinary extern called before its definition, which is the case
 * the implicit declaration is actually for: it must still resolve to
 * the global.
 */
int
callsext()
{
	return extfn(5);
}

int
extfn(n)
int n;
{
	return n * 3;
}

main()
{
	char *p;

	/*
	 * Both callers reach the same function and share its state: if
	 * early() had bound to a different one, count would not carry.
	 */
	CHECK(1, early(), 1);
	CHECK(2, late(), 3);
	CHECK(3, early(), 4);
	CHECK(4, late(), 6);
	CHECK(5, count, 6);

	/* the static's own static, on the far side of the declaration */
	p = label();
	CHECK(6, *p, 'L');

	/* and an implicit call to a real extern still finds the global */
	CHECK(7, callsext(), 15);
	CHECK(8, extfn(2), 6);

	return 0;
}

/* vim: set tabstop=4 shiftwidth=4 noexpandtab: */
