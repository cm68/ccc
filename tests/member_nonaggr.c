/*
 * Naming a member of something that has no members.
 *
 * All four of these are errors and pass1 says so - "no member" - but
 * it used to say it and then die.  The recovery node it returns in
 * place of the bad expression was built with a null type, and every
 * other CONST in pass1 carries inttype; phase 2 emits the recovery
 * node like any other and emitOperand reads e->type->size.  So c0
 * segfaulted a moment after diagnosing correctly, which from outside
 * looks like the compiler falling over on valid code.
 *
 * The undeclared cases are the ones that turn up in practice: a
 * missing header leaves the base with no type at all.  Dropping
 * <stdlib.h> from a source that used NULL is how this was found.
 *
 * The last two are here so the fix cannot be "never emit member
 * access" - they are correct and must still compile.
 */

f1()
{
	return zzz.qqq;		/* undeclared base */
}

f2()
{
	return zzz->qqq;	/* undeclared base, through a pointer */
}

f3()
{
	int a;

	return a.qqq;		/* declared, but not an aggregate */
}

f4()
{
	int a;

	a.qqq = 1;		/* the same as an lvalue */
	return 0;
}

struct s {
	int m;
};

struct s v;
struct s *p;

f5()
{
	return v.m;		/* correct, and must keep working */
}

f6()
{
	return p->m;
}
