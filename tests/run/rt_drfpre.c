/*
 * A dereference of a pre-incremented pointer.
 *
 * "*++p" is emitted as the comma expression "(++p, *p)", and the second
 * half of it has to be the value at p.  For a variable in memory that is
 * two fetches - read the pointer, then read what it points at - and only
 * the first was emitted, which is how a plain "p" is spelled.  So the
 * whole expression was worth the pointer instead of what it pointed at.
 *
 * Nothing reported it.  The tree was well formed, every node reduced,
 * pass2 emitted what it was handed and ccc exited 0.  That is the
 * difference between this and PREINCBUG, which is also a mishandled
 * prefix step: that one is counted and fails the build, this one ships.
 * So these have to check the value - the code compiles, assembles, links
 * and runs either way.  See DEREFPREINC.
 *
 * v6 ls lists the current directory by pointing argv one short of a
 * static "." and letting the loop's own "*++argv" walk onto it.  It was
 * handed &argv, printed the bytes of a pointer as a filename, and then
 * spun in qsort over a list it never filled.
 *
 * A register variable was right all along and is here to keep it that
 * way: it has no address, the value is the register, so "*p" over one is
 * a single fetch and not two.
 */
#include "rt.h"

char	ca[4];
char	lb[4];
char   *sa[4];
short	wa[4];

char   *cp;
char  **pp;
short  *wp;
char   *r;

/* the callee reads what it was given, the way gstat does */
short one(s) char *s; { return *s; }
short onew(s) short *s; { return *s; }

/* the same, with the interesting argument either side of another */
short first(s, a) char *s; short a; { return *s; }
short second(a, s) short a; char *s; { return *s; }

setup()
{
	ca[0] = 'a'; ca[1] = 'b'; ca[2] = 'c'; ca[3] = 'd';
	lb[0] = 'w'; lb[1] = 'x'; lb[2] = 'y'; lb[3] = 'z';
	sa[0] = &ca[0]; sa[1] = &ca[1]; sa[2] = &ca[2]; sa[3] = &ca[3];
	wa[0] = 10; wa[1] = 20; wa[2] = 30; wa[3] = 40;
}

/* the reported shape, over each kind of storage a pointer can live in */
short garg() { pp = &sa[0]; return one(*++pp); }

short
larg()
{
	char **p;

	p = &sa[0];
	return one(*++p);
}

short
rarg()
{
	register char **p;

	p = &sa[0];
	return one(*++p);
}

/* and written as the two statements it is meant to be the same as */
short
split()
{
	pp = &sa[0];
	++pp;
	return one(*pp);
}

/* the step is scaled by what is pointed at, so check both widths */
short gbyte() { cp = &ca[0]; return *++cp; }
short gword() { wp = &wa[0]; return *++wp; }
short gwcall() { wp = &wa[0]; return onew(++wp); }

/* down as well as up */
short gdec() { pp = &sa[2]; return one(*--pp); }
short gdecb() { cp = &ca[2]; return *--cp; }

/* the argument in either position, and not alone */
short afirst() { pp = &sa[0]; return first(*++pp, 1); }
short asecond() { pp = &sa[0]; return second(1, *++pp); }

/* not confined to arguments: a plain assignment loses it the same way */
short
asgn()
{
	pp = &sa[0];
	r = *++pp;
	return *r;
}

/* postfix yields the value from before the step, and was already right */
short post() { pp = &sa[1]; return one(*pp++); }

/* the pointer really moved, and only once */
short
moved()
{
	pp = &sa[0];
	one(*++pp);
	return pp == &sa[1];
}

/* as an lvalue the address is what is wanted, which is a different path */
short
lval()
{
	char *p;

	p = &lb[0];
	*++p = 'Z';
	return lb[1];
}

/*
 * Two deep, as an lvalue.  "*++p = c" never reaches the comma rewrite -
 * the assignment parser unwraps it - but this does, with the outer store
 * unwrapped and "*++pp" left as the address to store through.  That
 * address is a fetch past the pointer's value just as the rvalue is, so
 * a rewrite that skipped the second fetch here stored the character into
 * the pointer instead of through it, and left the array it named alone.
 */
short
lval2()
{
	pp = &sa[0];
	**++pp = 'Y';
	return ca[1];
}

main()
{
	setup();

	/* the value at the stepped pointer, not the pointer */
	CHECK(1, garg(), 'b');
	CHECK(2, larg(), 'b');
	CHECK(3, rarg(), 'b');
	CHECK(4, split(), 'b');

	/* the two forms have to agree */
	CHECK(5, garg(), split());

	/* one step is one element, whatever the element is */
	CHECK(6, gbyte(), 'b');
	CHECK(7, gword(), 20);
	CHECK(8, gwcall(), 20);

	CHECK(9, gdec(), 'b');
	CHECK(10, gdecb(), 'b');

	/* wherever the argument sits in the list */
	CHECK(11, afirst(), 'b');
	CHECK(12, asecond(), 'b');

	/* and outside an argument list altogether */
	CHECK(13, asgn(), 'b');
	CHECK(14, r == &ca[1], 1);

	/* postfix is the value from before */
	CHECK(15, post(), 'b');
	CHECK(16, pp == &sa[2], 1);

	/* the side effect happened, once */
	CHECK(17, moved(), 1);

	/* the lvalue path stores through the stepped pointer */
	CHECK(18, lval(), 'Z');
	CHECK(19, lb[0], 'w');

	/* two deep, where the store must go through the pointer it names */
	CHECK(20, lval2(), 'Y');
	CHECK(21, sa[1] == &ca[1], 1);	/* and not into the pointer itself */

	return 0;
}

/* vim: set tabstop=4 shiftwidth=4 noexpandtab: */
