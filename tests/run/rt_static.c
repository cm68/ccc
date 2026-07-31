/*
 * static locals.
 *
 * A static local is a local by scope and a global by storage, and it
 * was being treated as a local in three places at once:
 *
 *   - cpp splits a local's initializer into a declaration and an
 *     assignment, which is exactly right for a plain local and wrong
 *     for this one: the assignment ran on every call, so
 *     "static int n = 5; n++;" returned 6 for ever.  An aggregate
 *     initializer was already left alone, which is why a static array
 *     behaved differently from a static scalar.
 *
 *   - pass1 emitted a reference to it as a frame slot.  Its storage is
 *     at an S<n> label with the globals, so the value did not survive
 *     the call that set it, and a static array read whatever was on
 *     the stack.
 *
 *   - the register allocator offered it a register, a local being what
 *     it looked like.  A register does not survive the call either,
 *     and a read of one came back as the address.
 *
 * pass1's own sclassBit is
 *
 *	static unsigned char sc_bit[] = { SC_TYPEDEF, ... };
 *
 * so the c0 that ccc built rejected every typedef it was given, which
 * is most of a header.  Found with tests/diffpass1.sh.
 */
#include "rt.h"

short
counter()
{
	static short n;

	n++;
	return n;
}

short
initcounter()
{
	static short n = 10;

	n++;
	return n;
}

/* the shape pass1 uses: a static table indexed by a parameter */
unsigned char
bit(i)
unsigned char i;
{
	static unsigned char tab[5] = { 1, 2, 4, 8, 16 };

	if (i > 4)
		return 0;
	return tab[i];
}

short
statarr()
{
	static short a[4];

	a[0]++;
	a[2] = a[0] * 10;
	return a[2];
}

char
statchar()
{
	static char c = 'x';

	return c;
}

char *
statptr()
{
	static char *p = "hello";

	return p;
}

/* two functions with a static of the same name must not share one */
short
mine()
{
	static short v = 100;

	v++;
	return v;
}

short
yours()
{
	static short v = 200;

	v++;
	return v;
}

main()
{
	/* the value has to survive the call that set it */
	CHECK(1, counter(), 1);
	CHECK(2, counter(), 2);
	CHECK(3, counter(), 3);

	/* and the initializer must not run again */
	CHECK(4, initcounter(), 11);
	CHECK(5, initcounter(), 12);

	/* a static table, which is what pass1 does */
	CHECK(6, bit(0), 1);
	CHECK(7, bit(1), 2);
	CHECK(8, bit(2), 4);
	CHECK(9, bit(3), 8);
	CHECK(10, bit(4), 16);
	CHECK(11, bit(9), 0);

	CHECK(12, statarr(), 10);
	CHECK(13, statarr(), 20);

	CHECK(14, statchar(), 'x');
	CHECK(15, strcmp(statptr(), "hello"), 0);

	/* same name, different functions, separate storage */
	CHECK(16, mine(), 101);
	CHECK(17, yours(), 201);
	CHECK(18, mine(), 102);
	CHECK(19, yours(), 202);

	return 0;
}
