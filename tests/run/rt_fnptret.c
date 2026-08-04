/*
 * A function that hands back a pointer to a function, declared twice.
 *
 * pass1 checks that a redeclaration agrees with the first declaration
 * about the return type, because an int read out of HL where a long
 * was returned takes half of it and nothing used to say so.  The test
 * it used was pointer identity on the interned type, which is right
 * until the return type has a function inside it: a function type is
 * built fresh from its parameter list, so
 *
 *	void (*signal(int, void (*)(int)))(int);	// the header
 *	void (*signal(sig, action))()		// the definition
 *
 * intern to different nodes.  Both hand back a two-byte pointer and
 * agree about everything a caller can see - they differ only in
 * whether the parameters of the function pointed at are spelled out,
 * which is the K&R silence the outer list is already allowed - and
 * the compiler called it a conflict.  libcpm's signal.c stopped
 * building.
 *
 * The declarators here are assembled outwards, so the prototype's
 * inner function type is still unattached when phase 1 reaches the
 * definition; a half-built side has said nothing to disagree with.
 */
#include "rt.h"

/*
 * Prototypes with the inner parameter list spelled out.  The outer
 * list is left silent, and the inner one is int - the type signal()
 * uses.  A parameter narrower than int cannot appear in a prototype
 * that an old-style declaration has to match, because an old-style
 * one promotes it, and that is a different argument than this test's.
 * The values below are small enough to be the same number in a
 * 16-bit int and a 32-bit one.
 */
short (*	getop())(int);
void (*	sethook())(void);

short
twice(v)
int v;
{
	return v * 2;
}

short
thrice(v)
int v;
{
	return v * 3;
}

static void (*	held)();
static short	hookran;

/*
 * The definitions say the same thing with the inner lists left
 * silent - the shape signal() has.
 */
short (*
getop(which, alt))()
short	which;
short (*	alt)(short);
{
	if (which)
		return alt;
	return twice;
}

void
hook()
{
	hookran = 1;
}

void (*
sethook(h))()
void (*	h)(void);
{
	void (*	prev)();

	prev = held;
	held = h;
	return prev;
}

main()
{
	short (*	f)();
	void (*	old)();

	/* the returned pointer is a real one and calls through */
	f = getop(0, thrice);
	CHECK(1, f(21), 42);

	f = getop(1, thrice);
	CHECK(2, f(21), 63);

	/* the return travels as two bytes, not as the int it is not */
	CHECK(3, getop(0, thrice) == twice, 1);
	CHECK(4, getop(1, thrice) == thrice, 1);

	/* a void-returning one, the exact shape of signal() */
	old = sethook(hook);
	CHECK(5, old, (void (*)())0);
	CHECK(6, hookran, 0);

	old = sethook(hook);
	CHECK(7, old == (void (*)())hook, 1);

	held();
	CHECK(8, hookran, 1);

	return 0;
}
