/*
 * A function declared in among the variables, inside a function.
 *
 *	char *file, *s_getmsg(), msg[80], *reply;
 *
 * Declaring the function that returns a pointer beside the variables
 * that will hold its result is an ordinary K&R habit - there is
 * nowhere else to put it before prototypes - and old sources are full
 * of it.  cmd/s does exactly this, and used "file" eight lines later.
 *
 * cpp's declaration splitter dropped every declarator collected ahead
 * of the function one.  A function declarator emits its own name and
 * then leaves the walk - the rest of the line flows through unchanged,
 * which is why the names AFTER it survived - and nothing emitted what
 * had been collected before it.  So the name reached pass1 as if it
 * had never been written, and its use was reported as an unknown name
 * being called: "bad op (not fn)".
 *
 * File scope was never affected; only a declaration inside a block
 * goes through the splitter.
 *
 * These have to USE the names, not just declare them.  The failure is
 * a name silently absent, so a test that declared them and stopped
 * would compile clean whether or not it was fixed.  See LOCALFUNCDECL.
 */
#include "rt.h"

char	buf[8];

char *
retp()
{
	buf[0] = 'p';
	return buf;
}

int
reti()
{
	return 7;
}

char
retc()
{
	return 'k';
}

/* the reported shape: a name before, one after */
int
mid()
{
	char *a, *retp(), *b;
	char m[4];

	m[0] = 'x';
	a = m;
	b = m;
	return (*a == 'x') + (*b == 'x');
}

/* two before it, none after */
int
before()
{
	char *a, *b, *retp();
	char m[4];

	m[0] = 'y';
	a = m;
	b = m;
	return (*a == 'y') + (*b == 'y');
}

/* the function first, which always worked - keep it working */
int
first()
{
	char *retp(), *a;
	char m[4];

	m[0] = 'z';
	a = m;
	return *a == 'z';
}

/* no function declarator at all - the ordinary case */
int
plain()
{
	char *a, *b;
	char m[4];

	m[0] = 'w';
	a = m;
	b = m;
	return (*a == 'w') + (*b == 'w');
}

/* not about char, and not about pointers */
int
ints()
{
	int a, reti(), b;

	a = 3;
	b = 4;
	return a + b;
}

/* not about the return type either: a plain-char function among pointers */
int
mixret()
{
	char *a, retc(), *b;
	char m[4];

	m[0] = 'v';
	a = m;
	b = m;
	return (*a == 'v') + (*b == 'v');
}

/*
 * An array declarator in the same position.  It stays inside the
 * splitter's walk rather than leaving it, so the names around it were
 * never lost - this is the guard that the fix left that path alone.
 */
int
witharr()
{
	char *a, arr[4], *b;
	char m[4];

	m[0] = 'u';
	a = m;
	b = m;
	arr[0] = 3;
	return (*a == 'u') + (*b == 'u') + arr[0];
}

/* the declared functions are real and callable */
int
called()
{
	char *a, *retp(), *b;

	a = retp();
	b = a;
	return (*a == 'p') + (*b == 'p');
}

main()
{
	CHECK(1, mid(), 2);
	CHECK(2, before(), 2);
	CHECK(3, first(), 1);
	CHECK(4, plain(), 2);
	CHECK(5, ints(), 7);
	CHECK(6, mixret(), 2);
	CHECK(7, witharr(), 5);
	CHECK(8, called(), 2);

	/* and the function declared inside a block really is that function */
	CHECK(9, reti(), 7);
	CHECK(10, *retp(), 'p');
	CHECK(11, retc(), 'k');

	return 0;
}

/* vim: set tabstop=4 shiftwidth=4 noexpandtab: */
