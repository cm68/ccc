/*
 * A static local is not on the frame.
 *
 * Its storage is in data at an S<n> label - that is what makes it
 * survive the call - but its size was ALSO added to the function's
 * stack frame, reserved on every call and never touched.  Every
 * static did it, scalars as well as arrays; the array is only where
 * it shows, because two bytes lost to a static int is invisible and
 * five hundred to a buffer is not.  wsld's copy_segment declared
 * "static unsigned char cbuf[512]" precisely to keep it off the frame
 * and reported frame=522.
 *
 * Nothing was wrong at runtime, which is why it stood so long, and
 * that is exactly what makes the fix worth testing hard: the bytes
 * removed were bytes that used to exist.  If anything still addressed
 * a static through a frame offset it now writes into the caller's
 * frame instead of into dead space, and the symptom would be a
 * corrupted local somewhere else entirely.
 *
 * So these check three things at once: that statics still persist
 * across calls, that they still hold what was put in them, and that
 * writing through them leaves every neighbouring local alone.  See
 * STATICARRAY.
 */
#include "rt.h"

/* a static scalar counts across calls */
int
counter()
{
	static int n;

	n++;
	return n;
}

/* every width, all of which used to take frame space */
int
widths()
{
	static char c;
	static int n;
	static long l;

	c++;
	n += 2;
	l += 3;
	return c + n + (int)l;
}

/* a static array persists, and reports its own address */
char *
tag()
{
	static char buf[8];

	buf[0]++;
	buf[1] = 'A';
	return buf;
}

/*
 * A big static beside real locals.  The locals are written first,
 * then the whole static is filled, then the locals are checked: if
 * the static were still addressed as frame space that no longer
 * exists, this is where it would show.
 */
int
bigstatic()
{
	static char big[512];
	int	i;
	int	a;
	int	b;
	char	loc[4];

	a = 0x1234;
	b = 0x5678;
	loc[0] = 11;
	loc[1] = 22;
	loc[2] = 33;
	loc[3] = 44;

	for (i = 0; i < 512; i++)
		big[i] = 0x5a;

	if (a != 0x1234 || b != 0x5678)
		return -1;
	if (loc[0] != 11 || loc[1] != 22 || loc[2] != 33 || loc[3] != 44)
		return -2;
	if (big[0] != 0x5a || big[511] != 0x5a)
		return -3;
	return 1;
}

/*
 * A static declared inside a nested block.
 *
 * symDecl gives any name in a nested block an id of its own out of
 * shadowCtr, so that two sibling blocks each declaring "b" do not
 * collide in the hoisted locals list; those are spelled L<n>.  A
 * static is spelled S<n>, and shadowCtr restarts at zero for every
 * function while staticCtr runs the length of the file - so one of
 * these took a per-function number into a file-wide namespace and
 * landed on another function's static:
 *
 *	S1:	.ds 2		counter()'s static int n
 *	S1:	.ds 16		this ibuf
 *
 * One label, two variables, sharing storage.  Silent: counter() went
 * 1, 2, 3 and then answered 2.  That is what check 20 at the bottom
 * is really watching, and why this function is called between the
 * counter's third call and its fourth.
 */
int
nested(n)
int n;
{
	int r;

	r = 0;
	if (n) {
		static int inner;
		static char ibuf[16];

		inner++;
		ibuf[0] = inner;
		r = inner + ibuf[0];
	}
	return r;
}

/* two sibling blocks, each with a static of the same name */
int
siblings(which)
int which;
{
	int r;

	r = 0;
	if (which == 0) {
		static char b[4];

		b[0]++;
		r = b[0];
	}
	if (which == 1) {
		static char b[4];

		b[0] += 10;
		r = b[0];
	}
	return r;
}

/* two statics in one function must not share storage */
int
twostat(which)
int which;
{
	static char p[4];
	static char q[4];

	if (which == 0) {
		p[0] = 'p';
		q[0] = 'q';
	}
	return (p[0] == 'p') + (q[0] == 'q');
}

/* a static array and an ordinary automatic array in the same frame */
int
bothkinds()
{
	static char s[8];
	char a[8];
	int i;

	for (i = 0; i < 8; i++) {
		s[i] = i + 1;
		a[i] = i + 100;
	}
	for (i = 0; i < 8; i++)
		if (s[i] != i + 1 || a[i] != i + 100)
			return 0;
	return 1;
}

main()
{
	char *t1;
	char *t2;

	/* persistence: the whole point of a static */
	CHECK(1, counter(), 1);
	CHECK(2, counter(), 2);
	CHECK(3, counter(), 3);

	CHECK(4, widths(), 1 + 2 + 3);
	CHECK(5, widths(), 2 + 4 + 6);

	/* the array keeps its value AND its address between calls */
	t1 = tag();
	CHECK(6, t1[0], 1);
	CHECK(7, t1[1], 'A');
	t2 = tag();
	CHECK(8, t2[0], 2);
	CHECK(9, t1 == t2, 1);

	/* it lives in data, not on the stack: below the stack pointer */
	CHECK(10, (unsigned)t1 < (unsigned)&t1, 1);

	CHECK(11, bigstatic(), 1);
	CHECK(12, bigstatic(), 1);

	CHECK(13, nested(1), 2);
	CHECK(14, nested(1), 4);
	CHECK(15, nested(0), 0);

	/* sibling blocks: each "b" is its own storage */
	CHECK(21, siblings(0), 1);
	CHECK(22, siblings(1), 10);
	CHECK(23, siblings(0), 2);
	CHECK(24, siblings(1), 20);

	CHECK(16, twostat(0), 2);
	CHECK(17, twostat(1), 2);	/* still there on the next call */

	CHECK(18, bothkinds(), 1);
	CHECK(19, bothkinds(), 1);

	/* and the counter was not disturbed by any of the above */
	CHECK(20, counter(), 4);

	return 0;
}

/* vim: set tabstop=4 shiftwidth=4 noexpandtab: */
