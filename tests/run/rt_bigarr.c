/*
 * A local array bigger than 255 bytes.
 *
 * A type node holds "how big is one of me" in a char, and an array
 * keeps its real extent as a count times the element - which is why
 * there is a typesize() to ask.  The frame allocator read the byte
 * instead, so every local array got its extent modulo 256: qsort's
 * "char xbuf[800]" was handed 32 bytes of frame and then copied 800
 * bytes through it, over the saved registers and the return address.
 *
 * The far end is what tells the truth here.  A short array is checked
 * beside it so a frame that is simply too small shows up as well.
 */
#include "rt.h"

short
big()
{
	char a[800];
	short i;

	for (i = 0; i < 800; i++)
		a[i] = (char)(i & 0x7f);
	return a[799] == (char)(799 & 0x7f) && a[0] == 0;
}

/* just over the byte, where the truncation leaves almost nothing */
short
justover()
{
	char a[300];
	short i;

	for (i = 0; i < 300; i++)
		a[i] = (char)(i & 0x7f);
	return a[299] == (char)(299 & 0x7f);
}

/*
 * Two of them, so the second one's base has to clear the whole of the
 * first: a truncated extent overlaps them and the writes collide.
 */
short
twobig()
{
	char a[400];
	char b[400];
	short i;

	for (i = 0; i < 400; i++)
		a[i] = 1;
	for (i = 0; i < 400; i++)
		b[i] = 2;
	for (i = 0; i < 400; i++)
		if (a[i] != 1 || b[i] != 2)
			return 0;
	return 1;
}

/* a local that has to survive a big array being filled beneath it */
short
neighbour()
{
	char a[600];
	short guard;
	short i;

	guard = 0x5a5a;
	for (i = 0; i < 600; i++)
		a[i] = (char)i;
	return guard == 0x5a5a;
}

main()
{
	CHECK(1, big(), 1);
	CHECK(2, justover(), 1);
	CHECK(3, twobig(), 1);
	CHECK(4, neighbour(), 1);
	return 0;
}
