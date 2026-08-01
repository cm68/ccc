/*
 * memset takes (pointer, VALUE, COUNT).  The Z80 libc had value and
 * count swapped, so every standard call set zero bytes and nothing
 * complained: permalloc's zeroing guarantee held only where the
 * freshly loaded image happened to be zero underneath, and cpp's
 * intern pool found the places where it was not - ids born dirty at
 * whatever the heap held before.  The permalloc leg reproduces that
 * exact shape: a malloc first, so the arena lands on ground the
 * loader did not sweep, then fresh allocations must still come back
 * clean after the arena has been dirtied and reused.
 */
#include "rt.h"

extern char *malloc();
extern char *permalloc();

char buf[32];

int
main()
{
	int i;
	char *p;

	/* standard order: value then count */
	for (i = 0; i < 32; i++)
		buf[i] = 0x5a;
	memset(buf + 4, 0, 8);
	if (buf[3] != 0x5a) return 1;
	if (buf[4] != 0 || buf[11] != 0) return 2;
	if (buf[12] != 0x5a) return 3;
	memset(buf, 7, 3);
	if (buf[0] != 7 || buf[2] != 7) return 4;
	if (buf[3] != 0x5a) return 5;
	memset(buf, 9, 0);
	if (buf[0] != 7) return 6;

#if defined(RT_CCC) || defined(RT_ZC3)
	/* permalloc zeroes what it hands out, wherever the chunk lands */
	malloc(44);
	for (i = 0; i < 60; i++) {
		int j;

		p = permalloc(12);
		if (!p) return 7;
		for (j = 0; j < 12; j++)
			if (p[j]) return 8;
		for (j = 0; j < 12; j++)
			p[j] = 0xEE;	/* dirty it for the next pass */
	}
#endif
	return 0;
}
