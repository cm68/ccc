/*
 * free(0) is a no-op in C.  This libc's free does not test for it -
 * it steps back one header and parks the allocation cursor there -
 * so the question is whether the heap survives, not whether the call
 * returns.  Allocate across it and check the data.
 */
#include "rt.h"

char *a, *b, *p;

main()
{
	short i;

	a = malloc(40);
	for (i = 0; i < 40; i++) a[i] = (char)(i + 1);
	p = 0;
	free(p);			/* the no-op C promises */
	b = malloc(40);
	CHECK(1, b != 0, 1);
	for (i = 0; i < 40; i++) b[i] = (char)(100 - i);
	/* the first block must be untouched by the second */
	for (i = 0; i < 40; i++)
		if (a[i] != (char)(i + 1))
			return 2;
	for (i = 0; i < 40; i++)
		if (b[i] != (char)(100 - i))
			return 3;
	CHECK(4, a != b, 1);
	free(a);
	free(b);
	return 0;
}
