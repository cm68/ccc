/*
 * The shape of emitGv's size computation: an int count times a byte
 * size reached through a pointer.  The ccc-built c0 answered this
 * with garbage that changed from build to build - .ds -27776 for
 * int a[5] - while every simpler multiply in the suite passed.
 */
#include "rt.h"

struct type {
	unsigned char size;
	int count;
	struct type *sub;
};

struct type inner;
struct type arr;
struct type *var;

int
main()
{
	int size;

	inner.size = 2;
	arr.count = 5;
	arr.sub = &inner;
	var = &arr;

	CHECK(1, arr.count * arr.sub->size, 10);
	CHECK(2, var->count * var->sub->size, 10);
	size = var->count * var->sub->size;
	CHECK(3, size, 10);
	return 0;
}
