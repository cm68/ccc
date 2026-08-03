/*
 * An array is its storage, and never a register.
 *
 * An array carries the pointer bit as well - it decays - so the
 * allocator that hands IX to "a pointer used for field access" saw
 * one in a local array of structs and took it.  "stack[0].l = 11"
 * then assigned to the register, and every field reference addressed
 * the frame from wherever that had left it.
 *
 * The two allocators that hand out BC and B have always excluded
 * aggregates; the IX one did not, and it only escaped notice because
 * a declared register pointer usually takes IX first.  qsort has one,
 * which is exactly what was hiding it there.
 */
#include "rt.h"

struct pair {
	short l, r;
};

/* no register pointer to take IX first, which is the case that broke */
short
noreg()
{
	struct pair stack[20];
	short s;

	s = 0;
	stack[0].l = 11;
	stack[0].r = 22;
	stack[1].l = 33;
	stack[1].r = 44;
	if (stack[s].l != 11 || stack[s].r != 22)
		return 0;
	s++;
	if (stack[s].l != 33 || stack[s].r != 44)
		return 0;
	return 1;
}

/* the index stepping as it is read, the way a stack of requests works */
short
stepped()
{
	struct pair stack[8];
	short s, l, r;

	s = 0;
	stack[0].l = 5;
	stack[0].r = 6;
	l = stack[s].l;
	r = stack[s--].r;
	if (l != 5 || r != 6)
		return 0;
	return s == -1;
}

/* pushing onto it, which names the element twice in one statement */
short
pushed()
{
	struct pair stack[8];
	short s;

	s = 0;
	stack[++s].l = 7;
	stack[s].r = 8;
	return s == 1 && stack[1].l == 7 && stack[1].r == 8;
}

/* a plain array of shorts, no members involved */
short
plain()
{
	short v[10];
	short i;

	for (i = 0; i < 10; i++)
		v[i] = i * 3;
	return v[9] == 27 && v[0] == 0;
}

main()
{
	CHECK(1, noreg(), 1);
	CHECK(2, stepped(), 1);
	CHECK(3, pushed(), 1);
	CHECK(4, plain(), 1);
	return 0;
}
