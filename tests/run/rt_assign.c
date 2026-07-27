/*
 * Assignment: compound operators, increment and decrement used for
 * their value, and the once-only rule.
 *
 * The lvalue of a compound assignment has to be evaluated exactly
 * once.  "*p++ += 5" must advance p by one, not two, and
 * "*get() += 5" must call get once - which is why the count matters
 * here rather than just the result.
 */
#include "rt.h"

short g, arr[8];
char cg;
short calls;

short *getp()
{
	calls++;
	return &g;
}

main()
{
	short i, k, *p;

	/* compound assignment on a local */
	i = 10;
	i += 5;  CHECK(1, i, 15);
	i -= 3;  CHECK(2, i, 12);
	i *= 2;  CHECK(3, i, 24);
	i /= 4;  CHECK(4, i, 6);
	i %= 4;  CHECK(5, i, 2);
	i = 0xff;
	i &= 0x0f;  CHECK(6, i, 0x0f);
	i |= 0xf0;  CHECK(7, i, 0xff);
	i ^= 0xaa;  CHECK(8, i, 0x55);
	i = 8;
	i <<= 2;  CHECK(9, i, 32);
	i >>= 1;  CHECK(10, i, 16);

	/* the same on a global, and on a char */
	g = 10;
	g += 5;  CHECK(11, g, 15);
	g *= 3;  CHECK(12, g, 45);
	cg = 10;
	cg += 5;  CHECK(13, cg, 15);
	cg |= 0x80;  CHECK(14, cg, (char)0x8f);

	/* and on an array element */
	arr[2] = 100;
	arr[2] += 50;  CHECK(15, arr[2], 150);
	i = 3;
	arr[i] = 7;
	arr[i] += 1;  CHECK(16, arr[3], 8);

	/* postfix yields the old value, prefix the new */
	i = 5;
	CHECK(17, i++, 5);
	CHECK(18, i, 6);
	CHECK(19, ++i, 7);
	CHECK(20, i, 7);
	CHECK(21, i--, 7);
	CHECK(22, i, 6);
	CHECK(23, --i, 5);
	CHECK(24, i, 5);

	/* in the middle of an expression */
	i = 5;
	CHECK(25, i++ + 10, 15);
	CHECK(26, i, 6);
	i = 0; k = 10;
	CHECK(27, ++i + k--, 11);
	CHECK(28, i, 1);
	CHECK(29, k, 9);

	/* through a pointer */
	g = 20;
	p = &g;
	*p += 10;  CHECK(30, g, 30);
	++*p;      CHECK(31, g, 31);
	(*p)++;    CHECK(32, g, 32);

	/* the lvalue is evaluated once: p advances by one, not two */
	arr[0] = 20;
	arr[1] = 99;
	p = arr;
	*p++ += 5;
	CHECK(33, arr[0], 25);
	CHECK(34, arr[1], 99);
	CHECK(35, p - arr, 1);

	/* and a call in the lvalue happens once */
	g = 10;
	calls = 0;
	*getp() += 5;
	CHECK(36, g, 15);
	CHECK(37, calls, 1);

	/* a subscript with a side effect steps once per statement */
	i = 0;
	arr[i++] = 5;
	arr[i++] = 10;
	CHECK(38, arr[0], 5);
	CHECK(39, arr[1], 10);
	CHECK(40, i, 2);

	/* assignment is right associative and yields its value */
	i = k = 5;
	CHECK(41, i, 5);
	CHECK(42, k, 5);
	i = 1; k = 2;
	i += k += 1;
	CHECK(43, k, 3);
	CHECK(44, i, 4);

	return 0;
}
