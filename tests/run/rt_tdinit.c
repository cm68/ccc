/*
 * Initializers on a type that came from a typedef.
 *
 * cpp dissolves typedefs before pass1 ever sees them, so "T a[2] = {
 * 1, 2 }" becomes "int a[2] = { 1, 2 }" on the way through.  The part
 * that rewrites it walks the declarator looking for the comma that
 * ends it, and counted depth for parentheses and brackets but not for
 * braces.  So the comma between two initializers looked like the one
 * between two declarators: the rewriter ended the declarator there,
 * and the caller then emitted the comma a second time as the one
 * introducing the next.  pass1 was handed
 *
 *	int a [ 2 ] = { 1 , , 2 } ;
 *
 * and said "bad op" at the stray comma, which was the right answer to
 * the wrong input.
 *
 * A single-element initializer has no comma at all, and a plain "int"
 * one is never rewritten, so this needed a typedef AND an aggregate
 * AND more than one element before anything went wrong.  All three
 * are here, and so are the shapes that share the code: several
 * declarators on one line, an inferred bound, a nested brace, and a
 * struct - a brace inside a brace is what the depth count is for.
 *
 * These check the VALUES, not that it compiled.  Emitting the right
 * number of commas and emitting them in the right places are not the
 * same thing.
 */
#include "rt.h"

typedef int T;
typedef char C;

typedef struct point {
	int x;
	int y;
} P;

T one[3] = { 7, 8, 9 };
T two[] = { 4, 5 };			/* bound inferred */
T pair[2] = { 1, 2 }, other[2] = { 3, 4 };  /* two declarators */
T grid[2][2] = { { 1, 2 }, { 3, 4 } };	/* brace inside brace */
C text[] = "hi";
P pt = { 11, 22 };
T single[1] = { 42 };			/* the case that always worked */

main()
{
	CHECK(1, one[0], 7);
	CHECK(2, one[1], 8);
	CHECK(3, one[2], 9);

	CHECK(4, two[0], 4);
	CHECK(5, two[1], 5);
	CHECK(6, sizeof(two), 2 * sizeof(int));

	/* the second declarator is the one that got the extra comma */
	CHECK(7, pair[0], 1);
	CHECK(8, pair[1], 2);
	CHECK(9, other[0], 3);
	CHECK(10, other[1], 4);

	CHECK(11, grid[0][0], 1);
	CHECK(12, grid[0][1], 2);
	CHECK(13, grid[1][0], 3);
	CHECK(14, grid[1][1], 4);

	CHECK(15, text[0], 'h');
	CHECK(16, text[1], 'i');
	CHECK(17, text[2], 0);

	CHECK(18, pt.x, 11);
	CHECK(19, pt.y, 22);

	CHECK(20, single[0], 42);

	return 0;
}
