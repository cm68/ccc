/*
 * An argument that folds away must not take the rest of the list.
 *
 * The argument list is threaded through the next pointer of each
 * argument node, and E_FUNARG is what marks a node as being on one.
 * Constant folding replaces a node with one of its children and frees
 * the original - and the original is the one holding the chain:
 *
 *	strncpy(n->name, name, 15);
 *
 * "name" is the first member of the struct, so its offset is zero,
 * "base + 0" folds to base, and the two arguments after it went out
 * with the node that had been holding them.  The call was emitted
 * with one argument, the other two were never pushed, and strncpy
 * took its length from whatever was on the stack.
 *
 * A member that is not first has a non-zero offset, so nothing folds
 * and the call is fine.  That is why this only ever showed on the
 * first member of a struct - and pass1's own newName does
 * strncpy(n->name, name, 15) on a struct whose name is first, so the
 * c0 that ccc built wrote over its own memory on every symbol it
 * interned.  Found with tests/diffpass1.sh.
 */
#include "rt.h"

struct rec {
	char name[8];		/* first: offset 0, folds */
	short k;
	char tail[8];		/* not first: offset 10, does not fold */
};

struct rec r;
short g1, g2, g3;
char *gp;

void
three(a, b, c)
char *a; char *b; short c;
{
	g1 = a ? 1 : 0;
	g2 = b ? 1 : 0;
	g3 = c;
}

short
four(a, b, c, d)
short a; short b; short c; short d;
{
	return a + b + c + d;
}

main()
{
	gp = "p";

	/* the first member: its address folds to the struct's own */
	g3 = 0;
	three(r.name, gp, 15);
	CHECK(1, g3, 15);
	CHECK(2, g1, 1);
	CHECK(3, g2, 1);

	/* a later member, which never folded */
	g3 = 0;
	three(r.tail, gp, 16);
	CHECK(4, g3, 16);

	/* the folding argument in the middle and at the end */
	g3 = 0;
	three(gp, r.name, 17);
	CHECK(5, g3, 17);

	/* other identity folds: x*1, x|0, x-0, x&~0 */
	CHECK(6, four(1 * 1, 2, 3, 4), 10);
	CHECK(7, four(1 | 0, 2, 3, 4), 10);
	CHECK(8, four(1 - 0, 2, 3, 4), 10);
	CHECK(9, four(2, 1 * 1, 3, 4), 10);
	CHECK(10, four(2, 3, 1 + 0, 4), 10);
	CHECK(11, four(2, 3, 4, 1 << 0), 10);

	/* a fully folded constant argument, which reuses the left node */
	CHECK(12, four(1 + 1, 2 * 2, 3, 4), 13);
	CHECK(13, four(1, 2 + 3, 4, 5), 15);

	/* and one folding to zero, which is the case that reuses and
	 * would otherwise lose the chain the same way */
	CHECK(14, four(0 * 5, 2, 3, 4), 9);

	return 0;
}
