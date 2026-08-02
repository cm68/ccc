/*
 * Stepping the pointer that a register-homed pointer POINTS AT.
 *
 * This is the whole of stdio's idiom - "*f->_ptr++" and "*--f->_ptr"
 * with the FILE homed in IX.  The rewriter deliberately leaves the
 * DEREF standing for a step so that a load rule cannot turn it into a
 * fetch, and the shape it leaves is i(D(V)).  Only the BC form of
 * that, ?(D(B)), had ever been written, so filbuf, ungetc and fclose
 * stepped nothing at all: the marker in the listing is a comment, and
 * the code went on to read and write through a pointer that never
 * moved.
 *
 * The member is at offset zero on purpose - a non-zero one folds into
 * an INDEX first and took a different path that already worked.
 */
#include "rt.h"

struct buf {
	char	*ptr;		/* offset 0: the shape that had no rule */
	int	cnt;
};

char area[8];
struct buf b;

/* postfix: the value is the pointer from BEFORE the step */
int
get(f)
register struct buf *f;
{
	f->cnt--;
	return *f->ptr++;
}

/* prefix on the pointed-at pointer, value used as the address */
void
unget(f, c)
register struct buf *f;
char c;
{
	*--f->ptr = c;
	f->cnt++;
}

/* prefix for its value alone */
char *
bump(f)
register struct buf *f;
{
	return ++f->ptr;
}

int
main()
{
	int i;

	for (i = 0; i < 8; i++)
		area[i] = 'A' + i;

	b.ptr = area;
	b.cnt = 8;

	CHECK(1, get(&b), 'A');		/* old value, pointer moved */
	CHECK(2, b.ptr - area, 1);
	CHECK(3, b.cnt, 7);
	CHECK(4, get(&b), 'B');
	CHECK(5, b.ptr - area, 2);

	unget(&b, 'z');			/* steps back, writes through */
	CHECK(6, b.ptr - area, 1);
	CHECK(7, area[1], 'z');
	CHECK(8, b.cnt, 7);
	CHECK(9, get(&b), 'z');		/* reads back what unget wrote */

	b.ptr = area;
	CHECK(10, bump(&b) - area, 1);	/* prefix answers the NEW value */
	CHECK(11, b.ptr - area, 1);

	return 0;
}

/* vim: set tabstop=4 shiftwidth=4 noexpandtab: */
