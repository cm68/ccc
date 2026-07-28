/*
 * The index register.
 *
 * pass1 puts a pointer in IX when it is used to reach struct fields,
 * and the indexed rules were written for exactly that.  The pointer
 * is still a value though - it gets assigned, compared, passed and
 * stored like any other - and almost none of that had a rule.
 *
 * Storing it is the awkward direction: IX cannot be written to memory
 * a half at a time, its halves being reachable only through the
 * undocumented byte forms and not from (hl) at all, so it has to go
 * out through DE.
 *
 * Nothing here is IX-specific by accident.  An indexed location
 * renders its own base register, so the same rules serve a frame
 * local through IY and a struct member through IX.
 */
#include "rt.h"

struct s { short a; short b; char c; };
struct s o1, o2;
struct s *slot;
struct s *arr[3];
short sv;

main()
{
	struct s *q;			/* field access puts this in IX */
	struct s **pp;
	short i;

	o1.a = 10; o1.b = 20; o1.c = 3;
	o2.a = 30; o2.b = 40; o2.c = 4;

	q = &o1;
	CHECK(1, q->a != 10, 0);	/* the use that earns it IX */

	/* stored through a pointer held in HL */
	pp = arr;
	*pp = q;
	CHECK(2, arr[0] != q, 0);
	CHECK(3, arr[0]->a != 10, 0);

	i = 1;
	pp[i] = q;
	CHECK(4, arr[1]->b != 20, 0);

	/* stored to a global and to a frame slot */
	slot = q;
	CHECK(5, slot->a != 10, 0);
	pp = &slot;
	*pp = q;
	CHECK(6, slot->b != 20, 0);

	/* a word narrowed on its way through the index register */
	sv = 300;			/* 0x12c - low byte 0x2c = 44 */
	q->c = sv;
	CHECK(7, (o1.c & 0xff) != 44, 0);
	q->c = 7;
	CHECK(8, o1.c != 7, 0);

	/* and the register still holds what it should afterwards */
	CHECK(9, q->a != 10, 0);
	q = &o2;
	CHECK(10, q->a != 30, 0);

	return 0;
}
