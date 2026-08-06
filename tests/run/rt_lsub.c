/*
 * A long index into a pointer.
 *
 * An address is sixteen bits, so a long subscript is narrowed and
 * added - it is not long arithmetic.  Nothing put that conversion in
 * the tree, so pass2 met an address add with one long operand, had no
 * rule for it, and emitted nothing at all: the read came from address
 * zero and the store went nowhere, with only a comment to say so.
 * wsnm walks a library file with a long offset and is where it showed.
 */
#include "rt.h"

unsigned char data[8];
unsigned char *bp;
short idx[4];
short *sp;

main()
{
	long off;
	unsigned char c;
	short v;

	data[0] = 11; data[1] = 22; data[2] = 33; data[3] = 44;
	bp = data;
	idx[0] = 100; idx[1] = 200; idx[2] = 300;
	sp = idx;

	/* the plain subscript */
	off = 2;
	CHECK(1, bp[off], 33);

	/* the postfix step, which is the shape wsnm writes */
	off = 1;
	c = bp[off++];
	CHECK(2, c, 22);
	CHECK(3, off == 2L, 1);

	/* the same sum written as pointer arithmetic */
	off = 3;
	CHECK(4, *(bp + off), 44);

	/* a scaled element, where the index is multiplied first */
	off = 2;
	CHECK(5, sp[off], 300);

	/* a store through a long subscript */
	off = 5;
	bp[off] = 77;
	CHECK(6, data[5], 77);

	/* a long index whose high half is set: an address is the low
	 * sixteen bits, and the top of the long is not part of it */
	off = 0x10000L + 1;
	CHECK(7, bp[(short)off], 22);

	/* a constant long index */
	CHECK(8, bp[3L], 44);

	/* the index computed from a long expression */
	off = 8;
	v = bp[off - 5];
	CHECK(9, v, 44);

	return 0;
}
