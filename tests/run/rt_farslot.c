/*
 * Stores to a frame slot the (iy+d) window cannot reach.
 *
 * The displacement in (iy+d) is seven bits signed.  Put a big array on
 * the frame and everything declared with it moves past that window, so
 * a slot stops being an addressing mode: its address has to be formed
 * with 16-bit arithmetic, which lands in HL.  The value being stored
 * wants HL too, and pass2 called a far slot a "location descriptor" -
 * something that needs no register - so it never staged the address on
 * the stack.  Both went to HL and the second overwrote the first.
 *
 * The visible failure was a missing store, not a wrong one, and it was
 * SILENT: with the address gone, the store matched a rule that emits
 * nothing.  libcpm's read() lost "buffer[0] = nbytes" before every
 * console read, write() lost its end-of-file byte, and qsort lost a
 * word of initialisation - none of which left so much as a marker.
 *
 * The native leg cannot fail any of this; a host frame has no window.
 * It is here as the reference the Z80 leg is compared against.
 */
#include "rt.h"

/* the byte store: a value in a register into a far slot - read.c */
short
farbyte()
{
	char big[2000];
	short n;

	n = 37;
	big[0] = (char)n;
	big[1999] = (char)(n + 1);
	return big[0] == 37 && big[1999] == 38;
}

/* the constant store: nothing to compute, but the address is far - qsort */
short
farconst()
{
	short big[1000];
	char pad[300];

	big[0] = 0;
	big[999] = 0x5a5a;
	pad[0] = 0;
	return big[0] == 0 && big[999] == 0x5a5a;
}

/*
 * The GAP case: the destination is an indexed store the tree works out
 * AND the value is a far slot's address.  Both want HL.  This is the
 * ordinary argument-vector idiom, and it is what stopped the driver
 * from building for Micronix.
 */
short
farvec()
{
	char big[2000];
	char *av[8];
	short i;

	i = 0;
	av[i++] = big;
	av[i++] = big + 100;
	big[0] = 'a';
	big[100] = 'b';
	return i == 2 && *av[0] == 'a' && *av[1] == 'b';
}

/* the same address into a plain pointer, then written through */
short
farptr()
{
	char big[1500];
	char *p;

	p = big;
	*p = 'x';
	p = &big[1499];
	*p = 'y';
	return big[0] == 'x' && big[1499] == 'y';
}

/* a far slot written from another far slot: address and value both far */
short
fartofar()
{
	short big[800];
	short i;

	for (i = 0; i < 800; i++)
		big[i] = i;
	big[799] = big[0] + 7;
	return big[799] == 7 && big[1] == 1;
}

/*
 * Members of a struct on a far frame, and a compound assignment
 * through a pointer into one.  The member offset rides on top of an
 * address that is already the wrong side of the window, so these take
 * the same staging as a bare slot.
 */
struct s {
	short	a;
	char	b;
	char	*p;
};

short
farmemb()
{
	struct s tab[100];
	char buf[1000];
	struct s *q;
	short i;

	i = 3;
	tab[i].a = 5;
	tab[i].b = 'z';
	tab[i].p = buf;
	q = &tab[i];
	q->a += 2;
	buf[0] = tab[i].b;
	buf[999] = (char)tab[i].a;
	return tab[i].a == 7 && buf[0] == 'z' && buf[999] == 7 &&
	    tab[i].p == buf;
}

main()
{
	CHECK(1, farbyte(), 1);
	CHECK(2, farconst(), 1);
	CHECK(3, farvec(), 1);
	CHECK(4, farptr(), 1);
	CHECK(5, fartofar(), 1);
	CHECK(6, farmemb(), 1);
	return 0;
}
