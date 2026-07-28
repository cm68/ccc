/*
 * Testing one bit out of a byte.
 *
 * The Z80 has an instruction for exactly this - bit n,r - and it sets
 * Z from the bit without disturbing A or the carry.  The alternative
 * is and with a mask, which needs the byte in A, and then a third
 * instruction to set the flags, because and leaves Z meaning the whole
 * result rather than the bit.
 *
 * The rule that reaches it takes the bit number from the mask, so a
 * wrong shift is the failure to look for and every position gets
 * checked.  A mask of 1 is deliberately not a power of two as far as
 * that rule is concerned - ispow2 answers zero for it and the match
 * reads that as no - so bit 0 goes the long way and has to still be
 * right.
 *
 * Masks of more than one bit must not take the rule at all.
 */
#include "rt.h"

unsigned char v;
unsigned char m;
char sc;
short r;

struct s { unsigned char f; unsigned char g; };
struct s st;
struct s *p;

short
regbits(c) register unsigned char c;
{
	short n;

	n = 0;
	if (c & 0x01) n += 1;
	if (c & 0x10) n += 2;
	if (c & 0x80) n += 4;
	return n;
}

short
locbits(x) unsigned char x;
{
	unsigned char loc;
	short n;

	loc = x;
	n = 0;
	if (loc & 0x04) n += 1;
	if (loc & 0x40) n += 2;
	return n;
}

main()
{
	/* every position on its own, so a shift off by one shows */
	v = 0x01; CHECK(1, (v & 0x01) != 0, 1);
	v = 0x02; CHECK(2, (v & 0x02) != 0, 1);
	v = 0x04; CHECK(3, (v & 0x04) != 0, 1);
	v = 0x08; CHECK(4, (v & 0x08) != 0, 1);
	v = 0x10; CHECK(5, (v & 0x10) != 0, 1);
	v = 0x20; CHECK(6, (v & 0x20) != 0, 1);
	v = 0x40; CHECK(7, (v & 0x40) != 0, 1);
	v = 0x80; CHECK(8, (v & 0x80) != 0, 1);

	/* and the neighbours it must not have tested instead */
	v = 0x10;
	CHECK(9, (v & 0x08) != 0, 0);
	CHECK(10, (v & 0x20) != 0, 0);
	CHECK(11, (v & 0x01) != 0, 0);
	CHECK(12, (v & 0x80) != 0, 0);

	/* the top bit, where a signed byte would go wrong */
	v = 0x80;
	CHECK(13, (v & 0x80) != 0, 1);
	v = 0x7f;
	CHECK(14, (v & 0x80) != 0, 0);
	CHECK(15, (v & 0x40) != 0, 1);

	/* a signed char with the top bit set */
	sc = 0x80;
	CHECK(16, (sc & 0x80) != 0, 1);
	sc = 0x01;
	CHECK(17, (sc & 0x80) != 0, 0);
	CHECK(18, (sc & 0x01) != 0, 1);

	/* a mask of more than one bit is not a bit test */
	v = 0x30;
	CHECK(19, (v & 0x30) != 0, 1);
	CHECK(20, (v & 0x03) != 0, 0);
	CHECK(21, (v & 0xf0) != 0, 1);
	CHECK(22, (v & 0x0f) != 0, 0);
	CHECK(23, (v & 0xff) != 0, 1);

	/* the value of the and, not just its truth */
	v = 0xff;
	CHECK(24, v & 0x10, 0x10);
	CHECK(25, v & 0x01, 1);
	CHECK(26, v & 0x80, 0x80);

	/* through a pointer, and on the far member */
	st.f = 0x08;
	st.g = 0x40;
	p = &st;
	CHECK(27, (p->f & 0x08) != 0, 1);
	CHECK(28, (p->f & 0x40) != 0, 0);
	CHECK(29, (p->g & 0x40) != 0, 1);
	CHECK(30, (p->g & 0x08) != 0, 0);

	/* a register variable */
	CHECK(31, regbits(0x01), 1);
	CHECK(32, regbits(0x10), 2);
	CHECK(33, regbits(0x80), 4);
	CHECK(34, regbits(0x91), 7);
	CHECK(35, regbits(0x6e), 0);

	/* a local, reached through the frame pointer */
	CHECK(36, locbits(0x04), 1);
	CHECK(37, locbits(0x40), 2);
	CHECK(38, locbits(0x44), 3);
	CHECK(39, locbits(0xbb), 0);

	/* the bit test as a value rather than a condition */
	v = 0x20;
	r = (v & 0x20) ? 1 : 2;
	CHECK(40, r, 1);
	r = (v & 0x10) ? 1 : 2;
	CHECK(41, r, 2);

	/* and inverted, which is the other flag out of the same test */
	v = 0x04;
	CHECK(42, !(v & 0x04), 0);
	CHECK(43, !(v & 0x08), 1);

	/*
	 * A mask that is not a constant cannot take the bit rule, whose
	 * pattern wants a NUMBER, and has to reach the same answer by the
	 * longer road.  Held in a variable rather than walked out of an
	 * array: indexing one by a register variable has gaps of its own
	 * and they are not what this file is about.
	 */
	v = 0x30;
	m = 0x10; CHECK(44, (v & m) != 0, 1);
	m = 0x20; CHECK(45, (v & m) != 0, 1);
	m = 0x08; CHECK(46, (v & m) != 0, 0);
	m = 0x80; CHECK(47, (v & m) != 0, 0);
	m = 0x30; CHECK(48, (v & m) != 0, 1);
	CHECK(49, v & m, 0x30);

	/* the constant and the variable form must agree */
	m = 0x10;
	CHECK(50, (v & 0x10) != 0, (v & m) != 0);
	m = 0x01;
	CHECK(51, (v & 0x01) != 0, (v & m) != 0);

	return 0;
}
