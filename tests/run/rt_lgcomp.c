/*
 * Compound assignment whose right hand side is a long.
 *
 * "a += b" is taken apart by pass2 into an assignment of "a + b", and
 * that inner operator is left holding operands of two widths when the
 * lvalue is narrower than the long.  pass1 used to hide it: candemote()
 * said the low bytes of a long were the ones lying at its own address
 * and demote() retyped the tree to read them, which costs no code.
 *
 * That was only ever true little-endian.  A long keeps its high word at
 * the lower address here (QLONG.md, NUXI), so the low half is two bytes
 * along and candemote() refuses - and with nothing put in its place,
 *
 *	f->_cnt -= roffs;
 *	f->_ptr += roffs;
 *
 * in libcpm's fseek reached pass2 as a short SUB with a long under it.
 * No rule matches that, so NO CODE AT ALL came out for either
 * statement: the count went down by nothing and the pointer never
 * moved.  Silent, because a missing production is not a wrong answer
 * until something reads the variable.
 *
 * The plain form "s = s + l" was always right, which is what kept this
 * rare - the two spellings of the same thing disagreed.
 *
 * Every check below is the compound form against the plain one, or
 * against the value computed by hand.
 */
#include "rt.h"

long l;
short s;
char c;
int i;
unsigned short us;
char buf[16];
char *p;

main()
{
	/* the two from fseek, in the shapes it wrote them */
	s = 100;
	l = 30L;
	s -= l;
	CHECK(1, s, 70);

	p = buf;
	l = 5L;
	p += l;
	CHECK(2, p - buf, 5);

	/* add and subtract, each against the plain spelling */
	s = 1000;
	l = 234L;
	s += l;
	CHECK(3, s, 1234);

	s = 1000;
	l = 234L;
	s = s + l;
	CHECK(4, s, 1234);

	i = 500;
	l = 1200L;
	i -= l;
	CHECK(5, i, -700);

	/*
	 * A long whose high word is set: only the low half may be used,
	 * because the result is a short and that is all it can hold.
	 *
	 * Written with short and not int - int is two bytes here and four
	 * on the machine the native leg runs on, so an int would be
	 * testing the host's width rather than the compiler's.
	 */
	s = 0;
	l = 0x10001L;
	s += l;
	CHECK(6, s, 1);

	s = 7;
	l = 0x20000L;
	s += l;			/* short both here and on the host */
	CHECK(7, s, 7);

	/* the other truncation-safe operators */
	s = 0xff;
	l = 0x0f0fL;
	s &= l;
	CHECK(8, s, 0x0f);

	s = 0xf0;
	l = 0x000fL;
	s |= l;
	CHECK(9, s, 0xff);

	s = 0xff;
	l = 0x0f0fL;
	s ^= l;
	CHECK(10, s, 0x0ff0);

	s = 3;
	l = 5L;
	s *= l;
	CHECK(11, s, 15);

	/* char, which narrows twice over */
	c = 10;
	l = 5L;
	c += l;
	CHECK(12, c, 15);

	c = 0x7f;
	l = 1L;
	c += l;
	CHECK(13, c, -128);		/* wraps in a signed char */

	/* unsigned short */
	us = 65000;
	l = 1000L;
	us += l;
	CHECK(14, us, 464);		/* 66000 wrapped to 16 bits */

	/* pointer stepping backwards, and by an element size over one */
	p = buf + 10;
	l = 4L;
	p -= l;
	CHECK(15, p - buf, 6);

	/* the value of the assignment expression itself */
	s = 10;
	l = 5L;
	CHECK(16, (s += l), 15);
	CHECK(17, s, 15);

	/* a long constant on the right, which retypes rather than
	 * narrowing - the literal is the same value at either width */
	s = 100;
	s += 25L;
	CHECK(18, s, 125);

	s = 100;
	s -= 0x10000L + 30L;	/* high word dropped, low word is 30 */
	CHECK(19, s, 70);

	/* through a pointer, so the lvalue is a DEREF and not a symbol */
	s = 40;
	l = 2L;
	*(&s) += l;
	CHECK(20, s, 42);

	/* stepping a pointer held in a struct member is the fseek shape */
	p = buf;
	l = 3L;
	p += l;
	p += l;
	CHECK(21, p - buf, 6);

	return 0;
}

/* vim: set tabstop=4 shiftwidth=4 noexpandtab: */
