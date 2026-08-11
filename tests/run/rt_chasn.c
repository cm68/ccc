/*
 * A chained assignment whose inner target is a dereference.
 *
 *	*p = *q = 0;
 *
 * pass2 had no rule for an assignment whose value is another
 * assignment's, when that one stored through a pointer.  It emitted
 * nothing at all for the statement - not wrong code, no code, both
 * stores gone - and counted an expression it could not build.  The
 * inner target being a plain variable had a rule and worked, which is
 * why this survived: "*p = a = 0" is fine and "*p = *q = 0" is not.
 * pass1 now emits the comma it is equivalent to, "(*q = 0, *p = *q)",
 * the same way it already turns "*++p" into "(++p, *p)".
 *
 * cmd/s did not build on it: bman.c clears two out-parameters at once
 * that way, which is an ordinary thing to write.
 *
 * These have to RUN.  A statement that silently disappears passes any
 * check that only asks whether the compiler succeeded, and the whole
 * failure is a store that never happened - so every case below reads
 * the memory back.  See CHAINASSIGN.
 */
#include "rt.h"

int	a, b;
int	u, v, w;
char	cbuf[4];
int	ibuf[4];

main()
{
	int *	p;
	int *	q;
	int *	r;
	char *	cp;
	int *	ip;

	/* the reported form: both stores must happen */
	u = 11;
	v = 22;
	p = &u;
	q = &v;
	*p = *q = 0;
	CHECK(1, u, 0);
	CHECK(2, v, 0);

	/* a value that is not a constant */
	a = 7;
	u = 11;
	v = 22;
	*p = *q = a;
	CHECK(3, u, 7);
	CHECK(4, v, 7);

	/* the same tree written as an index */
	ibuf[0] = 1;
	ibuf[1] = 2;
	p = &ibuf[0];
	q = &ibuf[1];
	p[0] = q[0] = 9;
	CHECK(5, ibuf[0], 9);
	CHECK(6, ibuf[1], 9);

	/* parenthesised - the same thing */
	u = 11;
	v = 22;
	p = &u;
	q = &v;
	*p = (*q = 5);
	CHECK(7, u, 5);
	CHECK(8, v, 5);

	/* outer target a plain global, inner a dereference */
	v = 22;
	a = 0;
	a = *q = 6;
	CHECK(9, a, 6);
	CHECK(10, v, 6);

	/* inner target a plain global - this always worked, keep it so */
	u = 11;
	a = 0;
	*p = a = 4;
	CHECK(11, u, 4);
	CHECK(12, a, 4);

	/* globals both ways */
	a = b = 3;
	CHECK(13, a, 3);
	CHECK(14, b, 3);

	/* three deep */
	u = 1;
	v = 2;
	w = 3;
	p = &u;
	q = &v;
	r = &w;
	*p = *q = *r = 8;
	CHECK(15, u, 8);
	CHECK(16, v, 8);
	CHECK(17, w, 8);

	/*
	 * Only a word is rewritten - an int or a pointer.  The read the
	 * rewrite puts back has to be one pass2 can build, and a byte
	 * lands in E and a long in a register pair, neither of which it
	 * can store from.  So "*ip = *cp = 300" is NOT here: it does not
	 * compile, exactly as it did not before, and writing it would
	 * make this file fail to build rather than fail a check.
	 *
	 * That boundary is also why nothing below tests the conversion
	 * the inner target imposes - it takes two widths to see one, and
	 * the rewrite only ever sees the single width it is allowed.
	 * The reading-back is still what makes the conversion right when
	 * those widths are unlocked; it just cannot be shown from here.
	 */
	cbuf[0] = 0;
	cp = &cbuf[0];
	ip = &u;
	CHECK(18, cp == &cbuf[0], 1);
	CHECK(19, ip == &u, 1);

	/*
	 * A stepped pointer is left alone by the rewrite - reading the
	 * target back would step it twice.  It has to keep working, and
	 * each pointer must move exactly one element.
	 */
	ibuf[0] = 0;
	ibuf[1] = 0;
	ibuf[2] = 0;
	ibuf[3] = 0;
	p = &ibuf[0];
	q = &ibuf[2];
	*p++ = *q++ = 5;
	CHECK(20, ibuf[0], 5);
	CHECK(21, ibuf[2], 5);
	CHECK(22, p == &ibuf[1], 1);
	CHECK(23, q == &ibuf[3], 1);
	CHECK(24, ibuf[1], 0);
	CHECK(25, ibuf[3], 0);

	/* two levels of indirection on both sides */
	u = 1;
	v = 2;
	p = &u;
	q = &v;
	*p = *q = 12;
	CHECK(26, u, 12);
	CHECK(27, v, 12);

	return 0;
}

/* vim: set tabstop=4 shiftwidth=4 noexpandtab: */
