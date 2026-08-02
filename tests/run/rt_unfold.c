/*
 * A unary operator on a constant that is itself an expression.
 *
 * Only the parser folded unary ops, and it looks at its operand
 * before that operand's own subtree has been folded.  So "~7" was a
 * number by the time pass2 saw it and "~(A|B|C)" was still a
 * complement of an OR tree - and pass2 has no form for the
 * complement of a constant, so
 *
 *	f->_flag &= ~(_IOREAD|_IOWRT|_IONBF);
 *
 * in fclose emitted no code whatever.  Every stream kept the flags it
 * was closed with, and the marker saying so is a comment.
 *
 * The masks below are written both ways on purpose: the literal form
 * folded before, the macro form did not, and they have to agree.
 */
#include "rt.h"

#define A 01
#define B 02
#define C 04

struct s {
	char		pad[6];		/* put the flag past the fold */
	unsigned char	flag;
};

struct s g;

void
clr(p)
register struct s *p;
{
	p->flag &= ~(A|B|C);
}

int
main()
{
	int i;
	unsigned char u;
	long l;

	g.flag = 0xff;
	clr(&g);
	CHECK(1, g.flag, 0xf8);

	/* the same complement written as a literal */
	g.flag = 0xff;
	g.flag &= ~7;
	CHECK(2, g.flag, 0xf8);

	/* negation of a folded expression */
	i = -(2 + 3);
	CHECK(3, i, -5);
	i = -(1 * 100);
	CHECK(4, i, -100);

	/* not, of a folded expression, both ways */
	i = !(3 - 3);
	CHECK(5, i, 1);
	i = !(2 | 1);
	CHECK(6, i, 0);

	/* complement at byte width wraps to the byte */
	u = ~(A|B);
	CHECK(7, u, 0xfc);

	/* and a long operand is left alone rather than folded wrong */
	l = 7;
	CHECK(8, ~l, -8L);
	CHECK(9, -l, -7L);

	return 0;
}

/* vim: set tabstop=4 shiftwidth=4 noexpandtab: */
