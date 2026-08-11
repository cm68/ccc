/*
 * Labels named after Z80 instructions.
 *
 * C keeps labels in a namespace of their own, so "out:" is a good
 * label and says nothing about any "out" elsewhere.  The assembler has
 * no such namespace: a line beginning "out" is the instruction, and
 * the label went in as an opcode with a stray colon after it - "need
 * an operand", pointing at a line the programmer would swear was a
 * label.  Every mnemonic was exposed this way, and "out", "end", "in",
 * "set" and "cp" are all names a person reaches for at the bottom of a
 * loop.  pass2 now writes every such label with an '@' in front, which
 * is a character C cannot spell and asz was taught to accept.
 *
 * The prefix could not be '_', which is why "out" appears here as a
 * function as well as a label: pass1 puts an underscore in front of
 * every global, so "_out" is already taken by the function, and a
 * label prefixed the same way would land on top of it.  C says these
 * two are unrelated and the test says so too.
 */
#include "rt.h"

/* a global whose name is also used as a label below */
int
out()
{
	return 42;
}

int
in()
{
	return 43;
}

/*
 * One label per mnemonic, reached by a goto, each leaving its own
 * answer - so a label that assembled but landed in the wrong place is
 * a wrong answer rather than a passing test.
 */
int
pick(n)
short n;
{
	short r;

	r = 0;
	if (n == 1) goto out;
	if (n == 2) goto end;
	if (n == 3) goto in;
	if (n == 4) goto set;
	if (n == 5) goto cp;
	if (n == 6) goto and;
	if (n == 7) goto or;
	if (n == 8) goto add;
	if (n == 9) goto ld;
	if (n == 10) goto jp;
	if (n == 11) goto ret;
	if (n == 12) goto push;
	r = 100;
	goto done;
out:	r = 1;	goto done;
end:	r = 2;	goto done;
in:	r = 3;	goto done;
set:	r = 4;	goto done;
cp:	r = 5;	goto done;
and:	r = 6;	goto done;
or:	r = 7;	goto done;
add:	r = 8;	goto done;
ld:	r = 9;	goto done;
jp:	r = 10;	goto done;
ret:	r = 11;	goto done;
push:	r = 12;
done:
	return r;
}

/*
 * A label named like one of pass2's own.  Generated labels are _L<n>,
 * so this never actually collided, but it is the thing the prefix is
 * there to keep true, and the && below makes pass2 generate some in
 * the same function.
 */
int
lzero(a, b)
short a, b;
{
	short r;

	r = 0;
	if (a && b)
		goto L0;
	r = 1;
	goto L1;
L0:	r = 2;
L1:
	return r;
}

/* a backward goto, so the label is a real branch target */
int
sum(n)
short n;
{
	short t;

	t = 0;
again:
	t = t + n;
	n = n - 1;
	if (n > 0)
		goto again;
	return t;
}

main()
{
	CHECK(1, pick(1), 1);
	CHECK(2, pick(2), 2);
	CHECK(3, pick(3), 3);
	CHECK(4, pick(4), 4);
	CHECK(5, pick(5), 5);
	CHECK(6, pick(6), 6);
	CHECK(7, pick(7), 7);
	CHECK(8, pick(8), 8);
	CHECK(9, pick(9), 9);
	CHECK(10, pick(10), 10);
	CHECK(11, pick(11), 11);
	CHECK(12, pick(12), 12);
	CHECK(13, pick(0), 100);

	/* the label "out" and the function out() are different things */
	CHECK(14, out(), 42);
	CHECK(15, in(), 43);

	CHECK(16, lzero(1, 1), 2);
	CHECK(17, lzero(1, 0), 1);
	CHECK(18, lzero(0, 1), 1);

	CHECK(19, sum(4), 10);
	CHECK(20, sum(1), 1);

	return 0;
}

/* vim: set tabstop=4 shiftwidth=4 noexpandtab: */
