/*
 * switch.
 *
 * pass2 used to read the control expression, rewrite it, free the
 * result, and then emit the case bodies one after another - no
 * comparison, no case labels.  Every switch took its first case
 * whatever the value, and a goto back over one never terminated.
 *
 * Nothing here had a switch in it, which is how that survived: stage1
 * only asks whether the sources compile, and the compiler's own passes
 * are built for the host by gcc, where switches work.
 *
 * Case values are bytes in this compiler.  The control expression need
 * not be - a state machine over an int is the usual shape - so a word
 * control has to reject a high byte before comparing the low one.
 */
#include "rt.h"

short
bylocal(v)
short v;
{
	switch (v) {
	case 0: return 100;
	case 1: return 101;
	case 2: return 102;
	}
	return -1;
}

short
withdefault(v)
short v;
{
	switch (v) {
	case 5: return 200;
	default: return 201;
	}
}

/* a word control whose value is beyond any byte case */
short
bigcontrol(v)
short v;
{
	switch (v) {
	case 0: return 300;
	case 1: return 301;
	}
	return 302;
}

short
fallthru(v)
short v;
{
	short n;

	n = 0;
	switch (v) {
	case 1:
		n += 1;
		/* falls into case 2 */
	case 2:
		n += 10;
		break;
	case 3:
		n += 100;
		break;
	}
	return n;
}

char cbuf[4];

/* a byte control, which is the common shape */
short
bybyte(c)
unsigned char c;
{
	switch (c) {
	case 'a': return 1;
	case 'z': return 2;
	case 200: return 3;
	}
	return 0;
}

/* the shape that hung: a state machine with a goto back over it */
short
statemachine(n)
short n;
{
	short state;
	short steps;

	state = 0;
	steps = 0;
again:
	steps++;
	if (steps > 20)
		return -1;		/* did not terminate */
	switch (state) {
	case 0:
		state = 1;
		goto again;
	case 1:
		state = 2;
		goto again;
	case 2:
		break;
	}
	return steps + n;
}

/* nested switches, so the dispatch has to stack */
short
nested(a, b)
short a; short b;
{
	short r;

	r = 0;
	switch (a) {
	case 1:
		switch (b) {
		case 1: r = 11; break;
		case 2: r = 12; break;
		}
		break;
	case 2:
		switch (b) {
		case 1: r = 21; break;
		case 2: r = 22; break;
		}
		break;
	}
	return r;
}

main()
{
	CHECK(1, bylocal(0), 100);
	CHECK(2, bylocal(1), 101);
	CHECK(3, bylocal(2), 102);
	CHECK(4, bylocal(3), -1);

	CHECK(5, withdefault(5), 200);
	CHECK(6, withdefault(6), 201);

	CHECK(7, bigcontrol(0), 300);
	CHECK(8, bigcontrol(1), 301);
	/* 256 has a zero low byte: a dispatch that compared only the low
	 * byte would take case 0 */
	CHECK(9, bigcontrol(256), 302);
	CHECK(10, bigcontrol(-1), 302);

	CHECK(11, fallthru(1), 11);
	CHECK(12, fallthru(2), 10);
	CHECK(13, fallthru(3), 100);
	CHECK(14, fallthru(4), 0);

	CHECK(15, bybyte('a'), 1);
	CHECK(16, bybyte('z'), 2);
	CHECK(17, bybyte(200), 3);
	CHECK(18, bybyte('b'), 0);

	CHECK(19, statemachine(0), 3);	/* three trips round */

	CHECK(20, nested(1, 1), 11);
	CHECK(21, nested(1, 2), 12);
	CHECK(22, nested(2, 1), 21);
	CHECK(23, nested(2, 2), 22);
	CHECK(24, nested(3, 1), 0);

	return 0;
}
