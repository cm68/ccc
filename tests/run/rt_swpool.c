/*
 * The pool that nested switches share.
 *
 * rt_swnest.c is the other half of this: it guards the case counts
 * themselves, which phase 1 and phase 2 once queued in different
 * orders.  This one is about where the values are kept.
 *
 * c1 used to give every nesting level a fixed 256-byte slice of case
 * values, whether or not the level was open - two kilobytes of bss for
 * a tree whose widest switch has 125 cases and which nests two deep.
 * The levels share one pool now, and a nested switch is handed the
 * enclosing switch's next free slot, so their value runs overlap.
 *
 * The overlap is safe only because of when things are read: a switch's
 * values go out in its own dispatch, at its own pop, and the enclosing
 * switch does not record another case until after that.  This checks
 * the ordering that argument depends on, so every test below has outer
 * arms on BOTH sides of a nested switch and checks the ones written
 * afterwards - those are the ones sharing space with the inner switch.
 *
 * What this does NOT check is the pool being given back on pop.  That
 * is a space property, not a correctness one: a switch always records
 * at its own base plus its own count, so leaking the pool costs room
 * and changes no answer.  Removing the release leaves this file
 * passing.  Exhaustion is caught instead by the .error in parseast.c,
 * which the assembler refuses.
 *
 * The dispatch has two shapes, a jump table for a dense run of values
 * and a compare chain for a sparse one, so both are exercised here: a
 * wrong value would otherwise only show up in whichever shape the test
 * happened to pick.
 */
#include "rt.h"

/* dense arms on both sides of a nested switch */
static int dense(a, b)
int a;
int b;
{
	int r;

	r = -1;
	switch (a) {
	case 0:
		r = 1000;
		break;
	case 1:
		switch (b) {
		case 0:
			r = 10;
			break;
		case 1:
			r = 11;
			break;
		case 2:
			r = 12;
			break;
		default:
			r = 19;
			break;
		}
		break;
	/* written after the inner switch, so sharing its space */
	case 2:
		r = 1002;
		break;
	case 3:
		r = 1003;
		break;
	case 4:
		r = 1004;
		break;
	default:
		r = 9999;
		break;
	}
	return r;
}

/* sparse arms, so the compare chain rather than the table */
static int sparse(a, b)
int a;
int b;
{
	int r;

	r = -1;
	switch (a) {
	case 5:
		r = 5;
		break;
	case 40:
		switch (b) {
		case 7:
			r = 407;
			break;
		case 90:
			r = 490;
			break;
		default:
			r = 499;
			break;
		}
		break;
	case 75:
		r = 75;
		break;
	case 120:
		r = 120;
		break;
	default:
		r = 999;
		break;
	}
	return r;
}

/* three deep, with arms after the pop at every level */
static int deep(a, b, c)
int a;
int b;
int c;
{
	int r;

	r = -1;
	switch (a) {
	case 1:
		switch (b) {
		case 1:
			switch (c) {
			case 1:
				r = 111;
				break;
			case 2:
				r = 112;
				break;
			default:
				r = 119;
				break;
			}
			break;
		case 2:
			r = 120;
			break;
		case 3:
			r = 130;
			break;
		default:
			r = 190;
			break;
		}
		break;
	case 2:
		r = 200;
		break;
	case 3:
		r = 300;
		break;
	default:
		r = 900;
		break;
	}
	return r;
}

/*
 * Two switches one after the other in the same function, neither
 * nested in the other.  Both answers still have to be right when the
 * second is handed space the first was using.
 */
static int twice(a)
int a;
{
	int r;

	r = 0;
	switch (a) {
	case 1:
		r = 1;
		break;
	case 2:
		r = 2;
		break;
	default:
		r = 9;
		break;
	}
	switch (a) {
	case 1:
		r = r + 10;
		break;
	case 2:
		r = r + 20;
		break;
	default:
		r = r + 90;
		break;
	}
	return r;
}

main()
{
	/* the arms before the nested switch */
	CHECK(1, dense(0, 0), 1000);
	/* the nested switch itself, every arm */
	CHECK(2, dense(1, 0), 10);
	CHECK(3, dense(1, 1), 11);
	CHECK(4, dense(1, 2), 12);
	CHECK(5, dense(1, 7), 19);
	/* the arms sharing space with the inner switch */
	CHECK(6, dense(2, 0), 1002);
	CHECK(7, dense(3, 0), 1003);
	CHECK(8, dense(4, 0), 1004);
	CHECK(9, dense(9, 0), 9999);

	CHECK(10, sparse(5, 0), 5);
	CHECK(11, sparse(40, 7), 407);
	CHECK(12, sparse(40, 90), 490);
	CHECK(13, sparse(40, 0), 499);
	CHECK(14, sparse(75, 0), 75);
	CHECK(15, sparse(120, 0), 120);
	CHECK(16, sparse(6, 0), 999);

	CHECK(17, deep(1, 1, 1), 111);
	CHECK(18, deep(1, 1, 2), 112);
	CHECK(19, deep(1, 1, 8), 119);
	CHECK(20, deep(1, 2, 0), 120);
	CHECK(21, deep(1, 3, 0), 130);
	CHECK(22, deep(1, 8, 0), 190);
	CHECK(23, deep(2, 0, 0), 200);
	CHECK(24, deep(3, 0, 0), 300);
	CHECK(25, deep(8, 0, 0), 900);

	CHECK(26, twice(1), 11);
	CHECK(27, twice(2), 22);
	CHECK(28, twice(5), 99);

	return 0;
}
