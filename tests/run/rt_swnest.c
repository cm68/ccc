/*
 * Nested and sibling switches, dense and sparse.
 *
 * Phase 1 pushed a switch's case count when the switch ENDED and
 * phase 2 read the queue when one BEGAN, so nesting rotated every
 * count one switch over: tables built with a neighbour's size,
 * the tail cases of an inner switch spilling into the outer.
 * When case values collided the assembler said "duplicate case";
 * when they did not, dispatch was silently wrong - which is what
 * these check.
 */
#include "rt.h"

short st;

short
scan(t)
short t;
{
	switch (st) {
	case 2:
		switch (t) {
		case 36:	return 360;
		case 20:	return 200;
		case 1:		return 21;
		case 2:		return 22;
		case 3:		return 23;
		}
		return -2;
	case 3:
		switch (t) {
		case 36:	return 1360;
		case 42:	return 1420;
		case 1:		return 31;
		case 3:		return 33;
		}
		return -3;
	}
	return -1;
}

short
dense(a, b)
short a;
short b;
{
	short r;

	r = 0;
	switch (a) {
	case 1:
		switch (b) {
		case 1: r = 11; break;
		case 2: r = 12; break;
		case 3: r = 13; break;
		case 4: r = 14; break;
		case 5: r = 15; break;
		case 6: r = 16; break;
		case 7: r = 17; break;
		case 8: r = 18; break;
		}
		break;
	case 2: r = 2; break;
	case 3: r = 3; break;
	case 4: r = 4; break;
	case 5: r = 5; break;
	case 6: r = 6; break;
	case 7: r = 7; break;
	case 8: r = 8; break;
	}
	return r;
}

int
main()
{
	st = 2;
	CHECK(1, scan(36), 360);
	CHECK(2, scan(3), 23);
	CHECK(3, scan(99), -2);
	st = 3;
	CHECK(4, scan(42), 1420);
	CHECK(5, scan(3), 33);
	CHECK(6, scan(2), -3);
	st = 9;
	CHECK(7, scan(1), -1);
	CHECK(8, dense(1, 5), 15);
	CHECK(9, dense(1, 8), 18);
	CHECK(10, dense(7, 3), 7);
	CHECK(11, dense(9, 9), 0);
	return 0;
}
