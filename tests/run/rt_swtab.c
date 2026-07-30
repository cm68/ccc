/*
 * The three switch dispatch shapes.
 *
 * A chain of "cp v / jp z,L" is 5 bytes a case, which over this tree's
 * own 85 switches and 835 cases was 4175 bytes of nothing but
 * dispatch.  Two helpers replace it where counting says they are
 * smaller: swtab scans a table of values, swidx biases the control and
 * indexes one.  Both put the table inline after the call and find it
 * through the return address, and both fall out of the end of the
 * table onto the no-match label - so "not found" is never an address
 * the compiler has to store.
 *
 *	chain	5n		swtab	4 + 3n		swidx	5 + 2*span
 *
 * so the chain holds to n=2, swidx takes over when 2*span < 3n-1, and
 * swtab has the rest.  Which one a function here gets is a
 * consequence of its case values, and the comments say which is meant.
 *
 * What these are really for is the edges the helpers can get wrong:
 * matching the last entry of a scan, falling off the end of one,
 * landing in a hole in an indexed table, and a word control too big to
 * be a byte at all.
 */
#include "rt.h"

/* ten cases over the whole byte: sparse, so swtab */
short
sparse(v)
short v;
{
	switch (v) {
	case 0: return 10;
	case 7: return 17;
	case 31: return 41;
	case 64: return 74;
	case 99: return 109;
	case 128: return 138;
	case 170: return 180;
	case 200: return 210;
	case 254: return 264;
	case 255: return 265;
	}
	return -1;
}

/* the same values with a default, so no-match has somewhere to go */
short
sparsedef(v)
short v;
{
	switch (v) {
	case 0: return 10;
	case 7: return 17;
	case 31: return 41;
	case 64: return 74;
	case 99: return 109;
	case 128: return 138;
	case 170: return 180;
	case 255: return 265;
	default: return 999;
	}
}

/* eleven contiguous values: dense, so swidx with a non-zero bias */
short
dense(v)
short v;
{
	switch (v) {
	case 10: return 110;
	case 11: return 111;
	case 12: return 112;
	case 13: return 113;
	case 14: return 114;
	case 15: return 115;
	case 16: return 116;
	case 17: return 117;
	case 18: return 118;
	case 19: return 119;
	case 20: return 120;
	}
	return -1;
}

/* dense enough for swidx but with a hole at 13, which has to reach
 * the no-match label like anything outside the span does */
short
gappy(v)
short v;
{
	switch (v) {
	case 10: return 210;
	case 11: return 211;
	case 12: return 212;
	case 14: return 214;
	case 15: return 215;
	case 16: return 216;
	case 17: return 217;
	}
	return -1;
}

/* two cases: below the crossover, so still a chain */
short
tiny(v)
short v;
{
	switch (v) {
	case 3: return 33;
	case 9: return 99;
	}
	return -1;
}

/* a byte control, which arrives in A already */
short
bysparse(c)
unsigned char c;
{
	switch (c) {
	case 1: return 1;
	case 50: return 50;
	case 100: return 100;
	case 150: return 150;
	case 200: return 200;
	case 255: return 255;
	}
	return -1;
}

/* different schemes nested inside each other */
short
mixed(a, b)
short a; short b;
{
	switch (a) {			/* dense, swidx */
	case 1:
	case 2:
	case 3:
		switch (b) {		/* sparse, swtab */
		case 0: return 100;
		case 60: return 160;
		case 120: return 220;
		case 240: return 340;
		}
		return -2;
	case 4:
		return 4;
	}
	return -1;
}

main()
{
	/* swtab: every entry, including the first and last of the scan */
	CHECK(1, sparse(0), 10);
	CHECK(2, sparse(7), 17);
	CHECK(3, sparse(31), 41);
	CHECK(4, sparse(64), 74);
	CHECK(5, sparse(99), 109);
	CHECK(6, sparse(128), 138);
	CHECK(7, sparse(170), 180);
	CHECK(8, sparse(200), 210);
	CHECK(9, sparse(254), 264);
	CHECK(10, sparse(255), 265);

	/* and falling off the end of the scan */
	CHECK(11, sparse(1), -1);
	CHECK(12, sparse(129), -1);
	CHECK(13, sparse(253), -1);

	/*
	 * a word control that no byte case can match.  The dispatch
	 * only ever sees the low byte, so these are the values that
	 * would collide with case 0 and case 7 if the high byte were
	 * not rejected first.
	 */
	CHECK(14, sparse(256), -1);
	CHECK(15, sparse(263), -1);
	CHECK(16, sparse(-1), -1);

	CHECK(17, sparsedef(0), 10);
	CHECK(18, sparsedef(255), 265);
	CHECK(19, sparsedef(1), 999);
	CHECK(20, sparsedef(256), 999);

	/* swidx: the whole span, then both sides of it */
	CHECK(21, dense(10), 110);
	CHECK(22, dense(15), 115);
	CHECK(23, dense(20), 120);
	CHECK(24, dense(9), -1);	/* just below: the bias underflows */
	CHECK(25, dense(21), -1);	/* just above */
	CHECK(26, dense(0), -1);
	CHECK(27, dense(266), -1);	/* low byte 10, high byte set */
	CHECK(28, dense(-1), -1);

	/* the hole inside an indexed table */
	CHECK(29, gappy(10), 210);
	CHECK(30, gappy(12), 212);
	CHECK(31, gappy(13), -1);	/* the hole */
	CHECK(32, gappy(14), 214);
	CHECK(33, gappy(17), 217);
	CHECK(34, gappy(18), -1);

	CHECK(35, tiny(3), 33);
	CHECK(36, tiny(9), 99);
	CHECK(37, tiny(4), -1);

	CHECK(38, bysparse(1), 1);
	CHECK(39, bysparse(150), 150);
	CHECK(40, bysparse(255), 255);
	CHECK(41, bysparse(2), -1);

	CHECK(42, mixed(1, 0), 100);
	CHECK(43, mixed(3, 240), 340);
	CHECK(44, mixed(2, 61), -2);
	CHECK(45, mixed(4, 0), 4);
	CHECK(46, mixed(5, 0), -1);

	return 0;
}
