/*
 * Switch on values that do not fit a byte.
 *
 * The dispatch loaded the control, sent it to the default arm if its
 * high byte was set, and compared the low byte - so a case label
 * outside 0..255 could never be matched, and pass2 counted it and
 * failed the compile rather than miscompiling it.  That ruled out the
 * shape every program that stats a file uses:
 *
 *	switch (statb.st_mode & S_IFMT) {
 *	case S_IFDIR:		0040000
 *	case S_IFBLK:		0060000
 *	case S_IFCHR:		0020000
 *	}
 *
 * all three of which are outside the byte, and cmd/ls does exactly
 * this.  Negative labels were as unreachable as large ones, which
 * matters for any switch over a value that can be -1 for "none".
 *
 * A switch is now wide if any of its labels is, and dispatches on HL
 * through swtabw; one whose labels all fit a byte is untouched and
 * still uses the byte helpers.  See SWITCHBYTE.
 *
 * These have to RUN.  The failure was a value reaching the wrong arm,
 * so every case below is called with its own value and says which arm
 * it landed in.
 */
#include "rt.h"

#define S_IFMT	0170000
#define S_IFDIR	0040000
#define S_IFBLK	0060000
#define S_IFCHR	0020000

/* the reported case, in the shape it is written in */
int
ftype(mode)
unsigned mode;
{
	switch (mode & S_IFMT) {
	case S_IFDIR:
		return 1;
	case S_IFBLK:
		return 2;
	case S_IFCHR:
		return 3;
	}
	return 0;
}

/* negative labels, and zero beside them */
int
neg(n)
int n;
{
	switch (n) {
	case -1:
		return 11;
	case 0:
		return 12;
	case 1000:
		return 13;
	case -32768:
		return 14;
	}
	return 0;
}

/*
 * Labels that share a low byte.  The old dispatch compared eight bits,
 * so these three were one value to it - and where two of them collided
 * the duplicate-case check fired instead, which is how the whole thing
 * was found.
 */
int
lowbyte(n)
int n;
{
	switch (n) {
	case 0x0001:
		return 21;
	case 0x0101:
		return 22;
	case 0x0201:
		return 23;
	}
	return 0;
}

/*
 * Narrow labels first and a wide one after, so the values already
 * stored have to be widened in place when the wide one arrives.
 */
int
mixed(n)
int n;
{
	switch (n) {
	case 1:
		return 31;
	case 2:
		return 32;
	case 3:
		return 33;
	case 4000:
		return 34;
	}
	return 0;
}

/* a single wide label, which takes the chain rather than a table */
int
one(n)
int n;
{
	switch (n) {
	case 9999:
		return 41;
	}
	return 0;
}

/* a wide switch with a default arm to fall into */
int
wdef(n)
int n;
{
	switch (n) {
	case 0x1234:
		return 51;
	case 0x5678:
		return 52;
	default:
		return 53;
	}
}

/* and one that must stay on the byte path - the regression guard */
int
narrow(n)
int n;
{
	switch (n) {
	case 1:
		return 61;
	case 2:
		return 62;
	case 200:
		return 63;
	}
	return 0;
}

main()
{
	CHECK(1, ftype(0040755), 1);
	CHECK(2, ftype(0060644), 2);
	CHECK(3, ftype(0020666), 3);
	CHECK(4, ftype(0100644), 0);	/* S_IFREG: no arm */

	CHECK(5, neg(-1), 11);
	CHECK(6, neg(0), 12);
	CHECK(7, neg(1000), 13);
	CHECK(8, neg(-32768), 14);
	CHECK(9, neg(1), 0);
	CHECK(10, neg(-2), 0);

	/* the low byte is 1 in all three: they must not be confused */
	CHECK(11, lowbyte(0x0001), 21);
	CHECK(12, lowbyte(0x0101), 22);
	CHECK(13, lowbyte(0x0201), 23);
	CHECK(14, lowbyte(0x0301), 0);

	CHECK(15, mixed(1), 31);
	CHECK(16, mixed(2), 32);
	CHECK(17, mixed(3), 33);
	CHECK(18, mixed(4000), 34);
	CHECK(19, mixed(5), 0);
	/* the low byte of 4000 is 160: it must not match on its own */
	CHECK(20, mixed(160), 0);

	CHECK(21, one(9999), 41);
	CHECK(22, one(0), 0);
	CHECK(23, one(15), 0);		/* 9999 & 0xff */

	CHECK(24, wdef(0x1234), 51);
	CHECK(25, wdef(0x5678), 52);
	CHECK(26, wdef(0), 53);
	CHECK(27, wdef(0x34), 53);	/* low byte of the first arm */

	CHECK(28, narrow(1), 61);
	CHECK(29, narrow(2), 62);
	CHECK(30, narrow(200), 63);
	CHECK(31, narrow(3), 0);

	return 0;
}

/* vim: set tabstop=4 shiftwidth=4 noexpandtab: */
