/*
 * An octal constant that travels through a #define.
 *
 * cpp stores a pure-number macro body as a value and respells it in
 * decimal at expansion - a memory optimisation, since a value is two
 * bytes and its text is not.  The parse at define time handled hex
 * and decimal, so a leading zero fell into the decimal branch and
 * 0200 became two hundred.  The respelling is fine; the value it
 * respelled was not.
 *
 * Every flag in stdio.h past 010 came out wrong that way: _IOBINARY
 * as 200 is _IOSTRG|_IOMYBUF with the real bit, so fgetc took every
 * binary stream for a string it had run off the end of and returned
 * EOF without reading.  Single-digit octal survived by coincidence -
 * 01 through 07 spell the same in either base - which is why the
 * small flags worked and the failures started at _IOMYBUF.
 *
 * Bare literals never had the bug: they reach the compiler as
 * themselves and pass1's lexer knows the bases.  The macro cases are
 * the test; a few bare ones stand guard anyway.
 */
#include "rt.h"

#define OCT8	010
#define OCT9	011
#define OCT128	0200
#define OCT255	0377
#define OCT4096	010000
#define MASK	(OCT128 | OCT8)
#define NOCT	-0200

short bare8 = 010;
short bare128 = 0200;
short viadef = OCT128;

main()
{
	CHECK(1, OCT8, 8);
	CHECK(2, OCT9, 9);
	CHECK(3, OCT128, 128);
	CHECK(4, OCT255, 255);
	CHECK(5, OCT4096, 4096);
	CHECK(6, MASK, 136);
	CHECK(7, NOCT, -128);
	CHECK(8, bare8, 8);
	CHECK(9, bare128, 128);
	CHECK(10, viadef, 128);
	CHECK(11, OCT255 & 0377, 255);
	CHECK(12, 0x80 | OCT8, 136);
	return 0;
}
