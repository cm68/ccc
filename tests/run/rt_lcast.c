/*
 * A cast that makes a value narrower.
 *
 * pass1 used to apply a cast by writing the new type over the node:
 *
 *	e1->type = tp;
 *
 * with a comment saying pass2 handled the conversion.  pass2 could
 * not - the fact that the value had been wider was the thing being
 * erased.  A long lives in HL:DE with the low word in DE, so
 * "(int)f()" on a long-returning f left pass2 reading HL, the high
 * word, and small values came back zero.
 *
 * NARROW says it in the tree instead.  It is unary, and it had been
 * sitting in the opcode table and the AST pretty-printer since they
 * were written - nothing had ever emitted one.
 *
 * Found in cpp, whose escint() is "(int)getint(base)", so every
 * numeric escape in every source came back zero: '\x1b' and '\033'
 * and '\xff' all became 0, while '\n' and '\t' were fine because they
 * are a table lookup rather than a conversion.  The named escapes
 * working is what made it look like a lexer bug rather than a cast.
 */
#include "rt.h"

long lv;
unsigned char uc;

long
give()
{
	return lv;
}

short
viacast()
{
	short v;

	v = (int)give();		/* the shape escint() has */
	return v;
}

short
nocast()
{
	short v;

	v = give();			/* implicit, which always worked */
	return v;
}

unsigned char
tobyte()
{
	unsigned char c;

	c = (int)give();
	return c;
}

unsigned char
castbyte()
{
	unsigned char c;

	c = (unsigned char)give();
	return c;
}

main()
{
	lv = 27L;
	CHECK(1, viacast(), 27);
	CHECK(2, nocast(), 27);
	CHECK(3, tobyte(), 27);
	CHECK(4, castbyte(), 27);

	lv = 65L;
	CHECK(5, viacast(), 65);

	/* a value whose low word alone is the answer */
	lv = 0x12345678L;
	CHECK(6, viacast() == 0x5678, 1);
	CHECK(7, tobyte(), 0x78);

	/* and one whose high word would look like a plausible answer */
	lv = 0x00010002L;
	CHECK(8, viacast(), 2);
	CHECK(9, tobyte(), 2);

	/*
	 * Narrowing something that was never a long.  "~uc" is done at
	 * int width because uc promotes, so the cast has to bring it
	 * back down - and the result is used as an int again straight
	 * after, so the high byte has to be right too.
	 */
	uc = 0x0f;
	CHECK(10, (unsigned char)~uc, 0xf0);
	/*
	 * "~uc" uncast is left out on purpose: it is -16, since uc
	 * promotes to int before the complement, and zc3 answers 240 -
	 * it does the work at byte width.  ccc and gcc agree on -16.
	 * That is a zc3 bug rather than anything this file is about,
	 * and everything here has to pass under all three.
	 */

	uc = 0;
	CHECK(12, (unsigned char)~uc, 0xff);

	/* a cast that widens, and one that changes nothing */
	CHECK(13, (long)uc == 0L, 1);
	uc = 200;
	CHECK(14, (unsigned char)uc, 200);
	CHECK(15, (int)uc, 200);

	return 0;
}
