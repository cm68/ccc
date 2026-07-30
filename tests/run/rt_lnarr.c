/*
 * A long stored into something narrower.
 *
 * A long lives in HL:DE with HL the high word, so the low byte of the
 * value is E - but every narrowing store rule takes the low half of
 * HL, which is the *third* byte.  There are ten such rules at byte
 * width alone, so the demotion belongs once in the rewriter rather
 * than in each of them.
 *
 * This looked like a broken shift, because the two halves of the same
 * function disagreed:
 *
 *	buf[1] = val & 0xff;		right
 *	buf[2] = (val >> 8) & 0xff;	wrong - always zero
 *
 * The first reads the low byte straight out of memory and never goes
 * through a register pair at all.  The shift itself was always
 * correct: printing (val >> 8) as a long gave the right answer, and
 * assigning it to a long temp and narrowing that gave the right answer
 * too.  Only the direct narrowing of a computed long was wrong.
 *
 * cpp emits every numeric token through exactly this shape, so every
 * constant over 255 lost its high bytes - 0644 arrived as 164 and 256
 * as 0.  Found by running the cpp that ccc built against the cpp that
 * zc3 built, on cpp's own sources.
 */
#include "rt.h"

unsigned char buf[5];
unsigned char c;
unsigned short w;
unsigned char arr[4];
unsigned long v;
unsigned long t;

/* cpp's emit4, which is where this was found */
void
emit4(val)
unsigned long val;
{
	buf[1] = val & 0xff;
	buf[2] = (val >> 8) & 0xff;
	buf[3] = (val >> 16) & 0xff;
	buf[4] = (val >> 24) & 0xff;
}

main()
{
	emit4(420L);			/* 0644, the mode cpp passes creat */
	CHECK(1, buf[1], 164);
	CHECK(2, buf[2], 1);
	CHECK(3, buf[3], 0);
	CHECK(4, buf[4], 0);

	emit4(256L);			/* char buf[256] */
	CHECK(5, buf[1], 0);
	CHECK(6, buf[2], 1);
	CHECK(7, buf[3], 0);
	CHECK(8, buf[4], 0);

	emit4(0x12345678L);		/* all four bytes distinct */
	CHECK(9, buf[1], 0x78);
	CHECK(10, buf[2], 0x56);
	CHECK(11, buf[3], 0x34);
	CHECK(12, buf[4], 0x12);

	/* the same narrowing without the mask */
	v = 0x12345678L;
	c = v >> 8;
	CHECK(13, c, 0x56);
	c = v >> 16;
	CHECK(14, c, 0x34);
	c = v;
	CHECK(15, c, 0x78);

	/* to a scalar, and to an array element */
	c = (v >> 8) & 0xff;
	CHECK(16, c, 0x56);
	arr[0] = (v >> 8) & 0xff;
	CHECK(17, arr[0], 0x56);
	arr[3] = (v >> 24) & 0xff;
	CHECK(18, arr[3], 0x12);

	/* through a long temp, which always worked - keep it that way */
	t = v >> 8;
	c = t & 0xff;
	CHECK(19, c, 0x56);

	/* narrowing to a short, not just a byte */
	w = v;
	CHECK(20, w == 0x5678, 1);
	w = v >> 16;
	CHECK(21, w == 0x1234, 1);

	/* arithmetic other than a shift narrows the same way */
	v = 0x00010000L;
	c = (v - 1L) & 0xff;
	CHECK(22, c, 0xff);
	w = v - 1L;
	CHECK(23, w == 0xffff, 1);

	/* and the value itself is still whole */
	v = 0x12345678L;
	CHECK(24, (v >> 8) == 0x123456L, 1);

	return 0;
}
