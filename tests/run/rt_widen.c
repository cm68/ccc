/*
 * A cast that makes a value wider.
 *
 * The mirror of the narrowing cast, and the same mistake read the
 * other way: pass1 applied a cast by writing the new type over the
 * node, so relabelling a byte as a long put nothing in the three
 * bytes above it and they kept whatever the register held.
 *
 *	val |= ((unsigned long)readByte()) << (i * 8);
 *
 * is how pass1 reads a four byte number out of the lexeme stream, so
 * every constant it read came back with its low byte right and the
 * rest garbage.  An array dimension of 8 arrived as 2, which made the
 * array incomplete, which gave the member pointer size, which made
 * every struct either empty or over the 127-byte limit.
 *
 * Which conversion it is depends on the source, not the target: a
 * signed value sign-extends and an unsigned one zero-extends, and the
 * instructions differ - which is why the tree has to carry it rather
 * than leaving pass2 to guess.
 */
#include "rt.h"

unsigned char ub;
char sb;
unsigned short uw;
short sw;
unsigned char stream[4];
short pos;

unsigned char rb() { return stream[pos++]; }

unsigned long
readLE4()
{
	unsigned long val;
	short i;

	val = 0;
	for (i = 0; i < 4; i++)
		val |= ((unsigned long)rb()) << (i * 8);
	return val;
}

main()
{
	unsigned long ul;
	long sl;

	/* unsigned sources zero-extend */
	ub = 8;
	ul = (unsigned long)ub;
	CHECK(1, ul == 8L, 1);
	ub = 255;
	ul = (unsigned long)ub;
	CHECK(2, ul == 255L, 1);
	uw = 0x1234;
	ul = (unsigned long)uw;
	CHECK(3, ul == 0x1234L, 1);

	/* signed sources sign-extend */
	sb = -1;
	sl = (long)sb;
	CHECK(4, sl == -1L, 1);
	sw = -300;
	sl = (long)sw;
	CHECK(5, sl == -300L, 1);
	sb = 5;
	sl = (long)sb;
	CHECK(6, sl == 5L, 1);

	/* byte to short */
	ub = 200;
	CHECK(7, (unsigned short)ub, 200);
	sb = -3;
	CHECK(8, (short)sb, -3);

	/* the shape it was found in */
	stream[0] = 8; stream[1] = 0; stream[2] = 0; stream[3] = 0;
	pos = 0;
	CHECK(9, readLE4() == 8L, 1);

	stream[0] = 0x78; stream[1] = 0x56; stream[2] = 0x34; stream[3] = 0x12;
	pos = 0;
	CHECK(10, readLE4() == 0x12345678L, 1);

	stream[0] = 1; stream[1] = 1; stream[2] = 0; stream[3] = 0;
	pos = 0;
	CHECK(11, readLE4() == 257L, 1);

	/* a widening cast used straight in an expression */
	ub = 4;
	CHECK(12, ((unsigned long)ub) << 8 == 1024L, 1);
	CHECK(13, ((unsigned long)ub) + 1L == 5L, 1);

	return 0;
}
