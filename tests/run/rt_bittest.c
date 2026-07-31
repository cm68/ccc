/*
 * A byte tested against a byte-range mask is a byte test.
 *
 * "flags & TF_X" is the most repeated expression in both compilers,
 * and every one of them was eleven instructions of word arithmetic:
 * C promoted the byte, so the AND arrived at pass2 a word wide and
 * the byte rules - one of which is the bit instruction - never saw
 * it.  normtree now drops the widening in flag context, where a
 * mask under 256 zeroes the high byte whatever the extension put
 * there: zeroes for WIDEN, the sign for SEXT.
 *
 * The signed cases are the ones worth pinning: 0x80 set on a
 * negative char sign-extends to 0xff80, and the old word test and
 * the new byte test must answer alike.
 */
#include "rt.h"

unsigned char uf;
char sf;

int
main()
{
	int v;

	uf = 0x88;
	CHECK(1, (uf & 8) ? 1 : 0, 1);		/* pow2: bit */
	CHECK(2, (uf & 4) ? 1 : 0, 0);
	CHECK(3, (uf & 0x88) ? 1 : 0, 1);	/* not pow2: and */
	CHECK(4, (uf & 0x77) ? 1 : 0, 0);

	sf = 0x80 | 2;				/* negative: SEXT high is 0xff */
	CHECK(5, (sf & 0x80) ? 1 : 0, 1);
	CHECK(6, (sf & 2) ? 1 : 0, 1);
	CHECK(7, (sf & 0x7d) ? 1 : 0, 0);

	/* value context is untouched: the word result must still be a
	 * word, high byte and all */
	v = uf & 0x88;
	CHECK(8, v, 0x88);
	v = sf & 0xff;
	CHECK(9, v, 0x82);
	return 0;
}
