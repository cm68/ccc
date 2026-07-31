/*
 * Byte variables shifted by runtime counts, through both compound
 * paths - lowercompound for a plain lvalue, docompound for one with
 * side effects - plus the signed/unsigned right-shift split.
 *
 * The whole family was missing from the rules table, and missing
 * SILENTLY on the docompound path: the store asserted the answer
 * was in A, no rule had put it there, and "m[i++] >>= n" wrote the
 * shift count into the array.  The docompound marker (this commit)
 * is what flagged it; these are the shapes it flagged.
 */
#include "rt.h"

unsigned char m[4];
char sm[2];
int i, n;

int
main()
{
	m[0] = 16;
	i = 0;
	n = 2;
	m[i] >>= n;			/* lowercompound */
	CHECK(1, m[0], 4);

	m[1] = 3;
	i = 1;
#ifndef RT_ZC3
	/*
	 * Not under zc3: HiTech V3.09 emits the undocumented SLL (CB 37,
	 * shift left inserting ONES) for a byte <<= by a variable, so on
	 * anything with faithful Z80 semantics 3 << 2 comes back 15.
	 * Its own assembler encodes it faithfully too - the object holds
	 * CB 37 - so this is wrong on real silicon, not a simulator
	 * quibble.  ccc and the native build agree on 12.
	 */
	m[i] <<= n;
	CHECK(2, m[1], 12);
#endif

	sm[0] = -16;			/* signed: sra keeps the sign */
	sm[0] >>= 2;
	CHECK(3, sm[0], -4);

	m[0] = 16;
	m[1] = 16;
	i = 0;
	m[i++] >>= n;			/* docompound: stepping lvalue */
	CHECK(4, m[0], 4);
#ifndef RT_ZC3
	m[i] <<= 1;			/* same SLL bug under zc3 */
	CHECK(5, m[1], 32);
#endif
	i++;
	CHECK(6, i, 2);
	return 0;
}
