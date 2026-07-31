/*
 * A bitmap kept in bytes, the bit picked by a runtime index - the
 * shape of pass1's else-if bookkeeping (ifHasElse), where it was
 * found broken.  "map[i >> 3] |= 1 << (i & 7)" computes its count as
 * a word, and the only variable-count byte shift rule wanted the
 * count in E as a byte - so the shift matched nothing, silently, and
 * the compound assignment stored the count itself into the bitmap.
 * Every else-if the ccc-built c0 emitted said has_else=0.
 */
#include "rt.h"

unsigned char map[4];
unsigned short idx;

int
rd(void)
{
	return (map[idx >> 3] >> (idx & 7)) & 1;
}

void
wr(void)
{
	map[idx >> 3] |= 1 << (idx & 7);
}

int
main()
{
	int r;

	idx = 1;
	wr();
	idx = 9;
	wr();
	r = 0;
	idx = 1; r += rd();		/* 1 */
	idx = 2; r += rd();		/* 0 */
	idx = 9; r += rd();		/* 1 */
	idx = 8; r += rd();		/* 0 */
	CHECK(1, r, 2);
	return 0;
}
