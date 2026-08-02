/*
 * Long constant folding must not happen in int width.
 *
 * foldNode's identity table said "x & 0xffff is x" at every width -
 * true of a word, and a LONG handed back whole with its high half
 * intact: "(long)v & 0xffff" simply vanished, which is how the
 * self-hosted c0 printed .dw -1 where the host wrote 65535.  And
 * its fold arithmetic ran in plain unsigned - 16 bits on the Z80 -
 * so two long constants folded to their low words.
 */
#include "rt.h"

int gi;
long gl;

long
maskit(v)
int v;
{
	return (long)v & 0xffff;
}

int
main()
{
	gi = -1;
	gl = maskit(gi);
	CHECK(1, (int)(gl >> 16), 0);
	CHECK(2, (gl == 65535L) ? 1 : 0, 1);

	/* long constant folds, all above 16 bits */
	gl = 100000L + 30000L;
	CHECK(3, (gl == 130000L) ? 1 : 0, 1);
	gl = 70000L & 0xffffL;
	CHECK(4, (int)gl, 4464);
	gl = 3L << 20;
	CHECK(5, (int)(gl >> 16), 48);
	CHECK(6, (200000L > 100000L) ? 1 : 0, 1);
	return 0;
}
