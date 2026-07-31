/*
 * A partly-initialized struct in a static array occupies all of it.
 *
 * The initializer streamer emitted the members it was given and
 * stopped.  A struct with more fields than the initializer names then
 * took only as many bytes as were written, so the elements of an
 * array of them sat closer together in memory than sizeof says - and
 * every subscript is computed against sizeof.
 *
 * pass1's own basicnames[] is seven entries of "{ name, type, chain }"
 * out of a struct with a dozen fields: 20 bytes written, 37 expected.
 * basicnames[1] was read at base+37+16, which lands in the middle of
 * basicnames[0]'s name, and came back null - so the c0 that ccc built
 * could not name a basic type.  "int", "long" and "unsigned char" as
 * parameters all answered "fn array"; index 0 worked, having no
 * stride to get wrong, which is what made it look like a problem with
 * particular types rather than with the table.
 *
 * Found with the simulator's monitor: dumping the table showed the
 * entries 20 bytes apart while the code multiplied by 37.
 */
#include "rt.h"

struct ent {
	char name[8];
	short *tp;
	struct ent *chain;
	short a;
	short b;
	char pad[4];
};

short v0, v1, v2;

/* three fields named out of six - the shape basicnames has */
struct ent tab[3] = {
	{ "a", &v0, 0 },
	{ "b", &v1, &tab[0] },
	{ "c", &v2, &tab[1] },
};

/* and a fully written one, which was always right */
struct ent full[2] = {
	{ "x", &v0, 0, 1, 2, { 0, 0, 0, 0 } },
	{ "y", &v1, 0, 3, 4, { 0, 0, 0, 0 } },
};

main()
{
	short i;

	v0 = 100; v1 = 101; v2 = 102;

	/*
	 * The stride is checked through the entries rather than by
	 * subtracting their addresses: every read below goes through a
	 * subscript, so a stride that disagrees with sizeof lands in the
	 * previous entry and the values come back wrong.  That is
	 * exactly how the bug presented.
	 *
	 * Not by address arithmetic, which has a defect of its own here:
	 * "(unsigned)&tab[1] - (unsigned)&tab[0]" answers zero even when
	 * the two addresses provably differ.  Written down rather than
	 * checked, so this file keeps testing the one thing it is for.
	 */
	/*
	 * Two further defects live in this neighbourhood and are left
	 * to their own test rather than checked here, so that this file
	 * keeps failing for one reason only:
	 *
	 *   (unsigned)&tab[1] - (unsigned)&tab[0]   answers zero
	 *   &tab[2] != &tab[1]                      answers false
	 *
	 * both with addresses that print distinct.
	 */

	/* each entry's pointer must be its own */
	CHECK(4, tab[0].tp == &v0, 1);
	CHECK(5, tab[1].tp == &v1, 1);
	CHECK(6, tab[2].tp == &v2, 1);
	CHECK(7, *tab[0].tp, 100);
	CHECK(8, *tab[1].tp, 101);
	CHECK(9, *tab[2].tp, 102);

	/* reached by a computed index, which is how the bug showed */
	for (i = 0; i < 3; i++)
		CHECK(10 + i, *tab[i].tp, 100 + i);

	/* the chain, which is what made the table walkable */
	CHECK(13, tab[1].chain == &tab[0], 1);
	CHECK(14, tab[2].chain == &tab[1], 1);
	CHECK(15, tab[0].chain == 0, 1);

	/* the unnamed members must be zero, not whatever followed */
	CHECK(16, tab[0].a, 0);
	CHECK(17, tab[0].b, 0);
	CHECK(18, tab[2].a, 0);

	CHECK(19, full[0].a, 1);
	CHECK(20, full[1].b, 4);
	CHECK(21, tab[1].a, 0);

	return 0;
}
