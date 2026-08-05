/*
 * Four wrong answers the generated corpus caught on its first full
 * run - every one compiled without a marker and computed something
 * else on the Z80.  Minimal spellings, one check each, so the fix
 * can be watched narrowing.
 */
#include "rt.h"

char cb, *pb;
short cs, *ps, gs, ts;
long cl, *pl, gl, tl;
unsigned long ul;

main()
{
	/* 1: chained store through a pointer, value feeding a global */
	cb = 0; pb = &cb;
	gs = 0;
	gs = *pb = 5;
	CHECK(1, gs, 5);
	CHECK(2, cb, 5);

	cl = 0L; pl = &cl;
	gl = 0L;
	gl = *pl = 7L;
	CHECK(3, gl, 7L);
	CHECK(4, cl, 7L);

	/* 2: long assignment as a value */
	tl = (gl = -789632464L);
	CHECK(5, tl, -789632464L);
	CHECK(6, gl, -789632464L);

	/* 3: ternary with a false condition, word arms */
	gs = 0;
	ts = gs ? 225 : 11104;
	CHECK(7, ts, 11104);
	gs = 1;
	ts = gs ? 225 : 11104;
	CHECK(8, ts, 225);

	/* 4: long shift by a variable count past 15 */
	gl = 224584094L;
	ul = 29L;
	tl = gl << ul;
	CHECK(9, tl, -1073741824L);
	ul = 3L;
	tl = gl << ul;
	CHECK(10, tl, 1796672752L);
	return 0;
}
