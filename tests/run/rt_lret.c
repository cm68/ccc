/*
 * Long truth, and the width a return value arrives at.
 *
 * Two bugs, one hiding the other.  A long lives in HL:DE with HL the
 * high word, and the flag-context rule for INHL carried no width - so
 * it matched a long and tested HL alone.  Every long that fit in an
 * int tested false, and 65536 tested true.
 *
 * Under that, the return value was never converted to the type the
 * function was declared to return: "long f() { return 7; }" loaded HL
 * and left DE alone, which is 458752 plus whatever DE held.  The first
 * bug hid the second, because the garbage was in the half that was not
 * being looked at.
 *
 * Found in cpp, where "#if 1" was false.  readcppconst() returns an
 * unsigned long and push_cond() takes one, so every conditional block
 * in every source was skipped - including the include guards, which is
 * why headers appeared to be empty rather than absent.
 */
#include "rt.h"

unsigned long
uret(n)
int n;
{
	return n;			/* int -> unsigned long */
}

long
sret(n)
int n;
{
	return n;			/* int -> long, sign extended */
}

long
cret()
{
	return 7;			/* a constant needs the width too */
}

unsigned long
bret(c)
unsigned char c;
{
	return c;			/* uchar -> ulong, zero extended */
}

/* truth of a long parameter: the shape push_cond() has */
short
truth(v)
unsigned long v;
{
	return v ? 5 : 0;
}

short
truthif(v)
long v;
{
	if (v)
		return 1;
	return 0;
}

short
truthnot(v)
unsigned long v;
{
	return !v;
}

/* a long against a while, which is the same test in a loop */
short
countdown(v)
unsigned long v;
{
	short n;

	n = 0;
	while (v) {
		n++;
		v = v - 1;
	}
	return n;
}

main()
{
	unsigned long u;
	long s;

	CHECK(1, uret(1) == 1, 1);
	CHECK(2, uret(0) == 0, 1);
	CHECK(3, uret(1000) == 1000, 1);
	CHECK(4, sret(-1) == -1, 1);
	CHECK(5, sret(-2) < 0, 1);
	CHECK(6, cret() == 7, 1);
	CHECK(7, bret(200) == 200, 1);

	/* the high word must actually be zero, not merely ignored */
	u = uret(1);
	CHECK(8, (short)(u >> 16), 0);
	s = sret(-1);
	CHECK(9, (short)(s >> 16), -1);

	/*
	 * Truth of a long: 1 is true even though its high word is zero.
	 *
	 * These are K&R definitions, so no prototype is in scope at the
	 * call and the argument has to carry its own width - "truth(0)"
	 * passes two bytes to a four byte parameter and reads garbage
	 * for the high word.  zc3 does the same thing.
	 */
	CHECK(10, truth(1L), 5);
	CHECK(11, truth(0L), 0);
	CHECK(12, truth(65536L), 5);	/* true in the high word only */
	CHECK(13, truth(uret(1)), 5);

	CHECK(14, truthif(1L), 1);
	CHECK(15, truthif(0L), 0);
	CHECK(16, truthif(-1L), 1);
	CHECK(17, truthif(65536L), 1);

	CHECK(18, truthnot(1L), 0);
	CHECK(19, truthnot(0L), 1);
	CHECK(20, truthnot(65536L), 0);

	CHECK(21, countdown(3L), 3);
	CHECK(22, countdown(0L), 0);

	return 0;
}
