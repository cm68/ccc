/*
 * Taking the address of something.
 *
 * A bare LOCALVAR in the tree is a place, not a value - reading one is
 * a DEREF wrapped around it - so "p = &v" arrives as an assignment
 * whose right operand is the slot itself.  The rules for that loaded
 * through it instead of taking it, which made every "&v" on a local
 * mean "v".  libcpm's time() passed the first two bytes of a struct
 * where it meant to pass its address.
 *
 * The other half was in pass1.  A variable whose address is taken
 * cannot live in a register, and the flag saying so was set while
 * phase 2 walked the statements - after phase 2 had already handed the
 * registers out at the top of the function.  It is set in phase 1 now,
 * beside the reference count, which is early enough to be believed.
 *
 * Taking the address of a variable declared register is refused rather
 * than worked around: it has no address to take.  There is no test for
 * that here because it does not compile; see tests/ for the diagnostic
 * cases.
 */
#include "rt.h"

short gv, *gp;
short garr[4];

struct pt { short x, y; };
struct pt gpt;

short
byref(p) short *p;
{
	*p = *p + 5;
	return *p;
}

/* the address of a plain local */
short
locl()
{
	short v;
	short *p;

	v = 3;
	p = &v;
	*p = *p + 5;
	return v;
}

/* the address of a parameter */
short
param(a) short a;
{
	short *p;

	p = &a;
	*p = *p + 1;
	return a;
}

/* handed straight to a function, which is how libcpm tripped over it */
short
passed()
{
	short v;

	v = 10;
	byref(&v);
	return v;
}

/* the address of a local struct's member, and of the struct */
short
member()
{
	struct pt lp;
	short *p;

	lp.x = 1;
	lp.y = 2;
	p = &lp.y;
	*p = 20;
	return lp.x + lp.y;
}

/* the address of a local array element */
short
elem()
{
	short a[4];
	short *p;
	short i;

	for (i = 0; i < 4; i++)
		a[i] = i;
	p = &a[2];
	*p = 9;
	return a[1] + a[2];
}

/* a local whose address is taken inside a nested block */
short
inblock(a) short a;
{
	short r;

	r = 0;
	{
		short v;
		short *vp;

		v = a;
		vp = &v;
		*vp = *vp + 5;
		r = v;
	}
	return r;
}

/* two locals, so the offsets have to be told apart */
short
two()
{
	short u, v;
	short *pu, *pv;

	u = 1;
	v = 2;
	pu = &u;
	pv = &v;
	*pu = 10;
	*pv = 20;
	return u + v * 10;	/* 10 and not 100: a multiply by an
				 * arbitrary constant has no rule yet */
}

main()
{
	CHECK(1, locl(), 8);
	CHECK(2, param(4), 5);
	CHECK(3, param(-1), 0);
	CHECK(4, passed(), 15);
	CHECK(5, member(), 21);
	CHECK(6, elem(), 10);		/* a[1]=1, a[2]=9 */
	CHECK(7, inblock(1), 6);
	CHECK(8, inblock(-5), 0);
	CHECK(9, two(), 210);

	/* the address of a global, which always worked */
	gv = 7;
	gp = &gv;
	CHECK(10, *gp, 7);
	*gp = 8;
	CHECK(11, gv, 8);
	CHECK(12, byref(&gv), 13);

	/* and of a global array element and struct member */
	garr[2] = 3;
	gp = &garr[2];
	CHECK(13, *gp, 3);
	gpt.y = 4;
	gp = &gpt.y;
	CHECK(14, *gp, 4);
	*gp = 5;
	CHECK(15, gpt.y, 5);

	return 0;
}
