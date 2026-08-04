/*
 * setjmp and longjmp, against the calling convention.
 *
 * The old setjmp parked its return address in BC - the caller's
 * register variable, callee-saved by convention - and neither return
 * path gave it back: the direct return left the return address in
 * it, and longjmp left the caller's old frame pointer.  A function
 * that kept anything in a register and called setjmp lost it both
 * ways, so the checks here run a register variable through every
 * path and look at it on the far side.
 *
 * The convention says five things come back to life at the second
 * return: SP, the return address, IY, and both register-variable
 * homes - BC and IX.  jmp_buf grew from four ints to five to hold
 * them all.
 */
#include "rt.h"
#ifdef RT_ZC3
/*
 * zc3's include area has no setjmp.h; its libc.a carries the same
 * longjmp.o this tree assembles, so the declarations are spelled
 * here.  Five ints, matching libc/longjmp.s.
 */
typedef int jmp_buf[5];
extern int setjmp();
extern void longjmp();
#else
#include <setjmp.h>
#endif

static jmp_buf env;
static short trail;

/* a deep callee, so the jump crosses real frames with real locals */
static short
inner(depth)
short depth;
{
	char pad[10];

	pad[0] = (char)depth;
	if (depth > 0)
		return inner(depth - 1) + pad[0];
	longjmp(env, 7);
	return 99;			/* never */
}

/*
 * A register variable alive across setjmp and the longjmp return.
 * C says an auto changed between the calls is indeterminate after
 * the jump - so r is set before setjmp and never touched again,
 * which the convention must preserve.
 */
static short
harness()
{
	register short r;
	register char *p;
	short got;

	r = 0x1234;
	p = (char *)&trail;
	got = setjmp(env);
	if (got == 0) {
		if (r != 0x1234)
			return 1;	/* BC lost on the DIRECT return */
		if (p != (char *)&trail)
			return 2;	/* IX lost on the direct return */
		trail = 5;
		inner(3);
		return 3;		/* longjmp did not jump */
	}
	if (got != 7)
		return 4;		/* wrong value delivered */
	if (r != 0x1234)
		return 5;		/* BC lost through the jump */
	if (p != (char *)&trail)
		return 6;		/* IX lost through the jump */
	if (trail != 5)
		return 7;		/* pre-jump store did not stick */
	*p = 9;				/* the register still WORKS */
	if (trail != 9 && (trail & 0xff) != 9)
		return 8;
	return 0;
}

/* longjmp(buf, 0) must come back as 1 */
static short
zerocase()
{
	short got;

	got = setjmp(env);
	if (got == 0) {
		longjmp(env, 0);
		return 90;
	}
	return got;
}

/* two jumps through one buf: the second return works twice */
static short
twice()
{
	static short hits;
	short got;

	hits = 0;
	got = setjmp(env);
	hits++;
	if (got < 2)
		longjmp(env, got + 1);
	return hits;			/* the call, then jumps with 1 and 2 */
}

main()
{
	CHECK(1, harness(), 0);
	CHECK(2, zerocase(), 1);
	CHECK(3, twice(), 3);
	return 0;
}
