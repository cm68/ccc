/*
 * Hand-written pins for rule shapes whose old drivers evaporated
 * when cpp began folding constants: the libc lines that exercised
 * them now reach pass2 as single numbers.  Everything here works
 * through variables, which nothing folds.  One function per shape,
 * so register homing stays predictable.
 */
#include "rt.h"

short g0_s, g1_s, *p0_s, t_s;
char g0_b;

/* =(D(H),E):sV - a short stored through a computed address, the
 * assignment used as a value: the address ends in HL, the value
 * waits in DE */
static short
pin_stv(void)
{
	t_s = (*(p0_s + g0_s) = g1_s) + 1;
	return t_s;
}

/* q(B,zero), p(B,zero) - signed ordered compares of a BC-homed
 * word against zero; no byte register vars to steal the pair */
static short
pin_bzero(void)
{
	register short r;
	short n;

	r = g1_s - 10;
	n = 0;
	if (r > 0)
		n = 1;
	if (r <= 0)
		n += 2;
	r = g1_s;
	if (r > 0)
		n += 4;
	if (r <= 0)
		n = 9;
	return n;
}

/* e(A,V), n(A,V) against the byte in C: both register chars
 * earn their homes, the busier one takes B, the compares target
 * the one left in C */
static short
pin_ac(void)
{
	register char b1;
	register char c2;
	short x;

	b1 = 1;
	b1 += g0_b;
	b1 += g0_b;
	b1 += g0_b;
	c2 = g0_b + 1;
	t_s = c2;
	t_s += c2;
	x = ((char)(g0_b + b1) == c2);
	x += ((char)(g0_b + 3) != c2) ? 2 : 0;
	return x + b1 + c2 + (short)(t_s - 10);
}

int
main()
{
	g0_s = 0;
	g1_s = 7;
	p0_s = &t_s;

	t_s = 0;
	pin_stv();
	CHECK(1, t_s, 8);

	CHECK(2, pin_bzero(), 6);

	g0_b = 4;
	CHECK(3, pin_ac(), 20);
	return 0;
}
