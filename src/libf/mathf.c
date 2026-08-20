/*
 * mathf.c - elementary functions on the FLOAT type (see float.h).
 *
 * Algorithms are the ones in the old double-based libc sources (sqrt.c
 * exp.c log.c sin.c atan.c atan2.c evalpoly.c), rewritten as explicit
 * function calls on FLOAT.  The coefficients are pre-encoded in the
 * float format (sign<<31 | exp<<24 | 24-bit mantissa).
 */
#include "float.h"

#define F_PI	0x42C90FDBL
#define F_PI2	0x41C90FDBL
#define F_LN2	0x40B17218L
#define F_LOG2E	0x41B8AA3BL
#define F_2PI	0x40A2F983L

static FLOAT expc[] = {
	0x41800000L, 0x40B17218L, 0x3EF5FDF0L, 0x3CE35847L, 0x3A9D9558L,
	0x37AEC482L, 0x34A178A8L, 0x318093EFL, 0x2DA792A0L, 0x2A955646L
};
static FLOAT logc[] = {
	0x00000000L, 0x40FFFFC4L, 0xBFFFEF80L, 0x3FA9E190L, 0xBEF682ECL,
	0x3EABAD82L, 0xBDC33C0EL, 0x3C93D187L, 0xB9D37841L
};
static FLOAT sina[] = {
	0x58CF3258L, 0xD796D878L, 0x53D6E4E2L, 0xCED85D17L, 0x4891F806L
};
static FLOAT sinb[] = {
	0x5883E7CFL, 0x53C74E67L, 0x4E93DC62L, 0x4884A74BL, 0x41800000L
};
static FLOAT atana[] = {
	0x46843C06L, 0x46EA9F7DL, 0x4681902DL, 0x43BB4D60L, 0x3EC7EC51L,
	0xB89F8DDEL
};
static FLOAT atanb[] = {
	0x46843C06L, 0x478B59C0L, 0x46C40474L, 0x44CF9BF9L, 0x41800000L
};

/* split f into a mantissa in [0.5,1) and an exponent in *e */
static FLOAT
ffrexp(f, e)
	FLOAT f;
	int *e;
{
	unsigned long m;
	int x;

	m = f & 0xffffffL;
	if (m == 0) {
		*e = 0;
		return 0;
	}
	x = (f >> 24) & 0x7f;
	*e = x - 64;
	return (f & 0x80000000L) | (64L << 24) | m;
}

/* f * 2^e */
static FLOAT
fldexp(f, e)
	FLOAT f;
	int e;
{
	unsigned long m;
	int x;

	m = f & 0xffffffL;
	if (m == 0)
		return 0;
	x = ((f >> 24) & 0x7f) + e;
	if (x < 0)
		return 0;
	if (x > 127)
		x = 127;
	return (f & 0x80000000L) | ((unsigned long)x << 24) | m;
}

/* Horner's method over d[0..n] */
static FLOAT
fevalpoly(x, d, n)
	FLOAT x;
	FLOAT d[];
	int n;
{
	int i;
	FLOAT r;

	r = d[n];
	for (i = n; i > 0; i--)
		r = fadd(fmul(x, r), d[i-1]);
	return r;
}

FLOAT
fabs(x)
	FLOAT x;
{
	return x & 0x7fffffffL;
}

FLOAT
floor(x)
	FLOAT x;
{
	FLOAT r;

	r = itof(ftoi(x));
	if (fcmp(r, x) > 0)
		return fsub(r, itof(1));
	return r;
}

FLOAT
sqrt(x)
	FLOAT x;
{
	FLOAT og, ng;
	int niter, e;

	if (fcmp(x, itof(0)) <= 0)
		return itof(0);
	og = x;
	if (fcmp(og, itof(1)) < 0)
		og = fdiv(itof(1), og);
	og = ffrexp(og, &e);
	og = fldexp(og, e / 2);
	if (fcmp(x, itof(1)) < 0)
		og = fdiv(itof(1), og);
	for (niter = 0; niter < 20; niter++) {
		ng = fdiv(fadd(fdiv(x, og), og), itof(2));
		if (ng == og)
			break;
		og = ng;
	}
	return og;
}

FLOAT
exp(x)
	FLOAT x;
{
	int e;
	char neg;
	FLOAT r;

	if (x == 0)
		return itof(1);
	neg = fcmp(x, itof(0)) < 0;
	if (neg)
		x = fsub(itof(0), x);
	x = fmul(x, F_LOG2E);
	e = ftoi(floor(x));
	x = fsub(x, itof(e));
	r = fldexp(fevalpoly(x, expc, 9), e);
	if (neg)
		return fdiv(itof(1), r);
	return r;
}

FLOAT
log(x)
	FLOAT x;
{
	int e;

	if (fcmp(x, itof(0)) <= 0)
		return itof(0);
	x = ffrexp(x, &e);
	x = fsub(fmul(x, itof(2)), itof(1));
	e--;
	return fadd(fevalpoly(x, logc, 8), fmul(F_LN2, itof(e)));
}

FLOAT
sin(f)
	FLOAT f;
{
	FLOAT y, ys;
	int sect, e;

	if (fcmp(f, itof(0)) < 0) {
		f = fsub(itof(0), f);
		sect = 2;
	} else
		sect = 0;
	f = fmul(f, F_2PI);
	if (fcmp(f, itof(4)) > 0)
		f = fsub(f, fmul(itof(4), floor(fdiv(f, itof(4)))));
	if (fcmp(f, itof(2)) > 0) {
		f = fsub(f, itof(2));
		sect = 2 - sect;
	}
	e = ftoi(f);
	y = fsub(f, itof(e));
	sect = (e + sect) % 4;
	if (sect & 1)
		y = fsub(itof(1), y);
	if (sect & 2)
		y = fsub(itof(0), y);
	ys = fmul(y, y);
	return fdiv(fmul(y, fevalpoly(ys, sina, 4)), fevalpoly(ys, sinb, 4));
}

FLOAT
cos(x)
	FLOAT x;
{
	return sin(fadd(x, F_PI2));
}

FLOAT
atan(f)
	FLOAT f;
{
	FLOAT val, vs;
	int recip;

	val = fabs(f);
	if (val == 0)
		return itof(0);
	recip = fcmp(val, itof(1)) > 0;
	if (recip)
		val = fdiv(itof(1), val);
	vs = fmul(val, val);
	val = fmul(val, fdiv(fevalpoly(vs, atana, 5), fevalpoly(vs, atanb, 4)));
	if (recip)
		val = fsub(F_PI2, val);
	if (fcmp(f, itof(0)) < 0)
		return fsub(itof(0), val);
	return val;
}

FLOAT
atan2(x, y)
	FLOAT x, y;
{
	FLOAT v;

	if (fcmp(fabs(y), fabs(x)) >= 0) {
		v = atan(fdiv(x, y));
		if (fcmp(y, itof(0)) < 0) {
			if (fcmp(x, itof(0)) >= 0)
				v = fadd(v, F_PI);
			else
				v = fsub(v, F_PI);
		}
		return v;
	}
	v = fsub(itof(0), atan(fdiv(y, x)));
	if (fcmp(y, itof(0)) < 0)
		v = fsub(v, F_PI2);
	else
		v = fadd(v, F_PI2);
	return v;
}

FLOAT
pow(x, y)
	FLOAT x, y;
{
	if (y == 0)
		return itof(1);
	if (fcmp(x, itof(0)) < 0)
		return itof(0);
	if (x == 0)
		return itof(0);
	return exp(fmul(log(x), y));
}
