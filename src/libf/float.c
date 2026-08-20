/*
 * float.c - software floating point on a 32-bit long.
 *
 * The representation (see float.h): bit 31 sign, bits 24..30 exponent
 * (bias 64), bits 0..23 mantissa with an explicit leading 1 in bit 23.
 * value = mantissa * 2^(exp - 88).  All arithmetic is done here in C on
 * the long bits, so the byte order never matters.
 */
#include "float.h"

#define F_MANT	0xffffffL
#define F_EXP	0x7fL
#define F_SIGN	0x80000000L

/*
 * Multiply two 24-bit mantissas, return the high 24 bits of the 48-bit
 * product (which is what a normalized mantissa needs).  Split each into
 * 12-bit halves so every partial product fits a 32-bit long.
 */
static unsigned long
mul24(a, b)
	unsigned long a, b;
{
	unsigned long ah, al, bh, bl, mid;

	ah = a >> 12; al = a & 0xfffL;
	bh = b >> 12; bl = b & 0xfffL;
	mid = ah * bl + al * bh;
	return ((ah * bh) << 1) + (mid >> 11);
}

/*
 * Divide two 24-bit mantissas, return (a / b) << 23 - a 24-bit
 * restoring division.
 */
static unsigned long
div24(a, b)
	unsigned long a, b;
{
	unsigned long q;
	int i;

	q = 0;
	for (i = 0; i < 24; i++) {
		q <<= 1;
		if (a >= b) {
			a -= b;
			q |= 1;
		}
		a <<= 1;
	}
	return q;
}

FLOAT
itof(i)
	int i;
{
	unsigned long m;
	int e, s;

	if (i == 0)
		return 0;
	s = 0;
	if (i < 0) {
		s = 1;
		i = -i;
	}
	m = (unsigned long)i;
	e = 88;
	while ((m & 0x800000L) == 0) {
		m <<= 1;
		e--;
	}
	return ((unsigned long)s << 31) | ((unsigned long)e << 24) | m;
}

int
ftoi(f)
	FLOAT f;
{
	unsigned long m;
	int e, s;
	long r;

	m = f & F_MANT;
	if (m == 0)
		return 0;
	e = (f >> 24) & F_EXP;
	s = (f >> 31) & 1;
	if (e >= 88)
		r = (long)(m << (e - 88));
	else if (e >= 64)
		r = (long)((m + (1L << (87 - e))) >> (88 - e));	/* round to nearest */
	else
		r = 0;	/* |value| < 1 */
	if (s)
		r = -r;
	return (int)r;
}

FLOAT
fneg(x)
	FLOAT x;
{
	return x ^ 0x80000000L;
}

FLOAT
fmod(a, b)
	FLOAT a, b;
{
	if ((b & F_MANT) == 0)
		return 0;
	return fsub(a, fmul(b, itof(ftoi(fdiv(a, b)))));
}

/* truncate toward zero (no rounding); used where the integer part is wanted */
int
ftrunc(f)
	FLOAT f;
{
	unsigned long m;
	int e, s;
	long r;

	m = f & F_MANT;
	if (m == 0)
		return 0;
	e = (f >> 24) & F_EXP;
	s = (f >> 31) & 1;
	if (e >= 88)
		r = (long)(m << (e - 88));
	else if (e >= 64)
		r = (long)(m >> (88 - e));
	else
		r = 0;
	if (s)
		r = -r;
	return (int)r;
}

FLOAT
fadd(a, b)
	FLOAT a, b;
{
	unsigned long ma, mb, m;
	int ea, eb, e, s;

	ma = a & F_MANT; mb = b & F_MANT;
	if (ma == 0)
		return b;
	if (mb == 0)
		return a;
	ea = (a >> 24) & F_EXP; eb = (b >> 24) & F_EXP;
	if (ea >= eb) {
		if (ea - eb > 24)
			return a;
		mb >>= (ea - eb);
		e = ea;
	} else {
		if (eb - ea > 24)
			return b;
		ma >>= (eb - ea);
		e = eb;
	}
	if ((a >> 31) == (b >> 31)) {
		m = ma + mb;
		s = (a >> 31) & 1;
	} else if (ma >= mb) {
		m = ma - mb;
		s = (a >> 31) & 1;
	} else {
		m = mb - ma;
		s = (b >> 31) & 1;
	}
	if (m == 0)
		return 0;
	if (m > F_MANT) {
		m >>= 1;
		e++;
	}
	while ((m & 0x800000L) == 0) {
		m <<= 1;
		e--;
	}
	return ((unsigned long)s << 31) | ((unsigned long)e << 24) | m;
}

FLOAT
fsub(a, b)
	FLOAT a, b;
{
	if ((b & F_MANT) == 0)
		return a;
	return fadd(a, b ^ F_SIGN);
}

FLOAT
fmul(a, b)
	FLOAT a, b;
{
	unsigned long ma, mb, m;
	int ea, eb, e, s;

	ma = a & F_MANT; mb = b & F_MANT;
	if (ma == 0 || mb == 0)
		return 0;
	ea = (a >> 24) & F_EXP; eb = (b >> 24) & F_EXP;
	s = (a >> 31) ^ (b >> 31);
	m = mul24(ma, mb);
	e = ea + eb - 65;
	if (m > F_MANT) {
		m >>= 1;
		e++;
	}
	while ((m & 0x800000L) == 0) {
		m <<= 1;
		e--;
	}
	return ((unsigned long)s << 31) | ((unsigned long)e << 24) | m;
}

FLOAT
fdiv(a, b)
	FLOAT a, b;
{
	unsigned long ma, mb, m;
	int ea, eb, e, s;

	mb = b & F_MANT;
	if (mb == 0)
		return 0;	/* divide by zero */
	ma = a & F_MANT;
	if (ma == 0)
		return 0;
	ea = (a >> 24) & F_EXP; eb = (b >> 24) & F_EXP;
	s = (a >> 31) ^ (b >> 31);
	m = div24(ma, mb);
	e = ea - eb + 65;
	if (m > F_MANT) {
		m >>= 1;
		e++;
	}
	while ((m & 0x800000L) == 0) {
		m <<= 1;
		e--;
	}
	return ((unsigned long)s << 31) | ((unsigned long)e << 24) | m;
}

int
fcmp(a, b)
	FLOAT a, b;
{
	unsigned long ua, ub;
	int sa, sb;

	ua = a & 0x7fffffffL; ub = b & 0x7fffffffL;
	sa = (a >> 31) & 1;   sb = (b >> 31) & 1;
	if (ua == 0 && ub == 0)
		return 0;
	if (sa != sb) {
		if (sa)
			return -1;
		return 1;
	}
	if (ua < ub) {
		if (sa)
			return 1;
		return -1;
	}
	if (ua > ub) {
		if (sa)
			return -1;
		return 1;
	}
	return 0;
}

/*
 * String <-> FLOAT conversions.
 */

FLOAT
fatof(s)
	char *s;
{
	FLOAT r, scale, t;
	int sign, e10, esign, i;

	r = itof(0);
	sign = 0;
	e10 = 0;
	while (*s == ' ' || *s == '\t')
		s++;
	if (*s == '-') { sign = 1; s++; }
	else if (*s == '+') s++;

	/* integer part */
	while (*s >= '0' && *s <= '9') {
		r = fadd(fmul(r, itof(10)), itof(*s - '0'));
		s++;
	}
	/* fractional part */
	if (*s == '.') {
		scale = itof(1);
		s++;
		while (*s >= '0' && *s <= '9') {
			scale = fdiv(scale, itof(10));
			r = fadd(r, fmul(itof(*s - '0'), scale));
			s++;
		}
	}
	/* exponent */
	esign = 0;
	if (*s == 'e' || *s == 'E') {
		s++;
		if (*s == '-') { esign = 1; s++; }
		else if (*s == '+') s++;
		while (*s >= '0' && *s <= '9') {
			e10 = e10 * 10 + (*s - '0');
			s++;
		}
	}
	t = itof(10);
	if (esign) {
		for (i = 0; i < e10; i++)
			r = fdiv(r, t);
	} else {
		for (i = 0; i < e10; i++)
			r = fmul(r, t);
	}
	if (sign)
		r = fsub(itof(0), r);
	return r;
}

/* integer -> decimal, into buf (returns the number of chars) */
static int
putint(buf, n)
	char *buf;
	int n;
{
	char tmp[8];
	int i, j;

	i = 0;
	if (n == 0)
		tmp[i++] = '0';
	while (n > 0) {
		tmp[i++] = '0' + (n % 10);
		n /= 10;
	}
	for (j = 0; j < i; j++)
		buf[j] = tmp[i - 1 - j];
	return i;
}

/*
 * FLOAT -> fixed-point string, fracdigits places after the point.
 * Returns a pointer to buf.  No exponent form; that is awk's business.
 */
char *
ftoa(buf, f, fracdigits)
	char *buf;
	FLOAT f;
	int fracdigits;
{
	char *p;
	FLOAT frac, t;
	int ipart, i;

	p = buf;
	if (f & 0x80000000L) {
		*p++ = '-';
		f = fsub(itof(0), f);
	}
	ipart = ftrunc(f);
	p += putint(p, ipart);
	if (fracdigits > 0) {
		*p++ = '.';
		frac = fsub(f, itof(ipart));
		for (i = 0; i < fracdigits; i++) {
			frac = fmul(frac, itof(10));
			t = floor(frac);
			*p++ = '0' + ftoi(t);
			frac = fsub(frac, t);
		}
	}
	*p = 0;
	return buf;
}
