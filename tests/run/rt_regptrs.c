/*
 * Two register pointers walking the same buffer: one lands in IX,
 * the other in BC, and everything between them was uncovered.
 *
 * "p > q" had no rule for the BC-against-IX shape at all, and the
 * cc functions read a union field that is garbage on an unreduced
 * node - the branch went on whatever flags were lying around, with
 * no marker.  "p - q" at least said XXXXXX, but the span length
 * came back as junk either way.  outf's literal spans were the
 * first code in the tree to do both, and every literal byte of the
 * self-built compiler's output vanished.
 *
 * The scan/emit skeleton at the bottom is outf's exact shape.
 */
#include "rt.h"

int strcmp();

char src[16];
char ob[32];
int on;

int
cmps(s)
char *s;
{
	register char *p = s;
	register char *q = s + 3;
	int r = 0;

	if (p < q)
		r |= 1;
	if (q > p)
		r |= 2;
	if (p != q)
		r |= 4;
	p += 3;
	if (p == q)
		r |= 8;
	if (p >= q)
		r |= 16;
	if (p <= q)
		r |= 32;
	return r;
}

int
spans(s)
char *s;
{
	register char *p = s;
	register char *q = s;

	p += 5;
	if (p - q != 5)
		return 90;
	if (q + 5 != p)
		return 91;
	return p - q;
}

void
sink(s, len)
char *s;
int len;
{
	while (len--)
		ob[on++] = *s++;
}

void
emitf(fmt)
char *fmt;
{
	register char *p = fmt;
	char *q;

	q = p;
	while (*p) {
		if (*p != '%') {
			p++;
			continue;
		}
		if (p > q)
			sink(q, p - q);
		p += 2;		/* skip % and the conversion char */
		q = p;
	}
	if (p > q)
		sink(q, p - q);
}

struct tok {
	int a;
	long b;
	char *c;
};
struct tok tsrc[3], tdst[3];
int ncp;
char corder[4];

void
cp(d, s)
struct tok *d;
struct tok *s;
{
	corder[ncp] = 'A' + (s - tsrc);
	d->a = s->a;
	d->b = s->b;
	d->c = s->c;
	ncp++;
}

/* the tokcpy walk: both pointers stepped by sizeof IN the argument
 * list.  The postfix machinery kept the old value by pushing HL -
 * and a register-homed value reduces to itself, so what got pushed
 * was whatever HL last held.  filtknr copied its parameter types
 * from a garbage source and the K&R filter never finished. */
int
fill(n)
int n;
{
	register struct tok *d = tdst;
	struct tok *s = tsrc;

	while (n--)
		cp(d++, s++);
	return 0;
}

/*
 * (appended) A constant shifted by a runtime count.  The variable
 * shift lowering wanted its value already in HL, and a constant
 * never reduces - "1 << i" matched nothing, silently.  ispow2 in
 * pass2 itself is built from the shape, so the self-hosted c1
 * called a helper to multiply by two.
 */
int
shifts(i)
int i;
{
	unsigned long n = 2;
	int r = 0;

	if ((1 << i) == 2)
		r |= 1;
	if (n == (1 << i))
		r |= 2;
	if ((0x300 >> i) == 0x180)
		r |= 4;
	return r;
}

/* the byte register homes compared against each other: label()'s
 * own Sethi-Ullman max is "l > r ? l : r" on two unsigned chars,
 * and with l in C and r in B the compare had no rule - ccguard's
 * stale-flag branch made the self-hosted compiler evaluate every
 * expression in a different order than its host-built twin. */
int
bmax(a, b)
unsigned char a;
unsigned char b;
{
	return a > b ? a : b;
}

int
main()
{
	int i;

	CHECK(40, bmax(1, 2), 2);
	CHECK(41, bmax(2, 1), 2);
	CHECK(42, bmax(200, 100), 200);
	CHECK(43, bmax(7, 7), 7);

	for (i = 0; i < 3; i++) {
		tsrc[i].a = i + 10;
		tsrc[i].b = 100000L + i;
		tsrc[i].c = (char *)(500 + i);
	}
	ncp = 0;
	fill(3);
	CHECK(20, ncp, 3);
	corder[3] = 0;
	CHECK(21, strcmp(corder, "ABC"), 0);
	for (i = 0; i < 3; i++) {
		if (tdst[i].a != i + 10)
			return 30 + i;
		if (tdst[i].b != 100000L + i)
			return 40 + i;
		if (tdst[i].c != (char *)(500 + i))
			return 50 + i;
	}

	CHECK(1, cmps(src), 63);
	CHECK(2, spans(src), 5);

	CHECK(30, shifts(1), 7);

	on = 0;
	emitf("AB%sCD%dEF");
	ob[on] = 0;
	CHECK(3, on, 6);
	CHECK(4, strcmp(ob, "ABCDEF"), 0);
	return 0;
}
