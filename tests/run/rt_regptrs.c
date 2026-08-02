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

int
main()
{
	int i;

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

	on = 0;
	emitf("AB%sCD%dEF");
	ob[on] = 0;
	CHECK(3, on, 6);
	CHECK(4, strcmp(ob, "ABCDEF"), 0);
	return 0;
}
