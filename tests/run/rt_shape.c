/*
 * Shapes taken from real code that pass2 could not compile.
 *
 * These are not interesting as programs.  Each one is the smallest
 * thing that reproduces a marker found in the tree's own sources, kept
 * because reproducing one of these took longer than writing it: the
 * failing shape is usually a particular combination of storage class,
 * width and register that the obvious test does not reach.  Two
 * attempts at the first one below compiled cleanly before the third
 * matched what lexread.c actually does.
 *
 * Where a case came from is recorded with it, so if it regresses the
 * original is findable.
 */
#include "rt.h"

/*
 * pass1/lexread.c readByte(): a static byte array indexed by a static
 * int that steps.  The array's address goes to HL and the step comes
 * back in HL too, so it added HL to itself.  Stepping a global is
 * costed like a call now, which is what puts it first.
 */
static unsigned char lexBuf[512];
static int lexPos = 0;
static int lexValid = 0;

static unsigned char
readByte()
{
	if (lexPos >= lexValid)
		return 0;
	return lexBuf[lexPos++];
}

/* the same with the step before the subscript, and going down */
static int wpos;
static unsigned char wbuf[8];

static unsigned char
readBack()
{
	return wbuf[--wpos];
}

/* stepping a frame slot instead, which the (iy+d) forms handle in
 * place and which must keep the shorter code it already had */
static unsigned char
localstep(n) short n;
{
	short p;
	unsigned char t;

	p = 0;
	t = 0;
	while (p < n)
		t += wbuf[p++];
	return t;
}

/*
 * libcpm/time.c: the address of a local struct handed to a function.
 * This used to pass the first two bytes of the struct instead.
 */
struct tod { short lo, hi; };

short
sumtod(t) struct tod *t;
{
	return t->lo + t->hi;
}

short
bytod()
{
	struct tod lt;

	lt.lo = 3;
	lt.hi = 4;
	return sumtod(&lt);
}


/*
 * tools/wssize.c byname(): a byte array subscripted by a long.  The
 * sum of a pointer and a long is a pointer, so only the low word can
 * reach the address - but the long operand was emitted at its own
 * width and pass2 has no rule for adding the two together, so nothing
 * came out.  Eight places in the tools did this.
 */
static unsigned char lbuf[32];
static short li;

short
bylong(size) long size;
{
	long pos;
	short n;

	pos = 2;
	n = 0;
	for (li = 0; li < 4 && lbuf[pos + li]; li++)
		n++;
	return n;
}

short
atlong(p) long p;
{
	return lbuf[p];
}

/* a long narrowed into short arithmetic, which must keep the low word */
short
mixlong(a) short a;
{
	long l;

	l = 100000L;			/* 0x186a0 */
	return a + (short)l;
}


/*
 * tools/wslib.c: a pointer register variable subscripted by a
 * variable.  A constant offset folds into an INDEX and never reaches
 * the add, so the only rules for the index register plus something
 * were for a constant and for a symbol - "p[i]" with i worked out had
 * none, and emitted nothing.  Eleven places, counting the ones that
 * then sign-extend what they read.
 */
struct ent { short a, b; };
struct ent etab[4];
char cbuf[8];
short ei;

short
byidx(p, i) register struct ent *p; short i;
{
	return p[i].a;
}

short
byidxb(p, i) register struct ent *p; short i;
{
	return p[i].b;
}

short
bychar(p, i) register char *p; short i;
{
	return p[i] + 1;		/* sign-extended, which is the 7-site form */
}

void
setidx(p, i, v) register struct ent *p; short i, v;
{
	p[i].a = v;
}


/*
 * tools/asm.c and wslib.c: a local array subscripted by a register
 * variable.  The frame slot's address plus the subscript had forms for
 * the subscript in HL and in DE and none for BC, so storing through it
 * emitted nothing.  Six places, all of them clearing a buffer.
 */
short
locidx(n) short n;
{
	char b[8];
	register short i;
	short t;

	for (i = 0; i < 8; i++)
		b[i] = 0;		/* a constant stored through it */
	for (i = 0; i < n; i++)
		b[i] = i + 1;
	t = 0;
	for (i = 0; i < 8; i++)
		t += b[i];
	return t;
}

short
locidxw(n) short n;
{
	short w[4];
	register short i;
	short t;

	for (i = 0; i < 4; i++)
		w[i] = 0;
	w[n] = 100;
	t = 0;
	for (i = 0; i < 4; i++)
		t += w[i];
	return t;
}


/*
 * cpp/macro.c and tools/asz.c: "*p++ = 0".
 *
 * An assignment's left is a location, so the lvalue here is the step
 * itself and not a dereference of it.  Nothing said the location had
 * to be worked out into a register, so the step reported its answer as
 * being nowhere and the store had no address to use.
 *
 * The word case was worse and silent: read as discarded, a step of
 * more than one took the form that does not produce the old pointer,
 * and the store went through p after the step instead of before.
 */
char pbuf[8];
short pwbuf[4];
long plbuf[3];

void
pclear(p, n) char *p; short n;
{
	while (n-- > 0)
		*p++ = 0;
}

void
pfill(p, n, v) char *p; short n; char v;
{
	while (n-- > 0)
		*p++ = v;
}

void
pwfill(p, n) short *p; short n;
{
	while (n-- > 0)
		*p++ = 7;		/* steps by two: the word case */
}

void
plfill(p, n) long *p; short n;
{
	while (n-- > 0)
		*p++ = 9L;		/* steps by four */
}


/*
 * pass1/expr.c: a byte stored through a pointer held in a frame slot.
 * The word form of that store was in the table and the byte one was
 * not.
 */
char sbuf[8];

short
bytestore(v) short v;
{
	char *p;

	p = sbuf;
	p[0] = v;			/* the value arrives in HL */
	*p = v + 1;
	return sbuf[0];
}

/*
 * cpp/lex.c: A compared against a byte register variable.  cp takes b
 * and c directly; there were forms for E and for a constant only.
 */
short
bytecmp(c, d) register char c; char d;
{
	short n;

	n = 0;
	if (d == c) n += 1;
	if (d != c) n += 2;
	return n;
}

/*
 * tools/wsld.c: a pointer register variable stored to a global.  IX
 * has no register node of its own, so a value in it stays a CODE node
 * and the rule asking for a REGVAR stopped matching once it had been
 * reduced.
 */
struct snode { short v; struct snode *next; };
struct snode sn1, sn2;
struct snode *slist;

void
setlist(p) register struct snode *p;
{
	slist = p;
	p->v = 5;
	slist = p->next;
}

/*
 * pass2/rewrite.c: the comma operator.  A comma is its right operand
 * once the left has been emitted, but no rule reduces a bare constant
 * - a constant only becomes a load as part of a parent rule that
 * names it - so a comma whose right was a constant or a frame slot
 * matched nothing and emitted nothing.  The left must still happen:
 * cwit witnesses that it did.
 */
short cwit;

short
commaconst()
{
	short r;

	r = (cwit = 11, 0);		/* right is a constant */
	return r;
}

short
commaslot(v) short v;
{
	short slot;
	short r;

	slot = v;
	r = (cwit = 12, slot);		/* right is a frame slot */
	return r;
}

short
commanest()
{
	short r;

	r = (cwit = 13, (cwit = 14, 115));
	return r;
}

short
commabyte()
{
	char c;

	c = (cwit = 14, 115);		/* byte width */
	return c;
}

short
commafor()
{
	short i, n;

	n = 0;
	for (i = 0; i < 5; i++, n++)	/* comma discarding its right */
		;
	return i + n;
}

short
commaside()
{
	short a;

	a = 8;
	/* left is a step whose effect must survive the collapse */
	return (a++, a);
}

/*
 * pass2/rewrite.c: a long compared against something narrower.  The
 * comparison carries the width of its answer, not of its operands, so
 * reading only the left one meant "s < l" was judged by s; and nobody
 * put the conversion in the tree, so even "l < s" reached the gate
 * with an operand that could not land in HL:DE.  Both sides matter,
 * and so does which conversion: a signed source sign-extends and an
 * unsigned one does not, which is what separates -1 from 65535.
 */
long lbig, lneg;

short
cmpLU(l, u) long l; register unsigned short u;
{
	short n;

	n = 0;
	if (l <  u) n |= 1;
	if (l <= u) n |= 2;
	if (l >  u) n |= 4;
	if (l >= u) n |= 8;
	if (l == u) n |= 16;
	if (l != u) n |= 32;
	return n;
}

short
cmpUL(u, l) register unsigned short u; long l;
{
	short n;

	n = 0;
	if (u <  l) n |= 1;
	if (u <= l) n |= 2;
	if (u >  l) n |= 4;
	if (u >= l) n |= 8;
	return n;
}

short
cmpLS(l, s) long l; register short s;
{
	short n;

	n = 0;
	if (l <  s) n |= 1;
	if (l >  s) n |= 2;
	if (l == s) n |= 4;
	return n;
}

/*
 * pass2/rules.c: stepping a long through a pointer - "(*lp)++", as in
 * tools/wsnm.c's read_reloc.  The step rules had the global and the
 * frame slot at long width and the pointer only at short, so this
 * emitted nothing.  A prefix wanted for its value has to read the long
 * back afterwards: lainc hands back what was there before and consumes
 * the address, so the address is kept across the call.
 */
long lstepv;

short
lbumpst(p) long *p;
{
	(*p)++;			/* value unused */
	return 1;
}

short
lprest(p) long *p;
{
	++(*p);			/* value unused */
	return 2;
}

long
lprev(p) long *p;
{
	return ++(*p);		/* prefix yields the new value */
}

long
lpostv(p) long *p;
{
	return (*p)++;		/* postfix yields the old one */
}

short
ldropst(p) long *p;
{
	(*p)--;
	--(*p);
	return 3;
}

/*
 * pass1/outast.c and pass2/rewrite.c: stepping through a pointer that
 * the allocator put in a register.  Two uses is enough to promote it,
 * which is why one-use functions above did not show this.
 *
 * pass1 dropped the DEREF on DEREF(REGVAR) unless it was an
 * assignment's lvalue, so "(*p)++" became "p++" and stepped the
 * pointer.  pass2 then had its own half: with the DEREF back, the
 * ordinary reduction fetched what p points at and stepped that - and
 * at short width took the fetched value for an address and stepped
 * what *that* pointed at.  Neither left a marker.
 */
long lregv;
short sregv;

short
lregstep(p) long *p;
{
	(*p)++;
	(*p)++;
	return 1;
}

short
lregdrop(p) long *p;
{
	(*p)--;
	(*p)--;
	return 2;
}

short
sregstep(p) short *p;
{
	(*p)++;
	(*p)++;
	return 3;
}

short
sregdrop(p) short *p;
{
	(*p)--;
	(*p)--;
	return 4;
}

/* the pointer itself must still be steppable, which is the rule the
 * lost DEREF was wrongly matching */
short
ptrstep(p) short *p;
{
	p++;
	p++;
	return *p;
}

/*
 * pass2/rules.c: reading through a pointer in BC when the answer is
 * wanted in DE.  D(B):s moved the pointer into HL to read through it
 * and landed there whatever it was asked for, so as the right operand
 * of a comparison it overwrote the left one - and then nothing matched
 * a comparison of HL against HL, which is what left the marker.  The
 * label was right; the rule could not do as it was told.
 *
 * Two pointers, so the second is pushed out of IX into BC: read
 * through IX the field goes straight to DE and nothing collides.
 */
struct dsym {
	unsigned short value;
	struct dsym *next;
};

struct dsym dsa, dsb, dsc;
struct dsym *dsyms;
struct dsym *dothers;

short
dcount(ca) unsigned short ca;
{
	struct dsym *s;
	struct dsym *t;
	short n;

	n = 0;
	for (t = dothers; t; t = t->next)
		n += t->value;
	for (s = dsyms; s; s = s->next) {
		if (s->value > ca)
			n++;
	}
	return n;
}

/* the pointer must survive the read: inc bc then dec bc */
short
dwalk()
{
	struct dsym *s;
	struct dsym *t;
	short n;

	n = 0;
	for (t = dothers; t; t = t->next)
		n += t->value;
	for (s = dsyms; s; s = s->next) {
		if (s->value > 10)
			n++;
		if (s->value > 20)
			n++;
	}
	return n;
}

/*
 * pass2: stepping a byte that lives in B or C.
 *
 * A byte register variable reduces into A to be worked on, and the
 * "k(A):b" rule then stepped it there and called that the answer - but
 * A is a copy, so the variable never changed.  "while (--n)" did not
 * terminate.  The assignment side of this was already guarded, with a
 * comment saying "c = -1 came out as ld a,b and ld a,-1"; the steps
 * were not.
 *
 * dec b and dec c set Z themselves, so a step used as a condition
 * needs nothing after it.
 */
char stepbuf[40];

char
cdown(n) register char n;
{
	char s;

	s = 0;
	while (--n)			/* the loop that hung */
		s += stepbuf[n];
	return s;
}

char
cupto(n) register char n;
{
	register char i;
	char s;

	s = 0;
	i = 0;
	while (++i <= n)		/* two byte register variables */
		s += stepbuf[i];
	return s;
}

char cpleft;			/* what the variable was left holding */

short
cpost(n) register char n;
{
	char a;

	a = n--;			/* postfix yields the old value */
	cpleft = n;
	return a;
}

short
cpre(n) register char n;
{
	char a;

	a = --n;			/* prefix yields the new one */
	cpleft = n;
	return a;
}

/* the user's shape: a step used directly as a condition */
short
cif(n) register char n;
{
	if (--n)
		return 1;
	return 0;
}

/*
 * pass2/rules.c: word bitwise operators with BC on one side.  There
 * was a form for HL against DE and no other, so one of these on a
 * register variable emitted nothing.  Two uses is what puts the
 * variable in a register.
 */
unsigned short bitg;

unsigned short
andreg(a, b) unsigned short a; unsigned short b;
{
	register unsigned short r;

	r = a;
	r = r & b;			/* value in BC, other side in DE */
	r = r & 0xff0f;
	return r;
}

unsigned short
orreg(a, b) unsigned short a; unsigned short b;
{
	register unsigned short r;

	r = a;
	r = r | b;
	r = r | 0x0100;
	return r;
}

unsigned short
xorreg(a, b) unsigned short a; unsigned short b;
{
	register unsigned short r;

	r = a;
	r = r ^ b;
	r = r ^ 0x00f0;
	return r;
}

/* and with the register variable on the right instead */
unsigned short
andrhs(a, b) unsigned short a; unsigned short b;
{
	register unsigned short r;

	r = b;
	bitg = a & r;
	return bitg & r;
}

/*
 * A word in a register sign-extended to a word - nothing to extend,
 * but it still has to come over to HL.  No rule named that, so a
 * pointer difference or a cast on a register variable stopped there.
 */
char sxbuf[16];

short
sxreg(n) short n;
{
	register char *p;
	short d;

	p = sxbuf;
	p = p + n;
	d = (short)(p - sxbuf);
	return d + 5;
}

/*
 * pass2/rules.c: a byte reached through a pointer in a frame slot plus
 * a register index.  The address is the slot sign-extended - nothing
 * to extend, a word already - added to the index, and no rule named a
 * sign extension whose operand was a frame slot.  Both the store and
 * the test of what was stored stopped there.
 */
char sxb[16];

short
locptridx(n) short n;
{
	char *p;
	register short i;
	short t;

	p = sxb;
	for (i = 0; i < n; i++)
		p[i] = 0;		/* store a byte through it */
	t = 0;
	for (i = 0; i < n; i++)
		if (!p[i])		/* and read one back as a condition */
			t++;
	return t;
}

/*
 * A batch of forms the table simply did not have: a byte stored
 * through a pointer in BC with the value in HL, HL minus a frame slot,
 * HL times BC, and unary minus on a byte.
 *
 * The last one is not a rule at all.  "-c" on a char is the negation
 * of an int, not of a byte - a rule that negated the byte and widened
 * afterwards would have answered 251 for -5 and emitted code for it,
 * which is worse than the marker.  So the operator promotes, and what
 * it is applied to promotes with it.
 */
char bcsb[8];
char bcsb2[8];

void
stbcptr(v) short v;
{
	register char *q;
	register char *p;

	/* two pointers, so the second is out of IX and into BC - with
	 * only one the allocator uses IX and the shape never appears */
	q = bcsb2;
	*q = 1;
	q[1] = 2;
	p = bcsb;
	*p = v;
	p[1] = v + 1;
}

short
subslot(a) short a;
{
	short s0, s1, s2, s3, s4, s5, s6, s7, s8, s9;

	s0=1; s1=2; s2=3; s3=4; s4=5; s5=6; s6=7; s7=8; s8=9; s9=a;
	/* something computed in HL, less a frame slot */
	return (s0 + s1 + s2 + s3 + s4 + s5 + s6 + s7 + s8) - s9;
}

short
mulbc(a, n) short a; short n;
{
	register short r;
	short t;

	r = n;
	t = a * r;			/* HL times BC */
	return t + r;			/* and r has to survive it */
}

short
negbyte(c) unsigned char c;
{
	return -c;			/* -5, not 251 */
}

short
notbyte(c) unsigned char c;
{
	return ~c;			/* -6, not 250 */
}

/*
 * pass2/rules.c: a pointer difference against an array declared with
 * no size.  Given a size, pass1 puts a conversion over the symbol and
 * the subtraction arrives as -(B,E); an array of unknown size has size
 * zero, so nothing looks wider than nothing, the conversion is not
 * inserted, and the symbol reaches the rules bare - where there was a
 * form for BC less DE and none for BC less a symbol.
 *
 * cpp.h declares strbuf this way, which is how it was found.
 */
extern char sizeless[];

short
ptrdiff(n) short n;
{
	register char *s;

	s = sizeless;
	s = s + n;
	return (short)(s - sizeless);
}

short
ptrdcmp(n) short n;
{
	register char *s;

	s = sizeless;
	s = s + n;
	return (s - sizeless) > 3;
}

char sizeless[16];

main()
{
	short i;

	lexBuf[0] = 10; lexBuf[1] = 20; lexBuf[2] = 30;
	lexPos = 0;
	lexValid = 3;
	CHECK(1, readByte(), 10);
	CHECK(2, readByte(), 20);
	CHECK(3, readByte(), 30);
	CHECK(4, readByte(), 0);
	CHECK(5, lexPos, 3);

	wbuf[0] = 1; wbuf[1] = 2; wbuf[2] = 4; wbuf[3] = 8;
	wpos = 3;
	CHECK(6, readBack(), 4);
	CHECK(7, wpos, 2);
	CHECK(8, readBack(), 2);
	CHECK(9, wpos, 1);

	CHECK(10, localstep(0), 0);
	CHECK(11, localstep(1), 1);
	CHECK(12, localstep(4), 15);

	CHECK(13, bytod(), 7);

	for (li = 0; li < 32; li++)
		lbuf[li] = li;
	lbuf[5] = 0;
	CHECK(14, bylong(0L), 3);	/* [2],[3],[4] set, [5] clear */
	CHECK(15, atlong(7L), 7);
	CHECK(16, atlong(0L), 0);
	CHECK(17, atlong(31L), 31);
	CHECK(18, mixlong(0), (short)0x86a0);
	CHECK(19, mixlong(1), (short)0x86a1);

	etab[0].a = 10; etab[0].b = 11;
	etab[1].a = 20; etab[1].b = 21;
	etab[2].a = 30; etab[2].b = 31;
	ei = 1;
	CHECK(20, byidx(etab, ei), 20);
	CHECK(21, byidxb(etab, ei), 21);
	ei = 0;
	CHECK(22, byidx(etab, ei), 10);
	ei = 2;
	CHECK(23, byidx(etab, ei), 30);
	CHECK(24, byidxb(etab, ei), 31);

	cbuf[0] = 5; cbuf[1] = -3; cbuf[2] = 100;
	ei = 0;
	CHECK(25, bychar(cbuf, ei), 6);
	ei = 1;
	CHECK(26, bychar(cbuf, ei), -2);
	ei = 2;
	CHECK(27, bychar(cbuf, ei), 101);

	setidx(etab, 1, 99);
	CHECK(28, etab[1].a, 99);
	CHECK(29, etab[0].a, 10);
	CHECK(30, etab[2].a, 30);

	CHECK(31, locidx(0), 0);
	CHECK(32, locidx(1), 1);
	CHECK(33, locidx(3), 6);	/* 1+2+3 */
	CHECK(34, locidx(8), 36);	/* 1..8 */
	CHECK(35, locidxw(0), 100);
	CHECK(36, locidxw(3), 100);

	for (ei = 0; ei < 8; ei++)
		pbuf[ei] = 99;
	pclear(pbuf, 4);
	CHECK(37, pbuf[0], 0);
	CHECK(38, pbuf[3], 0);
	CHECK(39, pbuf[4], 99);
	pfill(pbuf, 3, 5);
	CHECK(40, pbuf[0], 5);
	CHECK(41, pbuf[2], 5);
	CHECK(42, pbuf[3], 0);

	pwfill(pwbuf, 3);
	CHECK(43, pwbuf[0], 7);
	CHECK(44, pwbuf[1], 7);
	CHECK(45, pwbuf[2], 7);
	CHECK(46, pwbuf[3], 0);

	plfill(plbuf, 2);
	CHECK(47, plbuf[0] == 9L, 1);
	CHECK(48, plbuf[1] == 9L, 1);
	CHECK(49, plbuf[2] == 0L, 1);

	sbuf[0] = 0;
	CHECK(50, bytestore(65), 66);
	CHECK(51, sbuf[0], 66);
	CHECK(52, bytestore(0), 1);

	CHECK(53, bytecmp(5, 5), 1);
	CHECK(54, bytecmp(5, 6), 2);
	CHECK(55, bytecmp(-1, -1), 1);
	CHECK(56, bytecmp(0, 0), 1);

	sn1.v = 1; sn1.next = &sn2;
	sn2.v = 2; sn2.next = 0;
	slist = 0;
	setlist(&sn1);
	CHECK(57, slist == &sn2, 1);
	CHECK(58, sn1.v, 5);

	cwit = 0;
	CHECK(59, commaconst(), 0);
	CHECK(60, cwit, 11);		/* the left still happened */
	CHECK(61, commaslot(6), 6);
	CHECK(62, cwit, 12);
	CHECK(63, commanest(), 115);
	CHECK(64, cwit, 14);
	CHECK(65, commabyte(), 115);
	CHECK(66, commafor(), 10);
	CHECK(67, commaside(), 9);
	CHECK(68, cwit, 14);		/* untouched since commabyte */

	CHECK(69, cmpLU(100000L, 5), 4|8|32);
	/*
	 * A negative long against an unsigned short.  long can represent
	 * every unsigned short, so both convert to signed long and -1 is
	 * below 5.  zc3 takes the whole comparison unsigned because one
	 * side is, making -1 read as 4294967295 and answer 44 where gcc
	 * and ccc both answer 35 - so this one case is not asked of it.
	 * Checked by hand against the promotion rules, not just against
	 * the reference: two of three agreeing is not what settles it.
	 */
#ifndef RT_ZC3
	CHECK(70, cmpLU(-1L, 5), 1|2|32);	/* the long stays signed */
#endif
	CHECK(71, cmpLU(65535L, 65535), 2|8|16);/* and 65535 is not -1 */
	CHECK(72, cmpUL(5, 100000L), 1|2);
	/* 12 not 10: sign-extending the unsigned side would make these
	 * equal, and 65535 is above -1, not the same as it.  Not asked of
	 * zc3 - the same unsigned-wins bug as check 70, operands the
	 * other way round. */
#ifndef RT_ZC3
	CHECK(73, cmpUL(65535, -1L), 4|8);
#endif
	CHECK(74, cmpLS(-5L, -1), 1);
	/* 2 not 4: the signed side must sign-extend, or -1 reads as
	 * 65535 and compares equal to the long */
	CHECK(75, cmpLS(65535L, -1), 2);
	CHECK(76, cmpLS(-1L, -1), 4);

	lstepv = 5L;
	CHECK(77, lbumpst(&lstepv), 1);
	CHECK(78, lstepv == 6L, 1);
	CHECK(79, lprest(&lstepv), 2);
	CHECK(80, lstepv == 7L, 1);
	CHECK(81, lprev(&lstepv) == 8L, 1);	/* new value */
	CHECK(82, lpostv(&lstepv) == 8L, 1);	/* old value */
	CHECK(83, lstepv == 9L, 1);
	CHECK(84, ldropst(&lstepv), 3);
	CHECK(85, lstepv == 7L, 1);

	/* carry across the 16-bit halves, which a 32-bit step has to do
	 * and a 16-bit one silently would not */
	lstepv = 65535L;
	lbumpst(&lstepv);
	CHECK(86, lstepv == 65536L, 1);
	lprest(&lstepv);
	CHECK(87, lstepv == 65537L, 1);
	lstepv = 65536L;
	ldropst(&lstepv);
	CHECK(88, lstepv == 65534L, 1);

	/* and through zero, where the high word borrows */
	lstepv = 0L;
	ldropst(&lstepv);
	CHECK(89, lstepv == -2L, 1);
	lbumpst(&lstepv);
	CHECK(90, lstepv == -1L, 1);
	lbumpst(&lstepv);
	CHECK(91, lstepv == 0L, 1);

	lregv = 100000L;
	CHECK(92, lregstep(&lregv), 1);
	CHECK(93, lregv == 100002L, 1);	/* the long, not the pointer */
	CHECK(94, lregdrop(&lregv), 2);
	CHECK(95, lregv == 100000L, 1);

	sregv = 40;
	CHECK(96, sregstep(&sregv), 3);
	CHECK(97, sregv, 42);
	CHECK(98, sregdrop(&sregv), 4);
	CHECK(99, sregv, 40);

	pwbuf[0] = 1; pwbuf[1] = 2; pwbuf[2] = 3;
	CHECK(100, ptrstep(pwbuf), 3);	/* stepping the pointer still works */

	dsa.value = 5;  dsa.next = &dsb;
	dsb.value = 25; dsb.next = &dsc;
	dsc.value = 15; dsc.next = 0;
	dsyms = &dsa;
	dothers = 0;
	CHECK(101, dcount(10), 2);	/* 25 and 15 are above 10 */
	CHECK(102, dcount(20), 1);	/* only 25 */
	CHECK(103, dcount(30), 0);
	/* 5:none  25:both  15:one  -> 3, and the walk must still finish,
	 * which it only does if BC came back from every read */
	CHECK(104, dwalk(), 3);

	for (i = 0; i < 40; i++)
		stepbuf[i] = i;
	/* 5: buf[4]+buf[3]+buf[2]+buf[1] - and it has to stop */
	CHECK(105, cdown(5), 10);
	CHECK(106, cdown(1), 0);	/* --1 is zero, body never runs */
	CHECK(107, cupto(4), 10);	/* buf[1..4] */
	CHECK(108, cpost(7), 7);	/* postfix: the old value */
	CHECK(109, cpleft, 6);		/* and the variable did change */
	CHECK(110, cpre(7), 6);		/* prefix: the new value */
	CHECK(111, cpleft, 6);
	CHECK(112, cif(3), 1);
	CHECK(113, cif(1), 0);		/* --1 is zero */

	/* 0xf0f0 & 0x3333 = 0x3030, & 0xff0f = 0x3000 */
	CHECK(114, andreg(0xf0f0, 0x3333) == 0x3000, 1);
	/* 0xf000 | 0x000f = 0xf00f, | 0x0100 = 0xf10f */
	CHECK(115, orreg(0xf000, 0x000f) == 0xf10f, 1);
	/* 0xff00 ^ 0x0ff0 = 0xf0f0, ^ 0x00f0 = 0xf000 */
	CHECK(116, xorreg(0xff00, 0x0ff0) == 0xf000, 1);
	/* 0xfff0 & 0x0ff0 = 0x0ff0, & again is the same */
	CHECK(117, andrhs(0xfff0, 0x0ff0) == 0x0ff0, 1);
	CHECK(118, bitg == 0x0ff0, 1);

	CHECK(119, sxreg(0), 5);
	CHECK(120, sxreg(7), 12);

	for (i = 0; i < 16; i++)
		sxb[i] = 9;
	CHECK(121, locptridx(6), 6);	/* six cleared, six read back zero */
	CHECK(122, sxb[5], 0);
	CHECK(123, sxb[6], 9);		/* and it stopped where it should */

	bcsb[0] = 0; bcsb[1] = 0; bcsb2[0] = 0; bcsb2[1] = 0;
	stbcptr(65);
	CHECK(124, bcsb[0], 65);
	CHECK(125, bcsb[1], 66);
	CHECK(126, bcsb2[0], 1);
	CHECK(127, bcsb2[1], 2);
	CHECK(128, subslot(6), 39);	/* 45 - 6 */
	CHECK(129, mulbc(7, 6), 48);	/* 42, plus the 6 that survived */
	/*
	 * Unary minus and complement promote, so these are the negation
	 * of an int and not of a byte.  Not asked of zc3, which negates
	 * in the byte and answers 251 and 250 - the same bug this pair
	 * was written for, and gcc and ccc agree against it.
	 */
#ifndef RT_ZC3
	CHECK(130, negbyte(5), -5);	/* promoted, so not 251 */
	CHECK(131, notbyte(5), -6);	/* promoted, so not 250 */
#endif

	CHECK(132, ptrdiff(0), 0);
	CHECK(133, ptrdiff(7), 7);
	CHECK(134, ptrdcmp(7), 1);
	CHECK(135, ptrdcmp(2), 0);

	return 0;
}
