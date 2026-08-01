/*
 * cpp's declaration-lex window, small: intern entries created while
 * token records stream through growing queues, ids minted at
 * emission.  On the Z80 the full cpp corrupted freshly made intern
 * entries at exactly this point - each victim's id went to garbage
 * the moment its own declaration was lexed - and the garbage came
 * in 6-byte strides, some pointer-shaped, some small ints.  This
 * models that traffic with the same structures at the same sizes.
 */
#include "rt.h"

struct ient {
	unsigned short id;
	struct ient *next;
	char name[1];
};
#define HASHN 31
struct ient *pool[HASHN];
unsigned short nextid = 1;

char arena[2048];
char *ap = arena;

char *
grab(int n)
{
	char *p = ap;
	int i;

	ap += n;
	for (i = 0; i < n; i++)
		p[i] = 0;
	return p;
}

char *
intern(char *s)
{
	unsigned short h = 0;
	char *p;
	struct ient *e;

	for (p = s; *p; p++)
		h = h * 31 + (unsigned char)*p;
	h %= HASHN;
	for (e = pool[h]; e; e = e->next)
		if (strcmp(e->name, s) == 0)
			return e->name;
	e = (struct ient *)grab(sizeof(*e) + strlen(s));
	strcpy(e->name, s);
	e->next = pool[h];
	pool[h] = e;
	return e->name;
}

unsigned short
idOf(char *s)
{
	struct ient *e;
	char *c;

	c = intern(s);
	e = (struct ient *)(c - ((char *)&((struct ient *)0)->name - (char *)0));
	if (e->id == 0)
		e->id = nextid++;
	return e->id;
}

/* the streaming side: cpp's exact token record and queues */
struct tok {
	unsigned char type;
	int lineno;
	char *filename;
	union {
		long numeric;
		char *str;
		unsigned short id;
		unsigned char b[4];
	} v;
};

struct stkent {
	unsigned char a, b, c;
	int d, e;
	struct tok *f;
	int g;
};

struct tok pend[8];
int pw;

void
tkcp(struct tok *d, struct tok *s)
{
	d->type = s->type;
	d->lineno = s->lineno;
	d->filename = s->filename;
	d->v.numeric = s->v.numeric;
	d->v.str = s->v.str;
}

void
stcp(struct stkent *d, struct stkent *s)
{
	d->a = s->a; d->b = s->b; d->c = s->c;
	d->d = s->d; d->e = s->e; d->f = s->f; d->g = s->g;
}

int
poolbad(void)
{
	int i, n;
	struct ient *e;

	n = 0;
	for (i = 0; i < HASHN; i++)
		for (e = pool[i]; e; e = e->next)
			if (e->id >= nextid)
				n++;
	return n;
}

char nmbuf[4];

int
main()
{
	int i, j;
	struct tok t;
	struct stkent st;
	struct tok *tq;
	struct stkent *sq;
	int tqa, sqa, tqc, sqc;
	unsigned short want;

	tqa = 8; tq = (struct tok *)malloc(tqa * sizeof(struct tok));
	sqa = 4; sq = (struct stkent *)malloc(sqa * sizeof(struct stkent));
	tqc = sqc = 0;

	for (i = 0; i < 60; i++) {
		/* a fresh name arrives - like a declaration's identifier */
		nmbuf[0] = 'a' + i % 26;
		nmbuf[1] = 'a' + (i / 26) % 26;
		nmbuf[2] = 0;
		t.type = 20;
		t.lineno = 100 + i;
		t.filename = "f.h";
		t.v.str = intern(nmbuf);

		/* it queues, queues grow, stack records churn */
		if (tqc >= tqa) {
			struct tok *nb = (struct tok *)malloc(tqa * 2 * sizeof(struct tok));
			for (j = 0; j < tqc; j++)
				tkcp(&nb[j], &tq[j]);
			free((char *)tq);
			tq = nb; tqa *= 2;
		}
		tkcp(&tq[tqc], &t);
		tqc++;
		st.a = i; st.d = 200 + i; st.f = &tq[tqc - 1]; st.g = i * 6;
		if (sqc >= sqa) {
			struct stkent *nb = (struct stkent *)malloc(sqa * 2 * sizeof(struct stkent));
			for (j = 0; j < sqc; j++)
				stcp(&nb[j], &sq[j]);
			free((char *)sq);
			sq = nb; sqa *= 2;
		}
		stcp(&sq[sqc], &st);
		sqc++;
		tkcp(&pend[pw & 7], &t);
		pw++;

		/* and gets emitted: id minted */
		idOf(tq[tqc - 1].v.str);

		/* pool must stay sane after every step */
		if (poolbad())
			return 10 + (i > 9 ? 9 : i);
	}

	/* ids are 1..60 in arrival order */
	nmbuf[0] = 'a'; nmbuf[1] = 'a'; nmbuf[2] = 0;
	want = 1;
	if (idOf(nmbuf) != want)
		return 2;
	if (nextid != 61)
		return 3;
	return 0;
}
