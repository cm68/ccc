/*
 * filtutil.c - Shared filter utilities
 */
#include <unistd.h>
#include <stdlib.h>
#include "cpp.h"
#include "lexeme.h"

#ifdef DEBUG
/*
 * Live bytes across every filter buffer, and the high-water mark.
 * poolstats prints the peak: this is the number that has to fit in
 * the Z80's gap, and the one to watch when a filter's buffers are
 * resized.
 */
long fbufB, fbufPk;

/* every buffer ever inited, so poolstats can dump each one's
 * capacity; capacities never shrink, so alloc IS the high water.
 * (NB: the first wording of this comment tripped a latent cpp bug:
 * inside a false #ifdef, an odd count of hyphens made the lexer
 * hit EOF early - repro preserved in the session scratchpad) */
#define NREG 40
static void *bufreg[NREG];
static char bufkind[NREG];
static int nbufreg;

void
bufdump(void)
{
	int i;

	for (i = 0; i < nbufreg; i++) {
		if (bufkind[i] == 't') {
			struct tokarray *a = (struct tokarray *)bufreg[i];
			fdprintf(2, "  tarr %d alloc=%d\n", i, a->alloc);
		} else {
			struct pendbuf *p = (struct pendbuf *)bufreg[i];
			fdprintf(2, "  pend %d size=%d\n", i, p->size);
		}
	}
}

static void
bufregister(void *p, char kind)
{
	if (nbufreg < NREG) {
		bufreg[nbufreg] = p;
		bufkind[nbufreg] = kind;
		nbufreg++;
	}
}

static void
bufacct(long d)
{
	fbufB += d;
	if (fbufB > fbufPk)
		fbufPk = fbufB;
}
#define BUFACCT(d) bufacct(d)
#else
#define BUFACCT(d)
#endif

/*
 * Check if token type is a type keyword
 */
int
is_type_kw(unsigned char type)
{
	return token_props[type] & TF_TYPE;
}

/*
 * Check if token is a type (keyword or typedef name)
 */
int
is_type_tok(struct token *t)
{
	/* filttdef dissolved every typedef name upstream, so a type
	 * is recognisable from its leading keyword alone */
	return (token_props[t->type] & TF_TYPE) != 0;
}

/*
 * Queue t and emit the oldest pending token - the standard way for a
 * filter to pass the current token through its output queue.
 */
void
pend_thru(struct pendbuf *p, struct token *t, struct token *out)
{
	pend_push(p, t);
	pend_pop(p, out);
}

/*
 * First-call init / later-call reset for the static buffers every
 * filter keeps.
 */
void
tarr_setup(struct tokarray *a, int initial)
{
	if (!a->buf)
		tarr_init(a, initial);
	else
		tarr_reset(a);
}

void
pend_setup(struct pendbuf *p, int initial)
{
	if (!p->buf)
		pend_init(p, initial);
	else
		p->rd = p->wr = 0;
}

/*
 * First-call init / later-call reset for a filter stack.
 */
void
fstack_setup(struct filter_stack *s, int initial, int elemsize)
{
	if (!s->buf)
		fstack_init(s, initial, elemsize);
	else
		s->sp = 0;
}

/*
 * Is the last buffered token a struct/union keyword?  (The SYM that
 * follows is then a tag, part of the type, not a name.)
 */
int
tag_pending(struct tokarray *a)
{
	unsigned char last;

	if (a->count == 0)
		return 0;
	last = a->buf[a->count - 1].type;
	return last == STRUCT || last == UNION;
}

/*
 * Track BEGIN/END nesting depth.
 */
void
tok_depth(struct token *t, unsigned char *depth)
{
	if (t->type == BEGIN)
		(*depth)++;
	else if (t->type == END)
		(*depth)--;
}

/*
 * Generic filter stack operations
 */
void
fstack_init(struct filter_stack *s, int initial, int elemsize)
{
	if (initial < 4) initial = 4;
	s->buf = xalloc(initial * elemsize);
	BUFACCT(initial * elemsize);
	s->sp = 0;
	s->alloc = initial;
	s->elemsize = elemsize;
}

void
fstack_push(struct filter_stack *s, void *data)
{
	if (s->sp >= s->alloc) {
		int newcap = s->alloc + GROWSTEP;
		char *nb = realloc(s->buf, newcap * s->elemsize);
		BUFACCT(GROWSTEP * s->elemsize);

		if (!nb)			/* see xalloc in util.c */
			xnomem();
		s->buf = nb;
		s->alloc = newcap;
	}
	memcpy((char *)s->buf + (s->sp++ * s->elemsize), data, s->elemsize);
}

void
fstack_pop(struct filter_stack *s, void *out)
{
	if (s->sp > 0) {
		s->sp--;
		if (out)
			memcpy(out, (char *)s->buf + (s->sp * s->elemsize), s->elemsize);
	}
}

void *
fstack_top(struct filter_stack *s)
{
	return s->sp > 0 ? (char *)s->buf + ((s->sp - 1) * s->elemsize) : NULL;
}

/*
 * Pending buffer operations - dynamic growth
 */
void
pend_init(struct pendbuf *p, int initial)
{
	if (initial < 8) initial = 8;
	p->buf = (struct token *)xalloc(initial * sizeof(struct token));
	BUFACCT(initial * sizeof(struct token));
#ifdef DEBUG
	bufregister(p, 'p');
#endif
	p->size = initial;
	p->rd = p->wr = 0;
}

/* reverse buf[lo..hi] - the rotation helper */
static void
tokrev(struct token *buf, int lo, int hi)
{
	struct token tmp;

	while (lo < hi) {
		tokcpy(&tmp, &buf[lo]);
		tokcpy(&buf[lo], &buf[hi]);
		tokcpy(&buf[hi], &tmp);
		lo++;
		hi--;
	}
}

void
pend_grow(struct pendbuf *p)
{
	struct token *newbuf;
	int newmax, count;

	/*
	 * A step, not a doubling.  These queues are bounded by how deep
	 * a construct nests and how long one expression is, not by the
	 * size of the input, so the doubling never amortised anything
	 * and overshot by up to half a buffer whenever it fired.
	 *
	 * Rotate the ring flat first, then realloc.  The old
	 * alloc-copy-free left the previous buffer dead in the free
	 * list at every step - too small for the next grow, never
	 * reusable - and briefly held both buffers live.  On the 64K
	 * machine that stair of fragments was the difference between
	 * a big header closure fitting and not.  A grow only fires
	 * full, so every cell but the write slot is live; rotating
	 * left by rd is three reversals in place.
	 */
	count = p->size - 1;
	if (p->rd != 0) {
		tokrev(p->buf, 0, p->rd - 1);
		tokrev(p->buf, p->rd, p->size - 1);
		tokrev(p->buf, 0, p->size - 1);
	}
	newmax = p->size + GROWSTEP;
	BUFACCT(GROWSTEP * sizeof(struct token));
	newbuf = (struct token *)realloc(p->buf, newmax * sizeof(struct token));
	if (!newbuf)
		xnomem();
	p->buf = newbuf;
	p->size = newmax;
	p->rd = 0;
	p->wr = count;
}

void
pend_push(struct pendbuf *p, struct token *t)
{
	int next = (p->wr + 1) % p->size;
	if (next == p->rd)
		pend_grow(p);
	tokcpy(&p->buf[p->wr], t);
	p->wr = (p->wr + 1) % p->size;
}

int
pend_has(struct pendbuf *p)
{
	return p->rd != p->wr;
}

void
pend_pop(struct pendbuf *p, struct token *out)
{
	tokcpy(out, &p->buf[p->rd]);
	p->rd = (p->rd + 1) % p->size;
}

void
pend_tok(struct pendbuf *p, unsigned char type)
{
	struct token tmp;
	toksynth(&tmp, type);
	pend_push(p, &tmp);
}

void
pend_tok_at(struct pendbuf *p, unsigned char type, struct token *ref)
{
	struct token tmp;
	tmp.type = type;
	tmp.v.numeric = 0;
	tmp.lineno = ref->lineno;
	tmp.filename = ref->filename;  /* Already interned */
	pend_push(p, &tmp);
}

/*
 * Push array of tokens to pending buffer
 */
void
pend_buf(struct pendbuf *p, struct token *buf, int len)
{
	int i;
	for (i = 0; i < len; i++)
		pend_push(p, &buf[i]);
}

/*
 * Push a 0-terminated sequence of synthetic token types.
 * (E_O_F is 0 and never appears in a sequence, so it doubles as terminator.)
 */
void
pend_seq(struct pendbuf *p, unsigned char *seq)
{
	while (*seq)
		pend_tok(p, *seq++);
}

/*
 * Filter entry: check pending, get upstream, handle EOF
 * Returns 1 if caller should return (out is set), 0 to continue
 */
int
filt_entry(struct pendbuf *pb, struct token *out,
           void (*up)(), struct token *t)
{
	if (pend_has(pb)) {
		pend_pop(pb, out);
		return 1;
	}
	up(t);
	if (t->type == 0) {
		tokcpy(out, t);
		return 1;
	}
	return 0;
}

/*
 * Emit synthetic label: __XnS:
 */
void
emit_label(struct pendbuf *p, char pfx, int num, char sfx)
{
	struct token tmp;
	char buf[16];
	fmtstr(buf, "__%c%d%c", pfx, num, sfx);
	toksynthnam(&tmp, LABEL, intern(buf));
	pend_push(p, &tmp);
	pend_tok(p, SEMI);
}

/*
 * Emit goto synthetic label: goto __XnS
 */
void
emit_goto(struct pendbuf *p, char pfx, int num, char sfx)
{
	struct token tmp;
	char buf[16];
	pend_tok(p, GOTO);
	fmtstr(buf, "__%c%d%c", pfx, num, sfx);
	toksynthnam(&tmp, SYM, intern(buf));
	pend_push(p, &tmp);
}

/*
 * Dynamic token array - linear growable array
 */
void
tarr_init(struct tokarray *a, int initial)
{
	if (initial < 8) initial = 8;
	a->buf = (struct token *)xalloc(initial * sizeof(struct token));
	BUFACCT(initial * sizeof(struct token));
#ifdef DEBUG
	bufregister(a, 't');
#endif
	a->count = 0;
	a->alloc = initial;
}

void
tarr_push(struct tokarray *a, struct token *t)
{
	if (a->count >= a->alloc) {
		struct token *newbuf;
		int newcap = a->alloc + GROWSTEP;	/* see pend_grow */

		BUFACCT(GROWSTEP * sizeof(struct token));
		newbuf = (struct token *)realloc(a->buf,
		    newcap * sizeof(struct token));
		if (!newbuf)
			xnomem();
		a->buf = newbuf;
		a->alloc = newcap;
	}
	tokcpy(&a->buf[a->count++], t);
}

void
tarr_reset(struct tokarray *a)
{
	a->count = 0;
}
