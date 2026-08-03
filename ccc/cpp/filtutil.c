/*
 * filtutil.c - Shared filter utilities
 */
#include <unistd.h>
#include <stdlib.h>
#include "cpp.h"
#include "lexeme.h"

/* External typedef table */
extern int isTypedef(char *name);

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
	if (token_props[t->type] & TF_TYPE)
		return 1;
	if (t->type == SYM && t->v.name && isTypedef(t->v.name))
		return 1;
	return 0;
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
	p->size = initial;
	p->rd = p->wr = 0;
}

void
pend_grow(struct pendbuf *p)
{
	struct token *newbuf;
	int newmax, count, i;

	/*
	 * A step, not a doubling.  These queues are bounded by how deep
	 * a construct nests and how long one expression is, not by the
	 * size of the input, so the doubling never amortised anything
	 * and overshot by up to half a buffer whenever it fired.
	 */
	newmax = p->size + GROWSTEP;
	newbuf = (struct token *)xalloc(newmax * sizeof(struct token));

	/* Copy elements in order from rd to wr */
	count = 0;
	i = p->rd;
	while (i != p->wr) {
		tokcpy(&newbuf[count++], &p->buf[i]);
		i = (i + 1) % p->size;
	}

	free(p->buf);
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
	a->count = 0;
	a->alloc = initial;
}

void
tarr_push(struct tokarray *a, struct token *t)
{
	if (a->count >= a->alloc) {
		struct token *newbuf;
		int newcap = a->alloc + GROWSTEP;	/* see pend_grow */
		int i;
		newbuf = (struct token *)xalloc(newcap * sizeof(struct token));
		for (i = 0; i < a->count; i++) {
			tokcpy(&newbuf[i], &a->buf[i]);
		}
		free(a->buf);
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
