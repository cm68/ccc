/*
 * filtutil.c - Shared filter utilities
 */
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
	return type == INT || type == CHAR || type == SHORT ||
	       type == LONG || type == UNSIGNED || type == SIGNED ||
	       type == VOID || type == FLOAT || type == DOUBLE ||
	       type == STRUCT || type == UNION || type == ENUM ||
	       type == CONST || type == STATIC || type == EXTERN ||
	       type == REGISTER || type == AUTO || type == VOLATILE ||
	       type == TYPEDEF;
}

/*
 * Check if token is a type (keyword or typedef name)
 */
int
is_type_tok(struct token *t)
{
	if (is_type_kw(t->type))
		return 1;
	if (t->type == SYM && t->v.name && isTypedef(t->v.name))
		return 1;
	return 0;
}

/*
 * Pending buffer operations - dynamic growth
 */
void
pend_init(struct pendbuf *p, int initial)
{
	if (initial < 8) initial = 8;
	p->buf = malloc(initial * sizeof(struct token));
	p->size = initial;
	p->rd = p->wr = 0;
}

static void
pend_grow(struct pendbuf *p)
{
	struct token *newbuf;
	int newmax, count, i;

	newmax = p->size * 2;
	newbuf = malloc(newmax * sizeof(struct token));

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
 * Filter entry: check pending, get upstream, handle EOF
 * Returns 1 if caller should return (out is set), 0 to continue
 */
int
filt_entry(struct pendbuf *pb, struct token *out,
           void (*up)(struct token *), struct token *t)
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
	toksynthnam(&tmp, LABEL, strdup(buf));
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
	toksynthnam(&tmp, SYM, strdup(buf));
	pend_push(p, &tmp);
}

/*
 * Dynamic token array - linear growable array
 */
void
tarr_init(struct tokarray *a, int initial)
{
	if (initial < 8) initial = 8;
	a->buf = malloc(initial * sizeof(struct token));
	a->count = 0;
	a->alloc = initial;
}

void
tarr_push(struct tokarray *a, struct token *t)
{
	if (a->count >= a->alloc) {
		struct token *newbuf;
		int newcap = a->alloc * 2;
		int i;
		newbuf = malloc(newcap * sizeof(struct token));
		for (i = 0; i < a->count; i++)
			tokcpy(&newbuf[i], &a->buf[i]);
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
