/*
 * norm.c - unified statement normalizer
 *
 * One recursive walker replacing the filtdecl -> filtbrace -> filtctrl
 * tail of the filter pipeline.  It pulls tokens from upstream (filtknr)
 * and pushes the normalized stream straight to emit - the C call stack
 * carries what the three filters kept in hand-rolled continuations:
 * context stacks, saved tokens, redispatch states, and the malloc'd
 * copy of an outer for's increment.
 *
 * The transformations are unchanged:
 *   - local declaration initializers split into assignments (statics
 *     and arrays keep theirs inline)
 *   - unbraced control-structure bodies get synthetic braces
 *   - loops lower to labels and gotos, switch gains its break label,
 *     break/continue become gotos to the innermost matching target
 *
 * Output-byte fidelity: .x line markers derive from per-token line
 * stamps, and synthesized tokens stamp the lexer's current position -
 * so every synthesis here happens at the same stream offset as in the
 * filters it replaces (a synthetic { is emitted only after the body's
 * first token has been pulled, a loop header only after the condition's
 * closing paren, a deferred if-} only after the else-check token).
 * Buffering boundaries match for the same reason: a for's three
 * clauses are collected fully and emitted at the final paren, exactly
 * as filtctrl did.
 */
#include "cpp.h"
#include "lexeme.h"

/* cpp.c: the lexer wrapper the source layer draws from */
extern void lex_get(struct token *);

/* the cooked-token source: lexer + enum lowering + typedef expansion */
static void srcget(struct token *);

/* one-token pushback: the else-check and statement-end lookaheads */
static struct token backtok;
static unsigned char haveback;

static int next_label;

/*
 * break/continue targets of the innermost enclosing construct.
 * pfx 0 = no target (the keyword passes through raw).  Loops set
 * both; switch sets only the break target, so a continue inside a
 * switch still reaches the enclosing loop.  Nesting saves and
 * restores these in the recursion's locals.
 */
static char brkpfx;
static int brknum;
static char cntpfx;
static int cntnum;
static char cntsfx;

static unsigned char bdepth;	/* statement-block brace depth */
static unsigned char inagg;	/* file scope: struct/union/enum head seen */

/* control-clause buffers; only a for's increment outlives a body,
 * and that one lives in do_for's frame */
static struct tokarray cond_a;
static struct tokarray init_a;

/* declaration buffers (a declaration never nests inside another) */
static struct tokarray spec_a;
static struct tokarray dini_a;

#define NAME_MAX 16
struct dname {
	char *name;
	char star_count;
};
static struct dname names[NAME_MAX];
static unsigned char name_cnt;
static unsigned char cur_stars;

#define ASSIGN_MAX 16
struct dinit {
	char *name;
	struct token *init;
	int init_len;
};
static struct dinit assigns[ASSIGN_MAX];
static unsigned char assign_cnt;

static void stmt(struct token *t);
static void exprstmt(struct token *t);
static int decl(struct token *t);
static void aggprime(void);
static void mpush(char *tag, unsigned char isu);
static void mtok(struct token *t);
static void arrstart(char *name, unsigned char stars);
static void arrtok(struct token *t);
static void arrdone(void);
static void dosizeof(struct token *t);
static void kscan(struct token *t);

/*
 * Token I/O
 */
static struct token szq[12];
static unsigned char szqr, szqw;
static unsigned char insz;

static void
pull(struct token *t)
{
	if (haveback) {
		haveback = 0;
		tokcpy(t, &backtok);
		return;
	}
	if (szqr < szqw) {
		/* an unfoldable sizeof replaying: no re-interception */
		tokcpy(t, &szq[szqr++]);
		if (szqr == szqw)
			szqr = szqw = 0;
		return;
	}
	srcget(t);
	if (t->type == SIZEOF_KW && !insz)
		dosizeof(t);
}

static void
pushb(struct token *t)
{
	tokcpy(&backtok, t);
	haveback = 1;
}

static void
out(struct token *t)
{
	emitStructTok(t);
}

/* synthesize at the lexer's current position - stamp parity with
 * the old filters requires calling this at matching stream offsets */
static void
outt(unsigned char type)
{
	struct token t;

	toksynth(&t, type);
	out(&t);
}

static void
outat(unsigned char type, struct token *ref)
{
	struct token t;

	t.type = type;
	t.v.numeric = 0;
	t.lineno = ref->lineno;
	t.filename = ref->filename;
	out(&t);
}

static void
outarr(struct tokarray *a)
{
	int i;

	for (i = 0; i < a->count; i++)
		out(&a->buf[i]);
}

/* __XnS: - label plus the semicolon that makes it a statement */
static void
outlab(char pfx, int num, char sfx)
{
	struct token t;
	char buf[16];

	fmtstr(buf, "__%c%d%c", pfx, num, sfx);
	toksynthnam(&t, LABEL, intern(buf));
	out(&t);
	outt(SEMI);
}

static void
outgoto(char pfx, int num, char sfx)
{
	struct token t;
	char buf[16];

	outt(GOTO);
	fmtstr(buf, "__%c%d%c", pfx, num, sfx);
	toksynthnam(&t, SYM, intern(buf));
	out(&t);
}

/*
 * Declaration splitting (from filtdecl)
 */
static int
specs_static(void)
{
	register struct token *tp = spec_a.buf;
	int n = spec_a.count;

	while (n--) {
		if (tp->type == STATIC)
			return 1;
		tp++;
	}
	return 0;
}

static struct dinit *
init_for(char *name)
{
	register struct dinit *ap = assigns;
	unsigned char n = assign_cnt + 1;

	while (--n) {
		if (ap->name == name)
			return ap;
		ap++;
	}
	return 0;
}

/* type SYM [, SYM]* ; - initializers stay inline only for statics */
static void
emit_decl(void)
{
	register struct dname *np;
	unsigned char n;
	struct dinit *ap;
	int j;
	int keep = specs_static();
	struct token tmp;
	struct token *ref = &spec_a.buf[0];

	outarr(&spec_a);

	np = names;
	n = name_cnt + 1;
	while (--n) {
		for (j = np->star_count; j > 0; j--)
			outat(STAR, ref);
		tmp.type = SYM;
		tmp.v.name = np->name;
		tmp.lineno = ref->lineno;
		tmp.filename = ref->filename;
		out(&tmp);
		if (keep && (ap = init_for(np->name))) {
			outat(ASSIGN, ref);
			for (j = 0; j < ap->init_len; j++)
				out(&ap->init[j]);
		}
		if (n > 1)
			outat(COMMA, ref);
		np++;
	}
	outat(SEMI, ref);
}

/* name = init ; for each captured initializer */
static void
emit_assigns(void)
{
	register struct dinit *ap;
	unsigned char n;
	int j;
	struct token tmp;
	struct token *ref;

	ap = assigns;
	n = assign_cnt + 1;
	while (--n) {
		ref = ap->init_len > 0 ? &ap->init[0] : &spec_a.buf[0];
		tmp.type = SYM;
		tmp.v.name = ap->name;
		tmp.lineno = ref->lineno;
		tmp.filename = ref->filename;
		out(&tmp);
		outat(ASSIGN, ref);
		for (j = 0; j < ap->init_len; j++)
			out(&ap->init[j]);
		outat(SEMI, ref);
		free(ap->init);
		ap++;
	}
	assign_cnt = 0;
}

static void
drop_assigns(void)
{
	register struct dinit *ap = assigns;
	unsigned char n = assign_cnt + 1;

	while (--n)
		free((ap++)->init);
	assign_cnt = 0;
}

static void
save_init(char *name)
{
	if (assign_cnt < ASSIGN_MAX && dini_a.count > 0) {
		register struct token *d;
		struct token *s;
		struct dinit *ap;
		int n;

		n = dini_a.count;
		ap = &assigns[assign_cnt++];
		ap->name = name;
		d = ap->init = (struct token *)xalloc(n * sizeof(struct token));
		s = dini_a.buf;
		while (n--)
			tokcpy(d++, s++);
		ap->init_len = dini_a.count;
	}
	tarr_reset(&dini_a);
}

static void
save_name(char *name)
{
	if (name_cnt < NAME_MAX) {
		register struct dname *np = &names[name_cnt++];

		np->name = name;
		np->star_count = cur_stars;
	}
	cur_stars = 0;
}


/*
 * Sizes (stage 2 of the c0 migration): registries fed by the walks
 * this file already does, and sizeof answered on the spot.
 *
 * Aggregate layouts are packed and byte-aligned, arrays multiply,
 * unions take the max - pass1's typesize arithmetic, computed here
 * so a sizeof folds to a number before pass1 exists.  Anything the
 * registries cannot price goes downstream unfolded, where pass1
 * still answers it; the tree's .s output is byte-identical either
 * way, which is the gate this rides.
 *
 * By the time the walker sees a token, typedefs are dissolved: a
 * type is its keywords, a struct tag, or a name in the registry.
 */
struct streg {
	char *tag;			/* interned */
	unsigned short size;
	struct streg *next;
};
static struct streg *stags;

struct vreg {
	char *name;			/* interned */
	unsigned short total, elem, deref;
	unsigned char depth;
	struct vreg *next;
};
static struct vreg *vregs, *vfree;
static unsigned char scopedep;

static unsigned short
stfind(char *tag)
{
	struct streg *s;

	for (s = stags; s; s = s->next)
		if (s->tag == tag)
			return s->size;
	return 0;
}

static void
stadd(char *tag, unsigned short size)
{
	struct streg *s;

	if (!tag || !size)
		return;
	s = (struct streg *)permalloc(sizeof(*s));
	s->tag = tag;
	s->size = size;
	s->next = stags;
	stags = s;
}

static void
vadd(char *name, unsigned short total, unsigned short elem,
    unsigned short deref)
{
	struct vreg *v;

	if (!name || !total)
		return;
	if (vfree) {
		v = vfree;
		vfree = v->next;
	} else
		v = (struct vreg *)permalloc(sizeof(*v));
	v->name = name;
	v->total = total;
	v->elem = elem;
	v->deref = deref;
	v->depth = scopedep;
	v->next = vregs;
	vregs = v;
}

static struct vreg *
vfind(char *name)
{
	struct vreg *v;

	for (v = vregs; v; v = v->next)
		if (v->name == name)
			return v;
	return 0;
}

static void
vpop(void)
{
	struct vreg *v;

	while (vregs && vregs->depth > scopedep) {
		v = vregs;
		vregs = v->next;
		v->next = vfree;
		vfree = v;
	}
}

/* one keyword's contribution to a basic type's size */
static unsigned short
kwsz(unsigned char c, unsigned short base)
{
	switch (c) {
	case CHAR:	return 1;
	case SHORT:	return 2;
	case INT:	return (base == 1 || base == 4) ? base : 2;
	case LONG:	return 4;
	case UNSIGNED:	return base ? base : 2;
	}
	return base;
}

static int
szkw(unsigned char c)
{
	return c == CHAR || c == SHORT || c == INT || c == LONG ||
	    c == UNSIGNED;
}

/*
 * The base size the current spec_a describes: keywords, or a
 * struct/union tag in the registry.  Zero = not priceable (an enum,
 * an unknown tag, void).
 */
static unsigned short
specbase(void)
{
	unsigned short base = 0;
	int i;

	for (i = 0; i < spec_a.count; i++) {
		unsigned char c = spec_a.buf[i].type;

		if (c == STRUCT || c == UNION) {
			if (i + 1 < spec_a.count &&
			    spec_a.buf[i+1].type == SYM)
				return stfind(spec_a.buf[i+1].v.name);
			return 0;
		}
		if (c == ENUM)
			return 0;
		if (szkw(c))
			base = kwsz(c, base);
	}
	return base;
}

/* the scalar/pointer declarators finish_decl is about to emit */
static void
sizedecl(void)
{
	unsigned short base = specbase();
	unsigned char i;

	for (i = 0; i < name_cnt; i++) {
		unsigned char st = names[i].star_count;

		if (st)
			vadd(names[i].name, 2, 0,
			    st > 1 ? 2 : base);
		else if (base)
			vadd(names[i].name, base, 0, 0);
	}
}

/*
 * An array declarator: decl() streams the brackets, this collects
 * the verdict.  Multiplied counts, one bracket group for an
 * indexable element size, anything unpriceable drops the entry.
 */
static char *arr_name;
static unsigned char arr_stars;
static unsigned short arr_base;
static unsigned short arr_cnt;
static unsigned char arr_nbrk;
static unsigned char arr_bad;

static void
arrstart(char *name, unsigned char stars)
{
	arr_name = name;
	arr_stars = stars;
	arr_base = specbase();
	arr_cnt = 0;
	arr_nbrk = 0;
	arr_bad = 0;
}

static void
arrtok(struct token *t)
{
	if (!arr_name)
		return;
	if (t->type == NUMBER || t->type == INUMBER) {
		unsigned short v = (unsigned short)t->v.numeric;

		arr_cnt = arr_cnt ? arr_cnt * v : v;
	} else if (t->type == RBRACK)
		arr_nbrk++;
	else if (t->type != LBRACK)
		arr_bad = 1;	/* an expression: not priced */
}

static void
arrdone(void)
{
	unsigned short el;

	if (arr_name && !arr_bad && arr_cnt) {
		el = arr_stars ? 2 : arr_base;
		if (el)
			vadd(arr_name, el * arr_cnt,
			    arr_nbrk == 1 ? el : 0, 0);
	}
	arr_name = 0;
}

/*
 * Member pricing, fed a token at a time from aggrpass.  A small
 * frame per nested body; the same declarator walk as everywhere,
 * shrunk to what members can be.
 */
struct mframe {
	char *tag;
	unsigned short off;
	unsigned short base;
	unsigned short arr;
	unsigned char isunion;
	unsigned char ptr;
	unsigned char nbrk;
	unsigned char pd;
	unsigned char inbrk;
	unsigned char bad;
};
#define MAXMFR 4
static struct mframe mfr[MAXMFR];
static unsigned char mfeed;	/* aggrpass feeds the pricer */
static char mtop = -1;
static unsigned char msawtag;
static char *mtag;
static unsigned char mkind;

static void
mmember(struct mframe *m)
{
	unsigned short sz;

	sz = m->ptr ? 2 : m->base;
	if (m->arr)
		sz *= m->arr;
	if (!sz)
		m->bad = 1;
	else if (m->isunion) {
		if (sz > m->off)
			m->off = sz;
	} else
		m->off += sz;
	m->ptr = 0;
	m->arr = 0;
	m->nbrk = 0;
}

static void
mpush(char *tag, unsigned char isu)
{
	struct mframe *m;

	if (mtop + 1 >= MAXMFR) {
		mfr[MAXMFR - 1].bad = 1;
		return;
	}
	m = &mfr[(int)++mtop];
	m->tag = tag;
	m->off = 0;
	m->base = 0;
	m->arr = 0;
	m->isunion = isu;
	m->ptr = 0;
	m->nbrk = 0;
	m->pd = 0;
	m->inbrk = 0;
	m->bad = 0;
}

static void
mpop(void)
{
	struct mframe *m = &mfr[(int)mtop];

	if (!m->bad)
		stadd(m->tag, m->off);
	mtop--;
	if (mtop >= 0) {
		/* "} name;" continues as the enclosing member */
		if (m->bad)
			mfr[(int)mtop].bad = 1;
		mfr[(int)mtop].base = m->off;
	}
}

static void
mtok(struct token *t)
{
	struct mframe *m;

	if (t->type == NEWLINE || t->type == LINENO)
		return;
	if (msawtag) {
		msawtag = 0;
		if (t->type == SYM) {
			mtag = t->v.name;
			return;
		}
		mtag = 0;
		/* anonymous body: the kind survives to the BEGIN */
	}
	if (t->type == STRUCT || t->type == UNION) {
		msawtag = 1;
		mkind = (t->type == UNION) ? 2 : 1;
		return;
	}
	if (mtop < 0)
		return;
	m = &mfr[(int)mtop];
	if (mkind && t->type == BEGIN) {
		mpush(mtag, mkind == 2);
		mtag = 0;
		mkind = 0;
		return;
	}
	if (mtag) {
		/* "struct x" by reference; a pointer member never
		 * needs the base, mmember flags the ones that do */
		m->base = stfind(mtag);
		mtag = 0;
		mkind = 0;
	}
	if (m->inbrk) {
		if (t->type == NUMBER || t->type == INUMBER)
			m->arr = m->arr ? m->arr *
			    (unsigned short)t->v.numeric
			    : (unsigned short)t->v.numeric;
		else if (t->type == RBRACK) {
			if (!m->arr)
				m->bad = 1;
			m->nbrk++;
			m->inbrk = 0;
		} else
			m->bad = 1;
		return;
	}
	switch (t->type) {
	case STAR:	if (!m->pd) m->ptr++; break;
	case LPAR:	m->pd++; break;
	case RPAR:	if (m->pd) m->pd--; break;
	case LBRACK:	if (!m->pd) m->inbrk = 1; break;
	case COMMA:	if (!m->pd) mmember(m); break;
	case SEMI:	mmember(m);
			m->base = 0;
			break;
	case BEGIN:	mpush(0, 0); break;	/* anonymous body */
	case END:	mpop(); break;
	default:
		if (szkw(t->type))
			m->base = kwsz(t->type, m->base);
		break;
	}
}

/*
 * sizeof, answered from the registries.  On entry the SIZEOF_KW has
 * been pulled; the walk consumes the operand.  What folds becomes
 * an INUMBER - typed int like the construct it replaces, not by
 * magnitude - and what does not is replayed for pass1 through a
 * small queue ahead of the source.
 */
static struct token szsv[10];
static char szn;
static struct token szcur;

static int
szstep(void)
{
	if (szn >= 10)
		return 1;
	tokcpy(&szsv[(int)szn], &szcur);
	szn++;
	pull(&szcur);
	return 0;
}

static void
dosizeof(struct token *t)
{
	unsigned short base = 0;
	unsigned char parens = 0, stars = 0, kind = 0;
	unsigned long ans = 0;
	struct vreg *v;
	int i;

	szn = 0;
	insz = 1;
	pull(&szcur);
	if (szcur.type != LPAR && szcur.type != SYM)
		goto unfold;
	if (szcur.type == LPAR) {
		parens = 1;
		if (szstep())
			goto unfold;
	}
	while (szcur.type == STAR) {
		stars++;
		if (szstep())
			goto unfold;
	}
	if (szkw(szcur.type)) {
		kind = 1;
		while (szkw(szcur.type)) {
			base = kwsz(szcur.type, base);
			if (szstep())
				goto unfold;
		}
	} else if (szcur.type == STRUCT || szcur.type == UNION) {
		kind = 1;
		if (szstep())
			goto unfold;
		if (szcur.type != SYM)
			goto unfold;
		base = stfind(szcur.v.name);
		if (szstep())
			goto unfold;
	} else if (szcur.type == SYM && !stars) {
		if ((v = vfind(szcur.v.name)) == 0)
			goto unfold;
		ans = v->total;
		if (szstep())
			goto unfold;
		if (szcur.type == LBRACK) {
			ans = v->elem;	/* any constant index */
			if (szstep())
				goto unfold;
			if (szcur.type != NUMBER &&
			    szcur.type != INUMBER)
				goto unfold;
			if (szstep())
				goto unfold;
			if (szcur.type != RBRACK)
				goto unfold;
			if (szstep())
				goto unfold;
		}
	} else if (szcur.type == SYM && stars == 1) {
		if ((v = vfind(szcur.v.name)) == 0 || !v->deref)
			goto unfold;
		ans = v->deref;
		if (szstep())
			goto unfold;
	} else
		goto unfold;

	if (kind) {
		while (szcur.type == STAR) {
			stars++;
			if (szstep())
				goto unfold;
		}
		ans = stars ? 2 : base;
		/*
		 * An abstract array - sizeof(int [5]) - is what a
		 * typedef'd array dissolves to, and pass1's type
		 * parser never could read one: this is the only
		 * place with an answer.
		 */
		while (szcur.type == LBRACK) {
			unsigned short cnt;

			if (szstep())
				goto unfold;
			if (szcur.type != NUMBER &&
			    szcur.type != INUMBER)
				goto unfold;
			cnt = (unsigned short)szcur.v.numeric;
			if (szstep())
				goto unfold;
			if (szcur.type != RBRACK)
				goto unfold;
			ans *= cnt;
			if (szstep())
				goto unfold;
		}
	}
	if (parens) {
		if (szcur.type != RPAR)
			goto unfold;
	}
	if (!ans)
		goto unfold;
	toksynth(t, INUMBER);
	t->v.numeric = (long)ans;
	if (!parens) {
		/* the bare form's follower re-enters ahead of the
		 * source */
		if (szqw < 12)
			tokcpy(&szq[szqw++], &szcur);
	}
	insz = 0;
	return;

unfold:
	/* not ours to answer: replay for pass1 */
	toksynth(t, SIZEOF_KW);
	for (i = 0; i < szn; i++)
		if (szqw < 12)
			tokcpy(&szq[szqw++], &szsv[i]);
	if (szqw < 12)
		tokcpy(&szq[szqw++], &szcur);
	insz = 0;
}

/*
 * File-scope declarations, watched off kout's stream.  One flat
 * walk: specs, stars, a name, brackets, an initializer to skip.
 * Function declarators drop the name (nothing may sizeof one);
 * a struct body does not pass this way at all - norm_run routes
 * it through aggrpass - so a tag reference resolves through the
 * registry by the time the following declarators arrive.
 */
#define KS_SPECS	0
#define KS_DECL		1
#define KS_BRK		2
#define KS_INIT		3
static unsigned char ks_st;
static unsigned short ks_base;
static unsigned char ks_stars;
static char *ks_name;
static unsigned short ks_cnt;
static unsigned char ks_nbrk;
static unsigned char ks_bad;
static unsigned char ks_pd;
static unsigned char ks_id;
static unsigned char ks_awtag;
static char *ks_tag;		/* aggregate head, for norm_run */
static unsigned char ks_kind;

static void
ksdone(void)
{
	unsigned short el;

	if (ks_name && !ks_bad) {
		if (ks_cnt) {
			el = ks_stars ? 2 : ks_base;
			if (el)
				vadd(ks_name, el * ks_cnt,
				    ks_nbrk == 1 ? el : 0, 0);
		} else if (ks_stars)
			vadd(ks_name, 2, 0,
			    ks_stars > 1 ? 2 : ks_base);
		else if (ks_base)
			vadd(ks_name, ks_base, 0, 0);
	}
	ks_name = 0;
	ks_stars = 0;
	ks_cnt = 0;
	ks_nbrk = 0;
	ks_bad = 0;
}

static void
kscan(struct token *t)
{
	if (t->type == NEWLINE || t->type == LINENO)
		return;
	if (ks_awtag) {
		ks_awtag = 0;
		if (t->type == SYM) {
			ks_tag = t->v.name;
			ks_base = stfind(ks_tag);
			return;
		}
	}
	if (t->type == STRUCT || t->type == UNION) {
		ks_awtag = 1;
		ks_kind = (t->type == UNION) ? 2 : 1;
		ks_base = 0;
		if (ks_st == KS_INIT)
			return;
		ks_st = KS_SPECS;
		return;
	}
	if (t->type == ENUM) {
		ks_kind = 0;
		ks_base = 0;
		ks_bad = 1;
		return;
	}
	switch (ks_st) {
	case KS_SPECS:
		if (szkw(t->type)) {
			ks_base = kwsz(t->type, ks_base);
			return;
		}
		if (t->type == STAR) {
			ks_stars++;
			ks_st = KS_DECL;
			return;
		}
		if (t->type == SYM) {
			ks_name = t->v.name;
			ks_st = KS_DECL;
			return;
		}
		if (t->type == SEMI) {
			ks_base = 0;
			ks_bad = 0;
		}
		return;
	case KS_DECL:
		switch (t->type) {
		case STAR:	if (!ks_pd && !ks_name) ks_stars++;
				break;
		case SYM:	if (!ks_pd && !ks_name)
					ks_name = t->v.name;
				break;
		case LPAR:	if (!ks_pd && ks_name)
					ks_name = 0;  /* a function */
				ks_pd++;
				break;
		case RPAR:	if (ks_pd) ks_pd--; break;
		case LBRACK:	if (!ks_pd) ks_st = KS_BRK; break;
		case ASSIGN:	if (ks_pd) break;
				ksdone();
				ks_st = KS_INIT;
				ks_id = 0;
				break;
		case COMMA:	if (!ks_pd) ksdone(); break;
		case SEMI:	ksdone();
				ks_base = 0;
				ks_st = KS_SPECS;
				break;
		}
		return;
	case KS_BRK:
		if (t->type == NUMBER || t->type == INUMBER)
			ks_cnt = ks_cnt ? ks_cnt *
			    (unsigned short)t->v.numeric
			    : (unsigned short)t->v.numeric;
		else if (t->type == RBRACK) {
			if (!ks_cnt)
				ks_bad = 1;
			ks_nbrk++;
			ks_st = KS_DECL;
		} else
			ks_bad = 1;
		return;
	case KS_INIT:
		switch (t->type) {
		case BEGIN:
		case LPAR:
		case LBRACK:	ks_id++; break;
		case END:
		case RPAR:
		case RBRACK:	if (ks_id) ks_id--; break;
		case COMMA:	if (!ks_id) ks_st = KS_DECL; break;
		case SEMI:	if (!ks_id) {
					ks_base = 0;
					ks_st = KS_SPECS;
				}
				break;
		}
		return;
	}
}

#ifdef DEBUG
void
sizedump(void)
{
	struct streg *sp;
	struct vreg *v;

	for (sp = stags; sp; sp = sp->next)
		fdprintf(2, "  struct %s = %d\n", sp->tag, sp->size);
	for (v = vregs; v; v = v->next)
		fdprintf(2, "  var %s = %d elem %d deref %d @%d\n",
		    v->name, v->total, v->elem, v->deref, v->depth);
}
#endif

static void sizedecl(void);

static void
finish_decl(void)
{
	sizedecl();
	if (name_cnt > 0) {
		int keep = specs_static();

		emit_decl();
		if (keep)
			drop_assigns();
		else
			emit_assigns();
	}
	tarr_reset(&spec_a);
	name_cnt = 0;
	cur_stars = 0;
}

/*
 * Stream a brace-balanced region verbatim: the body of a struct or
 * union defined mid-function or at file scope.  Members are not
 * declarations to split - a bitfield's colon would tear one apart.
 * The BEGIN has been emitted; consumes through the matching END.
 */
static void
aggrpass(void)
{
	struct token t;
	int d = 1;

	for (;;) {
		pull(&t);
		if (t.type == E_O_F)
			return;
		if (t.type == BEGIN)
			d++;
		else if (t.type == END && --d == 0) {
			if (mfeed) {
				mtok(&t);
				mfeed = 0;
			}
			out(&t);
			return;
		}
		if (mfeed)
			mtok(&t);
		out(&t);
	}
}

/*
 * Prime the member pricer from spec_a: the body about to stream is
 * the definition of this tag.  Enums have no members to price.
 */
static void
aggprime(void)
{
	char *tag = 0;
	unsigned char isu = 0;
	int i;

	for (i = 0; i < spec_a.count; i++) {
		unsigned char c = spec_a.buf[i].type;

		if (c == ENUM)
			return;
		if (c == UNION)
			isu = 1;
		if ((c == STRUCT || c == UNION) &&
		    i + 1 < spec_a.count &&
		    spec_a.buf[i+1].type == SYM)
			tag = spec_a.buf[i+1].v.name;
	}
	mpush(tag, isu);
	mfeed = 1;
}

/* decl() states */
#define D_DECL	0	/* collecting specs, stars, names */
#define D_NAME	1	/* after a declarator name */
#define D_INIT	2	/* collecting a scalar initializer */
#define D_ARR	3	/* inside an array declarator's brackets */
#define D_ARRE	4	/* just past the closing bracket */
#define D_AINIT	5	/* an array's inline initializer */

/*
 * One declaration engagement, entered on a type token wherever the
 * old filtdecl would have left ST_NORMAL - a statement head or the
 * middle of an expression (a cast's type flows through the same
 * flush).  Returns 1 when the declaration consumed its semicolon
 * (the statement is over), 0 when it flushed mid-stream and the
 * caller should keep going.
 */
static int
decl(struct token *t)
{
	struct token cur;
	int st = D_DECL;
	int xtag;		/* struct/union/enum: next SYM is the tag */
	int pdepth = 0;		/* paren depth in initializers */
	int idepth = 0;		/* brace depth in an array initializer */
	int arrd = 0;		/* bracket depth in an array declarator */
	int i;

	tarr_reset(&spec_a);
	name_cnt = 0;
	assign_cnt = 0;
	cur_stars = 0;
	xtag = (t->type == STRUCT || t->type == UNION || t->type == ENUM);
	tarr_push(&spec_a, t);

	for (;;) {
		pull(&cur);
		if (cur.type == E_O_F) {
			outarr(&spec_a);
			tarr_reset(&spec_a);
			return 1;
		}

		if (cur.type == BEGIN) {
			if (st == D_AINIT) {
				idepth++;
				out(&cur);
				continue;
			}
			if (st == D_DECL || st == D_NAME) {
				/*
				 * Aggregate definition in declaration
				 * position: flush the specs and stream
				 * the body verbatim.
				 */
				register struct token *tp = spec_a.buf;
				int n = spec_a.count;
				int isagg = 0;

				while (n--) {
					if (tp->type == STRUCT ||
					    tp->type == UNION ||
					    tp->type == ENUM) {
						isagg = 1;
						break;
					}
					tp++;
				}
				if (isagg)
					aggprime();
				outarr(&spec_a);
				out(&cur);
				tarr_reset(&spec_a);
				name_cnt = 0;
				cur_stars = 0;
				if (isagg)
					aggrpass();
				return 0;
			}
			/* D_INIT and the rest: straight out (mirrors the
			 * old intercept; a brace here is broken input) */
			out(&cur);
			continue;
		}
		if (cur.type == END) {
			if (st == D_AINIT) {
				idepth--;
				out(&cur);
				continue;
			}
			out(&cur);
			continue;
		}

		switch (st) {
		case D_DECL:
			if (is_type_tok(&cur)) {
				xtag = (cur.type == STRUCT ||
				    cur.type == UNION || cur.type == ENUM);
				tarr_push(&spec_a, &cur);
				continue;
			}
			if (cur.type == STAR) {
				cur_stars++;
				xtag = 0;
				continue;
			}
			if (cur.type == SYM) {
				if (xtag) {
					tarr_push(&spec_a, &cur);
					xtag = 0;
					continue;
				}
				save_name(cur.v.name);
				st = D_NAME;
				continue;
			}
			/* not a declaration: flush specs, stars, token */
			outarr(&spec_a);
			for (i = 0; i < cur_stars; i++)
				outt(STAR);
			out(&cur);
			tarr_reset(&spec_a);
			cur_stars = 0;
			return 0;

		case D_NAME:
			if (cur.type == ASSIGN) {
				st = D_INIT;
				pdepth = 0;
				idepth = 0;
				tarr_reset(&dini_a);
				continue;
			}
			if (cur.type == COMMA) {
				st = D_DECL;
				continue;
			}
			if (cur.type == SEMI) {
				/* the declaration's own ; comes from
				 * emit_decl; the source's is dropped */
				finish_decl();
				return 1;
			}
			if (cur.type == STAR) {
				out(&cur);
				continue;
			}
			if (cur.type == LPAR || cur.type == LBRACK) {
				/*
				 * Function or array declarator: emit
				 * type, stars, name, opener; a function's
				 * tail flows through unchanged, an
				 * array's dimension is streamed and the
				 * declaration continues after it.
				 */
				struct token tmp;
				struct dname *np = &names[name_cnt - 1];
				struct token *ref = spec_a.count > 0 ?
				    &spec_a.buf[0] : &cur;

				outarr(&spec_a);
				for (i = np->star_count; i > 0; i--)
					outat(STAR, ref);
				tmp.type = SYM;
				tmp.v.name = np->name;
				tmp.lineno = ref->lineno;
				tmp.filename = ref->filename;
				out(&tmp);
				out(&cur);
				name_cnt--;
				if (cur.type == LPAR) {
					tarr_reset(&spec_a);
					return 0;
				}
				/* the type is kept: declarators after
				 * the array still split */
				arrstart(np->name, np->star_count);
				arrd = 1;
				st = D_ARR;
				continue;
			}
			finish_decl();
			out(&cur);
			return 0;

		case D_ARR:
			out(&cur);
			arrtok(&cur);
			if (cur.type == LBRACK)
				arrd++;
			else if (cur.type == RBRACK && --arrd == 0)
				st = D_ARRE;
			continue;

		case D_ARRE:
			if (cur.type == LBRACK) {
				out(&cur);
				arrd = 1;
				st = D_ARR;
				continue;
			}
			if (cur.type == ASSIGN) {
				/*
				 * An array initializer stays inline: an
				 * aggregate cannot become an assignment,
				 * and its only legal homes want it
				 * inline anyway.
				 */
				arrdone();
				out(&cur);
				pdepth = 0;
				idepth = 0;
				st = D_AINIT;
				continue;
			}
			if (cur.type == COMMA) {
				/* close this declaration, keep the type:
				 * char buf[12], *p  ->  two declarations */
				arrdone();
				outt(SEMI);
				st = D_DECL;
				continue;
			}
			if (cur.type == SEMI) {
				arrdone();
				outt(SEMI);
				finish_decl();
				return 1;
			}
			/* not a shape this splits: flush and step aside */
			arr_name = 0;
			out(&cur);
			tarr_reset(&spec_a);
			name_cnt = 0;
			return 0;

		case D_AINIT:
			if (cur.type == LPAR)
				pdepth++;
			else if (cur.type == RPAR)
				pdepth--;
			if (cur.type == COMMA && idepth == 0 && pdepth == 0) {
				outt(SEMI);
				st = D_DECL;
				continue;
			}
			if (cur.type == SEMI && idepth == 0 && pdepth == 0) {
				outt(SEMI);
				finish_decl();
				return 1;
			}
			out(&cur);
			continue;

		case D_INIT:
			if (cur.type == LPAR)
				pdepth++;
			else if (cur.type == RPAR)
				pdepth--;
			if (cur.type == COMMA && pdepth == 0 && idepth == 0) {
				save_init(names[name_cnt - 1].name);
				st = D_DECL;
				continue;
			}
			if (cur.type == SEMI && pdepth == 0 && idepth == 0) {
				save_init(names[name_cnt - 1].name);
				finish_decl();
				return 1;
			}
			tarr_push(&dini_a, &cur);
			continue;
		}
	}
}

/*
 * Control lowering (from filtbrace + filtctrl)
 */

/* stream a ( cond ) verbatim - if and switch keep their conditions */
static void
copycond(void)
{
	struct token t;
	int d = 0;

	for (;;) {
		pull(&t);
		if (t.type == E_O_F)
			return;
		if (t.type == LPAR)
			d++;
		else if (t.type == RPAR)
			d--;
		out(&t);
		if (d <= 0)
			return;
	}
}

/*
 * Collect the tokens inside ( ... ) into a buffer, outer parens
 * dropped.  Nothing is emitted: the caller re-emits the buffer inside
 * the construct it is lowering, which is why stamps ride along on the
 * buffered tokens themselves.
 */
static void
grabcond(void)
{
	struct token t;
	int d = 0;

	tarr_reset(&cond_a);
	for (;;) {
		pull(&t);
		if (t.type == E_O_F)
			return;
		if (t.type == LPAR) {
			d++;
			if (d == 1)
				continue;
		} else if (t.type == RPAR) {
			d--;
			if (d == 0)
				return;
		}
		if (d > 0)
			tarr_push(&cond_a, &t);
	}
}

/* if ( ! ( cond ) ) { goto __XnB ; } - the loop's entry test */
static void
outcondjmp(char pfx, int n)
{
	outt(IF);
	outt(LPAR);
	outt(BANG);
	outt(LPAR);
	outarr(&cond_a);
	outt(RPAR);
	outt(RPAR);
	outt(BEGIN);
	outgoto(pfx, n, 'B');
	outt(SEMI);
	outt(END);
}

/*
 * A loop or switch body: brace-normalized, one statement or a block.
 * The synthetic { is emitted only after the first body token is in
 * hand (stamp parity), the } right after the statement completes.
 */
static void
body(void)
{
	struct token t;

	pull(&t);
	if (t.type == E_O_F)
		return;
	scopedep++;
	if (t.type == BEGIN) {
		stmt(&t);
	} else {
		outt(BEGIN);
		stmt(&t);
		outt(END);
	}
	scopedep--;
	vpop();
}

static void do_else(void);

/*
 * if: condition passes through; an unbraced body gets braces, but its
 * closing } waits until the token after the body shows whether an
 * else follows - the } must precede the else either way, and it is
 * synthesized only after that token is pulled.
 */
static void
do_if(struct token *t0)
{
	struct token t;

	out(t0);
	copycond();
	pull(&t);
	if (t.type == E_O_F)
		return;
	if (t.type == BEGIN) {
		stmt(&t);
		pull(&t);
		if (t.type == ELSE) {
			out(&t);
			do_else();
			return;
		}
		if (t.type != E_O_F)
			pushb(&t);
		return;
	}
	outt(BEGIN);
	stmt(&t);
	pull(&t);
	if (t.type == ELSE) {
		outt(END);
		out(&t);
		do_else();
		return;
	}
	outt(END);
	if (t.type != E_O_F)
		pushb(&t);
}

/* else body; else-if chains continue without an extra brace level */
static void
do_else(void)
{
	struct token t;

	pull(&t);
	if (t.type == E_O_F)
		return;
	if (t.type == IF) {
		do_if(&t);
		return;
	}
	if (t.type == BEGIN) {
		stmt(&t);
		return;
	}
	outt(BEGIN);
	stmt(&t);
	outt(END);
}

/*
 * while (cond) body ->
 *	__WnT: ; if (!(cond)) { goto __WnB ; } body goto __WnT ; __WnB: ;
 */
static void
do_while(void)
{
	int n = next_label++;
	char obp = brkpfx, ocp = cntpfx, ocs = cntsfx;
	int obn = brknum, ocn = cntnum;

	grabcond();
	outlab('W', n, 'T');
	if (cond_a.count)
		outcondjmp('W', n);

	brkpfx = 'W'; brknum = n;
	cntpfx = 'W'; cntnum = n; cntsfx = 'T';
	body();
	brkpfx = obp; brknum = obn;
	cntpfx = ocp; cntnum = ocn; cntsfx = ocs;

	outgoto('W', n, 'T');
	outt(SEMI);
	outlab('W', n, 'B');
}

/*
 * for (init; cond; incr) body ->
 *	init ; __FnT: ; if (!(cond)) { goto __FnB ; } body
 *	__FnC: ; incr ; goto __FnT ; __FnB: ;
 * All three clauses are collected before anything is emitted, as
 * filtctrl did; only the increment outlives the body, in this frame.
 */
static void
do_for(void)
{
	int n = next_label++;
	struct token t;
	int d = 0;
	struct tokarray incr;
	char obp = brkpfx, ocp = cntpfx, ocs = cntsfx;
	int obn = brknum, ocn = cntnum;

	/* init: up to the ; at paren depth 1 */
	tarr_reset(&init_a);
	for (;;) {
		pull(&t);
		if (t.type == E_O_F)
			return;
		if (t.type == LPAR) {
			d++;
			if (d == 1)
				continue;
		} else if (t.type == RPAR) {
			d--;
		} else if (t.type == SEMI && d == 1) {
			break;
		}
		if (d > 0)
			tarr_push(&init_a, &t);
	}

	/* cond: up to the next ; */
	tarr_reset(&cond_a);
	for (;;) {
		pull(&t);
		if (t.type == E_O_F)
			return;
		if (t.type == SEMI)
			break;
		tarr_push(&cond_a, &t);
	}

	/* incr: up to the ) that closes the header */
	tarr_init(&incr, 16);
	for (;;) {
		pull(&t);
		if (t.type == E_O_F) {
			free(incr.buf);
			return;
		}
		if (t.type == RPAR) {
			if (--d == 0)
				break;
		} else if (t.type == LPAR) {
			d++;
		}
		tarr_push(&incr, &t);
	}

	if (init_a.count) {
		outarr(&init_a);
		outt(SEMI);
	}
	outlab('F', n, 'T');
	if (cond_a.count)
		outcondjmp('F', n);

	brkpfx = 'F'; brknum = n;
	cntpfx = 'F'; cntnum = n; cntsfx = 'C';
	body();
	brkpfx = obp; brknum = obn;
	cntpfx = ocp; cntnum = ocn; cntsfx = ocs;

	outlab('F', n, 'C');
	if (incr.count) {
		outarr(&incr);
		outt(SEMI);
	}
	outgoto('F', n, 'T');
	outt(SEMI);
	outlab('F', n, 'B');
	free(incr.buf);
}

/*
 * do body while (cond); ->
 *	__DnT: ; body __DnC: ; if (cond) { goto __DnT ; } __DnB: ;
 * __DnC precedes the test so continue re-tests it (C semantics).
 */
static void
do_do(void)
{
	int n = next_label++;
	struct token t;
	char obp = brkpfx, ocp = cntpfx, ocs = cntsfx;
	int obn = brknum, ocn = cntnum;

	outlab('D', n, 'T');

	brkpfx = 'D'; brknum = n;
	cntpfx = 'D'; cntnum = n; cntsfx = 'C';
	body();
	brkpfx = obp; brknum = obn;
	cntpfx = ocp; cntnum = ocn; cntsfx = ocs;

	pull(&t);
	if (t.type == E_O_F)
		return;
	if (t.type != WHILE) {
		/* not the trailing while: close the loop and step aside */
		pushb(&t);
		outlab('D', n, 'C');
		outlab('D', n, 'B');
		return;
	}
	grabcond();
	outlab('D', n, 'C');
	if (cond_a.count) {
		outt(IF);
		outt(LPAR);
		outarr(&cond_a);
		outt(RPAR);
		outt(BEGIN);
		outgoto('D', n, 'T');
		outt(SEMI);
		outt(END);
	}
	outlab('D', n, 'B');
	/* the source's trailing ; is the loop's own now */
	pull(&t);
	if (t.type != E_O_F && t.type != SEMI)
		out(&t);
}

/* switch passes through; it just gains its break label at the end */
static void
do_switch(struct token *t0)
{
	int n = next_label++;
	char obp = brkpfx;
	int obn = brknum;

	out(t0);
	copycond();

	brkpfx = 'S'; brknum = n;
	body();
	brkpfx = obp; brknum = obn;

	outlab('S', n, 'B');
}

/*
 * An expression statement (or a label, case, goto, return...):
 * tokens stream through to the semicolon.  A colon hands the rest
 * back to stmt() - what follows a label is a statement of its own,
 * and after a ternary's colon the dispatch is a no-op.  A type
 * keyword hands off to decl(), which either takes the declaration
 * or flushes a cast's tokens through unchanged.
 */
static void
exprstmt(struct token *t)
{
	struct token cur;

	tokcpy(&cur, t);
	for (;;) {
		if (cur.type == E_O_F)
			return;
		if (cur.type == SEMI) {
			out(&cur);
			return;
		}
		if (cur.type == END) {
			/* statement ran into its block's close */
			pushb(&cur);
			return;
		}
		if (cur.type == COLON) {
			out(&cur);
			pull(&cur);
			if (cur.type == E_O_F)
				return;
			stmt(&cur);
			return;
		}
		if ((token_props[cur.type] & (TF_COND | TF_DO | TF_ELSE)) ||
		    cur.type == BREAK || cur.type == CONTINUE) {
			/*
			 * A control keyword mid-stream: after a LABEL
			 * token (ident: is one lexeme), or broken input.
			 * The filters engaged on these anywhere; so does
			 * the dispatch.
			 */
			stmt(&cur);
			return;
		}
		if (is_type_tok(&cur)) {
			if (decl(&cur))
				return;
			pull(&cur);
			continue;
		}
		if (cur.type == BEGIN) {
			/* a block: a labeled compound statement lands
			 * here; its contents are statements */
			stmt(&cur);
			return;
		}
		out(&cur);
		pull(&cur);
	}
}

/*
 * One statement, t already pulled.  This is the dispatch the three
 * filters each re-derived per token; nesting is the call stack.
 */
static void
stmt(struct token *t)
{
	struct token u;

	switch (t->type) {
	case BEGIN:
		out(t);
		bdepth++;
		scopedep++;
		for (;;) {
			pull(&u);
			if (u.type == E_O_F)
				return;
			if (u.type == END) {
				bdepth--;
				scopedep--;
				vpop();
				out(&u);
				return;
			}
			stmt(&u);
		}

	case IF:
		do_if(t);
		return;

	case ELSE:
		/* a dangling else: emit and take its body normally */
		out(t);
		do_else();
		return;

	case WHILE:
		do_while();
		return;

	case FOR:
		do_for();
		return;

	case DO:
		do_do();
		return;

	case SWITCH:
		do_switch(t);
		return;

	case BREAK:
	case CONTINUE:
		/*
		 * The keyword becomes a goto (or passes through when
		 * there is no target); the statement's own ; follows
		 * in the stream and still ends it.
		 */
		if (t->type == BREAK && brkpfx)
			outgoto(brkpfx, brknum, 'B');
		else if (t->type == CONTINUE && cntpfx)
			outgoto(cntpfx, cntnum, cntsfx);
		else
			out(t);
		pull(&u);
		if (u.type != E_O_F)
			exprstmt(&u);
		return;

	case SEMI:
		out(t);
		return;

	default:
		if (is_type_tok(t)) {
			if (decl(t))
				return;
			/* flushed mid-stream: the statement continues */
			pull(&u);
			if (u.type != E_O_F)
				exprstmt(&u);
			return;
		}
		exprstmt(t);
		return;
	}
}

/*
 * K&R function normalization (from filtknr) - file scope only.
 *
 *	int foo(a, b)		int foo(int a, char *b)
 *	int a;		->	{
 *	char *b;
 *	{
 *
 * The header is buffered from its first type token (or bare name -
 * implicit int) through the { or ; that ends it, then re-emitted in
 * ANSI form.  Anything that stops looking like a K&R definition
 * flushes what was buffered and steps aside.
 */
#define PARAM_MAX 24
struct kparm {
	char *name;
	struct token *type;
	char type_len;
	char stars;
	struct token *post;	/* fn-ptr declarator tail: `)(int)` */
	char post_len;
};
static struct kparm kparms[PARAM_MAX];
static unsigned char kp_cnt;

static struct tokarray rtype_a;
static struct tokarray ptype_a;
static struct tokarray tail_a;
static struct tokarray ptail_a;
static int kp_stars;
static int kp_pdepth;
static int kp_preopen;
static char *cur_pname;
static struct token fname;
static struct token save_lp;

/* mid declarator-list at file scope: later declarators share the
 * first one's base type - no implicit int for them */
static int dlist;

/* file-scope emission: track aggregate heads so a following { is
 * recognized as a struct body, not a statement block */
static void
kout(struct token *t)
{
	if (t->type == STRUCT || t->type == UNION || t->type == ENUM)
		inagg = 1;
	else if (t->type != SYM && t->type != BEGIN)
		inagg = 0;
	kscan(t);
	out(t);
}

static void
koutarr(struct tokarray *a)
{
	int i;

	for (i = 0; i < a->count; i++)
		kout(&a->buf[i]);
}

/* synthesized header tokens go through the same tracking - a TIMES
 * or RPAR must clear a struct head just like a real one, or the
 * body { of a struct-param function reads as a struct body */
static void
kouta(unsigned char type, struct token *ref)
{
	struct token t;

	t.type = type;
	t.v.numeric = 0;
	t.lineno = ref->lineno;
	t.filename = ref->filename;
	kout(&t);
}

static void
koutt(unsigned char type)
{
	struct token t;

	toksynth(&t, type);
	kout(&t);
}

static struct kparm *
find_kparm(char *name)
{
	register struct kparm *pp = kparms;
	unsigned char n = kp_cnt + 1;

	while (--n) {
		if (strcmp(pp->name, name) == 0)
			return pp;
		pp++;
	}
	return 0;
}

static void
save_ptype(char *name, int stars)
{
	register struct kparm *pp = find_kparm(name);

	if (pp && ptype_a.count > 0) {
		register struct token *d;
		struct token *s;
		int n;

		n = ptype_a.count;
		d = pp->type = (struct token *)xalloc(n * sizeof(struct token));
		s = ptype_a.buf;
		while (n--)
			tokcpy(d++, s++);
		pp->type_len = ptype_a.count;
		pp->stars = stars;
		n = ptail_a.count;
		if (n > 0) {
			d = pp->post = (struct token *)xalloc(n * sizeof(struct token));
			s = ptail_a.buf;
			while (n--)
				tokcpy(d++, s++);
			pp->post_len = ptail_a.count;
		}
	}
	tarr_reset(&ptail_a);
	/* base type stays buffered: char *a, *b; reuses it for b */
}

/* the merged ANSI-style declaration */
static void
emit_ansi(void)
{
	register struct kparm *pp;
	unsigned char n;
	int j;
	struct token tmp;

	/* synthesize the implicit int: c0 requires a return type */
	if (rtype_a.count == 0 && !dlist)
		kouta(INT, &fname);
	else
		koutarr(&rtype_a);
	kout(&fname);
	kouta(LPAR, &fname);

	pp = kparms;
	n = kp_cnt + 1;
	while (--n) {
		if (pp->type_len > 0) {
			j = pp->type_len;
			{
				struct token *s = pp->type;
				while (j--)
					kout(s++);
			}
		} else {
			/* K&R default: untyped params are int */
			kouta(INT, &fname);
		}
		for (j = pp->stars; j > 0; j--)
			kouta(TIMES, &fname);
		tmp.type = SYM;
		tmp.v.name = pp->name;
		tmp.lineno = fname.lineno;
		tmp.filename = fname.filename;
		kout(&tmp);
		for (j = 0; j < pp->post_len; j++)
			kout(&pp->post[j]);
		if (n > 1)
			kouta(COMMA, &fname);
		if (pp->type) {
			free(pp->type);
			pp->type = 0;
		}
		if (pp->post) {
			free(pp->post);
			pp->post = 0;
			pp->post_len = 0;
		}
		pp++;
	}

	kouta(RPAR, &fname);
	/* declarator tail: `)) (args)` of void (*signal(...))(args) */
	koutarr(&tail_a);
}

static void
kreset(void)
{
	register struct kparm *pp = kparms;
	unsigned char n = kp_cnt + 1;

	while (--n) {
		if (pp->type)
			free(pp->type);
		pp->type = 0;
		if (pp->post)
			free(pp->post);
		pp->post = 0;
		pp->post_len = 0;
		pp++;
	}
	tarr_reset(&rtype_a);
	kp_cnt = 0;
	tarr_reset(&ptype_a);
	tarr_reset(&tail_a);
	tarr_reset(&ptail_a);
	kp_preopen = 0;
	kp_pdepth = 0;
}

/* knr() states */
#define K_RTYPE		0	/* buffering the return type */
#define K_NAME		1	/* saw a candidate function name */
#define K_PARAMS	2	/* inside the () name list */
#define K_PDECL		3	/* reading K&R parameter declarations */
#define K_PTYPE		4	/* buffering one parameter's type */
#define K_TAIL		5	/* declarator tail: `)) (args)` */

/*
 * Abort: this stopped looking like K&R.  Re-emit what was consumed
 * (with the implicit int - an abort happens only after `name (`,
 * always a function) and let the rest flow.  st is the state the
 * machine was in when it gave up.
 */
static void
abort_knr(int st)
{
	register struct kparm *pp;
	unsigned char n;
	int i;
	struct token tmp;

	if (rtype_a.count == 0 && !dlist)
		kouta(INT, &fname);
	else
		koutarr(&rtype_a);
	kout(&fname);
	kout(&save_lp);
	pp = kparms;
	n = kp_cnt + 1;
	while (--n) {
		toksynthnam(&tmp, SYM, pp->name);
		kout(&tmp);
		if (n > 1 || st == K_PARAMS)
			koutt(COMMA);
		pp++;
	}
	if (st == K_PDECL || st == K_PTYPE || st == K_TAIL) {
		/* the ) was consumed on leaving K_PARAMS - put it back,
		 * then flush the tail and any partial K&R param decl */
		koutt(RPAR);
		koutarr(&tail_a);
		koutarr(&ptype_a);
		for (i = 0; i < kp_stars; i++)
			koutt(TIMES);
		if (cur_pname) {
			toksynthnam(&tmp, SYM, cur_pname);
			kout(&tmp);
		}
		koutarr(&ptail_a);
		tarr_reset(&ptype_a);
		tarr_reset(&ptail_a);
		kp_stars = 0;
		kp_pdepth = 0;
		cur_pname = 0;
	}
	/* in K_PARAMS the ) is still upcoming and flows through */
	tarr_reset(&rtype_a);
	tarr_reset(&tail_a);
	kp_preopen = 0;
	kp_cnt = 0;
}

/* flush a non-function: buffered tokens, then the terminator - a {
 * goes back for dispatch (it opens a struct body or a block) */
static void
kflush(struct token *t)
{
	koutarr(&rtype_a);
	tarr_reset(&rtype_a);
	kp_preopen = 0;
	if (t->type == BEGIN)
		pushb(t);
	else
		kout(t);
}

/*
 * One file-scope engagement, entered on a type token or a bare SYM.
 * Everything through the end of the construct is consumed; on return
 * the next pull starts fresh.
 */
static void
knr(struct token *t)
{
	struct token cur;
	int st;

	tarr_reset(&rtype_a);
	kp_preopen = 0;
	if (t->type == SYM) {
		/* implicit-int definition: fseek(f, o, p) char *f; */
		tokcpy(&fname, t);
		st = K_NAME;
	} else {
		tarr_push(&rtype_a, t);
		st = K_RTYPE;
	}

	for (;;) {
		pull(&cur);
		if (cur.type == E_O_F) {
			if (st == K_RTYPE)
				koutarr(&rtype_a);
			tarr_reset(&rtype_a);
			return;
		}

		switch (st) {
		case K_RTYPE:
			/* `( *` of a parenthesized declarator buffers as
			 * part of the prefix */
			if (cur.type == LPAR) {
				tarr_push(&rtype_a, &cur);
				kp_preopen++;
				continue;
			}
			if (is_type_tok(&cur) || cur.type == STAR ||
			    cur.type == TIMES) {
				tarr_push(&rtype_a, &cur);
				continue;
			}
			if (cur.type == SYM) {
				if (tag_pending(&rtype_a)) {
					tarr_push(&rtype_a, &cur);
					continue;
				}
				tokcpy(&fname, &cur);
				st = K_NAME;
				continue;
			}
			kflush(&cur);
			return;

		case K_NAME:
			if (cur.type == LPAR) {
				tokcpy(&save_lp, &cur);
				st = K_PARAMS;
				kp_pdepth = 1;
				kp_cnt = 0;
				continue;
			}
			if (cur.type == COMMA)
				dlist = 1;
			else if (cur.type == SEMI)
				dlist = 0;
			koutarr(&rtype_a);
			kout(&fname);
			tarr_reset(&rtype_a);
			kp_preopen = 0;
			if (cur.type == BEGIN)
				pushb(&cur);
			else
				kout(&cur);
			return;

		case K_PARAMS:
			if (cur.type == RPAR) {
				kp_pdepth--;
				if (kp_pdepth == 0)
					st = K_PDECL;
				continue;
			}
			if (cur.type == LPAR) {
				kp_pdepth++;
				continue;
			}
			if (cur.type != COMMA) {
				if (is_type_tok(&cur)) {
					/* types in the list = ANSI */
					abort_knr(st);
					kout(&cur);
					return;
				}
				if (cur.type == SYM) {
					if (kp_cnt >= PARAM_MAX) {
						gripe(ER_C_PC);
						continue;
					}
					{
						register struct kparm *pp;

						pp = &kparms[kp_cnt++];
						pp->name = cur.v.name;
						pp->type = 0;
						pp->type_len = 0;
						pp->stars = 0;
						pp->post = 0;
						pp->post_len = 0;
					}
				}
			}
			continue;

		case K_TAIL:
			if (cur.type == RPAR && kp_preopen > 0) {
				tarr_push(&tail_a, &cur);
				kp_preopen--;
				continue;
			}
			if (cur.type == LPAR) {
				tarr_push(&tail_a, &cur);
				kp_preopen++;
				continue;
			}
			if (kp_preopen > 0) {
				tarr_push(&tail_a, &cur);
				continue;
			}
			/* tail complete: reprocess as K_PDECL */
			st = K_PDECL;
			/* FALLTHROUGH */
		case K_PDECL:
			if (cur.type == BEGIN) {
				/* function body: the merged declaration,
				 * then the { dispatches as a block */
				emit_ansi();
				dlist = 0;
				kreset();
				pushb(&cur);
				return;
			}
			if (cur.type == SEMI) {
				if (ptype_a.count == 0 && cur_pname == 0) {
					/* no K&R declarations: a prototype */
					emit_ansi();
					dlist = 0;
					kreset();
					kout(&cur);
					return;
				}
				if (cur_pname)
					save_ptype(cur_pname, kp_stars);
				cur_pname = 0;
				tarr_reset(&ptype_a);
				kp_stars = 0;
				continue;
			}
			if (cur.type == COMMA && ptype_a.count == 0 &&
			    cur_pname == 0) {
				/* declarator list: type f(), g(); */
				emit_ansi();
				dlist = 1;
				kreset();
				kout(&cur);
				return;
			}
			if (is_type_tok(&cur)) {
				tarr_push(&ptype_a, &cur);
				st = K_PTYPE;
				continue;
			}
			if (cur.type == RPAR && kp_preopen > 0) {
				tarr_push(&tail_a, &cur);
				kp_preopen--;
				st = K_TAIL;
				continue;
			}
			abort_knr(st);
			if (cur.type == BEGIN)
				pushb(&cur);
			else
				kout(&cur);
			return;

		case K_PTYPE:
			/*
			 * Parenthesized declarator in a K&R param decl,
			 * void (*action)(int);: prefix into ptype_a, the
			 * SYM inside is the name, the rest into ptail_a.
			 */
			if (kp_pdepth > 0 || ptail_a.count > 0) {
				if (cur.type == LPAR) {
					if (cur_pname)
						tarr_push(&ptail_a, &cur);
					else
						tarr_push(&ptype_a, &cur);
					kp_pdepth++;
					continue;
				}
				if (cur.type == RPAR) {
					tarr_push(&ptail_a, &cur);
					kp_pdepth--;
					continue;
				}
				if (kp_pdepth > 0) {
					if (!cur_pname && (cur.type == STAR ||
					    cur.type == TIMES)) {
						tarr_push(&ptype_a, &cur);
						continue;
					}
					if (!cur_pname && cur.type == SYM) {
						cur_pname = cur.v.name;
						continue;
					}
					tarr_push(&ptail_a, &cur);
					continue;
				}
				/* depth 0 with a tail: fall through to
				 * the , / ; handling */
			}
			if (cur.type == LPAR && cur_pname == 0) {
				tarr_push(&ptype_a, &cur);
				kp_pdepth = 1;
				continue;
			}
			if (is_type_tok(&cur)) {
				tarr_push(&ptype_a, &cur);
				continue;
			}
			if (cur.type == STAR || cur.type == TIMES) {
				kp_stars++;
				continue;
			}
			if (cur.type == SYM) {
				if (tag_pending(&ptype_a)) {
					tarr_push(&ptype_a, &cur);
					continue;
				}
				cur_pname = cur.v.name;
				continue;
			}
			if (cur.type == COMMA) {
				/* int a, b; - same type, next param */
				int had_tail = (ptail_a.count > 0);

				if (cur_pname)
					save_ptype(cur_pname, kp_stars);
				cur_pname = 0;
				kp_stars = 0;
				if (had_tail) {
					/* fn-ptr `( *` prefix is
					 * per-declarator, no sharing */
					tarr_reset(&ptype_a);
					kp_pdepth = 0;
				}
				continue;
			}
			if (cur.type == SEMI) {
				if (cur_pname)
					save_ptype(cur_pname, kp_stars);
				cur_pname = 0;
				tarr_reset(&ptype_a);
				tarr_reset(&ptail_a);
				kp_stars = 0;
				kp_pdepth = 0;
				st = K_PDECL;
				continue;
			}
			abort_knr(st);
			if (cur.type == BEGIN)
				pushb(&cur);
			else
				kout(&cur);
			return;
		}
	}
}

/*
 * The source layer: lexer -> enum lowering -> typedef dissolution.
 * What the walker pulls is already free of ENUM, TYPEDEF, and every
 * typedef name.
 */

/*
 * Enum lowering (from filtenum).  Enum constants are glorified
 * #defines: each goes into the macro table as its value, the type
 * itself is rewritten to unsigned char, and a bare declaration
 * vanishes.  Constant names are global for the rest of the file,
 * exactly like #define.
 */
static struct token eqbuf[2];	/* CHAR + the lookahead, at most */
static unsigned char eqn, eqr;

static long enum_expr(struct token *t);

/* t holds the current lookahead on entry and exit */
static long
enum_prim(struct token *t)
{
	long v;

	if (t->type == MINUS) {
		lex_get(t);
		return -enum_prim(t);
	}
	if (t->type == TWIDDLE) {
		lex_get(t);
		return ~enum_prim(t);
	}
	if (t->type == LPAR) {
		lex_get(t);
		v = enum_expr(t);
		if (t->type == RPAR)
			lex_get(t);
		else
			gripe(ER_C_EV);
		return v;
	}
	/* an enum constant is an int however the value was spelled */
	if (t->type == NUMBER || t->type == LNUMBER) {
		v = t->v.numeric;
		lex_get(t);
		return v;
	}
	gripe(ER_C_EV);
	if (t->type != END && t->type != E_O_F)
		lex_get(t);
	return 0;
}

static long
enum_term(struct token *t)
{
	long v = enum_prim(t);

	while (t->type == STAR || t->type == TIMES) {
		lex_get(t);
		v *= enum_prim(t);
	}
	return v;
}

static long
enum_expr(struct token *t)
{
	long v = enum_term(t);
	unsigned char op;

	while (t->type == PLUS || t->type == MINUS) {
		op = t->type;
		lex_get(t);
		if (op == PLUS)
			v += enum_term(t);
		else
			v -= enum_term(t);
	}
	return v;
}

static void
epull(struct token *out)
{
	struct token t;
	struct token syn;
	char def[48];
	char *p;
	long val;

	if (eqr < eqn) {
		tokcpy(out, &eqbuf[eqr++]);
		if (eqr == eqn)
			eqr = eqn = 0;
		return;
	}

	lex_get(&t);
	while (t.type == ENUM) {
		lex_get(&t);			/* consume 'enum' */
		if (t.type == SYM)
			lex_get(&t);		/* tag: documentation only */

		if (t.type == BEGIN) {
			val = 0;
			lex_get(&t);
			while (t.type != END && t.type != E_O_F) {
				if (t.type != SYM) {
					gripe(ER_C_EV);
					lex_get(&t);
					continue;
				}
				/* build "NAME=" while the name is live */
				p = def;
				{
					char *n = t.v.name;
					while (*n && p < def + 32)
						*p++ = *n++;
				}
				*p++ = '=';
				lex_get(&t);
				if (t.type == ASSIGN) {
					lex_get(&t);
					val = enum_expr(&t);
				}
				fmtstr(p, "%ld", val);
				addDefine(def);
				val++;
				if (t.type == COMMA)
					lex_get(&t);
			}
			if (t.type == END)
				lex_get(&t);	/* consume '}' */
			else
				gripe(ER_C_EV);
		}

		/* t is now the token after the enum construct */
		if (t.type == SEMI) {
			/* bare "enum [tag] { ... };" - swallow entirely */
			lex_get(&t);
			continue;
		}
		/* type reference: replace with 'unsigned char' */
		toksynth(out, UNSIGNED);
		toksynth(&syn, CHAR);
		tokcpy(&eqbuf[eqn++], &syn);
		tokcpy(&eqbuf[eqn++], &t);
		return;
	}
	tokcpy(out, &t);
}

/*
 * Typedef dissolution (from filttdef).  A typedef is a declarator
 * with a hole where the name sits; using the name composes the
 * use-site declarator into the hole.  See the phase-1a commit for
 * the full rules - the engine is unchanged, its output queue is now
 * the walker's token source.
 */
struct tdent {
	char *name;			/* interned */
	struct token *spec, *pre, *post;
	unsigned char nspec, npre, npost;
	struct tdent *next;
};
static struct tdent *tdefs;

/*
 * One set of split arrays per live expansion.  Depth grows only
 * through a typedef inside a declarator of another typedef's use,
 * and one more for the K&R parameter line a terminator can begin.
 */
#define MAXDEPTH 3
static struct tokarray pres[MAXDEPTH], posts[MAXDEPTH];
static unsigned char xdepth;
static struct tokarray tdspec;

/* the expansion output queue - the one buffer between the source
 * layer and the walker */
static struct pendbuf tdq;

static struct tdent *
tdfind(char *name)
{
	struct tdent *t;

	for (t = tdefs; t; t = t->next)
		if (t->name == name)	/* interned: pointer compare */
			return t;
	return 0;
}

/* a sink is the queue (null) or a collection in progress */
static void
sink1(struct tokarray *sink, struct token *t)
{
	if (sink)
		tarr_push(sink, t);
	else
		pend_push(&tdq, t);
}

static void
sinkn(struct tokarray *sink, struct token *buf, int n)
{
	int i;

	for (i = 0; i < n; i++)
		sink1(sink, &buf[i]);
}

static void
sinkt(struct tokarray *sink, unsigned char type)
{
	struct token t;

	toksynth(&t, type);
	sink1(sink, &t);
}

/*
 * Pull the next token, letting line housekeeping flow straight out.
 */
static struct token tdbktok;
static unsigned char tdbkhv;

static void
tdpull(struct token *t)
{
	if (tdbkhv) {
		tdbkhv = 0;
		tokcpy(t, &tdbktok);
		return;
	}
	for (;;) {
		epull(t);
		if (t->type != NEWLINE && t->type != LINENO)
			return;
		pend_push(&tdq, t);
	}
}

static void expand(struct tdent *e, struct token *t,
    struct tokarray *sink);

/*
 * Collect one balanced ( ) or [ ] group into the sink, expanding
 * typedef names inside.  On entry t holds the opener; on exit the
 * first token past the closer.
 */
static void
group(struct token *t, struct tokarray *sink)
{
	unsigned char d = 0;
	struct tdent *e;

	for (;;) {
		if (t->type == LBRACK || t->type == LPAR) {
			d++;
		} else if (t->type == RBRACK || t->type == RPAR) {
			d--;
			sink1(sink, t);
			tdpull(t);
			if (!d)
				return;
			continue;
		} else if (t->type == SYM &&
		    (e = tdfind(t->v.name)) != 0) {
			expand(e, t, sink);
			/* the nested expansion sank its own terminator;
			 * account for it if it closed a level */
			if (t->type == RBRACK || t->type == RPAR) {
				d--;
				if (!d) {
					tdpull(t);
					return;
				}
			}
			tdpull(t);
			continue;
		}
		sink1(sink, t);
		tdpull(t);
	}
}

/*
 * Split one (possibly abstract) declarator into pre / name / post,
 * expanding typedef names inside its groups.  The caller hands in
 * the first token; the terminator comes back in t.
 */
static void
splitdecl(struct token *t, struct tokarray *pre, struct token *name,
    struct tokarray *post)
{
	unsigned char pdepth = 0;

	tarr_reset(pre);
	tarr_reset(post);
	name->type = 0;

	while (t->type == STAR || t->type == TIMES || t->type == LPAR) {
		if (t->type == LPAR)
			pdepth++;
		tarr_push(pre, t);
		tdpull(t);
	}
	if (t->type == SYM) {
		if (!tdfind(t->v.name)) {
			tokcpy(name, t);
			tdpull(t);
		} else {
			/*
			 * A typedef name in the name position: the
			 * declarator's NAME shadowing the typedef, or a
			 * fresh K&R type line.  What follows tells them
			 * apart: a star or another name means type line.
			 */
			struct token peek;

			tdpull(&peek);
			if (peek.type == STAR || peek.type == TIMES ||
			    peek.type == SYM) {
				tokcpy(&tdbktok, &peek);
				tdbkhv = 1;
			} else {
				tokcpy(name, t);
				tokcpy(t, &peek);
			}
		}
	}
	for (;;) {
		if (t->type == RPAR && pdepth) {
			pdepth--;
			tarr_push(post, t);
			tdpull(t);
			continue;
		}
		if (t->type == LBRACK || t->type == LPAR) {
			group(t, post);
			continue;
		}
		return;
	}
}

/*
 * Emit one wrapped declarator: pre_t [(] pre_u name post_u [)]
 * post_t.  Parens exactly when the use-site starts prefix-ish and
 * the hole continues with a postfix.
 */
static void
wrap(struct tdent *e, struct tokarray *pre, struct token *name,
    struct tokarray *post, struct tokarray *sink)
{
	int parens = pre->count && e->npost;

	sinkn(sink, e->pre, e->npre);
	if (parens)
		sinkt(sink, LPAR);
	sinkn(sink, pre->buf, pre->count);
	if (name->type)
		sink1(sink, name);
	sinkn(sink, post->buf, post->count);
	if (parens)
		sinkt(sink, RPAR);
	sinkn(sink, e->post, e->npost);
}

/*
 * Save a token array into the permanent arena.
 */
#ifdef DEBUG
long tdkeepB;		/* poolstats: bytes kept for entries */
int tdkeepN;
#endif

static struct token *
keep(struct tokarray *a, unsigned char *n)
{
	struct token *p;
	int i;

	*n = a->count;
	if (!a->count)
		return 0;
	p = (struct token *)permalloc(a->count * sizeof(struct token));
#ifdef DEBUG
	tdkeepB += a->count * sizeof(struct token);
	tdkeepN++;
#endif
	for (i = 0; i < a->count; i++)
		tokcpy(&p[i], &a->buf[i]);
	return p;
}

/*
 * Pass tokens through until a depth-0 comma or semicolon, expanding
 * typedef names met on the way - an initialiser can hold a cast or
 * a sizeof.  The terminator comes back in t, already sunk.
 */
static void
drain(struct token *t, struct tokarray *sink)
{
	unsigned char d = 0;
	struct tdent *e;

	for (;;) {
		tdpull(t);
		if (t->type == SYM && (e = tdfind(t->v.name)) != 0) {
			expand(e, t, sink);
			if (t->type == SEMI ||
			    (t->type == COMMA && !d))
				return;
			if (t->type == RPAR || t->type == RBRACK) {
				if (!d)
					return;
				d--;
			}
			continue;
		}
		if (t->type == LPAR || t->type == LBRACK) {
			d++;
		} else if (t->type == RPAR || t->type == RBRACK) {
			if (!d) {
				sink1(sink, t);
				return;
			}
			d--;
		} else if (t->type == SEMI ||
		    (t->type == COMMA && !d)) {
			sink1(sink, t);
			return;
		}
		sink1(sink, t);
	}
}

/*
 * A typedef name met in the stream: emit the specs once, then wrap
 * declarators until the list ends.  On return the terminator has
 * been sunk and t holds it.
 */
static void
expand(struct tdent *e, struct token *t, struct tokarray *sink)
{
	struct token name;
	struct tokarray *pre, *post;
	int have = 0;

	if (xdepth >= MAXDEPTH) {
		error("typedefs nested too deep");
		return;
	}
	pre = &pres[xdepth];
	post = &posts[xdepth];
	xdepth++;

	sinkn(sink, e->spec, e->nspec);
	for (;;) {
		if (!have)
			tdpull(t);
		have = 0;
		splitdecl(t, pre, &name, post);
		wrap(e, pre, &name, post, sink);
		if (t->type == ASSIGN) {
			sink1(sink, t);
			drain(t, sink);
			if (t->type != COMMA)
				break;
		}
		if (t->type != COMMA) {
			/* the terminator can itself be a typedef name:
			 * a K&R parameter line begins with one */
			if (t->type == SYM) {
				struct tdent *e3 = tdfind(t->v.name);

				if (e3) {
					expand(e3, t, sink);
					break;
				}
			}
			sink1(sink, t);
			break;
		}
		/* past the comma: ours, or somebody else's? */
		sink1(sink, t);
		tdpull(t);
		if (t->type == STAR || t->type == TIMES ||
		    t->type == LPAR) {
			have = 1;
			continue;
		}
		if (t->type == SYM) {
			struct tdent *e2 = tdfind(t->v.name);

			if (!e2) {
				have = 1;
				continue;
			}
			expand(e2, t, sink);
			break;
		}
		/* a type keyword or anything else: not our list */
		sink1(sink, t);
		break;
	}
	xdepth--;
}

/*
 * The whole typedef declaration, "typedef" already consumed.
 * Nothing goes downstream except a struct body, which is streamed
 * one member token per srcget call to keep the queue small.
 */
static unsigned char tdbody;	/* mid-body: tdcur is live */
static unsigned char tdfin;	/* body closed: resume the specs */
static unsigned char tddepth;
static struct token tdcur;
static unsigned char aftertag;

static void capture2(struct token *t);

static void
capture(struct token *t)
{
	tarr_reset(&tdspec);
	tdpull(t);
	capture2(t);
}

static void
capture2(struct token *t)
{
	struct tdent *e;
	struct token name;

	/*
	 * specs: a struct/union head, keywords, or an earlier typedef.
	 * struct/union first - the generic arm would eat the keyword
	 * and leave the tag looking like a name.
	 */
	for (;;) {
		if (t->type == STRUCT || t->type == UNION) {
			tarr_push(&tdspec, t);
			tdpull(t);
			if (t->type != SYM) {
				error("typedef of unnamed struct needs a tag");
				return;
			}
			tarr_push(&tdspec, t);
			tdpull(t);
			if (t->type == BEGIN) {
				/* the body is a real declaration of the
				 * tag, streamed downstream once */
				pend_buf(&tdq, tdspec.buf, tdspec.count);
				pend_push(&tdq, t);
				tdpull(t);
				tokcpy(&tdcur, t);
				tddepth = 1;
				tdbody = 1;
				return;
			}
			continue;
		}
		if (is_type_kw(t->type)) {
			tarr_push(&tdspec, t);
			tdpull(t);
			continue;
		}
		if (t->type == SYM && tdfind(t->v.name) != 0) {
			/* a typedef of a typedef: loud, not silent */
			error("typedef of a typedef");
			while (t->type != SEMI && t->type != E_O_F)
				tdpull(t);
			return;
		}
		break;
	}

	/* declarators: each one becomes an entry */
	for (;;) {
		splitdecl(t, &pres[0], &name, &posts[0]);
		if (!name.type) {
			error("typedef with no name");
			return;
		}
		e = (struct tdent *)permalloc(sizeof(*e));
		e->name = name.v.name;
		e->spec = keep(&tdspec, &e->nspec);
		e->pre = keep(&pres[0], &e->npre);
		e->post = keep(&posts[0], &e->npost);
		e->next = tdefs;
		tdefs = e;

		if (t->type == COMMA) {
			tdpull(t);
			continue;
		}
		if (t->type != SEMI)
			error("junk in typedef");
		return;
	}
}

static void
srcget(struct token *out)
{
	struct token t;
	struct tdent *e;

	for (;;) {
		if (tdbody && !pend_has(&tdq)) {
			/* one member token per call, expansions in
			 * small bursts */
			struct tdent *m;

			for (;;) {
				if (tdcur.type == BEGIN)
					tddepth++;
				else if (tdcur.type == END) {
					if (--tddepth == 0) {
						pend_push(&tdq, &tdcur);
						tdpull(&tdcur);
						pend_tok(&tdq, SEMI);
						tdbody = 0;
						tdfin = 1;
						break;
					}
				} else if (tdcur.type == SYM &&
				    (m = tdfind(tdcur.v.name)) != 0) {
					expand(m, &tdcur, 0);
					continue;
				}
				pend_push(&tdq, &tdcur);
				tdpull(&tdcur);
				break;
			}
		}
		if (tdfin && !pend_has(&tdq)) {
			/* the body has drained; the declarators follow,
			 * starting with the token already in hand */
			tdfin = 0;
			tokcpy(&t, &tdcur);
			capture2(&t);
			continue;
		}
		if (pend_has(&tdq)) {
			pend_pop(&tdq, out);
			return;
		}
		epull(&t);
		if (t.type == E_O_F) {
			tokcpy(out, &t);
			return;
		}

		if (t.type == TYPEDEF) {
			capture(&t);
			continue;	/* nothing but the queue */
		}

		if (t.type == SYM && !aftertag &&
		    (e = tdfind(t.v.name)) != 0) {
			expand(e, &t, 0);
			continue;
		}

		/*
		 * A tag position is a different namespace: "struct
		 * Expr" must not have its tag expanded even though
		 * Expr is also a typedef name.  Member access likewise.
		 */
		aftertag = (t.type == STRUCT || t.type == UNION ||
		    t.type == DOT || t.type == ARROW);

		tokcpy(out, &t);
		return;
	}
}

/*
 * Entry points
 */
void
norm_init(void)
{
	int i;

	haveback = 0;
	next_label = 1;
	brkpfx = 0;
	cntpfx = 0;
	bdepth = 0;
	inagg = 0;
	tarr_setup(&cond_a, 48);
	tarr_setup(&init_a, 16);
	tarr_setup(&spec_a, 16);
	tarr_setup(&dini_a, 32);
	name_cnt = 0;
	assign_cnt = 0;
	cur_stars = 0;
	tarr_setup(&rtype_a, 16);
	tarr_setup(&ptype_a, 16);
	tarr_setup(&tail_a, 8);
	tarr_setup(&ptail_a, 8);
	kp_cnt = 0;
	kp_stars = 0;
	kp_pdepth = 0;
	kp_preopen = 0;
	cur_pname = 0;
	dlist = 0;
	/* source layer */
	eqn = eqr = 0;
	pend_setup(&tdq, 16);
	tarr_setup(&tdspec, 8);
	for (i = 0; i < MAXDEPTH; i++) {
		tarr_setup(&pres[i], 8);
		tarr_setup(&posts[i], 8);
	}
	tdefs = 0;
	xdepth = 0;
	tdbkhv = 0;
	tdbody = 0;
	tdfin = 0;
	aftertag = 0;
	stags = 0;
	vregs = 0;
	vfree = 0;
	scopedep = 0;
	mtop = -1;
	mfeed = 0;
	msawtag = 0;
	mtag = 0;
	arr_name = 0;
	szqr = szqw = 0;
	insz = 0;
	ks_st = 0;
	ks_base = 0;
	ks_stars = 0;
	ks_name = 0;
	ks_cnt = 0;
	ks_nbrk = 0;
	ks_bad = 0;
	ks_pd = 0;
	ks_awtag = 0;
	ks_tag = 0;
	ks_kind = 0;
}

/*
 * File scope: everything streams through untouched - declarations
 * here keep their initializers - except that a struct/union body is
 * skipped verbatim (members are not statements) and a bare { opens
 * either a function body or a brace initializer, both of which the
 * statement machinery handles.
 */
void
norm_run(void)
{
	struct token t;

	for (;;) {
		pull(&t);
		if (t.type == E_O_F)
			return;
		if (t.type == BEGIN) {
			if (inagg) {
				inagg = 0;
				if (ks_kind) {
					mpush(ks_tag, ks_kind == 2);
					mfeed = 1;
					ks_kind = 0;
				}
				out(&t);
				aggrpass();
				continue;
			}
			stmt(&t);
			continue;
		}
		if (t.type == SYM || is_type_tok(&t)) {
			/* a declaration or K&R function head */
			knr(&t);
			continue;
		}
		kout(&t);
		if (t.type == COMMA)
			dlist = 1;
		else if (t.type == SEMI)
			dlist = 0;
	}
}
