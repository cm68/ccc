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

static void (*up)(struct token *);

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

/*
 * Token I/O
 */
static void
pull(struct token *t)
{
	if (haveback) {
		haveback = 0;
		tokcpy(t, &backtok);
		return;
	}
	up(t);
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

static void
finish_decl(void)
{
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
			out(&t);
			return;
		}
		out(&t);
	}
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
				arrd = 1;
				st = D_ARR;
				continue;
			}
			finish_decl();
			out(&cur);
			return 0;

		case D_ARR:
			out(&cur);
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
				out(&cur);
				pdepth = 0;
				idepth = 0;
				st = D_AINIT;
				continue;
			}
			if (cur.type == COMMA) {
				/* close this declaration, keep the type:
				 * char buf[12], *p  ->  two declarations */
				outt(SEMI);
				st = D_DECL;
				continue;
			}
			if (cur.type == SEMI) {
				outt(SEMI);
				finish_decl();
				return 1;
			}
			/* not a shape this splits: flush and step aside */
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
	if (t.type == BEGIN) {
		stmt(&t);
		return;
	}
	outt(BEGIN);
	stmt(&t);
	outt(END);
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
		for (;;) {
			pull(&u);
			if (u.type == E_O_F)
				return;
			if (u.type == END) {
				bdepth--;
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
 * Entry points
 */
void
norm_init(void (*upstream)(struct token *))
{
	up = upstream;
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
		if (t.type == STRUCT || t.type == UNION || t.type == ENUM) {
			inagg = 1;
			out(&t);
			continue;
		}
		if (t.type == BEGIN) {
			if (inagg) {
				inagg = 0;
				out(&t);
				aggrpass();
				continue;
			}
			stmt(&t);
			continue;
		}
		if (t.type != SYM)
			inagg = 0;
		out(&t);
	}
}
