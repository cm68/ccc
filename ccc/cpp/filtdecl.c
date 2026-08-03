/*
 * filtdecl.c - Local declaration initializer separation filter
 *
 * Separates initializers from local variable declarations:
 *   int x = 5, y = foo();
 * Becomes:
 *   int x, y;
 *   x = 5;
 *   y = foo();
 */

#include "cpp.h"
#include "lexeme.h"
#include <string.h>

/* External typedef table */
extern void addTypedef(char *name);

/* States */
#define ST_NORMAL   0
#define ST_DECL     1
#define ST_NAME     2
#define ST_INIT     3
#define ST_ARR      4	/* inside an array declarator's brackets */
#define ST_ARRE     5	/* just past the closing bracket */
#define ST_AINIT    6	/* an array's inline initializer */

static int state = ST_NORMAL;
static int brace_depth = 0;
static int aggr_depth = 0;	/* Struct/union/enum brace depth */
static int paren_depth = 0;
static int init_depth = 0;
static int arr_depth = 0;	/* bracket nesting in ST_ARR */
static int in_typedef = 0;
static int expect_tag = 0;	/* Expecting struct/union/enum tag name */
static int in_aggr_def = 0;	/* In struct/union/enum definition */
static char *typedef_name = 0;

/* Declaration buffer - dynamically allocated */
static struct tokarray decl_arr;

#define NAME_MAX 16
struct dname {
	char *name;
	char star_count;
};
static struct dname names[NAME_MAX];
static unsigned char name_count = 0;
static unsigned char cur_stars = 0;

/* Initializer buffer - dynamically allocated */
static struct tokarray init_arr;

#define ASSIGN_MAX 16
struct dinit {
	char *name;
	struct token *init;
	int init_len;
};
static struct dinit assigns[ASSIGN_MAX];
static unsigned char assign_count = 0;

/* Pending token queue - dynamically allocated */
static struct pendbuf pb;

static void (*upstream)(struct token *);

void
filtdecl_init(void (*up)(struct token *))
{
	upstream = up;
	state = ST_NORMAL;
	brace_depth = 0;
	aggr_depth = 0;
	name_count = 0;
	assign_count = 0;
	cur_stars = 0;
	in_typedef = 0;
	expect_tag = 0;
	in_aggr_def = 0;
	tarr_setup(&decl_arr, 16);
	tarr_setup(&init_arr, 32);
	pend_setup(&pb, 16);
}

/*
 * Does this declaration say static?
 *
 * It decides whether the initializer may be split off into an
 * assignment.  A local's initializer is normally lowered that way -
 * pass1 has no code path to run one, and a plain local is initialized
 * every time the block is entered anyway, so an assignment is exactly
 * what it means.  A static is not: its initializer is data, written
 * once before the program runs, and lowering it to an assignment made
 * it run on every call.  "static int n = 5; n++;" returned 6 for ever.
 *
 * An aggregate initializer is already left alone here, which is why a
 * static array was right and a static scalar was not.
 */
static int
decl_is_static(void)
{
	register struct token *tp = decl_arr.buf;
	int n = decl_arr.count;

	while (n--) {
		if (tp->type == STATIC)
			return 1;
		tp++;
	}
	return 0;
}

/*
 * The initializer saved for this name, or 0.
 */
static struct dinit *
init_for(char *name)
{
	register struct dinit *ap = assigns;
	unsigned char n = assign_count + 1;

	while (--n) {
		if (ap->name == name)
			return ap;
		ap++;
	}
	return 0;
}

static void
emit_decl(void)
{
	register struct dname *np;
	unsigned char n;
	struct dinit *ap;
	int j;
	int keep = decl_is_static();
	struct token tmp;
	struct token *ref = &decl_arr.buf[0];  /* Reference for line info */

	pend_buf(&pb, decl_arr.buf, decl_arr.count);

	np = names;
	n = name_count + 1;
	while (--n) {
		for (j = np->star_count; j > 0; j--)
			pend_tok_at(&pb, STAR, ref);
		/* Synthesize name token with proper line info */
		tmp.type = SYM;
		tmp.v.name = np->name;
		tmp.lineno = ref->lineno;
		tmp.filename = ref->filename;
		pend_push(&pb, &tmp);
		if (keep && (ap = init_for(np->name))) {
			pend_tok_at(&pb, ASSIGN, ref);
			pend_buf(&pb, ap->init, ap->init_len);
		}
		if (n > 1)
			pend_tok_at(&pb, COMMA, ref);
		np++;
	}
	pend_tok_at(&pb, SEMI, ref);
}

static void
emit_assigns(void)
{
	register struct dinit *ap;
	unsigned char n;
	struct token tmp;
	struct token *ref;

	ap = assigns;
	n = assign_count + 1;
	while (--n) {
		/* Use first init token for line info */
		ref = ap->init_len > 0 ? &ap->init[0] : &decl_arr.buf[0];
		/* Synthesize name token with proper line info */
		tmp.type = SYM;
		tmp.v.name = ap->name;
		tmp.lineno = ref->lineno;
		tmp.filename = ref->filename;
		pend_push(&pb, &tmp);
		pend_tok_at(&pb, ASSIGN, ref);
		pend_buf(&pb, ap->init, ap->init_len);
		pend_tok_at(&pb, SEMI, ref);
		free(ap->init);
		ap++;
	}
	assign_count = 0;
}

/*
 * Throw away what emit_decl has already emitted inline.
 */
static void
drop_assigns(void)
{
	register struct dinit *ap = assigns;
	unsigned char n = assign_count + 1;

	while (--n)
		free((ap++)->init);
	assign_count = 0;
}

static void
save_init(char *name)
{
	if (assign_count < ASSIGN_MAX && init_arr.count > 0) {
		register struct token *d;
		struct token *s;
		struct dinit *ap;
		int n;

		n = init_arr.count;
		ap = &assigns[assign_count++];
		ap->name = name;
		d = ap->init = (struct token *)xalloc(n * sizeof(struct token));
		s = init_arr.buf;
		while (n--)
			tokcpy(d++, s++);
		ap->init_len = init_arr.count;
	}
	tarr_reset(&init_arr);
}

static void
save_name(char *name)
{
	if (name_count < NAME_MAX) {
		register struct dname *np = &names[name_count++];

		np->name = name;
		np->star_count = cur_stars;
	}
	cur_stars = 0;
}

static void
finish_decl(void)
{
	if (name_count > 0) {
		int keep = decl_is_static();

		emit_decl();
		if (keep)
			drop_assigns();
		else
			emit_assigns();
	}
	tarr_reset(&decl_arr);
	name_count = 0;
	cur_stars = 0;
	expect_tag = 0;
	state = ST_NORMAL;
}

void
filtdecl(struct token *out)
{
	struct token t;
	int i;

restart:
#ifdef DEBUG
	if (pend_has(&pb) && VERBOSE(V_FILTER))
		fdprintf(2, "filtdecl: pop type=%d\n", pb.buf[pb.rd].type);
#endif
	if (filt_entry(&pb, out, upstream, &t))
		return;
#ifdef DEBUG
	if (VERBOSE(V_FILTER))
		fdprintf(2, "filtdecl: up type=%d st=%d bd=%d ad=%d\n", t.type, state, brace_depth, aggr_depth);
#endif

	if (t.type == BEGIN) {
		brace_depth++;
		if (state == ST_AINIT) {
			/* an array initializer's braces belong to the state
			 * machine, not to the block tracking - this intercept
			 * was emitting them straight to out, ahead of the
			 * tokens pending behind them */
			init_depth++;
			pend_push(&pb, &t);
			goto restart;
		}
		if (in_aggr_def || aggr_depth > 0) {
			/* Inside struct/union/enum definition (or nested aggregate) */
			aggr_depth++;
			in_aggr_def = 0;
			tokcpy(out, &t);
			return;
		}
		if (state == ST_DECL || state == ST_NAME) {
			/* Check if this is start of aggregate definition */
			register struct token *tp = decl_arr.buf;
			int n = decl_arr.count;
			while (n--) {
				if (tp->type == STRUCT ||
				    tp->type == UNION ||
				    tp->type == ENUM) {
					aggr_depth++;
					break;
				}
				tp++;
			}
			/* Push type tokens first, then BEGIN, so order is correct */
			pend_buf(&pb, decl_arr.buf, decl_arr.count);
			pend_push(&pb, &t);  /* BEGIN after type */
			tarr_reset(&decl_arr);
			name_count = 0;
			cur_stars = 0;
			state = ST_NORMAL;
			pend_pop(&pb, out);
			return;
		}
		tokcpy(out, &t);
		return;
	}
	if (t.type == END) {
		brace_depth--;
		if (state == ST_AINIT) {
			init_depth--;
			pend_push(&pb, &t);
			goto restart;
		}
		if (aggr_depth > 0)
			aggr_depth--;
		tokcpy(out, &t);
		return;
	}

	/* Inside aggregate definition - pass through unchanged */
	if (aggr_depth > 0) {
		tokcpy(out, &t);
		return;
	}

	if (brace_depth == 0) {
		if (t.type == TYPEDEF) {
			in_typedef = 1;
			typedef_name = 0;
		} else if (in_typedef && t.type == SYM) {
			typedef_name = t.v.name;
		} else if (in_typedef && t.type == SEMI) {
			if (typedef_name)
				addTypedef(typedef_name);
			in_typedef = 0;
			typedef_name = 0;
		}
		/* Track start of struct/union/enum definition */
		if (t.type == STRUCT || t.type == UNION || t.type == ENUM)
			in_aggr_def = 1;
		else if (in_aggr_def && t.type != SYM && t.type != BEGIN)
			/* Not a definition - just a type reference like "struct foo *p" */
			in_aggr_def = 0;
		tokcpy(out, &t);
		return;
	}

	switch (state) {
	case ST_NORMAL:
		if (is_type_tok(&t)) {
			in_typedef = (t.type == TYPEDEF);
			expect_tag = (t.type == STRUCT || t.type == UNION ||
			              t.type == ENUM);
			tarr_push(&decl_arr, &t);
			state = ST_DECL;
			goto restart;
		}
		break;

	case ST_DECL:
		if (is_type_tok(&t) || t.type == STAR) {
			if (t.type == STAR) {
				cur_stars++;
				expect_tag = 0;
			} else {
				/* Set expect_tag for struct/union/enum */
				expect_tag = (t.type == STRUCT || t.type == UNION ||
				              t.type == ENUM);
				tarr_push(&decl_arr, &t);
			}
			goto restart;
		}
		if (t.type == SYM) {
			if (expect_tag) {
				/* Tag name - part of type, not variable name */
				tarr_push(&decl_arr, &t);
				expect_tag = 0;
				goto restart;
			}
			save_name(t.v.name);
			state = ST_NAME;
			goto restart;
		}
		/* Not a valid declaration - flush type, stars, and current token */
		pend_buf(&pb, decl_arr.buf, decl_arr.count);
		/* Emit accumulated stars (e.g., for casts like (int *)) */
		for (i = 0; i < cur_stars; i++)
			pend_tok(&pb, STAR);
		pend_push(&pb, &t);
		tarr_reset(&decl_arr);
		cur_stars = 0;
		expect_tag = 0;
		state = ST_NORMAL;
		pend_pop(&pb, out);
		return;

	case ST_NAME:
		if (t.type == ASSIGN) {
			state = ST_INIT;
			paren_depth = 0;
			init_depth = 0;
			tarr_reset(&init_arr);
			goto restart;
		}
		if (t.type == COMMA) {
			state = ST_DECL;
			goto restart;
		}
		if (t.type == SEMI) {
			finish_decl();
			pend_pop(&pb, out);
			return;
		}
		if (t.type == STAR) {
			tokcpy(out, &t);
			return;
		}
		if (t.type == LPAR) {
			struct token tmp;
			struct dname *np = &names[name_count-1];
			struct token *ref = decl_arr.count > 0 ? &decl_arr.buf[0] : &t;
			/* Function declarator (extern short f(), g();):
			 * no initializer possible - emit type, stars, name,
			 * paren; the rest of the declaration flows through
			 * unchanged */
			pend_buf(&pb, decl_arr.buf, decl_arr.count);
			for (i = np->star_count; i > 0; i--)
				pend_tok_at(&pb, STAR, ref);
			tmp.type = SYM;
			tmp.v.name = np->name;
			tmp.lineno = ref->lineno;
			tmp.filename = ref->filename;
			pend_push(&pb, &tmp);
			pend_push(&pb, &t);
			name_count--;
			tarr_reset(&decl_arr);
			state = ST_NORMAL;
			pend_pop(&pb, out);
			return;
		}
		if (t.type == LBRACK) {
			struct token tmp;
			struct dname *np = &names[name_count-1];
			struct token *ref = decl_arr.count > 0 ? &decl_arr.buf[0] : &t;
			/* Emit type first, then name, then array bracket */
			pend_buf(&pb, decl_arr.buf, decl_arr.count);
			/* Emit stars for this variable */
			for (i = np->star_count; i > 0; i--)
				pend_tok_at(&pb, STAR, ref);
			tmp.type = SYM;
			tmp.v.name = np->name;
			tmp.lineno = ref->lineno;
			tmp.filename = ref->filename;
			pend_push(&pb, &tmp);
			pend_push(&pb, &t);
			name_count--;
			/*
			 * NOT the end of the declaration.  This used to reset
			 * to ST_NORMAL here, so everything after the array -
			 * ", *p = buf + 11 ;" - flowed through untouched and
			 * the initializer reached pass1 unsplit, which drops
			 * an auto initializer on the floor.  c1's own outd()
			 * is that exact line: the self-build's c1 printed
			 * every number as an empty string.  The type is kept
			 * so declarators after the array still split.
			 */
			arr_depth = 1;
			state = ST_ARR;
			goto restart;
		}
		finish_decl();
		pend_thru(&pb, &t, out);
		return;

	case ST_ARR:
		/* the dimension flows through verbatim */
		pend_push(&pb, &t);
		if (t.type == LBRACK)
			arr_depth++;
		else if (t.type == RBRACK && --arr_depth == 0)
			state = ST_ARRE;
		goto restart;

	case ST_ARRE:
		if (t.type == LBRACK) {		/* next dimension */
			pend_push(&pb, &t);
			arr_depth = 1;
			state = ST_ARR;
			goto restart;
		}
		if (t.type == ASSIGN) {
			/*
			 * An array initializer stays inline: an aggregate
			 * cannot be split into an assignment, and the only
			 * legal homes for one - statics and file scope -
			 * want it inline anyway.
			 */
			pend_push(&pb, &t);
			paren_depth = 0;
			init_depth = 0;
			state = ST_AINIT;
			goto restart;
		}
		if (t.type == COMMA) {
			/* close the array's declaration and keep going with
			 * the same type: "char buf[12], *p" becomes
			 * "char buf[12]; char *p" */
			pend_tok(&pb, SEMI);
			state = ST_DECL;
			goto restart;
		}
		if (t.type == SEMI) {
			pend_tok(&pb, SEMI);
			finish_decl();
			pend_pop(&pb, out);
			return;
		}
		/* not a shape this filter splits: flush and step aside */
		pend_push(&pb, &t);
		tarr_reset(&decl_arr);
		name_count = 0;
		state = ST_NORMAL;
		pend_pop(&pb, out);
		return;

	case ST_AINIT:
		/* braces are counted at the entry intercept, which owns
		 * BEGIN and END for every state */
		if (t.type == LPAR)
			paren_depth++;
		else if (t.type == RPAR)
			paren_depth--;
		if (t.type == COMMA && init_depth == 0 && paren_depth == 0) {
			pend_tok(&pb, SEMI);
			state = ST_DECL;
			goto restart;
		}
		if (t.type == SEMI && init_depth == 0 && paren_depth == 0) {
			pend_tok(&pb, SEMI);
			finish_decl();
			pend_pop(&pb, out);
			return;
		}
		pend_push(&pb, &t);
		goto restart;

	case ST_INIT:
		if (t.type == LPAR)
			paren_depth++;
		else if (t.type == RPAR)
			paren_depth--;
		else if (t.type == BEGIN)
			init_depth++;
		else if (t.type == END)
			init_depth--;

		if (t.type == COMMA && paren_depth == 0 && init_depth == 0) {
			save_init(names[name_count-1].name);
			state = ST_DECL;
			goto restart;
		}
		if (t.type == SEMI && paren_depth == 0 && init_depth == 0) {
			save_init(names[name_count-1].name);
			finish_decl();
			pend_pop(&pb, out);
			return;
		}
		tarr_push(&init_arr, &t);
		goto restart;
	}

	tokcpy(out, &t);
}
