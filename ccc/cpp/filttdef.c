/*
 * filttdef - dissolve typedefs.
 *
 * A typedef is a declarator with a hole where the name sits:
 *
 *	typedef int jmp_buf[5];		(specs: int,  hole: .[5])
 *	typedef char *cstring;		(specs: char, hole: *.)
 *	typedef int (*fp)();		(specs: int,  hole: (*.)())
 *
 * Using the name composes the use-site declarator INTO the hole:
 *
 *	jmp_buf env;	->  int env[5];
 *	jmp_buf *p;	->  int (*p)[5];	parens: postfix binds
 *						tighter than the star
 *	cstring x, y;	->  char *x, *y;	the hole distributes
 *	sizeof(cstring)	->  sizeof(char *)	empty use-site
 *	(cstring)v	->  (char *)v		casts are the same
 *
 * Every legal appearance of a typedef name is followed by a (possibly
 * abstract, possibly empty) declarator, so one uniform rule serves
 * declarations, parameters, members, casts and sizeof alike: emit the
 * specs, split what follows into (pre, name, post) around the first
 * identifier, and emit pre_t pre_u name post_u post_t - with parens
 * around the use-site part exactly when it begins with a star and the
 * hole has a postfix, which is the one case precedence would misread.
 *
 * Expansion recurses: a parameter list inside a declarator, an
 * initialiser between declarators and a struct body inside a typedef
 * can all hold further typedef names.  A "sink" is where expansion
 * output goes - the filter's pending buffer, or a token array that
 * is mid-collection.  The split arrays are indexed by recursion
 * depth, because an inner expansion must not clobber the halves an
 * outer one has collected and not yet emitted.
 *
 * This is the same move filtenum made on enum and filtctrl on loops:
 * the language pass1 has to parse loses a feature, and with this one
 * it loses its last reason to consult a symbol table while parsing -
 * after here, a declaration is recognisable from its leading token.
 *
 * Restrictions, checked loudly rather than silently mis-expanded:
 * typedefs are file scope, and a typedef of an unnamed struct needs
 * a tag.
 */
#include "cpp.h"
#include "lexeme.h"

extern void error(char *msg);

static void (*up)(struct token *);
static struct pendbuf pb;

/*
 * A dissolved typedef: the spec tokens and the two halves of the
 * declarator around the hole.  Tokens are flat copies; names inside
 * them are interned, so nothing here owns storage.
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
 * through a typedef inside a declarator of another typedef's use -
 * a parameter list, mostly - and one more for the K&R parameter
 * line a terminator can begin.  Three covers the tree; the fourth
 * gets an error instead of a scribble.
 */
#define MAXDEPTH 3
static struct tokarray pres[MAXDEPTH], posts[MAXDEPTH];
static unsigned char depth;
static struct tokarray spec_a;

static struct tdent *
tdfind(char *name)
{
	struct tdent *t;

	for (t = tdefs; t; t = t->next)
		if (t->name == name)	/* interned: pointer compare */
			return t;
	return 0;
}

/* a sink is the pend buffer (null) or a collection in progress */
static void
sink1(struct tokarray *sink, struct token *t)
{
	if (sink)
		tarr_push(sink, t);
	else
		pend_push(&pb, t);
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
 * Pull the next token, letting line housekeeping flow straight out -
 * NEWLINE and LINENO are not part of any declarator and must not be
 * captured into one, but dropping them would shift every line number
 * downstream.
 */
static struct token backtok;
static unsigned char haveback;

static void
pull(struct token *t)
{
	if (haveback) {
		haveback = 0;
		tokcpy(t, &backtok);
		return;
	}
	for (;;) {
		up(t);
		if (t->type != NEWLINE && t->type != LINENO)
			return;
		pend_push(&pb, t);
	}
}

static void expand(struct tdent *e, struct token *t,
    struct tokarray *sink);

/*
 * Collect one balanced ( ) or [ ] group into the sink, expanding
 * typedef names inside - a parameter list is the common case.  On
 * entry t holds the opener; on exit it holds the first token past
 * the closer.
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
			pull(t);
			if (!d)
				return;
			continue;
		} else if (t->type == SYM &&
		    (e = tdfind(t->v.name)) != 0) {
			expand(e, t, sink);
			/*
			 * The nested expansion sank its own
			 * terminator; account for it here if it
			 * closed a level.
			 */
			if (t->type == RBRACK || t->type == RPAR) {
				d--;
				if (!d) {
					pull(t);
					return;
				}
			}
			pull(t);
			continue;
		}
		sink1(sink, t);
		pull(t);
	}
}

/*
 * Split one (possibly abstract) declarator into pre / name / post,
 * expanding typedef names inside its groups.
 *
 *	pre   { '*' | '(' }*
 *	name  SYM, or absent (abstract)
 *	post  { ')'-closing-a-pre-paren | (...) | [...] }*
 *
 * The caller hands in the first token; the terminator comes back in
 * t.  Balanced-group collection means a K&R parameter line, an
 * initialiser, a function body and the enclosing cast paren all
 * stop the walk without being special cases.
 */
static void
splitdecl(struct token *t, struct tokarray *pre, struct token *name,
    struct tokarray *post)
{
	unsigned char pdepth = 0;

	tarr_reset(pre);
	tarr_reset(post);
	name->type = 0;

	/* the stream spells "*" STAR before the emitter, TIMES after -
	 * accept either so this never cares where it runs */
	while (t->type == STAR || t->type == TIMES || t->type == LPAR) {
		if (t->type == LPAR)
			pdepth++;
		tarr_push(pre, t);
		pull(t);
	}
	if (t->type == SYM) {
		if (!tdfind(t->v.name)) {
			tokcpy(name, t);
			pull(t);
		} else {
			/*
			 * A typedef name in the name position is one of
			 * two things: the declarator's NAME shadowing
			 * the typedef - "kind kind;", a member named
			 * after its own type - or the start of a fresh
			 * type line, K&R style: "time_t *tp;" right
			 * after a declarator.  What follows tells them
			 * apart: a star or another name means a type
			 * line; anything else means this IS the name.
			 */
			struct token peek;

			pull(&peek);
			if (peek.type == STAR || peek.type == TIMES ||
			    peek.type == SYM) {
				/* a type line: leave both for the
				 * caller, name stays empty */
				tokcpy(&backtok, &peek);
				haveback = 1;
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
			pull(t);
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
 * post_t.  The parens go in exactly when the use-site starts with
 * something prefix-ish and the hole continues with a postfix.
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
 * any typedef name met on the way - an initialiser can hold a cast
 * or a sizeof.  The terminator comes back in t, already sunk.
 */
static void
drain(struct token *t, struct tokarray *sink)
{
	unsigned char d = 0;
	struct tdent *e;

	for (;;) {
		pull(t);
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
 * declarators until the list ends.  A comma continues the list when
 * what follows looks like another declarator; a fresh type after
 * the comma is a parameter list's comma and is not ours.  On return
 * the terminator has been sunk and t holds it.
 */
static void
expand(struct tdent *e, struct token *t, struct tokarray *sink)
{
	struct token name;
	struct tokarray *pre, *post;
	int have = 0;

	if (depth >= MAXDEPTH) {
		error("typedefs nested too deep");
		return;
	}
	pre = &pres[depth];
	post = &posts[depth];
	depth++;

	sinkn(sink, e->spec, e->nspec);
	for (;;) {
		if (!have)
			pull(t);
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
			/*
			 * The terminator can itself be a typedef name:
			 * a K&R parameter line - "time_t *tp;" right
			 * after the declarator - begins with one.
			 */
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
		pull(t);
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
	depth--;
}

/*
 * The whole typedef declaration, "typedef" already consumed.
 * Nothing goes downstream except a struct body, which has to exist
 * exactly once whether the typedef is ever used or not.
 */
static void
capture(struct token *t)
{
	struct tdent *e;
	struct token name;

	tarr_reset(&spec_a);
	pull(t);

	/*
	 * specs: a struct/union head, keywords, or an earlier
	 * typedef.  struct/union first - token_props counts them as
	 * type keywords too, and the generic arm would eat the
	 * keyword and leave the tag looking like a name.
	 */
	for (;;) {
		if (t->type == STRUCT || t->type == UNION) {
			tarr_push(&spec_a, t);
			pull(t);
			if (t->type != SYM) {
				error("typedef of unnamed struct needs a tag");
				return;
			}
			tarr_push(&spec_a, t);
			pull(t);
			if (t->type == BEGIN) {
				/*
				 * The body is a real declaration of
				 * the tag and goes downstream once,
				 * here.  Member declarations through
				 * earlier typedefs expand on the way.
				 */
				unsigned char d = 1;
				struct tdent *m;

				pend_buf(&pb, spec_a.buf, spec_a.count);
				pend_push(&pb, t);
				pull(t);
				while (d) {
					if (t->type == BEGIN)
						d++;
					else if (t->type == END)
						d--;
					else if (t->type == SYM &&
					    (m = tdfind(t->v.name)) != 0) {
						expand(m, t, 0);
						continue;
					}
					pend_push(&pb, t);
					pull(t);
				}
				pend_tok(&pb, SEMI);
			}
			continue;
		}
		if (is_type_kw(t->type)) {
			tarr_push(&spec_a, t);
			pull(t);
			continue;
		}
		if (t->type == SYM && tdfind(t->v.name) != 0) {
			/*
			 * A typedef of a typedef.  The tree never writes
			 * one, so the composition machinery it would take
			 * is text this pass cannot afford.  Loud, not
			 * silent: the declaration is consumed so the
			 * error does not cascade.
			 */
			error("typedef of a typedef");
			while (t->type != SEMI && t->type != E_O_F)
				pull(t);
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
		e->spec = keep(&spec_a, &e->nspec);
		e->pre = keep(&pres[0], &e->npre);
		e->post = keep(&posts[0], &e->npost);
		e->next = tdefs;
		tdefs = e;

		if (t->type == COMMA) {
			pull(t);
			continue;
		}
		if (t->type != SEMI)
			error("junk in typedef");
		return;
	}
}

void
filttdef_init(void (*upstream)(struct token *))
{
	int i;

	up = upstream;
	pend_setup(&pb, 16);
	tarr_setup(&spec_a, 8);
	for (i = 0; i < MAXDEPTH; i++) {
		tarr_setup(&pres[i], 8);
		tarr_setup(&posts[i], 8);
	}
	tdefs = 0;
	depth = 0;
	haveback = 0;
}

void
filttdef(struct token *out)
{
	struct token t;
	struct tdent *e;
	static unsigned char aftertag;

	for (;;) {
		if (filt_entry(&pb, out, up, &t))
			return;

		if (t.type == TYPEDEF) {
			capture(&t);
			continue;	/* nothing but the pend */
		}

		if (t.type == SYM && !aftertag &&
		    (e = tdfind(t.v.name)) != 0) {
			expand(e, &t, 0);
			continue;
		}

		/*
		 * A tag position is a different namespace: "struct
		 * Expr" must not have its tag expanded even though
		 * Expr is also a typedef name.  Member access
		 * likewise.
		 */
		aftertag = (t.type == STRUCT || t.type == UNION ||
		    t.type == DOT || t.type == ARROW);

		tokcpy(out, &t);
		return;
	}
}
