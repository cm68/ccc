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

static int state = ST_NORMAL;
static int brace_depth = 0;
static int aggr_depth = 0;	/* Struct/union/enum brace depth */
static int paren_depth = 0;
static int init_depth = 0;
static int in_typedef = 0;
static int expect_tag = 0;	/* Expecting struct/union/enum tag name */
static int in_aggr_def = 0;	/* In struct/union/enum definition */
static char *typedef_name = 0;

#define DECL_MAX 32
static struct token decl_buf[DECL_MAX];
static int decl_len = 0;

#define NAME_MAX 16
static struct {
	char *name;
	int star_count;
} names[NAME_MAX];
static int name_count = 0;
static int cur_stars = 0;

#define INIT_MAX 32
static struct token init_buf[INIT_MAX];
static int init_len = 0;

#define ASSIGN_MAX 16
static struct {
	char *name;
	struct token *init;
	int init_len;
} assigns[ASSIGN_MAX];
static int assign_count = 0;

#define PEND_MAX 32
static struct token pendbuf[PEND_MAX];
static struct pendbuf pb;

static void (*upstream)(struct token *);

void
filtdecl_init(void (*up)(struct token *))
{
	upstream = up;
	state = ST_NORMAL;
	brace_depth = 0;
	aggr_depth = 0;
	decl_len = 0;
	name_count = 0;
	assign_count = 0;
	cur_stars = 0;
	in_typedef = 0;
	expect_tag = 0;
	in_aggr_def = 0;
	pend_init(&pb, pendbuf, PEND_MAX);
}

static void
emit_decl(void)
{
	int i, j;
	struct token tmp;
	struct token *ref = &decl_buf[0];  /* Reference for line info */

	pend_buf(&pb, decl_buf, decl_len);

	for (i = 0; i < name_count; i++) {
		for (j = 0; j < names[i].star_count; j++)
			pend_tok_at(&pb, STAR, ref);
		/* Synthesize name token with proper line info */
		tmp.type = SYM;
		tmp.v.name = names[i].name;
		tmp.lineno = ref->lineno;
		tmp.filename = ref->filename;
		pend_push(&pb, &tmp);
		if (i < name_count - 1)
			pend_tok_at(&pb, COMMA, ref);
	}
	pend_tok_at(&pb, SEMI, ref);
}

static void
emit_assigns(void)
{
	int i;
	struct token tmp;
	struct token *ref;

	for (i = 0; i < assign_count; i++) {
		/* Use first init token for line info */
		ref = assigns[i].init_len > 0 ? &assigns[i].init[0] : &decl_buf[0];
		/* Synthesize name token with proper line info */
		tmp.type = SYM;
		tmp.v.name = assigns[i].name;
		tmp.lineno = ref->lineno;
		tmp.filename = ref->filename;
		pend_push(&pb, &tmp);
		pend_tok_at(&pb, ASSIGN, ref);
		pend_buf(&pb, assigns[i].init, assigns[i].init_len);
		pend_tok_at(&pb, SEMI, ref);
		free(assigns[i].init);
	}
	assign_count = 0;
}

static void
save_init(char *name)
{
	if (assign_count < ASSIGN_MAX && init_len > 0) {
		int i;
		assigns[assign_count].name = name;
		assigns[assign_count].init = malloc(init_len * sizeof(struct token));
		for (i = 0; i < init_len; i++)
			tokcpy(&assigns[assign_count].init[i], &init_buf[i]);
		assigns[assign_count].init_len = init_len;
		assign_count++;
	}
	init_len = 0;
}

static void
save_name(char *name)
{
	if (name_count < NAME_MAX) {
		names[name_count].name = name;
		names[name_count].star_count = cur_stars;
		name_count++;
	}
	cur_stars = 0;
}

static void
finish_decl(void)
{
	if (name_count > 0) {
		emit_decl();
		emit_assigns();
	}
	decl_len = 0;
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
		if (in_aggr_def || aggr_depth > 0) {
			/* Inside struct/union/enum definition (or nested aggregate) */
			aggr_depth++;
			in_aggr_def = 0;
			tokcpy(out, &t);
			return;
		}
		if (state == ST_DECL || state == ST_NAME) {
			/* Check if this is start of aggregate definition */
			for (i = 0; i < decl_len; i++) {
				if (decl_buf[i].type == STRUCT ||
				    decl_buf[i].type == UNION ||
				    decl_buf[i].type == ENUM) {
					aggr_depth++;
					break;
				}
			}
			/* Push type tokens first, then BEGIN, so order is correct */
			pend_buf(&pb, decl_buf, decl_len);
			pend_push(&pb, &t);  /* BEGIN after type */
			decl_len = 0;
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
			tokcpy(&decl_buf[decl_len++], &t);
			state = ST_DECL;
			filtdecl(out);
			return;
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
				if (decl_len < DECL_MAX)
					tokcpy(&decl_buf[decl_len++], &t);
			}
			filtdecl(out);
			return;
		}
		if (t.type == SYM) {
			if (expect_tag) {
				/* Tag name - part of type, not variable name */
				if (decl_len < DECL_MAX)
					tokcpy(&decl_buf[decl_len++], &t);
				expect_tag = 0;
				filtdecl(out);
				return;
			}
			save_name(t.v.name);
			state = ST_NAME;
			filtdecl(out);
			return;
		}
		/* Not a valid declaration - flush type, stars, and current token */
		pend_buf(&pb, decl_buf, decl_len);
		/* Emit accumulated stars (e.g., for casts like (int *)) */
		for (i = 0; i < cur_stars; i++)
			pend_tok(&pb, STAR);
		pend_push(&pb, &t);
		decl_len = 0;
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
			init_len = 0;
			filtdecl(out);
			return;
		}
		if (t.type == COMMA) {
			state = ST_DECL;
			filtdecl(out);
			return;
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
		if (t.type == LBRACK) {
			struct token tmp;
			struct token *ref = decl_len > 0 ? &decl_buf[0] : &t;
			/* Emit type first, then name, then array bracket */
			pend_buf(&pb, decl_buf, decl_len);
			/* Emit stars for this variable */
			for (i = 0; i < names[name_count-1].star_count; i++)
				pend_tok_at(&pb, STAR, ref);
			tmp.type = SYM;
			tmp.v.name = names[name_count-1].name;
			tmp.lineno = ref->lineno;
			tmp.filename = ref->filename;
			pend_push(&pb, &tmp);
			pend_push(&pb, &t);
			name_count--;
			decl_len = 0;
			state = ST_NORMAL;
			pend_pop(&pb, out);
			return;
		}
		finish_decl();
		pend_push(&pb, &t);
		pend_pop(&pb, out);
		return;

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
			filtdecl(out);
			return;
		}
		if (t.type == SEMI && paren_depth == 0 && init_depth == 0) {
			save_init(names[name_count-1].name);
			finish_decl();
			pend_pop(&pb, out);
			return;
		}
		if (init_len < INIT_MAX)
			tokcpy(&init_buf[init_len++], &t);
		filtdecl(out);
		return;
	}

	tokcpy(out, &t);
}
