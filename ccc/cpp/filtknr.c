/*
 * filtknr.c - K&R function declaration normalization filter
 *
 * Transforms K&R style function definitions:
 *   int foo(a, b)
 *   int a;
 *   char *b;
 *   {
 *
 * Into ANSI style:
 *   int foo(int a, char *b)
 *   {
 *
 * Pipeline: lex -> filtknr -> filtdecl -> filtbrace -> filtctrl -> emit
 */

#include "cpp.h"
#include "lexeme.h"
#include <string.h>

/* States */
#define ST_NORMAL   0	/* Looking for function start */
#define ST_RTYPE    1	/* Buffering return type */
#define ST_NAME     2	/* Saw function name */
#define ST_PARAMS   3	/* Inside parameter list () */
#define ST_PDECL    4	/* Reading parameter declarations */
#define ST_PTYPE    5	/* Buffering param type tokens */

static int state = ST_NORMAL;
static int paren_depth = 0;
static int brace_depth = 0;  /* Track nesting - only parse K&R at file scope */

/* Return type buffer - dynamically allocated */
static struct tokarray rtype_arr;

/* Function name */
static struct token func_name;

/* Saved LPAR token for proper line numbers */
static struct token saved_lpar;

/* Parameter names from () */
#define PARAM_MAX 10
static struct {
	char *name;
	struct token *type;	/* Type tokens for this param */
	int type_len;
	int stars;		/* Pointer depth */
} params[PARAM_MAX];
static int param_count = 0;

/* Current param type being parsed - dynamically allocated */
static struct tokarray ptype_arr;
static int ptype_stars = 0;

/* Output queue - dynamically allocated */
static struct pendbuf pb;

/* Current param name being typed */
static char *cur_pname = 0;

static void (*upstream)(struct token *);

void
filtknr_init(void (*up)(struct token *))
{
#ifdef DEBUG
	extern short verbose;
	extern int fdprintf(int, char *, ...);
	if (verbose & V_FILTER)
		fdprintf(2, "filtknr_init: &rtype_arr.len=%p\n", (void*)&rtype_arr.len);
#endif
	upstream = up;
	state = ST_NORMAL;
	brace_depth = 0;
	param_count = 0;
	cur_pname = 0;
	/* Initialize dynamic buffers (first call only) */
	if (!rtype_arr.buf) {
		tarr_init(&rtype_arr, 16);
		tarr_init(&ptype_arr, 16);
		pend_init(&pb, 16);
	} else {
		tarr_reset(&rtype_arr);
		tarr_reset(&ptype_arr);
	}
}

/*
 * Find param index by name, -1 if not found
 */
static int
find_param(char *name)
{
	int i;
	for (i = 0; i < param_count; i++)
		if (strcmp(params[i].name, name) == 0)
			return i;
	return -1;
}

/*
 * Save current type to a parameter
 */
static void
save_ptype(char *name, int stars)
{
	int idx = find_param(name);
	if (idx >= 0 && ptype_arr.len > 0) {
		int i;
		params[idx].type = malloc(ptype_arr.len * sizeof(struct token));
		for (i = 0; i < ptype_arr.len; i++)
			tokcpy(&params[idx].type[i], &ptype_arr.buf[i]);
		params[idx].type_len = ptype_arr.len;
		params[idx].stars = stars;
	}
	tarr_reset(&ptype_arr);
	ptype_stars = 0;
}

/*
 * Emit the merged ANSI-style declaration
 */
static void
emit_ansi(void)
{
	int i, j;
	struct token tmp;

	/* Emit return type */
	pend_buf(&pb, rtype_arr.buf, rtype_arr.len);

	/* Emit function name */
	pend_push(&pb, &func_name);

	/* Emit ( - use func_name line info for synthesized tokens */
	pend_tok_at(&pb, LPAR, &func_name);

	/* Emit params with types */
	for (i = 0; i < param_count; i++) {
		if (params[i].type_len > 0) {
			/* Type tokens */
			pend_buf(&pb, params[i].type, params[i].type_len);
		} else {
			/* K&R default: untyped params are int */
			pend_tok_at(&pb, INT, &func_name);
		}
		/* Stars */
		for (j = 0; j < params[i].stars; j++)
			pend_tok_at(&pb, TIMES, &func_name);
		/* Name */
		tmp.type = SYM;
		tmp.v.name = params[i].name;
		tmp.lineno = func_name.lineno;
		tmp.filename = func_name.filename;
		pend_push(&pb, &tmp);
		/* Comma if not last */
		if (i < param_count - 1)
			pend_tok_at(&pb, COMMA, &func_name);
		/* Free type buffer */
		if (params[i].type) {
			free(params[i].type);
			params[i].type = 0;
		}
	}

	/* Emit ) */
	pend_tok_at(&pb, RPAR, &func_name);
}

/*
 * Abort K&R parsing - emit buffered tokens as-is, let rest flow through
 */
static void
abort_knr(void)
{
	int i;
	struct token tmp;

	pend_buf(&pb, rtype_arr.buf, rtype_arr.len);
	pend_push(&pb, &func_name);
	pend_push(&pb, &saved_lpar);  /* Use saved LPAR with correct line info */
	/* Emit any param names we collected (with commas) */
	for (i = 0; i < param_count; i++) {
		toksynthnam(&tmp, SYM, params[i].name);
		pend_push(&pb, &tmp);
		pend_tok(&pb, COMMA);
	}
	/* Don't emit RPAR - let the rest of input flow through */

	tarr_reset(&rtype_arr);
	param_count = 0;
	state = ST_NORMAL;
}

/*
 * Reset state for new function
 */
static void
reset_state(void)
{
	int i;
	for (i = 0; i < param_count; i++) {
		if (params[i].type)
			free(params[i].type);
		params[i].type = 0;
	}
	tarr_reset(&rtype_arr);
	param_count = 0;
	tarr_reset(&ptype_arr);
	state = ST_NORMAL;
}

void
filtknr(struct token *out)
{
	struct token t;

#ifdef DEBUG
	if (pend_has(&pb) && VERBOSE(V_FILTER))
		fdprintf(2, "filtknr: pop type=%d rd=%d wr=%d\n",
			pb.buf[pb.rd].type, pb.rd, pb.wr);
#endif
	if (filt_entry(&pb, out, upstream, &t))
		return;
#ifdef DEBUG
	if (VERBOSE(V_FILTER))
		fdprintf(2, "filtknr: up type=%d state=%d\n", t.type, state);
#endif

	/* Track brace depth - only parse K&R at file scope */
	if (t.type == BEGIN)
		brace_depth++;
	else if (t.type == END)
		brace_depth--;

	switch (state) {
	case ST_NORMAL:
		/* Only look for K&R functions at file scope (brace_depth == 0) */
		if (brace_depth == 0 && is_type_tok(&t)) {
#ifdef DEBUG
			if (VERBOSE(V_FILTER))
				fdprintf(2, "filtknr: ST_NORMAL &rtype_arr.len=%p buf type=%d at %d\n",
					(void*)&rtype_arr.len, t.type, rtype_arr.len);
#endif
			tarr_push(&rtype_arr, &t);
#ifdef DEBUG
			if (VERBOSE(V_FILTER))
				fdprintf(2, "filtknr: after buf rtype_arr.len=%d\n", rtype_arr.len);
#endif
			state = ST_RTYPE;
			filtknr(out);
			return;
		}
		break;

	case ST_RTYPE:
		/* Buffering return type, look for function name */
		/* Note: lexer produces TIMES (42) for *, not STAR (36) */
		if (is_type_tok(&t) || t.type == STAR || t.type == TIMES) {
#ifdef DEBUG
			if (VERBOSE(V_FILTER))
				fdprintf(2, "filtknr: ST_RTYPE buf type=%d at %d\n",
					t.type, rtype_arr.len);
#endif
			tarr_push(&rtype_arr, &t);
			filtknr(out);
			return;
		}
		if (t.type == SYM) {
			/* Could be function name */
#ifdef DEBUG
			if (VERBOSE(V_FILTER))
				fdprintf(2, "filtknr: ST_RTYPE SYM rtype_arr.len=%d\n", rtype_arr.len);
#endif
			tokcpy(&func_name, &t);
			state = ST_NAME;
			filtknr(out);
			return;
		}
		/* Not a function - emit buffered and this token */
		pend_buf(&pb, rtype_arr.buf, rtype_arr.len);
		pend_push(&pb, &t);
		tarr_reset(&rtype_arr);
		state = ST_NORMAL;
		pend_pop(&pb, out);
		return;

	case ST_NAME:
		/* After potential function name, expect ( */
		if (t.type == LPAR) {
			tokcpy(&saved_lpar, &t);  /* Save for abort_knr */
			state = ST_PARAMS;
			paren_depth = 1;
			param_count = 0;
			filtknr(out);
			return;
		}
		/* Not a function - emit return type, name, and this token */
#ifdef DEBUG
		if (VERBOSE(V_FILTER))
			fdprintf(2, "filtknr: ST_NAME &rtype_arr.len=%p rtype_arr.len=%d func_name.type=%d\n",
				(void*)&rtype_arr.len, rtype_arr.len, func_name.type);
#endif
		pend_buf(&pb, rtype_arr.buf, rtype_arr.len);
		pend_push(&pb, &func_name);
		pend_push(&pb, &t);
		tarr_reset(&rtype_arr);
		state = ST_NORMAL;
		pend_pop(&pb, out);
		return;

	case ST_PARAMS:
		/* Inside parameter list - collect names only */
		if (t.type == RPAR) {
			paren_depth--;
			if (paren_depth == 0) {
				/* End of param list - check what follows */
				state = ST_PDECL;
				filtknr(out);
				return;
			}
		} else if (t.type == LPAR) {
			paren_depth++;
		} else if (t.type != COMMA) {
			/* Type token in params = ANSI style, abort */
			if (is_type_tok(&t)) {
				abort_knr();
				pend_push(&pb, &t);
				pend_pop(&pb, out);
				return;
			}
			/* Plain SYM (not typedef) = parameter name */
			if (t.type == SYM && param_count < PARAM_MAX) {
				params[param_count].name = t.v.name;
				params[param_count].type = 0;
				params[param_count].type_len = 0;
				params[param_count].stars = 0;
				param_count++;
			}
		}
		/* Consume commas silently */
		filtknr(out);
		return;

	case ST_PDECL:
		/* After ), look for param type declarations or { */
		if (t.type == BEGIN) {
			/* Function body - emit merged declaration */
			emit_ansi();
			pend_push(&pb, &t);
			reset_state();
			pend_pop(&pb, out);
			return;
		}
		if (t.type == SEMI) {
			if (ptype_arr.len == 0 && cur_pname == 0) {
				/* No K&R declarations seen - this is a prototype */
				emit_ansi();
				pend_push(&pb, &t);
				reset_state();
				pend_pop(&pb, out);
				return;
			}
			/* End of a K&R param type declaration */
			if (cur_pname)
				save_ptype(cur_pname, ptype_stars);
			cur_pname = 0;
			tarr_reset(&ptype_arr);  /* Reset for next declaration */
			filtknr(out);
			return;
		}
		if (is_type_tok(&t)) {
			/* Start of param type declaration */
			tarr_push(&ptype_arr, &t);
			state = ST_PTYPE;
			filtknr(out);
			return;
		}
		/* Unexpected - abort */
		abort_knr();
		pend_push(&pb, &t);
		pend_pop(&pb, out);
		return;

	case ST_PTYPE:
		/* Buffering param type, may have seen a name (cur_pname) */
		if (is_type_tok(&t)) {
			tarr_push(&ptype_arr, &t);
			filtknr(out);
			return;
		}
		/* Note: lexer produces TIMES for * */
		if (t.type == STAR || t.type == TIMES) {
			ptype_stars++;
			filtknr(out);
			return;
		}
		if (t.type == SYM) {
			/* Param name - remember it, stay in ST_PTYPE for ,/; */
			cur_pname = t.v.name;
			filtknr(out);
			return;
		}
		if (t.type == COMMA) {
			/* Multiple params with same type: int a, b; */
			if (cur_pname)
				save_ptype(cur_pname, ptype_stars);
			cur_pname = 0;
			/* Reset stars for next param but keep type */
			ptype_stars = 0;
			filtknr(out);
			return;
		}
		if (t.type == SEMI) {
			/* End of declaration */
			if (cur_pname)
				save_ptype(cur_pname, ptype_stars);
			cur_pname = 0;
			state = ST_PDECL;
			filtknr(out);
			return;
		}
		/* Unexpected */
		abort_knr();
		pend_push(&pb, &t);
		pend_pop(&pb, out);
		return;
	}

	tokcpy(out, &t);
}
