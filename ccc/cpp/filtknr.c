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
#define ST_TAIL     6	/* Declarator tail after params: `)) (args)` */

static int state = ST_NORMAL;
static int paren_depth = 0;
static int brace_depth = 0;  /* Track nesting - only parse K&R at file scope */

/* Return type buffer - dynamically allocated */
static struct tokarray rtype_arr;

/* Function name */
static struct token func_name;

/* Saved LPAR token for proper line numbers */
static struct token saved_lpar;

/*
 * Parenthesized declarators - void (*signal(sig, act))(int):
 * `( *` prefix parens are buffered into rtype_arr (pre_open counts the
 * unclosed ones); after the parameter list the remaining `))(int)` is
 * collected into tail_arr and re-emitted after the converted params.
 */
static int pre_open = 0;
static struct tokarray tail_arr;

/*
 * Parameter names from ().  A K&R header names them all before any of
 * them is typed, so they have to be held until the declarations that
 * follow arrive - this is not a bound on how many a function may have,
 * only on how many can be waiting at once, but it is the same number.
 * Ten was under what the tools in this tree already use.
 */
#define PARAM_MAX 24
static struct {
	char *name;
	struct token *type;	/* Type tokens for this param */
	int type_len;
	int stars;		/* Pointer depth */
	struct token *post;	/* fn-ptr declarator tail: `)(int)` */
	int post_len;
} params[PARAM_MAX];
static int param_count = 0;

/* Current param type being parsed - dynamically allocated */
static struct tokarray ptype_arr;
static int ptype_stars = 0;
/* fn-ptr K&R param decls (void (*action)(int);): paren depth within
 * the declarator, and the tokens following the name */
static int pdepth = 0;
static struct tokarray ptail_arr;

/* Output queue - dynamically allocated */
static struct pendbuf pb;

/* Current param name being typed */
static char *cur_pname = 0;

/* Mid declarator-list at file scope (after a top-level comma): later
 * declarators share the first one's base type, so never synthesize an
 * implicit int for them */
static int dlist = 0;

static void (*upstream)(struct token *);

void
filtknr_init(void (*up)(struct token *))
{
#ifdef DEBUG
	extern short verbose;
	extern int fdprintf(int, char *, ...);
	if (verbose & V_FILTER)
		fdprintf(2, "filtknr_init: &rtype_arr.count=%p\n", (void*)&rtype_arr.count);
#endif
	upstream = up;
	state = ST_NORMAL;
	brace_depth = 0;
	param_count = 0;
	cur_pname = 0;
	dlist = 0;
	pre_open = 0;
	pdepth = 0;
	tarr_setup(&rtype_arr, 16);
	tarr_setup(&ptype_arr, 16);
	tarr_setup(&tail_arr, 8);
	tarr_setup(&ptail_arr, 8);
	pend_setup(&pb, 16);
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
	if (idx >= 0 && ptype_arr.count > 0) {
		int i;
		params[idx].type = malloc(ptype_arr.count * sizeof(struct token));
		for (i = 0; i < ptype_arr.count; i++)
			tokcpy(&params[idx].type[i], &ptype_arr.buf[i]);
		params[idx].type_len = ptype_arr.count;
		params[idx].stars = stars;
		if (ptail_arr.count > 0) {
			/* fn-ptr declarator: tokens after the name */
			params[idx].post = malloc(ptail_arr.count *
						  sizeof(struct token));
			for (i = 0; i < ptail_arr.count; i++)
				tokcpy(&params[idx].post[i], &ptail_arr.buf[i]);
			params[idx].post_len = ptail_arr.count;
		}
	}
	tarr_reset(&ptail_arr);
	/* base type stays buffered: char *a, *b; reuses it for b.
	 * Callers reset ptype_arr/ptype_stars at end of declaration. */
}

/*
 * Emit the merged ANSI-style declaration
 */
static void
emit_ansi(void)
{
	int i, j;
	struct token tmp;

	/* Emit return type (synthesize implicit int: c0 requires one) */
	if (rtype_arr.count == 0 && !dlist)
		pend_tok_at(&pb, INT, &func_name);
	else
		pend_buf(&pb, rtype_arr.buf, rtype_arr.count);

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
		/* fn-ptr declarator tail: `)(int)` after the name */
		if (params[i].post_len > 0)
			pend_buf(&pb, params[i].post, params[i].post_len);
		/* Comma if not last */
		if (i < param_count - 1)
			pend_tok_at(&pb, COMMA, &func_name);
		/* Free buffers */
		if (params[i].type) {
			free(params[i].type);
			params[i].type = 0;
		}
		if (params[i].post) {
			free(params[i].post);
			params[i].post = 0;
			params[i].post_len = 0;
		}
	}

	/* Emit ) */
	pend_tok_at(&pb, RPAR, &func_name);

	/* Declarator tail: `)) (args)` of void (*signal(...))(args) */
	pend_buf(&pb, tail_arr.buf, tail_arr.count);
}

/*
 * Abort K&R parsing - emit buffered tokens as-is, let rest flow through
 */
static void
abort_knr(void)
{
	int i;
	struct token tmp;

	/* Aborts happen only after `name (` - always a function, so the
	 * implicit-int synthesis applies here too */
	if (rtype_arr.count == 0 && !dlist)
		pend_tok_at(&pb, INT, &func_name);
	else
		pend_buf(&pb, rtype_arr.buf, rtype_arr.count);
	pend_push(&pb, &func_name);
	pend_push(&pb, &saved_lpar);  /* Use saved LPAR with correct line info */
	/* Emit any param names we collected (with commas) */
	for (i = 0; i < param_count; i++) {
		toksynthnam(&tmp, SYM, params[i].name);
		pend_push(&pb, &tmp);
		if (i < param_count - 1 || state == ST_PARAMS)
			pend_tok(&pb, COMMA);
	}
	if (state == ST_PDECL || state == ST_PTYPE || state == ST_TAIL) {
		/* ) was consumed on leaving ST_PARAMS - put it back, then
		 * flush the declarator tail and any partially buffered
		 * K&R param declaration */
		pend_tok(&pb, RPAR);
		pend_buf(&pb, tail_arr.buf, tail_arr.count);
		pend_buf(&pb, ptype_arr.buf, ptype_arr.count);
		for (i = 0; i < ptype_stars; i++)
			pend_tok(&pb, TIMES);
		if (cur_pname) {
			toksynthnam(&tmp, SYM, cur_pname);
			pend_push(&pb, &tmp);
		}
		pend_buf(&pb, ptail_arr.buf, ptail_arr.count);
		tarr_reset(&ptype_arr);
		tarr_reset(&ptail_arr);
		ptype_stars = 0;
		pdepth = 0;
		cur_pname = 0;
	}
	/* In ST_PARAMS the ) is still upcoming and flows through */

	tarr_reset(&rtype_arr);
	tarr_reset(&tail_arr);
	pre_open = 0;
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
		if (params[i].post)
			free(params[i].post);
		params[i].post = 0;
		params[i].post_len = 0;
	}
	tarr_reset(&rtype_arr);
	param_count = 0;
	tarr_reset(&ptype_arr);
	tarr_reset(&tail_arr);
	tarr_reset(&ptail_arr);
	pre_open = 0;
	pdepth = 0;
	state = ST_NORMAL;
}

void
filtknr(struct token *out)
{
	struct token t;

restart:
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
	tok_depth(&t, &brace_depth);

	switch (state) {
	case ST_NORMAL:
		/* Only look for K&R functions at file scope (brace_depth == 0) */
		if (brace_depth == 0 && !is_type_tok(&t) && t.type == SYM) {
			/* Implicit-int definition: fseek(f, offs, ptr) char *f; */
			tokcpy(&func_name, &t);
			state = ST_NAME;	/* rtype stays empty */
			goto restart;
		}
		if (brace_depth == 0 && is_type_tok(&t)) {
#ifdef DEBUG
			if (VERBOSE(V_FILTER))
				fdprintf(2, "filtknr: ST_NORMAL &rtype_arr.count=%p buf type=%d at %d\n",
					(void*)&rtype_arr.count, t.type, rtype_arr.count);
#endif
			tarr_push(&rtype_arr, &t);
#ifdef DEBUG
			if (VERBOSE(V_FILTER))
				fdprintf(2, "filtknr: after buf rtype_arr.count=%d\n", rtype_arr.count);
#endif
			state = ST_RTYPE;
			goto restart;
		}
		break;

	case ST_RTYPE:
		/* Parenthesized declarator: void (*signal(...))(...) -
		 * buffer the `(` (and following stars) as part of the
		 * prefix; the matching parens are collected as the tail
		 * after the parameter list */
		if (t.type == LPAR) {
			tarr_push(&rtype_arr, &t);
			pre_open++;
			goto restart;
		}
		/* Buffering return type, look for function name */
		/* Note: lexer produces TIMES (42) for *, not STAR (36) */
		if (is_type_tok(&t) || t.type == STAR || t.type == TIMES) {
#ifdef DEBUG
			if (VERBOSE(V_FILTER))
				fdprintf(2, "filtknr: ST_RTYPE buf type=%d at %d\n",
					t.type, rtype_arr.count);
#endif
			tarr_push(&rtype_arr, &t);
			goto restart;
		}
		if (t.type == SYM) {
			/* SYM after struct/union is the tag, part of the type */
			if (tag_pending(&rtype_arr)) {
				tarr_push(&rtype_arr, &t);
				goto restart;
			}
			/* Could be function name */
#ifdef DEBUG
			if (VERBOSE(V_FILTER))
				fdprintf(2, "filtknr: ST_RTYPE SYM rtype_arr.count=%d\n", rtype_arr.count);
#endif
			tokcpy(&func_name, &t);
			state = ST_NAME;
			goto restart;
		}
		/* Not a function - emit buffered and this token */
		pend_buf(&pb, rtype_arr.buf, rtype_arr.count);
		pend_push(&pb, &t);
		tarr_reset(&rtype_arr);
		pre_open = 0;
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
			goto restart;
		}
		/* Not a function - emit return type, name, and this token */
#ifdef DEBUG
		if (VERBOSE(V_FILTER))
			fdprintf(2, "filtknr: ST_NAME &rtype_arr.count=%p rtype_arr.count=%d func_name.type=%d\n",
				(void*)&rtype_arr.count, rtype_arr.count, func_name.type);
#endif
		if (t.type == COMMA)
			dlist = 1;	/* declarator list continues */
		else if (t.type == SEMI)
			dlist = 0;
		pend_buf(&pb, rtype_arr.buf, rtype_arr.count);
		pend_push(&pb, &func_name);
		pend_push(&pb, &t);
		tarr_reset(&rtype_arr);
		pre_open = 0;
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
				goto restart;
			}
		} else if (t.type == LPAR) {
			paren_depth++;
		} else if (t.type != COMMA) {
			/* Type token in params = ANSI style, abort */
			if (is_type_tok(&t)) {
				abort_knr();
				pend_thru(&pb, &t, out);
				return;
			}
			/* Plain SYM (not typedef) = parameter name */
			if (t.type == SYM) {
				/*
				 * Past the limit the name used to be dropped and
				 * nothing said.  Its declaration then had no
				 * parameter to attach to, and the first use of it
				 * came out of pass1 as an undefined symbol on a line
				 * nowhere near the function header.
				 */
				if (param_count >= PARAM_MAX) {
					gripe(ER_C_PC);
					goto restart;
				}
				params[param_count].name = t.v.name;
				params[param_count].type = 0;
				params[param_count].type_len = 0;
				params[param_count].stars = 0;
				params[param_count].post = 0;
				params[param_count].post_len = 0;
				param_count++;
			}
		}
		/* Consume commas silently */
		goto restart;

	case ST_TAIL:
		/* Collecting the declarator tail: close the prefix parens,
		 * then any suffix parens (the fn-ptr's own arg list) */
		if (t.type == RPAR && pre_open > 0) {
			tarr_push(&tail_arr, &t);
			pre_open--;
			goto restart;
		}
		if (t.type == LPAR) {
			tarr_push(&tail_arr, &t);
			pre_open++;
			goto restart;
		}
		if (pre_open > 0) {
			/* inside suffix parens: arg types etc. */
			tarr_push(&tail_arr, &t);
			goto restart;
		}
		/* Tail complete: reprocess this token as ST_PDECL
		 * (`;`, `{`, or the start of K&R param declarations) */
		state = ST_PDECL;
		/* FALLTHROUGH */
	case ST_PDECL:
		/* After ), look for param type declarations or { */
		if (t.type == BEGIN) {
			/* Function body - emit merged declaration */
			emit_ansi();
			dlist = 0;
			reset_state();
			pend_thru(&pb, &t, out);
			return;
		}
		if (t.type == SEMI) {
			if (ptype_arr.count == 0 && cur_pname == 0) {
				/* No K&R declarations seen - this is a prototype */
				emit_ansi();
				dlist = 0;
				reset_state();
				pend_thru(&pb, &t, out);
				return;
			}
			/* End of a K&R param type declaration */
			if (cur_pname)
				save_ptype(cur_pname, ptype_stars);
			cur_pname = 0;
			tarr_reset(&ptype_arr);  /* Reset for next declaration */
			ptype_stars = 0;
			goto restart;
		}
		if (t.type == COMMA && ptype_arr.count == 0 && cur_pname == 0) {
			/* Declarator list: type f(), g(); - not a K&R definition */
			emit_ansi();
			dlist = 1;
			reset_state();
			pend_thru(&pb, &t, out);
			return;
		}
		if (is_type_tok(&t)) {
			/* Start of param type declaration */
			tarr_push(&ptype_arr, &t);
			state = ST_PTYPE;
			goto restart;
		}
		if (t.type == RPAR && pre_open > 0) {
			/* Declarator tail of void (*signal(...))(...):
			 * collect `))  (args)` for re-emission after the
			 * converted parameter list */
			tarr_push(&tail_arr, &t);
			pre_open--;
			state = ST_TAIL;
			goto restart;
		}
		/* Unexpected - abort */
		abort_knr();
		pend_thru(&pb, &t, out);
		return;

	case ST_PTYPE:
		/*
		 * Parenthesized declarator (fn-ptr param decl like
		 * void (*action)(int);): tokens before the name go into
		 * ptype_arr, the SYM inside the parens is the name, and
		 * everything after it (closing paren, suffix parens) is
		 * collected into ptail_arr until depth returns to 0.
		 */
		if (pdepth > 0 || ptail_arr.count > 0) {
			if (t.type == LPAR) {
				if (cur_pname)
					tarr_push(&ptail_arr, &t);
				else
					tarr_push(&ptype_arr, &t);
				pdepth++;
				goto restart;
			}
			if (t.type == RPAR) {
				tarr_push(&ptail_arr, &t);
				pdepth--;
				goto restart;
			}
			if (pdepth > 0) {
				if (!cur_pname &&
				    (t.type == STAR || t.type == TIMES)) {
					tarr_push(&ptype_arr, &t);
					goto restart;
				}
				if (!cur_pname && t.type == SYM) {
					cur_pname = t.v.name;
					goto restart;
				}
				/* suffix tokens: the fn-ptr's arg types */
				tarr_push(&ptail_arr, &t);
				goto restart;
			}
			/* pdepth == 0 with a collected tail: fall through
			 * to the , / ; handling below */
		}
		if (t.type == LPAR && cur_pname == 0) {
			/* start of a parenthesized declarator */
			tarr_push(&ptype_arr, &t);
			pdepth = 1;
			goto restart;
		}
		/* Buffering param type, may have seen a name (cur_pname) */
		if (is_type_tok(&t)) {
			tarr_push(&ptype_arr, &t);
			goto restart;
		}
		/* Note: lexer produces TIMES for * */
		if (t.type == STAR || t.type == TIMES) {
			ptype_stars++;
			goto restart;
		}
		if (t.type == SYM) {
			/* SYM after struct/union is the tag, part of the type
			 * (FILE expands to struct _iobuf) */
			if (tag_pending(&ptype_arr)) {
				tarr_push(&ptype_arr, &t);
				goto restart;
			}
			/* Param name - remember it, stay in ST_PTYPE for ,/; */
			cur_pname = t.v.name;
			goto restart;
		}
		if (t.type == COMMA) {
			/* Multiple params with same type: int a, b; */
			int had_tail = (ptail_arr.count > 0);
			if (cur_pname)
				save_ptype(cur_pname, ptype_stars);
			cur_pname = 0;
			/* Reset stars for next param but keep type */
			ptype_stars = 0;
			if (had_tail) {
				/* fn-ptr declarator: the `( *` prefix in
				 * ptype_arr is per-declarator, no sharing */
				tarr_reset(&ptype_arr);
				pdepth = 0;
			}
			goto restart;
		}
		if (t.type == SEMI) {
			/* End of declaration */
			if (cur_pname)
				save_ptype(cur_pname, ptype_stars);
			cur_pname = 0;
			tarr_reset(&ptype_arr);
			tarr_reset(&ptail_arr);
			ptype_stars = 0;
			pdepth = 0;
			state = ST_PDECL;
			goto restart;
		}
		/* Unexpected */
		abort_knr();
		pend_thru(&pb, &t, out);
		return;
	}

	/* Track declarator-list state on tokens flowing through untouched
	 * (e.g. `char buf[4], foo();` - the comma passes through here) */
	if (brace_depth == 0) {
		if (t.type == COMMA)
			dlist = 1;
		else if (t.type == SEMI)
			dlist = 0;
	}

	tokcpy(out, &t);
}
