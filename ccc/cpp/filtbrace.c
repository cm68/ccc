/*
 * filtbrace.c - Brace insertion filter
 *
 * Inserts braces around single-statement bodies of IF/ELSE/WHILE/FOR/DO.
 * Pull-based: call filtbrace() to get next token.
 */

#include "cpp.h"
#include "lexeme.h"

/* States */
#define ST_NORMAL    0	/* Normal pass-through */
#define ST_COND      1	/* Inside condition parens (IF/WHILE/FOR) */
#define ST_PENDING   2	/* After condition, checking for { */
#define ST_BODY      3	/* Inside synthetic single-stmt body */
#define ST_ELSE_CHK  4	/* After body SEMI, check for ELSE before cascade */

/* State stack for nested control structures */
#define STK_MAX 16
static struct {
	unsigned char state;
	unsigned char ctrl_type;
	int depth;
	unsigned char synthetic;	/* 1 if we inserted { */
} stk[STK_MAX];
static int stk_sp = 0;

static int state = ST_NORMAL;
static int depth = 0;
static unsigned char ctrl_type = 0;

/* Pending token queue */
#define PEND_MAX 8
static struct token pendbuf[PEND_MAX];
static struct pendbuf pb;

/* Upstream token source */
static void (*upstream)(struct token *);

void
filtbrace_init(void (*up)(struct token *))
{
	upstream = up;
	state = ST_NORMAL;
	depth = 0;
	ctrl_type = 0;
	stk_sp = 0;
	pend_init(&pb, pendbuf, PEND_MAX);
}

static void
push_state(unsigned char synthetic)
{
	if (stk_sp < STK_MAX) {
		stk[stk_sp].state = state;
		stk[stk_sp].ctrl_type = ctrl_type;
		stk[stk_sp].depth = depth;
		stk[stk_sp].synthetic = synthetic;
		stk_sp++;
	}
}

static void
pop_state(void)
{
	if (stk_sp > 0) {
		stk_sp--;
		state = stk[stk_sp].state;
		ctrl_type = stk[stk_sp].ctrl_type;
		depth = stk[stk_sp].depth;
	} else {
		state = ST_NORMAL;
	}
}

/*
 * Body complete - emit ; }, pop state, enter ST_ELSE_CHK
 */
static void
end_body(struct token *semi)
{
	pend_push(&pb, semi);
	pend_tok(&pb, END);
	pop_state();
	if (ctrl_type == IF && state == ST_BODY && depth == 0)
		state = ST_ELSE_CHK;
}

/*
 * Cascade close: emit } for each nested single-stmt body
 */
static void
cascade_close(void)
{
	while (state == ST_BODY && depth == 0 && stk_sp > 0 &&
	       stk[stk_sp - 1].synthetic) {
		pend_tok(&pb, END);
		pop_state();
		if (ctrl_type == IF && state == ST_BODY && depth == 0) {
			state = ST_ELSE_CHK;
			return;
		}
	}
}

void
filtbrace(struct token *out)
{
	struct token t;

	if (filt_entry(&pb, out, upstream, &t))
		return;

	switch (state) {
	case ST_NORMAL:
		if (t.type == IF || t.type == WHILE || t.type == FOR) {
			ctrl_type = t.type;
			state = ST_COND;
			depth = 0;
		} else if (t.type == ELSE || t.type == DO) {
			ctrl_type = t.type;
			state = ST_PENDING;
		}
		break;

	case ST_COND:
		if (t.type == LPAR)
			depth++;
		else if (t.type == RPAR) {
			depth--;
			if (depth == 0)
				state = ST_PENDING;
		}
		break;

	case ST_PENDING:
		if (t.type == BEGIN) {
			state = ST_NORMAL;
			break;
		}
		if (ctrl_type == ELSE && t.type == IF) {
			ctrl_type = IF;
			state = ST_COND;
			depth = 0;
			break;
		}
		/* Insert { before this token */
		pend_push(&pb, &t);
		state = ST_BODY;
		depth = 0;
		toksynth(out, BEGIN);
		return;

	case ST_BODY:
		if (t.type == BEGIN || t.type == LPAR || t.type == LBRACK)
			depth++;
		else if (t.type == END || t.type == RPAR || t.type == RBRACK)
			depth--;

		if (depth == 0) {
			if (t.type == IF || t.type == WHILE || t.type == FOR) {
				push_state(1);
				ctrl_type = t.type;
				state = ST_COND;
				depth = 0;
				break;
			}
			if (t.type == ELSE || t.type == DO) {
				push_state(1);
				ctrl_type = t.type;
				state = ST_PENDING;
				break;
			}
			if (t.type == SEMI) {
				end_body(&t);
				pend_pop(&pb, out);
				return;
			}
		}
		break;

	case ST_ELSE_CHK:
		if (t.type == ELSE) {
			ctrl_type = ELSE;
			state = ST_PENDING;
			break;
		}
		cascade_close();
		pend_push(&pb, &t);
		pend_pop(&pb, out);
		return;
	}

	tokcpy(out, &t);
}
