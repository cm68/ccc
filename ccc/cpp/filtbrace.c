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
static struct token pend[PEND_MAX];
static int pend_rd = 0;
static int pend_wr = 0;

/* Upstream token source */
static struct token (*upstream)(void);

void
filtbrace_init(struct token (*up)(void))
{
	upstream = up;
	state = ST_NORMAL;
	depth = 0;
	ctrl_type = 0;
	stk_sp = 0;
	pend_rd = pend_wr = 0;
}

static void
pend_push(struct token t)
{
	pend[pend_wr] = t;
	pend_wr = (pend_wr + 1) % PEND_MAX;
}

static int
pend_has(void)
{
	return pend_rd != pend_wr;
}

static struct token
pend_pop(void)
{
	struct token t = pend[pend_rd];
	pend_rd = (pend_rd + 1) % PEND_MAX;
	return t;
}

static struct token
synth(unsigned char type)
{
	struct token t;
	t.type = type;
	t.v.numeric = 0;
	t.lineno = lineno;
	t.filename = filename;
	return t;
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
 * Cascade handled in ST_ELSE_CHK when we see non-ELSE token.
 */
static void
end_body(struct token semi)
{
	/* Emit the semicolon and closing brace */
	pend_push(semi);
	pend_push(synth(END));

	/* Pop to parent state */
	pop_state();

	/* If ctrl was IF and parent is ST_BODY, check for ELSE */
	if (ctrl_type == IF && state == ST_BODY && depth == 0) {
		state = ST_ELSE_CHK;
	}
}

/*
 * Cascade close: emit } for each nested single-stmt body
 */
static void
cascade_close(void)
{
	while (state == ST_BODY && depth == 0 && stk_sp > 0 &&
	       stk[stk_sp - 1].synthetic) {
		pend_push(synth(END));
		pop_state();
		/* If this was also IF, check for ELSE at this level */
		if (ctrl_type == IF && state == ST_BODY && depth == 0) {
			state = ST_ELSE_CHK;
			return;
		}
	}
}

struct token
filtbrace(void)
{
	struct token t;

	if (pend_has())
		return pend_pop();

	t = upstream();

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
		return t;

	case ST_COND:
		if (t.type == LPAR)
			depth++;
		else if (t.type == RPAR) {
			depth--;
			if (depth == 0)
				state = ST_PENDING;
		}
		return t;

	case ST_PENDING:
		if (t.type == BEGIN) {
			/* Already braced */
			state = ST_NORMAL;
			return t;
		}
		if (ctrl_type == ELSE && t.type == IF) {
			/* else if */
			ctrl_type = IF;
			state = ST_COND;
			depth = 0;
			return t;
		}
		/* Insert { before this token */
		pend_push(t);
		state = ST_BODY;
		depth = 0;
		return synth(BEGIN);

	case ST_BODY:
		/* Track depth */
		if (t.type == BEGIN || t.type == LPAR || t.type == LBRACK)
			depth++;
		else if (t.type == END || t.type == RPAR || t.type == RBRACK)
			depth--;

		/* Check for nested control */
		if (depth == 0) {
			if (t.type == IF || t.type == WHILE || t.type == FOR) {
				push_state(1);
				ctrl_type = t.type;
				state = ST_COND;
				depth = 0;
				return t;
			}
			if (t.type == ELSE || t.type == DO) {
				push_state(1);
				ctrl_type = t.type;
				state = ST_PENDING;
				return t;
			}
			if (t.type == SEMI) {
				end_body(t);
				return pend_pop();
			}
		}
		return t;

	case ST_ELSE_CHK:
		/*
		 * After IF body ended, check if ELSE follows.
		 * If ELSE: it's part of this if-else, handle it.
		 * If not: cascade close outer bodies.
		 */
		if (t.type == ELSE) {
			/* ELSE binds to the IF we just closed */
			ctrl_type = ELSE;
			state = ST_PENDING;
			return t;
		}
		/* No ELSE - cascade close outer bodies */
		cascade_close();
		/* Reprocess this token in new state */
		pend_push(t);
		return pend_pop();
	}

	return t;
}
