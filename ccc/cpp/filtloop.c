/*
 * filtloop.c - Loop lowering filter
 *
 * Transforms WHILE/FOR/DO loops to labels and gotos.
 * Assumes input has all bodies braced (from filtbrace).
 *
 * WHILE (cond) { body }
 * -> { __WnT: if (!(cond)) goto __WnB; { body } goto __WnT; __WnB: }
 *
 * FOR (init; cond; incr) { body }
 * -> { init; __FnT: if (!(cond)) goto __FnB; { body } __FnC: incr; goto __FnT; __FnB: }
 *
 * DO { body } WHILE (cond);
 * -> { __DnT: { body } if (cond) goto __DnT; __DnB: }
 */

#include "cpp.h"
#include "lexeme.h"
#include <string.h>
#include <stdio.h>

/* States */
#define ST_NORMAL    0
#define ST_WHILE_C   1	/* Buffering WHILE condition */
#define ST_FOR_INIT  2	/* Buffering FOR init */
#define ST_FOR_COND  3	/* Buffering FOR condition */
#define ST_FOR_INCR  4	/* Buffering FOR increment */
#define ST_BODY      5	/* Passing through loop body */
#define ST_DO_BODY   6	/* Passing through DO body */
#define ST_DO_COND   7	/* Buffering DO-WHILE condition */

/* Loop context stack */
#define STK_MAX 16
static struct {
	unsigned char state;
	unsigned char type;		/* WHILE, FOR, DO */
	int label_num;
	int body_depth;			/* Brace depth in body */
} stk[STK_MAX];
static int stk_sp = 0;

static int state = ST_NORMAL;
static int depth = 0;			/* Paren depth while buffering */
static int body_depth = 0;		/* Brace depth in body */
static unsigned char loop_type = 0;
static int label_num = 0;
static int next_label = 1;

/* Token buffers */
#define BUF_MAX 128
static struct token cond_buf[BUF_MAX];
static int cond_len = 0;
static struct token init_buf[BUF_MAX];
static int init_len = 0;
static struct token incr_buf[BUF_MAX];
static int incr_len = 0;

/* Output queue */
#define PEND_MAX 32
static struct token pend[PEND_MAX];
static int pend_rd = 0;
static int pend_wr = 0;

/* Upstream */
static struct token (*upstream)(void);

void
filtloop_init(struct token (*up)(void))
{
	upstream = up;
	state = ST_NORMAL;
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

static struct token
synth_label(char prefix, int num, char suffix)
{
	static char buf[16];
	struct token t;
	sprintf(buf, "__%c%d%c", prefix, num, suffix);
	t.type = LABEL;
	t.v.name = buf;
	t.lineno = lineno;
	t.filename = filename;
	return t;
}

static struct token
synth_sym(char prefix, int num, char suffix)
{
	static char buf[16];
	struct token t;
	sprintf(buf, "__%c%d%c", prefix, num, suffix);
	t.type = SYM;
	t.v.name = buf;
	t.lineno = lineno;
	t.filename = filename;
	return t;
}

static void
push_loop(void)
{
	if (stk_sp < STK_MAX) {
		stk[stk_sp].state = state;
		stk[stk_sp].type = loop_type;
		stk[stk_sp].label_num = label_num;
		stk[stk_sp].body_depth = body_depth;
		stk_sp++;
	}
}

static void
pop_loop(void)
{
	if (stk_sp > 0) {
		stk_sp--;
		state = stk[stk_sp].state;
		loop_type = stk[stk_sp].type;
		label_num = stk[stk_sp].label_num;
		body_depth = stk[stk_sp].body_depth;
	} else {
		state = ST_NORMAL;
	}
}

/* Emit buffered tokens */
static void
emit_buf(struct token *buf, int len)
{
	int i;
	for (i = 0; i < len; i++)
		pend_push(buf[i]);
}

/*
 * Emit WHILE/FOR header:
 * { __XnT: if (!(cond)) goto __XnB;
 */
static void
emit_loop_header(char prefix)
{
	pend_push(synth(BEGIN));
	if (init_len > 0) {
		/* FOR: emit init first */
		emit_buf(init_buf, init_len);
		pend_push(synth(SEMI));
	}
	pend_push(synth_label(prefix, label_num, 'T'));
	pend_push(synth(SEMI));
	if (cond_len > 0) {
		pend_push(synth(IF));
		pend_push(synth(LPAR));
		pend_push(synth(NOT));
		pend_push(synth(LPAR));
		emit_buf(cond_buf, cond_len);
		pend_push(synth(RPAR));
		pend_push(synth(RPAR));
		pend_push(synth(BEGIN));
		pend_push(synth(GOTO));
		pend_push(synth_sym(prefix, label_num, 'B'));
		pend_push(synth(SEMI));
		pend_push(synth(END));
	}
}

/*
 * Emit WHILE trailer: goto __WnT; __WnB: }
 */
static void
emit_while_trailer(void)
{
	pend_push(synth(GOTO));
	pend_push(synth_sym('W', label_num, 'T'));
	pend_push(synth(SEMI));
	pend_push(synth_label('W', label_num, 'B'));
	pend_push(synth(SEMI));
	pend_push(synth(END));
}

/*
 * Emit FOR trailer: __FnC: incr; goto __FnT; __FnB: }
 */
static void
emit_for_trailer(void)
{
	pend_push(synth_label('F', label_num, 'C'));
	pend_push(synth(SEMI));
	if (incr_len > 0) {
		emit_buf(incr_buf, incr_len);
		pend_push(synth(SEMI));
	}
	pend_push(synth(GOTO));
	pend_push(synth_sym('F', label_num, 'T'));
	pend_push(synth(SEMI));
	pend_push(synth_label('F', label_num, 'B'));
	pend_push(synth(SEMI));
	pend_push(synth(END));
}

/*
 * Emit DO header: { __DnT:
 */
static void
emit_do_header(void)
{
	pend_push(synth(BEGIN));
	pend_push(synth_label('D', label_num, 'T'));
	pend_push(synth(SEMI));
}

/*
 * Emit DO trailer: if (cond) goto __DnT; __DnB: }
 */
static void
emit_do_trailer(void)
{
	if (cond_len > 0) {
		pend_push(synth(IF));
		pend_push(synth(LPAR));
		emit_buf(cond_buf, cond_len);
		pend_push(synth(RPAR));
		pend_push(synth(BEGIN));
		pend_push(synth(GOTO));
		pend_push(synth_sym('D', label_num, 'T'));
		pend_push(synth(SEMI));
		pend_push(synth(END));
	}
	pend_push(synth_label('D', label_num, 'B'));
	pend_push(synth(SEMI));
	pend_push(synth(END));
}

struct token
filtloop(void)
{
	struct token t;

	if (pend_has())
		return pend_pop();

	t = upstream();

	switch (state) {
	case ST_NORMAL:
		if (t.type == WHILE) {
			loop_type = WHILE;
			label_num = next_label++;
			cond_len = 0;
			depth = 0;
			state = ST_WHILE_C;
			return filtloop();	/* Consume WHILE, get next */
		}
		if (t.type == FOR) {
			loop_type = FOR;
			label_num = next_label++;
			init_len = cond_len = incr_len = 0;
			depth = 0;
			state = ST_FOR_INIT;
			return filtloop();
		}
		if (t.type == DO) {
			loop_type = DO;
			label_num = next_label++;
			emit_do_header();
			body_depth = 0;
			state = ST_DO_BODY;
			return pend_pop();
		}
		return t;

	case ST_WHILE_C:
		/* Buffer condition until ) at depth 0 */
		if (t.type == LPAR) {
			depth++;
			if (depth == 1)
				return filtloop();	/* Skip opening ( */
		} else if (t.type == RPAR) {
			depth--;
			if (depth == 0) {
				/* Condition complete */
				emit_loop_header('W');
				body_depth = 0;
				state = ST_BODY;
				return pend_pop();
			}
		}
		if (depth > 0 && cond_len < BUF_MAX)
			cond_buf[cond_len++] = t;
		return filtloop();

	case ST_FOR_INIT:
		/* Buffer init until first ; */
		if (t.type == LPAR) {
			depth++;
			if (depth == 1)
				return filtloop();
		} else if (t.type == SEMI && depth == 1) {
			state = ST_FOR_COND;
			return filtloop();
		}
		if (depth > 0 && init_len < BUF_MAX)
			init_buf[init_len++] = t;
		return filtloop();

	case ST_FOR_COND:
		/* Buffer cond until second ; */
		if (t.type == SEMI) {
			state = ST_FOR_INCR;
			return filtloop();
		}
		if (cond_len < BUF_MAX)
			cond_buf[cond_len++] = t;
		return filtloop();

	case ST_FOR_INCR:
		/* Buffer incr until ) */
		if (t.type == RPAR) {
			depth--;
			if (depth == 0) {
				emit_loop_header('F');
				body_depth = 0;
				state = ST_BODY;
				return pend_pop();
			}
		} else if (t.type == LPAR) {
			depth++;
		}
		if (incr_len < BUF_MAX)
			incr_buf[incr_len++] = t;
		return filtloop();

	case ST_BODY:
		/* Pass through body, track depth */
		if (t.type == BEGIN)
			body_depth++;
		else if (t.type == END) {
			body_depth--;
			if (body_depth == 0) {
				/* Body complete - emit trailer */
				pend_push(t);	/* The closing } */
				if (loop_type == WHILE)
					emit_while_trailer();
				else
					emit_for_trailer();
				if (stk_sp > 0)
					pop_loop();
				else
					state = ST_NORMAL;
				return pend_pop();
			}
		}
		/* Check for nested loops */
		if (t.type == WHILE || t.type == FOR || t.type == DO) {
			push_loop();
			state = ST_NORMAL;
			pend_push(t);
			return filtloop();
		}
		return t;

	case ST_DO_BODY:
		/* Pass through DO body until closing } */
		if (t.type == BEGIN)
			body_depth++;
		else if (t.type == END) {
			body_depth--;
			if (body_depth == 0) {
				/* Body complete - wait for WHILE */
				pend_push(t);
				state = ST_DO_COND;
				depth = -1;	/* Waiting for WHILE keyword */
				cond_len = 0;
				return pend_pop();
			}
		}
		/* Check for nested loops */
		if (t.type == WHILE || t.type == FOR || t.type == DO) {
			push_loop();
			state = ST_NORMAL;
			pend_push(t);
			return filtloop();
		}
		return t;

	case ST_DO_COND:
		/* After DO body, buffer WHILE condition */
		if (depth == -1) {
			if (t.type == WHILE) {
				depth = 0;
				return filtloop();
			}
			/* Shouldn't happen - malformed */
			return t;
		}
		if (t.type == LPAR) {
			depth++;
			if (depth == 1)
				return filtloop();
		} else if (t.type == RPAR) {
			depth--;
			if (depth == 0) {
				/* Condition complete */
				emit_do_trailer();
				if (stk_sp > 0)
					pop_loop();
				else
					state = ST_NORMAL;
				/* Consume trailing ; */
				t = upstream();
				if (t.type != SEMI)
					pend_push(t);
				return pend_pop();
			}
		} else if (t.type == SEMI && depth == 0) {
			/* Empty condition - just ; after ) */
			emit_do_trailer();
			if (stk_sp > 0)
				pop_loop();
			else
				state = ST_NORMAL;
			return pend_pop();
		}
		if (depth > 0 && cond_len < BUF_MAX)
			cond_buf[cond_len++] = t;
		return filtloop();
	}

	return t;
}
