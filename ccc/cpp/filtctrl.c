/*
 * filtctrl.c - Control flow filter
 *
 * Unified handling of loops and switches:
 * - Loop lowering (WHILE/FOR/DO -> labels/gotos)
 * - Switch break labels
 * - break -> goto innermost loop or switch exit
 * - continue -> goto loop continue point
 *
 * Assumes input has braced bodies (from filtbrace).
 * Pipeline: lex -> filtbrace -> filtctrl -> emit
 */

#include "cpp.h"
#include "lexeme.h"
#include <stdio.h>

/* States */
#define ST_NORMAL    0
#define ST_WHILE_C   1	/* Buffering WHILE condition */
#define ST_FOR_INIT  2	/* Buffering FOR init */
#define ST_FOR_COND  3	/* Buffering FOR condition */
#define ST_FOR_INCR  4	/* Buffering FOR increment */
#define ST_LOOP_BODY 5	/* Inside loop body */
#define ST_DO_BODY   6	/* Inside DO body */
#define ST_DO_COND   7	/* Buffering DO-WHILE condition */
#define ST_SW_COND   8	/* Buffering SWITCH condition */
#define ST_SW_BODY   9	/* Inside switch body */

/* Control context types */
#define CTX_WHILE  1
#define CTX_FOR    2
#define CTX_DO     3
#define CTX_SWITCH 4

/* Unified control context stack */
#define STK_MAX 32
static struct {
	unsigned char ctx_type;		/* CTX_WHILE, CTX_FOR, CTX_DO, CTX_SWITCH */
	unsigned char saved_state;
	unsigned char saved_ctx;	/* Saved cur_ctx */
	int label_num;
	int body_depth;
} stk[STK_MAX];
static int stk_sp = 0;

static int state = ST_NORMAL;
static int depth = 0;			/* Paren depth while buffering */
static int body_depth = 0;		/* Brace depth in body */
static int label_num = 0;
static int next_label = 1;
static unsigned char cur_ctx = 0;	/* Current context type */

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

static struct token (*upstream)(void);

void
filtctrl_init(struct token (*up)(void))
{
	upstream = up;
	state = ST_NORMAL;
	stk_sp = 0;
	pend_rd = pend_wr = 0;
}

static void pend_push(struct token t) {
	pend[pend_wr] = t;
	pend_wr = (pend_wr + 1) % PEND_MAX;
}

static int pend_has(void) { return pend_rd != pend_wr; }

static struct token pend_pop(void) {
	struct token t = pend[pend_rd];
	pend_rd = (pend_rd + 1) % PEND_MAX;
	return t;
}

static struct token synth(unsigned char type) {
	struct token t;
	t.type = type;
	t.v.numeric = 0;
	t.lineno = lineno;
	t.filename = filename;
	return t;
}

static struct token synth_label(char prefix, int num, char suffix) {
	static char buf[16];
	struct token t;
	sprintf(buf, "__%c%d%c", prefix, num, suffix);
	t.type = LABEL;
	t.v.name = buf;
	t.lineno = lineno;
	t.filename = filename;
	return t;
}

static struct token synth_sym(char prefix, int num, char suffix) {
	static char buf[16];
	struct token t;
	sprintf(buf, "__%c%d%c", prefix, num, suffix);
	t.type = SYM;
	t.v.name = buf;
	t.lineno = lineno;
	t.filename = filename;
	return t;
}

static void push_ctx(unsigned char type, unsigned char saved) {
	if (stk_sp < STK_MAX) {
		stk[stk_sp].ctx_type = type;
		stk[stk_sp].saved_state = saved;
		stk[stk_sp].saved_ctx = cur_ctx;
		stk[stk_sp].label_num = label_num;
		stk[stk_sp].body_depth = body_depth;
		stk_sp++;
	}
}

static void pop_ctx(void) {
	if (stk_sp > 0) {
		stk_sp--;
		label_num = stk[stk_sp].label_num;
		body_depth = stk[stk_sp].body_depth;
		cur_ctx = stk[stk_sp].saved_ctx;
		state = stk[stk_sp].saved_state;
	} else {
		state = ST_NORMAL;
		cur_ctx = 0;
	}
}

static char ctx_prefix(unsigned char type) {
	switch (type) {
	case CTX_WHILE:  return 'W';
	case CTX_FOR:    return 'F';
	case CTX_DO:     return 'D';
	case CTX_SWITCH: return 'S';
	}
	return 'X';
}

/* Emit buffered tokens */
static void emit_buf(struct token *buf, int len) {
	int i;
	for (i = 0; i < len; i++)
		pend_push(buf[i]);
}

/* Emit loop header: { [init;] __XnT: if (!(cond)) goto __XnB; */
static void emit_loop_header(char prefix) {
	pend_push(synth(BEGIN));
	if (init_len > 0) {
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

/* Emit WHILE trailer: goto __WnT; __WnB: } */
static void emit_while_trailer(void) {
	pend_push(synth(GOTO));
	pend_push(synth_sym('W', label_num, 'T'));
	pend_push(synth(SEMI));
	pend_push(synth_label('W', label_num, 'B'));
	pend_push(synth(SEMI));
	pend_push(synth(END));
}

/* Emit FOR trailer: __FnC: incr; goto __FnT; __FnB: } */
static void emit_for_trailer(void) {
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

/* Emit DO header: { __DnT: */
static void emit_do_header(void) {
	pend_push(synth(BEGIN));
	pend_push(synth_label('D', label_num, 'T'));
	pend_push(synth(SEMI));
}

/* Emit DO trailer: if (cond) goto __DnT; __DnB: } */
static void emit_do_trailer(void) {
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

/* Emit switch exit label: __SnB: */
static void emit_switch_end(void) {
	pend_push(synth_label('S', label_num, 'B'));
	pend_push(synth(SEMI));
}

/*
 * Find innermost breakable context and emit appropriate goto.
 * Returns 1 if handled, 0 if no context (shouldn't happen).
 */
static int emit_break(void) {
	int i;
	for (i = stk_sp - 1; i >= 0; i--) {
		unsigned char t = stk[i].ctx_type;
		if (t == CTX_WHILE || t == CTX_FOR || t == CTX_DO || t == CTX_SWITCH) {
			pend_push(synth(GOTO));
			pend_push(synth_sym(ctx_prefix(t), stk[i].label_num, 'B'));
			return 1;
		}
	}
	return 0;
}

/*
 * Find innermost loop and emit continue goto.
 * Returns 1 if handled, 0 if not in a loop.
 */
static int emit_continue(void) {
	int i;
	for (i = stk_sp - 1; i >= 0; i--) {
		unsigned char t = stk[i].ctx_type;
		if (t == CTX_WHILE || t == CTX_DO) {
			pend_push(synth(GOTO));
			pend_push(synth_sym(ctx_prefix(t), stk[i].label_num, 'T'));
			return 1;
		}
		if (t == CTX_FOR) {
			pend_push(synth(GOTO));
			pend_push(synth_sym('F', stk[i].label_num, 'C'));
			return 1;
		}
		/* Skip switches - continue doesn't apply */
	}
	return 0;
}

/* Handle body state for both loops and switches */
static int handle_body(struct token t, unsigned char ctx_type) {
	if (t.type == BEGIN)
		body_depth++;
	else if (t.type == END) {
		body_depth--;
		if (body_depth == 0) {
			pend_push(t);
			return 1;	/* Body complete */
		}
	}
	/* Nested control structures */
	if (t.type == WHILE) {
		push_ctx(ctx_type, state);
		label_num = next_label++;
		cond_len = 0;
		depth = 0;
		state = ST_WHILE_C;
		return 2;	/* Consumed, recurse */
	}
	if (t.type == FOR) {
		push_ctx(ctx_type, state);
		label_num = next_label++;
		init_len = cond_len = incr_len = 0;
		depth = 0;
		state = ST_FOR_INIT;
		return 2;
	}
	if (t.type == DO) {
		push_ctx(ctx_type, state);
		label_num = next_label++;
		emit_do_header();
		body_depth = 0;
		state = ST_DO_BODY;
		return 3;	/* Return pend_pop */
	}
	if (t.type == SWITCH) {
		push_ctx(ctx_type, state);
		label_num = next_label++;
		depth = 0;
		state = ST_SW_COND;
		return 0;	/* Pass through */
	}
	if (t.type == BREAK) {
		if (emit_break())
			return 3;
	}
	if (t.type == CONTINUE) {
		if (emit_continue())
			return 3;
	}
	return 0;	/* Pass through */
}

struct token
filtctrl(void)
{
	struct token t;
	int r;

	if (pend_has())
		return pend_pop();

	t = upstream();

	switch (state) {
	case ST_NORMAL:
		if (t.type == WHILE) {
			label_num = next_label++;
			cond_len = 0;
			depth = 0;
			state = ST_WHILE_C;
			return filtctrl();
		}
		if (t.type == FOR) {
			label_num = next_label++;
			init_len = cond_len = incr_len = 0;
			depth = 0;
			state = ST_FOR_INIT;
			return filtctrl();
		}
		if (t.type == DO) {
			label_num = next_label++;
			emit_do_header();
			body_depth = 0;
			state = ST_DO_BODY;
			return pend_pop();
		}
		if (t.type == SWITCH) {
			label_num = next_label++;
			depth = 0;
			state = ST_SW_COND;
		}
		return t;

	case ST_WHILE_C:
		if (t.type == LPAR) {
			depth++;
			if (depth == 1) return filtctrl();
		} else if (t.type == RPAR) {
			depth--;
			if (depth == 0) {
				emit_loop_header('W');
				body_depth = 0;
				cur_ctx = CTX_WHILE;
				state = ST_LOOP_BODY;
				return pend_pop();
			}
		}
		if (depth > 0 && cond_len < BUF_MAX)
			cond_buf[cond_len++] = t;
		return filtctrl();

	case ST_FOR_INIT:
		if (t.type == LPAR) {
			depth++;
			if (depth == 1) return filtctrl();
		} else if (t.type == SEMI && depth == 1) {
			state = ST_FOR_COND;
			return filtctrl();
		}
		if (depth > 0 && init_len < BUF_MAX)
			init_buf[init_len++] = t;
		return filtctrl();

	case ST_FOR_COND:
		if (t.type == SEMI) {
			state = ST_FOR_INCR;
			return filtctrl();
		}
		if (cond_len < BUF_MAX)
			cond_buf[cond_len++] = t;
		return filtctrl();

	case ST_FOR_INCR:
		if (t.type == RPAR) {
			depth--;
			if (depth == 0) {
				emit_loop_header('F');
				body_depth = 0;
				cur_ctx = CTX_FOR;
				state = ST_LOOP_BODY;
				return pend_pop();
			}
		} else if (t.type == LPAR) {
			depth++;
		}
		if (incr_len < BUF_MAX)
			incr_buf[incr_len++] = t;
		return filtctrl();

	case ST_LOOP_BODY:
		r = handle_body(t, cur_ctx);
		if (r == 1) {
			/* Body complete - emit trailer based on current context */
			if (cur_ctx == CTX_FOR)
				emit_for_trailer();
			else
				emit_while_trailer();
			pop_ctx();
			return pend_pop();
		}
		if (r == 2) return filtctrl();
		if (r == 3) return pend_pop();
		return t;

	case ST_DO_BODY:
		r = handle_body(t, CTX_DO);
		if (r == 1) {
			state = ST_DO_COND;
			depth = -1;
			cond_len = 0;
			return pend_pop();
		}
		if (r == 2) return filtctrl();
		if (r == 3) return pend_pop();
		return t;

	case ST_DO_COND:
		if (depth == -1) {
			if (t.type == WHILE) {
				depth = 0;
				return filtctrl();
			}
			return t;
		}
		if (t.type == LPAR) {
			depth++;
			if (depth == 1) return filtctrl();
		} else if (t.type == RPAR) {
			depth--;
			if (depth == 0) {
				emit_do_trailer();
				pop_ctx();
				t = upstream();
				if (t.type != SEMI) pend_push(t);
				return pend_pop();
			}
		}
		if (depth > 0 && cond_len < BUF_MAX)
			cond_buf[cond_len++] = t;
		return filtctrl();

	case ST_SW_COND:
		if (t.type == LPAR)
			depth++;
		else if (t.type == RPAR) {
			depth--;
			if (depth == 0) {
				state = ST_SW_BODY;
				body_depth = 0;
			}
		}
		return t;

	case ST_SW_BODY:
		r = handle_body(t, CTX_SWITCH);
		if (r == 1) {
			emit_switch_end();
			pop_ctx();
			return pend_pop();
		}
		if (r == 2) return filtctrl();
		if (r == 3) return pend_pop();
		return t;
	}

	return t;
}
