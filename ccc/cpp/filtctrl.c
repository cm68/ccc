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
#define STK_MAX 8
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
#define BUF_MAX 64
static struct token cond_buf[BUF_MAX];
static int cond_len = 0;
static struct token init_buf[BUF_MAX];
static int init_len = 0;
static struct token incr_buf[BUF_MAX];
static int incr_len = 0;

/* Output queue */
#define PEND_MAX 64
static struct token pendbuf[PEND_MAX];
static struct pendbuf pb;

static void (*upstream)(struct token *);

#ifdef DEBUG
static int ctrl_balance = 0;  /* Track output brace balance */
#endif

void
filtctrl_init(void (*up)(struct token *))
{
	upstream = up;
	state = ST_NORMAL;
	stk_sp = 0;
	pend_init(&pb, pendbuf, PEND_MAX);
#ifdef DEBUG
	ctrl_balance = 0;
#endif
}

#ifdef DEBUG
static void
track_ctrl(struct token *t)
{
	if (t->type == BEGIN) {
		ctrl_balance++;
		if (VERBOSE(V_FILTER))
			fdprintf(2, "CTRL: out BEGIN bal=%d\n", ctrl_balance);
	} else if (t->type == END) {
		ctrl_balance--;
		if (VERBOSE(V_FILTER))
			fdprintf(2, "CTRL: out END bal=%d\n", ctrl_balance);
	}
}
#endif

/* Wrapper: pop from pending and track output */
static void
pop_out(struct token *out)
{
	pend_pop(&pb, out);
#ifdef DEBUG
	track_ctrl(out);
#endif
}

void
filtctrl_check(void)
{
#ifdef DEBUG
	if (VERBOSE(V_FILTER))
		fdprintf(2, "CTRL: EOF balance=%d stk=%d\n",
			 ctrl_balance, stk_sp);
	if (ctrl_balance != 0)
		fdprintf(2, "CTRL: WARNING balance=%d at EOF\n",
			 ctrl_balance);
	if (stk_sp != 0)
		fdprintf(2, "CTRL: WARNING stk_sp=%d at EOF\n", stk_sp);
#endif
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

/* Lookup: CTX_WHILE=1->'W', CTX_FOR=2->'F', CTX_DO=3->'D', CTX_SWITCH=4->'S' */
static char ctx_pfx[] = "XWFDS";
static char ctx_prefix(unsigned char t) { return ctx_pfx[t]; }

/* Check if context is breakable (all loop types + switch) */
static int is_breakable(unsigned char t) {
	return t >= CTX_WHILE && t <= CTX_SWITCH;
}

/* Emit loop header: [init;] __XnT: if (!(cond)) goto __XnB; */
static void emitLoopHdr(char prefix) {
#ifdef DEBUG
	if (VERBOSE(V_FILTER))
		fdprintf(2, "emitLoopHdr: pb.rd=%d pb.wr=%d\n", pb.rd, pb.wr);
#endif
	/* No wrapper braces - filtbrace guarantees braced body */
	if (init_len > 0) {
		pend_buf(&pb, init_buf, init_len);
		pend_tok(&pb, SEMI);
	}
	emit_label(&pb, prefix, label_num, 'T');
	if (cond_len > 0) {
		pend_tok(&pb, IF);
		pend_tok(&pb, LPAR);
		pend_tok(&pb, BANG);
		pend_tok(&pb, LPAR);
		pend_buf(&pb, cond_buf, cond_len);
		pend_tok(&pb, RPAR);
		pend_tok(&pb, RPAR);
		pend_tok(&pb, BEGIN);
		emit_goto(&pb, prefix, label_num, 'B');
		pend_tok(&pb, SEMI);
		pend_tok(&pb, END);
	}
#ifdef DEBUG
	if (VERBOSE(V_FILTER))
		fdprintf(2, "emitLoopHdr done: pb.rd=%d pb.wr=%d\n", pb.rd, pb.wr);
#endif
}

/* Emit WHILE trailer: goto __WnT; __WnB: */
static void emitWhileTrail(void) {
	emit_goto(&pb, 'W', label_num, 'T');
	pend_tok(&pb, SEMI);
	emit_label(&pb, 'W', label_num, 'B');
}

/* Emit FOR trailer: __FnC: incr; goto __FnT; __FnB: */
static void emitForTrail(void) {
	emit_label(&pb, 'F', label_num, 'C');
	if (incr_len > 0) {
		pend_buf(&pb, incr_buf, incr_len);
		pend_tok(&pb, SEMI);
	}
	emit_goto(&pb, 'F', label_num, 'T');
	pend_tok(&pb, SEMI);
	emit_label(&pb, 'F', label_num, 'B');
}

/* Emit DO header: __DnT: */
static void emitDoHdr(void) {
	emit_label(&pb, 'D', label_num, 'T');
}

/* Emit DO trailer: if (cond) { goto __DnT; } __DnB: */
static void emitDoTrail(void) {
	if (cond_len > 0) {
		pend_tok(&pb, IF);
		pend_tok(&pb, LPAR);
		pend_buf(&pb, cond_buf, cond_len);
		pend_tok(&pb, RPAR);
		pend_tok(&pb, BEGIN);
		emit_goto(&pb, 'D', label_num, 'T');
		pend_tok(&pb, SEMI);
		pend_tok(&pb, END);
	}
	emit_label(&pb, 'D', label_num, 'B');
}

/* Emit switch exit label: __SnB: */
static void emitSwitchEnd(void) {
	emit_label(&pb, 'S', label_num, 'B');
}

/*
 * Find innermost breakable context and emit appropriate goto.
 * Check current context first, then stack.
 * Returns 1 if handled, 0 if no context (shouldn't happen).
 */
static int emit_break(void) {
	int i;
	/* Check current context first */
	if (is_breakable(cur_ctx)) {
		emit_goto(&pb, ctx_prefix(cur_ctx), label_num, 'B');
		return 1;
	}
	/* Then check stack */
	for (i = stk_sp - 1; i >= 0; i--) {
		if (is_breakable(stk[i].ctx_type)) {
			emit_goto(&pb, ctx_prefix(stk[i].ctx_type), stk[i].label_num, 'B');
			return 1;
		}
	}
	return 0;
}

/*
 * Find innermost loop and emit continue goto.
 * Check current context first, then stack.
 * Returns 1 if handled, 0 if not in a loop.
 */
static int emit_continue(void) {
	int i;
	/* Check current context first */
	if (cur_ctx == CTX_WHILE || cur_ctx == CTX_DO) {
		emit_goto(&pb, ctx_prefix(cur_ctx), label_num, 'T');
		return 1;
	}
	if (cur_ctx == CTX_FOR) {
		emit_goto(&pb, 'F', label_num, 'C');
		return 1;
	}
	/* Then check stack (skip current switch if any) */
	for (i = stk_sp - 1; i >= 0; i--) {
		unsigned char t = stk[i].ctx_type;
		if (t == CTX_WHILE || t == CTX_DO) {
			emit_goto(&pb, ctx_prefix(t), stk[i].label_num, 'T');
			return 1;
		}
		if (t == CTX_FOR) {
			emit_goto(&pb, 'F', stk[i].label_num, 'C');
			return 1;
		}
	}
	return 0;
}

/* Handle body state for both loops and switches */
static int handle_body(struct token *t, unsigned char ctx_type) {
	if (t->type == BEGIN) {
		body_depth++;
#ifdef DEBUG
		if (VERBOSE(V_FILTER))
			fdprintf(2, "CTRL: body BEGIN depth=%d\n", body_depth);
#endif
	} else if (t->type == END) {
		body_depth--;
#ifdef DEBUG
		if (VERBOSE(V_FILTER))
			fdprintf(2, "CTRL: body END depth=%d\n", body_depth);
#endif
		if (body_depth == 0) {
			pend_push(&pb, t);
			return 1;	/* Body complete */
		}
	}
	/* Nested control structures */
	if (t->type == WHILE) {
		push_ctx(ctx_type, state);
		label_num = next_label++;
		init_len = cond_len = 0;
		depth = 0;
		state = ST_WHILE_C;
		return 2;	/* Consumed, recurse */
	}
	if (t->type == FOR) {
		push_ctx(ctx_type, state);
		label_num = next_label++;
		init_len = cond_len = incr_len = 0;
		depth = 0;
		state = ST_FOR_INIT;
		return 2;
	}
	if (t->type == DO) {
		push_ctx(ctx_type, state);
		label_num = next_label++;
		emitDoHdr();
		body_depth = 0;
		state = ST_DO_BODY;
		return 3;	/* Return pend_pop */
	}
	if (t->type == SWITCH) {
		push_ctx(ctx_type, state);
		label_num = next_label++;
		depth = 0;
		state = ST_SW_COND;
		return 0;	/* Pass through */
	}
	if (t->type == BREAK) {
		if (emit_break())
			return 3;
	}
	if (t->type == CONTINUE) {
		if (emit_continue())
			return 3;
	}
	return 0;	/* Pass through */
}

void
filtctrl(struct token *out)
{
	struct token t;
	int r;

	if (filt_entry(&pb, out, upstream, &t)) {
#ifdef DEBUG
		track_ctrl(out);
#endif
		return;
	}

	switch (state) {
	case ST_NORMAL:
		if (t.type == WHILE) {
			label_num = next_label++;
			init_len = cond_len = 0;
			depth = 0;
			state = ST_WHILE_C;
			filtctrl(out);
			return;
		}
		if (t.type == FOR) {
			label_num = next_label++;
			init_len = cond_len = incr_len = 0;
			depth = 0;
			state = ST_FOR_INIT;
			filtctrl(out);
			return;
		}
		if (t.type == DO) {
			label_num = next_label++;
			emitDoHdr();
			body_depth = 0;
			state = ST_DO_BODY;
			pop_out(out);
			return;
		}
		if (t.type == SWITCH) {
			label_num = next_label++;
			depth = 0;
			state = ST_SW_COND;
		}
		break;

	case ST_WHILE_C:
		if (t.type == LPAR) {
			depth++;
			if (depth == 1) {
				filtctrl(out);
				return;
			}
		} else if (t.type == RPAR) {
			depth--;
			if (depth == 0) {
				emitLoopHdr('W');
				body_depth = 0;
				cur_ctx = CTX_WHILE;
				state = ST_LOOP_BODY;
				pop_out(out);
				return;
			}
		}
		if (depth > 0 && cond_len < BUF_MAX)
			tokcpy(&cond_buf[cond_len++], &t);
		filtctrl(out);
		return;

	case ST_FOR_INIT:
		if (t.type == LPAR) {
			depth++;
			if (depth == 1) {
				filtctrl(out);
				return;
			}
		} else if (t.type == SEMI && depth == 1) {
			state = ST_FOR_COND;
			filtctrl(out);
			return;
		}
		if (depth > 0 && init_len < BUF_MAX)
			tokcpy(&init_buf[init_len++], &t);
		filtctrl(out);
		return;

	case ST_FOR_COND:
		if (t.type == SEMI) {
			state = ST_FOR_INCR;
			filtctrl(out);
			return;
		}
		if (cond_len < BUF_MAX)
			tokcpy(&cond_buf[cond_len++], &t);
		filtctrl(out);
		return;

	case ST_FOR_INCR:
		if (t.type == RPAR) {
			depth--;
			if (depth == 0) {
#ifdef DEBUG
				if (VERBOSE(V_FILTER))
					fdprintf(2, "filtctrl: FOR emit init=%d cond=%d incr=%d\n",
						init_len, cond_len, incr_len);
#endif
				emitLoopHdr('F');
				body_depth = 0;
				cur_ctx = CTX_FOR;
				state = ST_LOOP_BODY;
				pop_out(out);
				return;
			}
		} else if (t.type == LPAR) {
			depth++;
		}
		if (incr_len < BUF_MAX)
			tokcpy(&incr_buf[incr_len++], &t);
		filtctrl(out);
		return;

	case ST_LOOP_BODY:
		r = handle_body(&t, cur_ctx);
		if (r == 1) {
			/* Body complete - emit trailer based on current context */
			if (cur_ctx == CTX_FOR)
				emitForTrail();
			else
				emitWhileTrail();
			pop_ctx();
			pop_out(out);
			return;
		}
		if (r == 2) {
			filtctrl(out);
			return;
		}
		if (r == 3) {
			pop_out(out);
			return;
		}
		break;

	case ST_DO_BODY:
		r = handle_body(&t, CTX_DO);
		if (r == 1) {
			state = ST_DO_COND;
			depth = -1;
			cond_len = 0;
			pop_out(out);
			return;
		}
		if (r == 2) {
			filtctrl(out);
			return;
		}
		if (r == 3) {
			pop_out(out);
			return;
		}
		break;

	case ST_DO_COND:
		if (depth == -1) {
			if (t.type == WHILE) {
				depth = 0;
				filtctrl(out);
				return;
			}
			break;
		}
		if (t.type == LPAR) {
			depth++;
			if (depth == 1) {
				filtctrl(out);
				return;
			}
		} else if (t.type == RPAR) {
			depth--;
			if (depth == 0) {
				emitDoTrail();
				pop_ctx();
				upstream(&t);
				if (t.type != SEMI)
					pend_push(&pb, &t);
				pop_out(out);
				return;
			}
		}
		if (depth > 0 && cond_len < BUF_MAX)
			tokcpy(&cond_buf[cond_len++], &t);
		filtctrl(out);
		return;

	case ST_SW_COND:
		if (t.type == LPAR)
			depth++;
		else if (t.type == RPAR) {
			depth--;
			if (depth == 0) {
				state = ST_SW_BODY;
				body_depth = 0;
				cur_ctx = CTX_SWITCH;
			}
		}
		break;

	case ST_SW_BODY:
		r = handle_body(&t, CTX_SWITCH);
		if (r == 1) {
			emitSwitchEnd();
			pop_ctx();
			pop_out(out);
			return;
		}
		if (r == 2) {
			filtctrl(out);
			return;
		}
		if (r == 3) {
			pop_out(out);
			return;
		}
		break;
	}

	tokcpy(out, &t);
#ifdef DEBUG
	track_ctrl(out);
#endif
}
