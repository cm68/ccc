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
struct stkent {
	unsigned char ctx_type;		/* CTX_WHILE, CTX_FOR, CTX_DO, CTX_SWITCH */
	unsigned char saved_state;
	unsigned char saved_ctx;	/* Saved cur_ctx */
	int label_num;
	int body_depth;
	/* Saved FOR increment buffer (dynamically allocated when needed) */
	struct token *saved_incr;
	int saved_incr_len;
};
static struct filter_stack fs;

static int state = ST_NORMAL;
static int depth = 0;			/* Paren depth while buffering */
static int body_depth = 0;		/* Brace depth in body */
static int label_num = 0;
static int next_label = 1;
static unsigned char cur_ctx = 0;	/* Current context type */

/* Token buffers - dynamically allocated */
static struct tokarray cond_arr;
static struct tokarray init_arr;
static struct tokarray incr_arr;

/* Output queue - dynamically allocated */
static struct pendbuf pb;

static void (*upstream)(struct token *);

#ifdef DEBUG
static int ctrl_balance = 0;  /* Track output brace balance */
#endif

void
filtctrl_init(void (*up)(struct token *))
{
	int i;
	upstream = up;
	state = ST_NORMAL;
	/* Free any leftover saved_incr buffers from previous run */
	for (i = 0; i < fs.sp; i++) {
		struct stkent *ent = (struct stkent *)fs.buf + i;
		if (ent->saved_incr) {
			free(ent->saved_incr);
			ent->saved_incr = 0;
		}
	}
	fstack_init(&fs, 8, sizeof(struct stkent));

	/* Initialize dynamic buffers (first call only) */
	if (!cond_arr.buf) {
		tarr_init(&cond_arr, 16);
		tarr_init(&init_arr, 16);
		tarr_init(&incr_arr, 16);
		pend_init(&pb, 32);
	} else {
		tarr_reset(&cond_arr);
		tarr_reset(&init_arr);
		tarr_reset(&incr_arr);
	}
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
			 ctrl_balance, fs.sp);
	if (ctrl_balance != 0)
		fdprintf(2, "CTRL: WARNING balance=%d at EOF\n",
			 ctrl_balance);
	if (fs.sp != 0)
		fdprintf(2, "CTRL: WARNING stk_sp=%d at EOF\n", fs.sp);
#endif
}

static void push_ctx(unsigned char type, unsigned char saved) {
	int i;
	struct stkent ent;
	ent.ctx_type = type;
	ent.saved_state = saved;
	ent.saved_ctx = cur_ctx;
	ent.label_num = label_num;
	ent.body_depth = body_depth;
	/* Save FOR increment buffer (overwritten by nested loops) */
	if (cur_ctx == CTX_FOR && incr_arr.count > 0) {
		ent.saved_incr = malloc(incr_arr.count * sizeof(struct token));
		for (i = 0; i < incr_arr.count; i++)
			tokcpy(&ent.saved_incr[i], &incr_arr.buf[i]);
		ent.saved_incr_len = incr_arr.count;
	} else {
		ent.saved_incr = 0;
		ent.saved_incr_len = 0;
	}
	fstack_push(&fs, &ent);
}

static void pop_ctx(void) {
	int i;
	struct stkent ent;
	if (fs.sp > 0) {
		fstack_pop(&fs, &ent);
		label_num = ent.label_num;
		body_depth = ent.body_depth;
		cur_ctx = ent.saved_ctx;
		state = ent.saved_state;
		/* Restore FOR increment buffer if saved */
		if (ent.saved_incr) {
			incr_arr.count = ent.saved_incr_len;
			for (i = 0; i < incr_arr.count; i++)
				tokcpy(&incr_arr.buf[i], &ent.saved_incr[i]);
			free(ent.saved_incr);
		}
	} else {
		state = ST_NORMAL;
		cur_ctx = 0;
	}
}

/* Lookup: CTX_WHILE=1->'W', CTX_FOR=2->'F', CTX_DO=3->'D', CTX_SWITCH=4->'S' */
static char ctx_pfx[] = "XWFDS";

/*
 * Per-ctx target suffix for break / continue.  0 = not applicable.
 *      [0]=unused  WHILE  FOR  DO  SWITCH
 */
static char brk_sfx[] = { 0, 'B', 'B', 'B', 'B' };
static char cnt_sfx[] = { 0, 'T', 'C', 'C',  0  };

/* Token sequences for loop/condition emission */
static unsigned char loop_cond_pre[] = { IF, LPAR, BANG, LPAR, 0 };
static unsigned char loop_cond_mid[] = { RPAR, RPAR, BEGIN, 0 };
static unsigned char loop_cond_end[] = { SEMI, END, 0 };
static unsigned char do_cond_pre[]   = { IF, LPAR, 0 };
static unsigned char do_cond_mid[]   = { RPAR, BEGIN, 0 };

/* Emit loop header: [init;] __XnT: if (!(cond)) goto __XnB; */
static void emitLoopHdr(char prefix) {
#ifdef DEBUG
	if (VERBOSE(V_FILTER))
		fdprintf(2, "emitLoopHdr: pb.rd=%d pb.wr=%d\n", pb.rd, pb.wr);
#endif
	/* No wrapper braces - filtbrace guarantees braced body */
	if (init_arr.count > 0) {
		pend_buf(&pb, init_arr.buf, init_arr.count);
		pend_tok(&pb, SEMI);
	}
	emit_label(&pb, prefix, label_num, 'T');
	if (cond_arr.count > 0) {
		pend_seq(&pb, loop_cond_pre);
		pend_buf(&pb, cond_arr.buf, cond_arr.count);
		pend_seq(&pb, loop_cond_mid);
		emit_goto(&pb, prefix, label_num, 'B');
		pend_seq(&pb, loop_cond_end);
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
	if (incr_arr.count > 0) {
		pend_buf(&pb, incr_arr.buf, incr_arr.count);
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

/* Emit DO trailer: __DnC: if (cond) { goto __DnT; } __DnB:
 * __DnC precedes the condition so `continue` re-tests it (C semantics)
 * instead of jumping to the top of the body. */
static void emitDoTrail(void) {
	emit_label(&pb, 'D', label_num, 'C');
	if (cond_arr.count > 0) {
		pend_seq(&pb, do_cond_pre);
		pend_buf(&pb, cond_arr.buf, cond_arr.count);
		pend_seq(&pb, do_cond_mid);
		emit_goto(&pb, 'D', label_num, 'T');
		pend_seq(&pb, loop_cond_end);
	}
	emit_label(&pb, 'D', label_num, 'B');
}

/* Emit switch exit label: __SnB: */
static void emitSwitchEnd(void) {
	emit_label(&pb, 'S', label_num, 'B');
}

/*
 * Find innermost context matching sfx_tab (brk_sfx for break, cnt_sfx
 * for continue) and emit goto to its label.  Walks current ctx first,
 * then stack inner-to-outer.  Returns 1 if emitted, 0 if no match.
 */
static int emit_jump(char *sfx_tab) {
	int i;
	struct stkent *ent;
	unsigned char ct = cur_ctx;
	int ln = label_num;

	for (i = fs.sp; ; i--) {
		if (ct >= CTX_WHILE && ct <= CTX_SWITCH && sfx_tab[ct]) {
			emit_goto(&pb, ctx_pfx[ct], ln, sfx_tab[ct]);
			return 1;
		}
		if (i <= 0) return 0;
		ent = (struct stkent *)fs.buf + i - 1;
		ct = ent->ctx_type;
		ln = ent->label_num;
	}
}

/* Handle body state for both loops and switches */
static int handle_body(struct token *t, unsigned char ctx_type) {
	if (token_props[t->type] & TF_OPEN) {
		body_depth++;
#ifdef DEBUG
		if (VERBOSE(V_FILTER))
			fdprintf(2, "CTRL: body BEGIN depth=%d\n", body_depth);
#endif
	} else if (token_props[t->type] & TF_CLOSE) {
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
	/* Nested control structures (IF passes through to filtbrace) */
	if (t->type == WHILE || t->type == FOR || t->type == SWITCH) {
		push_ctx(ctx_type, state);
		label_num = next_label++;
		tarr_reset(&init_arr);
		tarr_reset(&cond_arr);
		depth = 0;
		if (t->type == SWITCH) {
			state = ST_SW_COND;
			return 0;	/* Pass through */
		} else if (t->type == WHILE) {
			state = ST_WHILE_C;
		} else { /* FOR */
			tarr_reset(&incr_arr);
			state = ST_FOR_INIT;
		}
		return 2;	/* Consumed, recurse */
	}
	if (token_props[t->type] & TF_DO) {
		push_ctx(ctx_type, state);
		label_num = next_label++;
		emitDoHdr();
		body_depth = 0;
		cur_ctx = CTX_DO;
		state = ST_DO_BODY;
		return 3;	/* Return pend_pop */
	}
	if (t->type == BREAK) {
		if (emit_jump(brk_sfx))
			return 3;
	}
	if (t->type == CONTINUE) {
		if (emit_jump(cnt_sfx))
			return 3;
	}
	return 0;	/* Pass through */
}

void
filtctrl(struct token *out)
{
	struct token t;
	int r;

restart:
	if (filt_entry(&pb, out, upstream, &t)) {
#ifdef DEBUG
		track_ctrl(out);
#endif
		return;
	}

	switch (state) {
	case ST_NORMAL:
		/*
		 * Only WHILE/FOR/SWITCH need lowering here; IF is also
		 * flagged TF_COND but is handled entirely by filtbrace.
		 */
		if (t.type == WHILE || t.type == FOR || t.type == SWITCH) {
			label_num = next_label++;
			tarr_reset(&init_arr);
			tarr_reset(&cond_arr);
			depth = 0;
			if (t.type == SWITCH) {
				/* SWITCH itself passes through to the output */
				state = ST_SW_COND;
				break;
			} else if (t.type == WHILE) {
				state = ST_WHILE_C;
			} else { /* FOR */
				tarr_reset(&incr_arr);
				state = ST_FOR_INIT;
			}
			goto restart;
		}
		if (token_props[t.type] & TF_DO) {
			label_num = next_label++;
			emitDoHdr();
			body_depth = 0;
			cur_ctx = CTX_DO;
			state = ST_DO_BODY;
			pop_out(out);
			return;
		}
		break;

	case ST_WHILE_C:
		if (t.type == LPAR) {
			depth++;
			if (depth == 1) {
				goto restart;
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
		if (depth > 0)
			tarr_push(&cond_arr, &t);
		goto restart;

	case ST_FOR_INIT:
		if (t.type == LPAR) {
			depth++;
			if (depth == 1) {
				goto restart;
			}
		} else if (t.type == SEMI && depth == 1) {
			state = ST_FOR_COND;
			goto restart;
		}
		if (depth > 0)
			tarr_push(&init_arr, &t);
		goto restart;

	case ST_FOR_COND:
		if (t.type == SEMI) {
			state = ST_FOR_INCR;
			goto restart;
		}
		tarr_push(&cond_arr, &t);
		goto restart;

	case ST_FOR_INCR:
		if (t.type == RPAR) {
			depth--;
			if (depth == 0) {
#ifdef DEBUG
				if (VERBOSE(V_FILTER))
					fdprintf(2, "filtctrl: FOR emit init=%d cond=%d incr=%d\n",
						init_arr.count, cond_arr.count, incr_arr.count);
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
		tarr_push(&incr_arr, &t);
		goto restart;

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
			goto restart;
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
			tarr_reset(&cond_arr);
			pop_out(out);
			return;
		}
		if (r == 2) {
			goto restart;
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
				goto restart;
			}
			break;
		}
		if (t.type == LPAR) {
			depth++;
			if (depth == 1) {
				goto restart;
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
		if (depth > 0)
			tarr_push(&cond_arr, &t);
		goto restart;

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
			goto restart;
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
