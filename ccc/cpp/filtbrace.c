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
#define ST_DO_WHILE  5	/* After DO body, waiting for while (cond) */

#ifdef DEBUG
static const char *stname[] = {
	"NORMAL", "COND", "PENDING", "BODY", "ELSE_CHK", "DO_WHILE"
};
#endif

/* State stack for nested control structures */
#define STK_MAX 16
static struct {
	unsigned char ctrl_type;
	unsigned char is_else;		/* 1 if this is an else body */
} stk[STK_MAX];
static int stk_sp = 0;

static int state = ST_NORMAL;
static int depth = 0;
static unsigned char ctrl_type = 0;
static struct token saved_ctrl;	/* Control keyword deferred from ST_PENDING */
static int has_saved = 0;

/* Pending token queue - dynamically allocated */
static struct pendbuf pb;

/* Upstream token source */
static void (*upstream)(struct token *);

/* Debug: track synthetic brace balance */
#ifdef DEBUG
static int synth_balance = 0;
static int out_balance = 0;  /* Track ALL output braces */
#endif

void
filtbrace_init(void (*up)(struct token *))
{
	upstream = up;
	state = ST_NORMAL;
	depth = 0;
	ctrl_type = 0;
	stk_sp = 0;
	has_saved = 0;
	pend_init(&pb, 8);
#ifdef DEBUG
	synth_balance = 0;
	out_balance = 0;
#endif
}

#ifdef DEBUG
/* Track output brace balance */
static void
track_out(struct token *t)
{
	if (t->type == BEGIN)
		out_balance++;
	else if (t->type == END)
		out_balance--;
}
#endif

#ifdef DEBUG
static const char *
ctrlname(unsigned char c)
{
	switch (c) {
	case IF: return "IF";
	case ELSE: return "ELSE";
	case WHILE: return "WHILE";
	case FOR: return "FOR";
	case DO: return "DO";
	default: return "?";
	}
}
#endif

/* Push a synthetic body entry */
static void
push_body(unsigned char ctrl, unsigned char is_else)
{
	if (stk_sp < STK_MAX) {
		stk[stk_sp].ctrl_type = ctrl;
		stk[stk_sp].is_else = is_else;
		stk_sp++;
#ifdef DEBUG
		if (VERBOSE(V_FILTER))
			fdprintf(2, "BRACE: push(%s,%d) sp=%d\n",
				 ctrlname(ctrl), is_else, stk_sp);
#endif
	}
}

/* Pop body entry */
static void
pop_body(void)
{
	if (stk_sp > 0) {
		stk_sp--;
#ifdef DEBUG
		if (VERBOSE(V_FILTER))
			fdprintf(2, "BRACE: pop() sp=%d\n", stk_sp);
#endif
	}
}

/* Emit synthetic BEGIN and track balance */
static void
emit_begin(struct token *out)
{
	toksynth(out, BEGIN);
#ifdef DEBUG
	synth_balance++;
	out_balance++;
	if (VERBOSE(V_FILTER))
		fdprintf(2, "BRACE: emit { (synth_bal=%d out_bal=%d)\n",
			 synth_balance, out_balance);
#endif
}

/* Queue synthetic END and track balance */
static void
queue_end(void)
{
	pend_tok(&pb, END);
#ifdef DEBUG
	synth_balance--;
	if (VERBOSE(V_FILTER))
		fdprintf(2, "BRACE: queue } (synth_bal=%d)\n", synth_balance);
#endif
}

void
filtbrace(struct token *out)
{
	struct token t;
#ifdef DEBUG
	int old_state = state;
#endif

	/* Check pending buffer FIRST - must drain before processing saved */
	if (pend_has(&pb)) {
		pend_pop(&pb, out);
#ifdef DEBUG
		track_out(out);
#endif
		return;
	}

	/* Then process saved control keyword */
	if (has_saved) {
		tokcpy(&t, &saved_ctrl);
		has_saved = 0;
#ifdef DEBUG
		if (VERBOSE(V_FILTER))
			fdprintf(2, "BRACE: restore saved t=%d\n", t.type);
#endif
	} else {
		upstream(&t);
		/* Pass through E_O_F */
		if (t.type == E_O_F) {
			tokcpy(out, &t);
			return;
		}
	}

#ifdef DEBUG
	if (VERBOSE(V_FILTER))
		fdprintf(2, "BRACE: [%s] sp=%d depth=%d t=%d\n",
			 stname[state], stk_sp, depth, t.type);
#endif

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
			if (depth == 0) {
				/* DO's while(cond) doesn't need brace insertion */
				if (ctrl_type == DO)
					state = ST_NORMAL;
				else
					state = ST_PENDING;
			}
		}
		break;

	case ST_PENDING:
		if (t.type == BEGIN) {
			/* User wrote braces */
			if (stk_sp > 0) {
				/* Inside synthetic body - track nested user-braced control */
				state = ST_BODY;
				depth = 1;	/* Count this BEGIN */
			} else {
				state = ST_NORMAL;
			}
			break;
		}
		if (ctrl_type == ELSE && t.type == IF) {
			/* else if - no brace needed, continue with if */
			ctrl_type = IF;
			state = ST_COND;
			depth = 0;
			break;
		}
		/* Insert { before this token */
		push_body(ctrl_type, ctrl_type == ELSE ? 1 : 0);
		if (t.type == IF || t.type == WHILE || t.type == FOR) {
			/* Nested control - save for processing after { */
			tokcpy(&saved_ctrl, &t);
			has_saved = 1;
			ctrl_type = t.type;
			state = ST_COND;
			depth = 0;
			emit_begin(out);
			return;
		}
		if (t.type == DO || t.type == ELSE) {
			/* Nested DO or ELSE - save for processing after { */
			tokcpy(&saved_ctrl, &t);
			has_saved = 1;
			ctrl_type = t.type;
			state = ST_PENDING;
			emit_begin(out);
			return;
		}
		/* Regular token - queue it, return { */
		pend_push(&pb, &t);
		state = ST_BODY;
		depth = 0;
		emit_begin(out);
		return;

	case ST_BODY:
		if (t.type == BEGIN || t.type == LPAR || t.type == LBRACK)
			depth++;
		else if (t.type == END || t.type == RPAR || t.type == RBRACK)
			depth--;

		if (depth == 0) {
			if (t.type == IF || t.type == WHILE || t.type == FOR) {
				ctrl_type = t.type;
				state = ST_COND;
				depth = 0;
				break;
			}
			if (t.type == ELSE || t.type == DO) {
				ctrl_type = t.type;
				state = ST_PENDING;
				break;
			}
			if (t.type == END) {
				/*
				 * User-braced control ended inside synthetic body.
				 * This control was the synthetic body's statement.
				 */
				if (ctrl_type == IF) {
					/* Check for else first */
					state = ST_ELSE_CHK;
				} else {
					/* Body complete - close synthetic body */
					pend_push(&pb, &t);
					queue_end();
					pop_body();
					/* Check if outer level needs else-check */
					if (stk_sp > 0 && stk[stk_sp - 1].ctrl_type == IF &&
					    !stk[stk_sp - 1].is_else)
						state = ST_ELSE_CHK;
					else
						state = (stk_sp > 0) ? ST_BODY : ST_NORMAL;
					pend_pop(&pb, out);
#ifdef DEBUG
					track_out(out);
#endif
					return;
				}
				break;
			}
			if (t.type == SEMI) {
				/* Body complete */
				pend_push(&pb, &t);
				if (stk_sp > 0 && stk[stk_sp - 1].is_else) {
					/* Else body - emit }, pop and cascade */
					queue_end();
					pop_body();
					/* Check if outer IF needs else-check */
					if (stk_sp > 0 && stk[stk_sp - 1].ctrl_type == IF &&
					    !stk[stk_sp - 1].is_else)
						state = ST_ELSE_CHK;
					else
						state = (stk_sp > 0) ? ST_BODY : ST_NORMAL;
				} else if (stk_sp > 0 && stk[stk_sp - 1].ctrl_type == IF) {
					/* If body - don't emit } yet, check for else */
					state = ST_ELSE_CHK;
				} else if (stk_sp > 0 && stk[stk_sp - 1].ctrl_type == DO) {
					/* Do body - emit }, wait for while */
					queue_end();
					pop_body();
					state = ST_DO_WHILE;
				} else if (stk_sp > 0) {
					/* Other (WHILE/FOR) - emit }, pop and continue */
					queue_end();
					pop_body();
					state = (stk_sp > 0) ? ST_BODY : ST_NORMAL;
				} else {
					/* No synthetic body - shouldn't happen */
					state = ST_NORMAL;
				}
				pend_pop(&pb, out);
#ifdef DEBUG
				track_out(out);
#endif
				return;
			}
		}
		break;

	case ST_ELSE_CHK:
		if (t.type == ELSE) {
			/* Found else - close if body, pop it, start else */
			if (stk_sp > 0 && stk[stk_sp - 1].ctrl_type == IF) {
				queue_end();
				pop_body();
			}
			ctrl_type = ELSE;
			state = ST_PENDING;
			pend_push(&pb, &t);
			pend_pop(&pb, out);
#ifdef DEBUG
			track_out(out);
#endif
			return;
		}
		/* No else found */
		if (stk_sp > 0 && stk[stk_sp - 1].ctrl_type != IF) {
			/*
			 * User-braced IF inside synthetic body (FOR/WHILE/DO).
			 * The IF was the synthetic body's single statement.
			 * Close the synthetic body, then check if outer level
			 * needs else-check.
			 */
			queue_end();
			pop_body();
			if (stk_sp > 0 && stk[stk_sp - 1].ctrl_type == IF &&
			    !stk[stk_sp - 1].is_else) {
				/* Outer level is IF - check for else */
				tokcpy(&saved_ctrl, &t);
				has_saved = 1;
				state = ST_ELSE_CHK;
				pend_pop(&pb, out);
#ifdef DEBUG
				track_out(out);
#endif
				return;
			}
			state = (stk_sp > 0) ? ST_BODY : ST_NORMAL;
			break;
		}
		/* Close all pending bodies */
		while (stk_sp > 0) {
			if (stk[stk_sp - 1].ctrl_type == IF && !stk[stk_sp - 1].is_else) {
				/* IF without else - close it */
				queue_end();
				pop_body();
				/* Check if next level needs else check */
				if (stk_sp > 0 && stk[stk_sp - 1].ctrl_type == IF &&
				    !stk[stk_sp - 1].is_else) {
					/* More IF to check - save token, return } */
					tokcpy(&saved_ctrl, &t);
					has_saved = 1;
					state = ST_ELSE_CHK;
					pend_pop(&pb, out);
#ifdef DEBUG
					track_out(out);
#endif
					return;
				}
			} else if (stk[stk_sp - 1].is_else) {
				/* Else body already closed, just pop */
				pop_body();
			} else {
				/* WHILE/FOR body - close it */
				queue_end();
				pop_body();
			}
		}
		state = ST_NORMAL;
		if (pend_has(&pb)) {
			/* Save current token for re-processing after pending */
			tokcpy(&saved_ctrl, &t);
			has_saved = 1;
			pend_pop(&pb, out);
#ifdef DEBUG
			track_out(out);
#endif
			return;
		}
		break;

	case ST_DO_WHILE:
		/* After DO body, wait for while (cond); */
		if (t.type == WHILE) {
			state = ST_COND;
			ctrl_type = DO;
			depth = 0;
			break;
		}
		/* Not WHILE - syntax error, but pass through */
		state = ST_NORMAL;
		break;
	}

#ifdef DEBUG
	if (VERBOSE(V_FILTER) && state != old_state)
		fdprintf(2, "BRACE: state %s -> %s\n",
			 stname[old_state], stname[state]);
#endif

	tokcpy(out, &t);
#ifdef DEBUG
	track_out(out);
#endif
}

/*
 * Check brace balance at end of file.
 * Call this before emitting E_O_F to catch filtbrace bugs.
 */
void
filtbraceChk(void)
{
#ifdef DEBUG
	if (VERBOSE(V_FILTER))
		fdprintf(2, "BRACE: EOF synth=%d out=%d stk=%d\n",
			 synth_balance, out_balance, stk_sp);
	if (synth_balance != 0)
		fdprintf(2, "BRACE: WARNING synth_balance=%d at EOF\n",
			 synth_balance);
	if (out_balance != 0)
		fdprintf(2, "BRACE: WARNING out_balance=%d at EOF\n",
			 out_balance);
	if (stk_sp != 0)
		fdprintf(2, "BRACE: WARNING stk_sp=%d at EOF\n", stk_sp);
#endif
}
