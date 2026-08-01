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
static char *stname[] = {
	"NORMAL", "COND", "PENDING", "BODY", "ELSE_CHK", "DO_WHILE"
};
#endif

/* State stack entry for nested control structures */
struct stkent {
	unsigned char ctrl_type;
	unsigned char is_else;		/* 1 if this is an else body */
	unsigned char owner;		/* USER_BRACE: ctrl owning the braces */
	unsigned char bdepth;		/* brace_depth at push (USER_BRACE) */
};

/* Pseudo ctrl_type: user-braced body inside a synthetic body.  Its
 * contents are processed normally in ST_NORMAL; the matching } closes
 * the enclosing synthetic bodies. */
#define USER_BRACE 255

/* Dynamic state stack */
static struct filter_stack fs;

static unsigned char state = ST_NORMAL;
static unsigned char depth = 0;
static unsigned char ctrl_type = 0;
/* ST_ELSE_CHK context: 1 = the if being checked had user braces (no
 * synthetic body entry to close when an else is found) */
static int else_user = 0;
static struct token saved_ctrl;	/* Control keyword deferred from ST_PENDING */
static int has_saved = 0;

/*
 * Tracking for user-braced `do { ... } while (cond);`.
 * brace_depth counts every input { (+1) and } (-1) seen by filtbrace.
 * do_stack[] holds the brace_depth value at each `do {` we are still inside.
 * When } drops brace_depth back to (top - 1), the matching `}` has arrived
 * and we transition to ST_DO_WHILE so the trailing `while (cond);` is not
 * mis-parsed as a fresh while loop.
 */
#define DO_STACK_MAX 16
static unsigned char brace_depth = 0;
static int do_stack[DO_STACK_MAX];
static int do_sp = 0;

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
	else_user = 0;
	fstack_setup(&fs, 8, sizeof(struct stkent));
	has_saved = 0;
	pend_setup(&pb, 8);
	brace_depth = 0;
	do_sp = 0;
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
static char *
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
	struct stkent ent;
	ent.ctrl_type = ctrl;
	ent.is_else = is_else;
	ent.owner = 0;
	ent.bdepth = brace_depth;
	fstack_push(&fs, &ent);
#ifdef DEBUG
	if (VERBOSE(V_FILTER))
		fdprintf(2, "BRACE: push(%s,%d) sp=%d\n",
			 ctrlname(ctrl), is_else, fs.sp);
#endif
}

static void pop_body(void);
static void queue_end(void);

/*
 * A synthetic body's single statement just completed: close every
 * enclosing synthetic body too (its statement has equally completed),
 * stopping at a user-braced block (it continues) or at an IF that
 * still needs its else-check.  Sets state accordingly.
 */
static void
cascade(void)
{
	struct stkent *top;

	while ((top = fstack_top(&fs)) != NULL) {
		unsigned char ct = top->ctrl_type;
		if (ct == USER_BRACE)
			break;	/* enclosing user block continues */
		if (ct == IF && !top->is_else) {
			/* defer its } until else-check */
			state = ST_ELSE_CHK;
			return;
		}
		queue_end();
		pop_body();
		if (ct == DO) {
			/* the trailing while (cond) ; follows */
			state = ST_DO_WHILE;
			return;
		}
	}
	state = ST_NORMAL;
}

/* Pop body entry */
static void
pop_body(void)
{
	fstack_pop(&fs, NULL);
#ifdef DEBUG
	if (VERBOSE(V_FILTER))
		fdprintf(2, "BRACE: pop() sp=%d\n", fs.sp);
#endif
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

/*
 * The statement that is a synthetic body has just ended.  Which body it
 * was decides what happens next: an if holds its } back until the
 * else-check, a do holds it until the trailing while, and anything else
 * closes here and lets the enclosing bodies close with it.
 */
static void
endbody(void)
{
	struct stkent *top = fstack_top(&fs);

	if (top && top->is_else) {
		/* Else body - emit }, pop and cascade */
		queue_end();
		pop_body();
		cascade();
	} else if (top && top->ctrl_type == IF) {
		/* If body - don't emit } yet, check for else */
		state = ST_ELSE_CHK;
	} else if (top && top->ctrl_type == DO) {
		/* Do body - emit }, wait for while */
		queue_end();
		pop_body();
		state = ST_DO_WHILE;
	} else if (fs.sp > 0) {
		/* Other (WHILE/FOR) - emit }, pop and cascade */
		queue_end();
		pop_body();
		cascade();
	} else {
		/* No synthetic body - shouldn't happen */
		state = ST_NORMAL;
	}
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

/*
 * If t is the closing } of a USER_BRACE frame, consume it: pop the
 * frame and close the enclosing synthetic bodies (pausing at an IF for
 * its else-check).  Returns 1 with *out filled, 0 if t is not ours.
 */
static int
user_end(struct token *t, struct token *out)
{
	struct stkent *top = fstack_top(&fs);
	unsigned char owner;

	if (t->type != END || !top || top->ctrl_type != USER_BRACE ||
	    brace_depth != top->bdepth - 1)
		return 0;
	owner = top->owner;
	pop_body();
	if (owner == IF) {
		/* an else may follow - defer closing the enclosing
		 * synthetic bodies */
		state = ST_ELSE_CHK;
		else_user = 1;
		tokcpy(out, t);
#ifdef DEBUG
		track_out(out);
#endif
		return 1;
	}
	pend_push(&pb, t);
	cascade();
	pend_pop(&pb, out);
#ifdef DEBUG
	track_out(out);
#endif
	return 1;
}

void
filtbrace(struct token *out)
{
	struct token t;
	struct stkent *top;
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
		/* Track depth of user-written braces from the input stream. */
		tok_depth(&t, &brace_depth);
	}

#ifdef DEBUG
	if (VERBOSE(V_FILTER))
		fdprintf(2, "BRACE: [%s] sp=%d depth=%d t=%d\n",
			 stname[state], fs.sp, depth, t.type);
#endif

redispatch:
	switch (state) {
	case ST_NORMAL:
		/*
		 * Matching `}` of a user-braced `do { ... }` body: expect
		 * the trailing while(cond);  brace_depth was already
		 * decremented for this END.
		 */
		if (t.type == END && do_sp > 0 &&
		    do_stack[do_sp - 1] == brace_depth + 1) {
			do_sp--;
			state = ST_DO_WHILE;
			break;
		}
		if (user_end(&t, out))
			return;
		if (token_props[t.type] & TF_COND) {
			ctrl_type = t.type;
			state = ST_COND;
			depth = 0;
		} else if (token_props[t.type] & (TF_ELSE | TF_DO)) {
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
			if (ctrl_type == DO && do_sp < DO_STACK_MAX) {
				/*
				 * `do {` - remember this brace depth so the
				 * matching `}` switches to ST_DO_WHILE
				 * regardless of fs.sp.
				 */
				do_stack[do_sp++] = brace_depth;
				if (fs.sp > 0) {
					/* legacy skip for do bodies */
					state = ST_BODY;
					depth = 1;	/* Count this BEGIN */
				} else {
					state = ST_NORMAL;
				}
				break;
			}
			if (fs.sp > 0) {
				/* User-braced body inside a synthetic body:
				 * process contents normally; the matching }
				 * (see ST_NORMAL) closes the synthetic bodies */
				push_body(USER_BRACE, 0);
				top = fstack_top(&fs);
				top->owner = ctrl_type;
			}
			state = ST_NORMAL;
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
		if (token_props[t.type] & TF_COND) {
			/* Nested control - save for processing after { */
			tokcpy(&saved_ctrl, &t);
			has_saved = 1;
			ctrl_type = t.type;
			state = ST_COND;
			depth = 0;
			emit_begin(out);
			return;
		}
		if (token_props[t.type] & (TF_DO | TF_ELSE)) {
			/* Nested DO or ELSE - save for processing after { */
			tokcpy(&saved_ctrl, &t);
			has_saved = 1;
			ctrl_type = t.type;
			state = ST_PENDING;
			emit_begin(out);
			return;
		}
		/* Regular token - queue it, return {.  The queued token skips
		 * ST_BODY's depth tracking, so count it here if it opens a
		 * group (statement starting with parens: `(*p).f = 1;`) */
		pend_push(&pb, &t);
		if (t.type == SEMI) {
			/*
			 * The body is the empty statement, and the token that
			 * is the whole of it was just queued past ST_BODY -
			 * where a semicolon is the thing that closes a
			 * synthetic body.  So nothing ever closed this one:
			 * "while (fgetc(fp) != '\0')\n\t;" opened a brace that
			 * stayed open, and pass1 reported the next function as
			 * unbalanced.  Close it here instead.
			 */
			endbody();
			emit_begin(out);
			return;
		}
		state = ST_BODY;
		depth = (token_props[t.type] & TF_OPEN) ? 1 : 0;
		emit_begin(out);
		return;

	case ST_BODY:
		if (token_props[t.type] & TF_OPEN)
			depth++;
		else if (token_props[t.type] & TF_CLOSE)
			depth--;

		if (depth == 0) {
			if (token_props[t.type] & TF_COND) {
				ctrl_type = t.type;
				state = ST_COND;
				depth = 0;
				break;
			}
			if (t.type == ELSE) {
				/*
				 * ELSE after single-stmt body (like `if (a) ; else`).
				 * Close the IF body first, then process ELSE.
				 */
				top = fstack_top(&fs);
				if (top && top->ctrl_type == IF) {
					queue_end();
					pop_body();
				}
				ctrl_type = ELSE;
				state = ST_PENDING;
				pend_thru(&pb, &t, out);
#ifdef DEBUG
				track_out(out);
#endif
				return;
			}
			if (t.type == DO) {
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
					else_user = 1;
				} else {
					/* Body complete - close synthetic body */
					pend_push(&pb, &t);
					queue_end();
					pop_body();
					cascade();
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
				endbody();
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
			/* Found else - close if body, pop it, start else.
			 * A user-braced if has no synthetic entry: the top IF
			 * (if any) belongs to an enclosing if - leave it. */
			top = fstack_top(&fs);
			if (!else_user && top && top->ctrl_type == IF) {
				queue_end();
				pop_body();
			}
			else_user = 0;
			ctrl_type = ELSE;
			state = ST_PENDING;
			pend_thru(&pb, &t, out);
#ifdef DEBUG
			track_out(out);
#endif
			return;
		}
		/* No else found: close the checked if (when synthetic - a
		 * user-braced if is already closed) and cascade.  cascade()
		 * pauses at an outer IF, re-entering ST_ELSE_CHK with this
		 * same token via the save/redispatch below. */
		if (!else_user) {
			top = fstack_top(&fs);
			if (top && top->ctrl_type == IF && !top->is_else) {
				queue_end();
				pop_body();
			}
		}
		else_user = 0;
		cascade();
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
		/* Reprocess: the token may itself be a control keyword or
		 * the closing } of an enclosing user-braced block */
		goto redispatch;

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
			 synth_balance, out_balance, fs.sp);
	if (synth_balance != 0)
		fdprintf(2, "BRACE: WARNING synth_balance=%d at EOF\n",
			 synth_balance);
	if (out_balance != 0)
		fdprintf(2, "BRACE: WARNING out_balance=%d at EOF\n",
			 out_balance);
	if (fs.sp != 0)
		fdprintf(2, "BRACE: WARNING stk_sp=%d at EOF\n", fs.sp);
#endif
}
