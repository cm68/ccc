/*
 * filter.c - token filter for normalization
 *
 * Operates as a filter between the lexer's emit calls and the
 * actual .x file output. Tokens flow through this layer which
 * detects patterns and transforms them.
 *
 * Architecture:
 *   lexer -> emit*() -> filter() -> actual .x write
 *
 * Transformations:
 * - K&R to ANSI: int foo(a, b) int a; char *b; { -> int foo(int a, char *b) {
 * - Loop lowering: while/for/do -> label/goto/if
 * - Brace insertion: if (x) stmt; -> if (x) { stmt; }
 * - Local init split: int a = 1; -> int a; a = 1;
 */
#include "cpp.h"
#include <unistd.h>

/*
 * Filter state machine
 */
#define ST_NORMAL    0		/* Pass tokens through */
#define ST_BUFFERING 1		/* Buffering potential K&R func def */
#define ST_PARAMS    2		/* Inside parameter list */
#define ST_POSTPAREN 3		/* After ), checking for K&R decls */
#define ST_DECLS     4		/* Parsing K&R declarations */
#define ST_TYPEDEF   5		/* Parsing typedef to find name */
#define ST_CTRL_COND 6		/* Inside control structure condition */
#define ST_CTRL_PEND 7		/* After condition, checking for { */
#define ST_CTRL_BODY 8		/* Inside synthetic block, waiting for ; */
#define ST_DO_BRACE  9		/* Inside braced DO body, waiting for } */
#define ST_LOCAL_DECL 10	/* Buffering local declaration for init split */
#define ST_LOCAL_POST 11	/* After local decl ;, checking for more decls */
#define ST_LOOP_COND  12	/* Buffering loop condition (while/for) */
#define ST_LOOP_BODY  13	/* Inside loop body, tracking depth */
#define ST_DO_BODY    14	/* Inside do body, waiting for while */
#define ST_DO_COND    15	/* Buffering do-while condition */

static unsigned char state = ST_NORMAL;
static int brace_depth = 0;

/*
 * Control structure brace insertion state
 * Normalizes single-statement bodies to blocks:
 *   if (cond) stmt;  ->  if (cond) { stmt; }
 *   while (cond) stmt;  ->  while (cond) { stmt; }
 *   for (...) stmt;  ->  for (...) { stmt; }
 *   do stmt; while  ->  do { stmt; } while
 *   if (...) {...} else stmt;  ->  if (...) {...} else { stmt; }
 */
static int ctrlParenDep = 0;		/* Paren depth in condition */
static int ctrlBodyDep = 0;			/* Brace/paren depth in synthetic body */
static unsigned char ctrl_type = 0;	/* IF, WHILE, FOR, DO, or ELSE */
static unsigned char saved_state = 0;	/* State to return to after ctrl */

/* Stack for nested synthetic blocks */
#define CTRL_STACK_SIZE 8
struct ctrl_frame {
	unsigned char ctrl_type;
	int ctrlBodyDep;
};
static struct ctrl_frame ctrl_stack[CTRL_STACK_SIZE];
static int ctrl_sp = 0;

/*
 * Typedef parsing state
 */
static int typedef_depth = 0;		/* Paren/bracket depth in typedef */
static char typedef_name[16];		/* Last identifier seen in typedef */
static int tdefNameDepth = 0;	/* Depth when name was seen */

/*
 * Token buffer - stores tokens during K&R detection (allocated on demand)
 */
#define MAX_TOKENS 256
struct buftok {
	unsigned char type;
	short lineno;		/* line number when token was scanned */
	union {
		long num;
		float fnum;
		char *str;		/* strdup'd for SYM, alloc'd for STRING */
	} v;
};
static struct buftok *tokbuf = NULL;
static int num_tokens = 0;

/* Track last emitted line for synthetic code */
static int lastEmitLine = 0;

/*
 * Parameter tracking - linked list, allocated on demand
 */
#define MAX_TYPE_TOKS 32
struct param {
	char name[16];
	struct buftok *type_toks;	/* allocated on demand */
	int num_type_toks;
	char has_type;
	struct param *next;
};
static struct param *params = NULL;
static int num_params = 0;
static int paren_depth = 0;	/* for tracking nested parens in decls */

/*
 * Typedef tracking - linked list
 */
struct typedef_node {
	char *name;
	struct typedef_node *next;
};
static struct typedef_node *typedefs = NULL;

/*
 * Local declaration initializer splitting
 * Transform: int a = 5, b = 6; → int a, b; a = 5; b = 6;
 *
 * Buffers declarations, strips = expr parts, and defers assignments
 * until a non-declaration is seen. Allocated on demand to save BSS.
 */
#define MAX_LOC_INITS 8
#define MAX_LOC_INIT_TOKS 16
#define MAX_LOC_DECL 64
struct local_init {
	char name[16];				/* Variable name */
	struct buftok *inittoks;	/* Init expression tokens (allocated) */
	int num_inittoks;
};
static struct local_init *locInits = NULL;
static int numLocInits = 0;

/* Buffer for current declaration (emitted without init parts) */
static struct buftok *locDecl = NULL;
static int numLocDecl = 0;

/* State within local declaration parsing */
static int locDeclParen = 0;	/* Paren depth within declaration */
static int locInInit = 0;		/* 1 if capturing init expr (after =) */
static char locCurName[16];		/* Current declarator name */

/*
 * Loop lowering state
 *
 * Transforms while/for/do loops into label/goto/if structures:
 *   while (cond) { body }  ->  { __W1T: if (!(cond)) goto __W1B; { body } goto __W1T; __W1B:; }
 *   for (i;c;n) { body }   ->  { i; __F1T: if (!(c)) goto __F1B; { body } __F1C: n; goto __F1T; __F1B:; }
 *   do { body } while (c); ->  { __D1T: { body } __D1C: if (c) goto __D1T; __D1B:; }
 *
 * Uses output buffer stack for body buffering (see io.c).
 */
#define LOOP_STACK_SIZE 8

struct loop_frame {
	unsigned char type;		/* WHILE, FOR, or DO */
	unsigned char savedState;	/* State to restore when loop ends */
	int label_num;			/* Unique label number */
	int body_depth;			/* Brace depth when body started */
	int saved_brace;		/* Saved brace_depth for restoration */
	/* Saved parent's FOR increment (for nested loops) */
	struct buftok *savedIncr;	/* Pointer to saved incr buffer (malloc) */
	int numSavedIncr;			/* Count of saved incr tokens */
};

static struct loop_frame loop_stack[LOOP_STACK_SIZE];
static int loop_sp = 0;
static int next_loop_num = 1;

/* Token buffer for loop condition (shared for while/for/do) - allocated on demand */
#define MAX_LOOP_TOKS 64
static struct buftok *loop_cond = NULL;
static int num_loop_cond = 0;
static int loopParen = 0;	/* Paren depth while buffering condition */

/* FOR loop: additional buffers for init and increment expressions */
static struct buftok *loop_init = NULL;
static int num_loop_init = 0;
static struct buftok *loop_incr = NULL;
static int num_loop_incr = 0;
static int for_part = 0;			/* 0=init, 1=cond, 2=incr */

/*
 * Switch statement tracking - break inside switch is lowered to goto.
 * We track switch body brace depths and label numbers. When BREAK is seen
 * inside a switch, emit goto __S<n>B. When switch body ends, emit __S<n>B: label.
 */
#define MAX_SWITCH_DEPTH 8
static int switchBraceStk[MAX_SWITCH_DEPTH];  /* Brace depths where switches started */
static int switchNumStk[MAX_SWITCH_DEPTH];    /* Label numbers for each switch */
static int switchStkTop = 0;
static int switchLblNum = 0;  /* Counter for switch labels */
static int switchParen = 0;   /* Paren depth in switch condition (>0 = waiting for body) */

/*
 * Add typedef name to tracking list
 */
void
filtAddTdef(char *name)
{
	struct typedef_node *node = malloc(sizeof(*node));
	node->name = strdup(name);
	node->next = typedefs;
	typedefs = node;
}

/*
 * Check if name is a known typedef
 */
static int
isTypedef(char *name)
{
	struct typedef_node *node;
	for (node = typedefs; node; node = node->next) {
		if (strcmp(node->name, name) == 0)
			return 1;
	}
	return 0;
}

/*
 * Check if token is a type-starting token
 */
static int
isTypeTok(unsigned char type, char *name)
{
	/* Type keywords: INT..VOID, TYPEDEF..REGISTER, ENUM, CONST, VOLATILE */
	if (type >= INT && type <= VOID)
		return 1;
	if (type >= TYPEDEF && type <= REGISTER)
		return 1;
	if (type == ENUM || type == CONST || type == VOLATILE)
		return 1;
	if (type == SYM && name && isTypedef(name))
		return 1;
	return 0;
}

/*
 * Fill a buftok with token data (core helper for all buffer functions)
 */
static void
fillBufTok(struct buftok *t, unsigned char type, long num, float fnum,
           char *str, int slen)
{
	t->type = type;
	if (type == SYM) {
		t->v.str = strdup(str);
	} else if (type == STRING) {
		t->v.str = malloc(slen + 1);
		memcpy(t->v.str, str, slen);
		t->v.str[slen] = 0;
	} else if (type == NUMBER) {
		t->v.num = num;
	} else if (type == FNUMBER) {
		t->v.fnum = fnum;
	}
}

/*
 * Buffer a token
 */
static void
bufToken(unsigned char type, long num, float fnum, char *str, int slen)
{
	if (num_tokens >= MAX_TOKENS)
		return;
	if (!tokbuf)
		tokbuf = malloc(MAX_TOKENS * sizeof(struct buftok));
	fillBufTok(&tokbuf[num_tokens], type, num, fnum, str, slen);
	tokbuf[num_tokens].lineno = lineno;
	num_tokens++;
}

/*
 * Emit a buffered token to real output
 */
static void
emitBufTok(struct buftok *t)
{
	switch (t->type) {
	case SYM:
		emitSym(t->v.str);
		break;
	case NUMBER:
		emitNumber(t->v.num);
		break;
	case FNUMBER:
		emitFNumber(t->v.fnum);
		break;
	case STRING:
		emitString(t->v.str, strlen(t->v.str));
		break;
	default:
		emitToken(t->type);
		break;
	}
}

/*
 * Emit a token by type (used throughout the filter)
 */
static void
emitByType(unsigned char type, long num, float fnum, char *str, int slen)
{
	switch (type) {
	case SYM:    emitSym(str); break;
	case NUMBER: emitNumber(num); break;
	case FNUMBER: emitFNumber(fnum); break;
	case STRING: emitString(str, slen); break;
	default:     emitToken(type); break;
	}
}

/*
 * Free buffered token strings
 */
static void
freeBufTok(struct buftok *t)
{
	if (t->type == SYM || t->type == STRING) {
		free(t->v.str);
		t->v.str = NULL;
	}
}

/*
 * Flush buffer - emit all buffered tokens as-is
 * Also updates brace_depth for any { or } in the buffer
 */
static void
flushBuf(void)
{
	int i;
	for (i = 0; i < num_tokens; i++) {
		if (tokbuf[i].type == BEGIN) {
			brace_depth++;
#ifdef DEBUG_KNR_CTRL
			fprintf(stderr, "KNR: flushBuf BEGIN, depth now %d\n", brace_depth);
#endif
		} else if (tokbuf[i].type == END) {
			brace_depth--;
#ifdef DEBUG_KNR_CTRL
			fprintf(stderr, "KNR: flushBuf END, depth now %d\n", brace_depth);
#endif
		}
		emitBufTok(&tokbuf[i]);
		freeBufTok(&tokbuf[i]);
	}
	num_tokens = 0;
}

/*
 * Clear buffer without emitting (used when transforming K&R)
 */
static void
clearBuf(void)
{
	int i;
	for (i = 0; i < num_tokens; i++) {
		freeBufTok(&tokbuf[i]);
	}
	num_tokens = 0;
}

/*
 * Buffer a token into local declaration buffer
 */
static void
bufLocDecl(unsigned char type, long num, float fnum, char *str, int slen)
{
	if (numLocDecl >= MAX_LOC_DECL)
		return;
	if (!locDecl)
		locDecl = malloc(MAX_LOC_DECL * sizeof(struct buftok));
	fillBufTok(&locDecl[numLocDecl], type, num, fnum, str, slen);
	numLocDecl++;
}

/*
 * Buffer a token into current local init expression
 */
static void
bufLocInit(unsigned char type, long num, float fnum, char *str, int slen)
{
	struct local_init *li;

	if (numLocInits == 0)
		return;
	li = &locInits[numLocInits - 1];
	if (li->num_inittoks >= MAX_LOC_INIT_TOKS)
		return;
	if (!li->inittoks)
		li->inittoks = malloc(MAX_LOC_INIT_TOKS * sizeof(struct buftok));
	fillBufTok(&li->inittoks[li->num_inittoks], type, num, fnum, str, slen);
	li->num_inittoks++;
}

/*
 * Emit local declaration buffer (declarations without init parts)
 * Also updates brace_depth for any { or } in the buffer
 */
static void
emitLocDecl(void)
{
	int i;
	if (!locDecl) {
		numLocDecl = 0;
		return;
	}
	for (i = 0; i < numLocDecl; i++) {
		if (locDecl[i].type == BEGIN)
			brace_depth++;
		else if (locDecl[i].type == END)
			brace_depth--;
		emitBufTok(&locDecl[i]);
		freeBufTok(&locDecl[i]);
	}
	numLocDecl = 0;
}

/*
 * Emit all saved local initializer assignments
 * Each becomes: name = expr ;
 */
static void
flushLocInits(void)
{
	int i, j;
	struct local_init *li;

	if (!locInits) {
		numLocInits = 0;
		return;
	}
	for (i = 0; i < numLocInits; i++) {
		li = &locInits[i];
		emitSym(li->name);
		emitToken(ASSIGN);
		if (li->inittoks) {
			for (j = 0; j < li->num_inittoks; j++) {
				emitBufTok(&li->inittoks[j]);
				freeBufTok(&li->inittoks[j]);
			}
		}
		emitToken(SEMI);
		li->num_inittoks = 0;
	}
	numLocInits = 0;
}

/*
 * Start a new local init for the given variable name
 */
static void
startLocInit(char *name)
{
	if (numLocInits >= MAX_LOC_INITS)
		return;
	if (!locInits)
		locInits = calloc(MAX_LOC_INITS, sizeof(struct local_init));
	strncpy(locInits[numLocInits].name, name, 15);
	locInits[numLocInits].name[15] = 0;
	locInits[numLocInits].num_inittoks = 0;
	numLocInits++;
	locInInit = 1;
}

/*
 * Loop lowering helper functions
 */

/* Buffer a token into loop condition buffer */
static void
bufLoopCond(unsigned char type, long num, float fnum, char *str, int slen)
{
	if (num_loop_cond >= MAX_LOOP_TOKS)
		return;
	if (!loop_cond)
		loop_cond = malloc(MAX_LOOP_TOKS * sizeof(struct buftok));
	fillBufTok(&loop_cond[num_loop_cond], type, num, fnum, str, slen);
	loop_cond[num_loop_cond].lineno = lineno;
	num_loop_cond++;
}

/* Buffer a token into FOR init buffer */
static void
bufLoopInit(unsigned char type, long num, float fnum, char *str, int slen)
{
	if (num_loop_init >= MAX_LOOP_TOKS)
		return;
	if (!loop_init)
		loop_init = malloc(MAX_LOOP_TOKS * sizeof(struct buftok));
	fillBufTok(&loop_init[num_loop_init], type, num, fnum, str, slen);
	loop_init[num_loop_init].lineno = lineno;
	num_loop_init++;
}

/* Buffer a token into FOR increment buffer */
static void
bufLoopIncr(unsigned char type, long num, float fnum, char *str, int slen)
{
	if (num_loop_incr >= MAX_LOOP_TOKS)
		return;
	if (!loop_incr)
		loop_incr = malloc(MAX_LOOP_TOKS * sizeof(struct buftok));
	fillBufTok(&loop_incr[num_loop_incr], type, num, fnum, str, slen);
	loop_incr[num_loop_incr].lineno = lineno;
	num_loop_incr++;
}

/* Clear a buftok array */
static void
clearBufArr(struct buftok *buf, int *cnt)
{
	int i;
	for (i = 0; i < *cnt; i++)
		freeBufTok(&buf[i]);
	*cnt = 0;
}

/*
 * Emit line marker if line changed from lastEmitLine.
 * Updates lastEmitLine to the new line.
 */
static void
syncLine(int line)
{
	if (line != lastEmitLine) {
		emitLine(line, filename ? filename : "");
		lastEmitLine = line;
	}
}

/* Emit buffered tokens with line tracking */
static void
emitBufArr(struct buftok *buf, int cnt)
{
	int i;
	for (i = 0; i < cnt; i++) {
		syncLine(buf[i].lineno);
		emitBufTok(&buf[i]);
	}
}

#define emitLoopCond() emitBufArr(loop_cond, num_loop_cond)
#define emitLoopInit() emitBufArr(loop_init, num_loop_init)
#define emitLoopIncr() emitBufArr(loop_incr, num_loop_incr)

/* Push a new loop frame, saving current state and parent's incr buffer */
static void
pushLoop(unsigned char type)
{
	int i;
	if (loop_sp >= LOOP_STACK_SIZE)
		return;
	loop_stack[loop_sp].type = type;
	loop_stack[loop_sp].savedState = state;
	loop_stack[loop_sp].label_num = next_loop_num++;
	loop_stack[loop_sp].body_depth = 0;
	loop_stack[loop_sp].saved_brace = brace_depth;
	/* Save parent FOR's incr buffer if any */
	if (num_loop_incr > 0) {
		loop_stack[loop_sp].savedIncr = malloc(num_loop_incr * sizeof(struct buftok));
		for (i = 0; i < num_loop_incr; i++)
			loop_stack[loop_sp].savedIncr[i] = loop_incr[i];
		loop_stack[loop_sp].numSavedIncr = num_loop_incr;
		num_loop_incr = 0;  /* Clear for nested loop */
	} else {
		loop_stack[loop_sp].savedIncr = NULL;
		loop_stack[loop_sp].numSavedIncr = 0;
	}
	loop_sp++;
}

/* Pop current loop frame, restoring parent's incr buffer */
static void
popLoop(void)
{
	int i;
	if (loop_sp > 0) {
		loop_sp--;
		/* Restore parent's incr buffer if saved */
		if (loop_stack[loop_sp].savedIncr) {
			for (i = 0; i < loop_stack[loop_sp].numSavedIncr; i++)
				loop_incr[i] = loop_stack[loop_sp].savedIncr[i];
			num_loop_incr = loop_stack[loop_sp].numSavedIncr;
			free(loop_stack[loop_sp].savedIncr);
			loop_stack[loop_sp].savedIncr = NULL;
		}
	}
}

/* Get current loop's label prefix character */
static char
loopLabelChar(void)
{
	if (loop_sp == 0)
		return '?';
	switch (loop_stack[loop_sp - 1].type) {
	case WHILE: return 'W';
	case FOR:   return 'F';
	case DO:    return 'D';
	default:    return '?';
	}
}

/* Emit a loop label or goto: __<L><N><suffix> */
static void
emitLoopLG(char suffix, int isLabel)
{
	char buf[16];
	if (loop_sp == 0)
		return;
	fmtstr(buf, "__%c%d%c", loopLabelChar(),
	        loop_stack[loop_sp - 1].label_num, suffix);
	if (isLabel) {
		emitSym(buf);
		emitToken(COLON);
		emitToken(SEMI);
	} else {
		emitKeyword(GOTO);
		emitSym(buf);
		emitToken(SEMI);
	}
}

#define emitLoopLabel(s) emitLoopLG(s, 1)
#define emitLoopGoto(s)  emitLoopLG(s, 0)

/* Emit: if (!(cond)) { goto __X#B; } and clear cond buffer */
static void
emitCondJump(void)
{
	if (num_loop_cond > 0) {
		emitKeyword(IF);
		emitToken(LPAR);
		emitToken(BANG);
		emitToken(LPAR);
		emitLoopCond();
		emitToken(RPAR);
		emitToken(RPAR);
		emitToken(BEGIN);
		emitLoopGoto('B');
		emitToken(END);
	}
	clearBufArr(loop_cond, &num_loop_cond);
}

/* Find innermost loop for break/continue (skip non-loop contexts) */
static int
findInnerLoop(void)
{
	int i;
	for (i = loop_sp - 1; i >= 0; i--) {
		if (loop_stack[i].type == WHILE ||
		    loop_stack[i].type == FOR ||
		    loop_stack[i].type == DO)
			return i;
	}
	return -1;
}

/* Emit goto for break (to __<L><N>B) using specific loop index */
/* Does NOT emit trailing semicolon - original SEMI from break; is reused */
static void
emitBreakGoto(int idx)
{
	char buf[16];
	char c;
	if (idx < 0)
		return;
	switch (loop_stack[idx].type) {
	case WHILE: c = 'W'; break;
	case FOR:   c = 'F'; break;
	case DO:    c = 'D'; break;
	default:    return;
	}
	fmtstr(buf, "__%c%d%c", c, loop_stack[idx].label_num, 'B');
	emitKeyword(GOTO);
	emitSym(buf);
}

/* Emit goto for continue (to __<L><N>C for for/do, __<L><N>T for while) */
/* Does NOT emit trailing semicolon - original SEMI from continue; is reused */
static void
emitContGoto(int idx)
{
	char buf[16];
	char c, suffix;
	if (idx < 0)
		return;
	switch (loop_stack[idx].type) {
	case WHILE: c = 'W'; suffix = 'T'; break;  /* while continues to top */
	case FOR:   c = 'F'; suffix = 'C'; break;  /* for continues to incr */
	case DO:    c = 'D'; suffix = 'C'; break;  /* do continues to cond */
	default:    return;
	}
	fmtstr(buf, "__%c%d%c", c, loop_stack[idx].label_num, suffix);
	emitKeyword(GOTO);
	emitSym(buf);
}

/* Emit switch break label or goto (__S<n>B) */
static void
emitSwBrk(int num, int isLabel)
{
	char buf[16];
	fmtstr(buf, "__S%dB", num);
	if (isLabel) {
		emitSym(buf);
		emitToken(COLON);
		emitToken(SEMI);
	} else {
		emitKeyword(GOTO);
		emitSym(buf);
	}
}

#define swBrkGoto(n)  emitSwBrk(n, 0)
#define swBrkLabel(n) emitSwBrk(n, 1)

/*
 * Handle nested loop keywords (WHILE/FOR/DO) inside loop bodies.
 * Returns 1 if handled (caller should return), 0 otherwise.
 */
static int
handleNestLoop(unsigned char type)
{
	if (type == WHILE) {
		pushLoop(WHILE);
		num_loop_cond = 0;
		loopParen = 0;
		state = ST_LOOP_COND;
		return 1;
	}
	if (type == FOR) {
		pushLoop(FOR);
		num_loop_init = 0;
		num_loop_cond = 0;
		num_loop_incr = 0;
		loopParen = 0;
		for_part = 0;
		state = ST_LOOP_COND;
		return 1;
	}
	if (type == DO) {
		pushLoop(DO);
		lastEmitLine = lineno;
		emitToken(BEGIN);
		brace_depth++;
		emitLoopLabel('T');
		outbufPush();
		loop_stack[loop_sp - 1].body_depth = 0;
		state = ST_DO_BODY;
		return 1;
	}
	return 0;
}

/*
 * Handle switch statement tracking inside loop bodies.
 * Returns 1 if token was fully handled (caller should return), 0 otherwise.
 */
static int
handleSwitch(unsigned char type)
{
	if (type == SWITCH) {
		switchParen = 0;
		return 0;  /* Pass through SWITCH keyword */
	}
	if (switchParen >= 0) {
		if (type == LPAR) {
			switchParen++;
		} else if (type == RPAR) {
			switchParen--;
		} else if (type == BEGIN && switchParen == 0) {
			if (switchStkTop < MAX_SWITCH_DEPTH) {
				switchBraceStk[switchStkTop] = loop_stack[loop_sp - 1].body_depth;
				switchNumStk[switchStkTop] = switchLblNum++;
				switchStkTop++;
			}
			switchParen = -1;
		}
	}
	/* Check if END closes a switch body */
	if (type == END && switchStkTop > 0) {
		if (loop_stack[loop_sp - 1].body_depth + 1 == switchBraceStk[switchStkTop - 1]) {
			emitToken(END);
			swBrkLabel(switchNumStk[switchStkTop - 1]);
			switchStkTop--;
			return 1;
		}
	}
	return 0;
}

/*
 * Handle break/continue inside loop bodies.
 * Returns 1 if handled (caller should return), 0 otherwise.
 */
static int
handleBrkCont(unsigned char type)
{
	if (type == BREAK) {
		if (switchStkTop > 0) {
			swBrkGoto(switchNumStk[switchStkTop - 1]);
			return 1;
		} else {
			int idx = findInnerLoop();
			if (idx >= 0) {
				emitBreakGoto(idx);
				return 1;
			}
		}
	}
	if (type == CONTINUE) {
		int idx = findInnerLoop();
		if (idx >= 0) {
			emitContGoto(idx);
			return 1;
		}
	}
	return 0;
}

/*
 * Find parameter by name, return pointer or NULL
 */
static struct param *
findParam(char *name)
{
	struct param *p;
	for (p = params; p; p = p->next) {
		if (strcmp(p->name, name) == 0)
			return p;
	}
	return NULL;
}

/*
 * Emit the transformed ANSI function header
 */
static void
emitAnsiHeader(void)
{
	struct param *p, *next;
	int i, k, first;

	/* Emit tokens up to LPAR */
	for (i = 0; i < num_tokens; i++) {
		if (tokbuf[i].type == LPAR)
			break;
		emitBufTok(&tokbuf[i]);
	}

	emitToken(LPAR);

	/* Emit parameters with types */
	first = 1;
	for (p = params; p; p = p->next) {
		if (!first)
			emitToken(COMMA);
		first = 0;

		if (p->has_type) {
			for (k = 0; k < p->num_type_toks; k++) {
				emitBufTok(&p->type_toks[k]);
			}
		} else {
			/* Default to int */
			emitKeyword(INT);
			emitSym(p->name);
		}
	}

	emitToken(RPAR);

#ifdef DEBUG_KNR
	fprintf(stderr, "KNR: emitted ANSI header with %d params\n", num_params);
	for (p = params; p; p = p->next) {
		fprintf(stderr, "  param '%s': %d toks:", p->name, p->num_type_toks);
		for (k = 0; k < p->num_type_toks; k++)
			fprintf(stderr, " %d", p->type_toks[k].type);
		fprintf(stderr, "\n");
	}
#endif

	/* Clean up token buffer */
	clearBuf();

	/* Free parameter list */
	for (p = params; p; p = next) {
		next = p->next;
		for (k = 0; k < p->num_type_toks; k++)
			freeBufTok(&p->type_toks[k]);
		free(p->type_toks);
		free(p);
	}
	params = NULL;
	num_params = 0;
}

/*
 * Process a token in ST_DECLS state (parsing K&R declarations)
 * Returns 1 if still in decls, 0 if done
 */
#define MAX_DECL_TOKS 64
static struct buftok *decl_toks = NULL;
static int num_decl_toks;

/*
 * Find end of base type in declaration tokens.
 * Base type ends when we hit *, identifier, or ( for complex declarator.
 * Examples:
 *   "int a" -> base ends after INT
 *   "unsigned long *p" -> base ends after LONG (before *)
 *   "struct foo *p" -> base ends after SYM(foo)
 */
static int
findBaseEnd(void)
{
	int i;
	for (i = 0; i < num_decl_toks; i++) {
		unsigned char t = decl_toks[i].type;
		/* Stop at pointer (STAR or TIMES), opening paren (for complex),
		 * or identifier that's a param name (not a struct tag).
		 * Note: lexer produces TIMES (42) for *, not STAR (36) */
		if (t == STAR || t == TIMES || t == LPAR) {
			return i;
		}
		if (t == SYM) {
			/* Check if it's a param name or a struct/typedef tag */
			if (findParam(decl_toks[i].v.str) != NULL) {
				return i;  /* It's a param name - base ends here */
			}
			/* Otherwise it's a tag (struct foo) - continue */
		}
	}
	return num_decl_toks;
}

static int
procDeclTok(unsigned char type, long num, float fnum, char *str, int slen)
{
	struct param *p;
	int i, base_end, decl_start;

	/* { ends the declarations */
	if (type == BEGIN) {
		emitAnsiHeader();
		emitToken(BEGIN);
		brace_depth++;
		state = ST_NORMAL;
		return 0;
	}

	/* Accumulate tokens for current declaration */
	if (num_decl_toks < MAX_DECL_TOKS) {
		if (!decl_toks)
			decl_toks = malloc(MAX_DECL_TOKS * sizeof(struct buftok));
		decl_toks[num_decl_toks].type = type;
		if (type == SYM) {
			decl_toks[num_decl_toks].v.str = strdup(str);
		} else if (type == STRING) {
			decl_toks[num_decl_toks].v.str = malloc(slen + 1);
			memcpy(decl_toks[num_decl_toks].v.str, str, slen);
		} else if (type == NUMBER) {
			decl_toks[num_decl_toks].v.num = num;
		} else if (type == FNUMBER) {
			decl_toks[num_decl_toks].v.fnum = fnum;
		}
		num_decl_toks++;
	}

	/* Track parens for complex declarators like int (*fp)() */
	if (type == LPAR)
		paren_depth++;
	else if (type == RPAR)
		paren_depth--;

	/* Semicolon at depth 0 ends this declaration */
	if (type == SEMI && paren_depth == 0) {
		/*
		 * Parse declaration: base_type declarator [, declarator]* ;
		 * For "int a, b;" -> base="int", declarators="a" and "b"
		 * For "int *a, b;" -> base="int", declarators="*a" and "b"
		 */
		base_end = findBaseEnd();
		decl_start = base_end;

#ifdef DEBUG_KNR
		fprintf(stderr, "KNR decl: base_end=%d num_decl_toks=%d\n", base_end, num_decl_toks);
		for (i = 0; i < num_decl_toks; i++)
			fprintf(stderr, "  tok[%d] type=%d\n", i, decl_toks[i].type);
#endif

		/*
		 * Process each comma-separated declarator.
		 * decl_start marks the beginning of the current declarator.
		 */
		for (i = base_end; i < num_decl_toks; i++) {
			/* Comma separates declarators - update start for next one */
			if (decl_toks[i].type == COMMA) {
				decl_start = i + 1;
				continue;
			}
			if (decl_toks[i].type == SEMI)
				break;

			if (decl_toks[i].type == SYM) {
				p = findParam(decl_toks[i].v.str);
#ifdef DEBUG_KNR
				fprintf(stderr, "KNR: found SYM at i=%d, decl_start=%d, p=%p\n", i, decl_start, (void*)p);
#endif
				if (p && !p->has_type) {
					int k = 0, j;
					/* Allocate type_toks on demand */
					if (!p->type_toks)
						p->type_toks = malloc(MAX_TYPE_TOKS * sizeof(struct buftok));
					/* Copy base type tokens */
					for (j = 0; j < base_end && k < MAX_TYPE_TOKS; j++) {
						p->type_toks[k].type = decl_toks[j].type;
						if (decl_toks[j].type == SYM)
							p->type_toks[k].v.str = strdup(decl_toks[j].v.str);
						else
							p->type_toks[k].v = decl_toks[j].v;
						k++;
					}
					/* Copy this declarator's tokens (from decl_start to name, inclusive) */
					for (j = decl_start; j <= i && k < MAX_TYPE_TOKS; j++) {
						p->type_toks[k].type = decl_toks[j].type;
						if (decl_toks[j].type == SYM)
							p->type_toks[k].v.str = strdup(decl_toks[j].v.str);
						else
							p->type_toks[k].v = decl_toks[j].v;
						k++;
					}
					/* Also copy any suffix after name ([] for arrays, () for func ptrs) */
					for (j = i + 1; j < num_decl_toks && k < MAX_TYPE_TOKS; j++) {
						if (decl_toks[j].type == COMMA)
							break;
						if (decl_toks[j].type == SEMI)
							break;
						p->type_toks[k].type = decl_toks[j].type;
						if (decl_toks[j].type == SYM)
							p->type_toks[k].v.str = strdup(decl_toks[j].v.str);
						else
							p->type_toks[k].v = decl_toks[j].v;
						k++;
					}
					p->num_type_toks = k;
					p->has_type = 1;
				}
			}
		}

		/* Free decl_toks */
		for (i = 0; i < num_decl_toks; i++) {
			freeBufTok(&decl_toks[i]);
		}
		num_decl_toks = 0;
		paren_depth = 0;
		return 1;  /* Still in decls, wait for more or { */
	}

	return 1;
}

/*
 * Filter entry point for tokens
 *
 * Called instead of direct emit functions. Implements the state machine
 * for K&R detection and transformation.
 */
void
filtToken(unsigned char type)
{
	filter(type, 0, 0.0, NULL, 0);
}

void
filtKw(unsigned char kw)
{
	filter(kw, 0, 0.0, NULL, 0);
}

void
filtSym(char *name)
{
	filter(SYM, 0, 0.0, name, strlen(name));
}

void
filtNum(long val)
{
	filter(NUMBER, val, 0.0, NULL, 0);
}

void
filtFNum(float val)
{
	filter(FNUMBER, 0, val, NULL, 0);
}

void
filtStr(char *str, int len)
{
	filter(STRING, 0, 0.0, str, len);
}

/*
 * Main filter function
 *
 * Brace depth is tracked carefully:
 * - Only updated when token actually passes through to output
 * - Not updated when buffering (buffered tokens update when flushed)
 */
void
filter(unsigned char type, long num, float fnum, char *str, int slen)
{
	int cur_depth = brace_depth;  /* Depth BEFORE this token */

#ifdef DEBUG_KNR_CTRL
	if (type == IF || type == WHILE || type == FOR || type == DO || type == ELSE)
		fprintf(stderr, "KNR: ctrl tok=%d state=%d depth=%d\n", type, state, cur_depth);
	if (type == BEGIN || type == END)
		fprintf(stderr, "KNR: brace tok=%d state=%d depth=%d\n", type, state, cur_depth);
#endif

	/* State machine */
	switch (state) {
	case ST_NORMAL:
		/*
		 * At file scope, watch for pattern that could be K&R:
		 * [type tokens] SYM LPAR
		 *
		 * Start buffering when we see SYM at file scope after
		 * possible type tokens.
		 */
		if (cur_depth == 0) {
			/* Track typedefs */
			if (type == TYPEDEF) {
				/* Enter typedef parsing state */
				typedef_depth = 0;
				typedef_name[0] = 0;
				state = ST_TYPEDEF;
				emitKeyword(TYPEDEF);
				return;
			}

			/*
			 * If we see a potential function name (SYM not a typedef)
			 * start buffering
			 */
			if (type == SYM && !isTypedef(str)) {
				bufToken(type, num, fnum, str, slen);
				state = ST_BUFFERING;
				return;
			}

			/* Type tokens at file scope - could be return type */
			if (isTypeTok(type, str)) {
				bufToken(type, num, fnum, str, slen);
				state = ST_BUFFERING;
				return;
			}
		}

		/*
		 * Control structure detection (inside functions only)
		 *
		 * Loop lowering: WHILE/FOR/DO are transformed to label/goto/if
		 * Brace insertion: IF/ELSE get braces around single-stmt bodies
		 */
		if (cur_depth > 0) {
			/* Loop lowering for WHILE */
			if (type == WHILE) {
				/* Start buffering condition */
				pushLoop(WHILE);
				num_loop_cond = 0;
				loopParen = 0;
				state = ST_LOOP_COND;
				return;
			}
			/* Loop lowering for FOR */
			if (type == FOR) {
				pushLoop(FOR);
				num_loop_init = 0;
				num_loop_cond = 0;
				num_loop_incr = 0;
				loopParen = 0;
				for_part = 0;  /* Start with init part */
				state = ST_LOOP_COND;
				return;
			}
			/* Loop lowering for DO */
			if (type == DO) {
				pushLoop(DO);
				/* Set baseline for line tracking before prefix */
				lastEmitLine = lineno;
				/* Emit: { __D<n>T: */
				emitToken(BEGIN);
				brace_depth++;
				emitLoopLabel('T');
				/* Push output buffer for body */
				outbufPush();
				loop_stack[loop_sp - 1].body_depth = 0;
				state = ST_DO_BODY;
				return;
			}
			/* IF still uses brace insertion (not loop lowering) */
			if (type == IF) {
				emitToken(type);
				ctrl_type = type;
				ctrlParenDep = 0;
				saved_state = state;
				state = ST_CTRL_COND;
				return;
			}
			/* ELSE uses brace insertion */
			if (type == ELSE) {
				emitToken(type);
				ctrl_type = type;
				saved_state = state;
				state = ST_CTRL_PEND;
				return;
			}
			/*
			 * Local declaration detection - start buffering
			 * type tokens to split off initializers.
			 */
			if (isTypeTok(type, str)) {
				bufLocDecl(type, num, fnum, str, slen);
				locDeclParen = 0;
				locInInit = 0;
				locCurName[0] = 0;
				state = ST_LOCAL_DECL;
				return;
			}
		}

		/* Pass through - update brace depth */
		if (type == BEGIN)
			brace_depth++;
		else if (type == END)
			brace_depth--;

		emitByType(type, num, fnum, str, slen);
		break;

	case ST_BUFFERING:
		/* Buffering potential function header */
		bufToken(type, num, fnum, str, slen);

		if (type == LPAR) {
			state = ST_PARAMS;
			num_params = 0;
			return;
		}

		/* If we see ; or { before LPAR, not a function def */
		if (type == SEMI || type == BEGIN) {
			flushBuf();
			state = ST_NORMAL;
			return;
		}

		/* Keep buffering type tokens and function name */
		break;

	case ST_PARAMS:
		/* Inside parameter list */
		bufToken(type, num, fnum, str, slen);

		if (type == SYM) {
			if (!isTypedef(str)) {
				/* Plain identifier - potential K&R param */
				struct param *newp = malloc(sizeof(struct param));
				struct param **pp;
				strncpy(newp->name, str, 15);
				newp->name[15] = 0;
				newp->has_type = 0;
				newp->num_type_toks = 0;
				newp->type_toks = NULL;
				newp->next = NULL;
				/* Append to end of list to preserve order */
				for (pp = &params; *pp; pp = &(*pp)->next)
					;
				*pp = newp;
				num_params++;
			} else {
				/* Typedef - this is ANSI style */
				flushBuf();
				state = ST_NORMAL;
				return;
			}
		} else if (type == RPAR) {
			state = ST_POSTPAREN;
			return;
		} else if (type == COMMA) {
			/* OK */
		} else if (isTypeTok(type, NULL)) {
			/* Type keyword inside parens - ANSI style */
			flushBuf();
			state = ST_NORMAL;
			return;
		} else if (type == STAR || type == LBRACK || type == ELLIPSIS) {
			/* Pointer/array/variadic - ANSI style */
			flushBuf();
			state = ST_NORMAL;
			return;
		}
		break;

	case ST_POSTPAREN:
		/* After ), check what follows */
		if (type == BEGIN) {
			/* K&R with no type declarations - all params default to int */
			emitAnsiHeader();
			emitToken(BEGIN);
			brace_depth++;
			state = ST_NORMAL;
		} else if (type == SEMI) {
			/* Prototype, not definition - emit as-is */
			flushBuf();
			emitToken(SEMI);
			state = ST_NORMAL;
		} else if (isTypeTok(type, str)) {
			/* Type token after ) - this is K&R! */
			state = ST_DECLS;
			num_decl_toks = 0;
			paren_depth = 0;
			procDeclTok(type, num, fnum, str, slen);
		} else {
			/* Something else - not K&R, flush and pass through */
			flushBuf();
			/* Update brace depth and pass this token through */
			if (type == BEGIN)
				brace_depth++;
			else if (type == END)
				brace_depth--;

			emitByType(type, num, fnum, str, slen);
			state = ST_NORMAL;
		}
		break;

	case ST_DECLS:
		/* Parsing K&R declarations */
		procDeclTok(type, num, fnum, str, slen);
		break;

	case ST_TYPEDEF:
		/*
		 * Parsing typedef to find the name being defined.
		 * Track paren/bracket depth; the typedef name is typically
		 * the last identifier seen at depth 0 before semicolon.
		 * For function pointers like typedef int (*fptr)(void),
		 * the name is inside parens but right after (*.
		 */
		if (type == LPAR || type == LBRACK)
			typedef_depth++;
		else if (type == RPAR || type == RBRACK)
			typedef_depth--;

		if (type == SYM) {
			/*
			 * Heuristic: save name if at depth 0, or if at depth 1
			 * right after (* which indicates function pointer typedef.
			 * Actually, simplest rule: always save the last SYM that
			 * isn't a struct/union/enum tag.
			 */
			strncpy(typedef_name, str, 15);
			typedef_name[15] = 0;
			tdefNameDepth = typedef_depth;
		}

		if (type == SEMI) {
			/* End of typedef - register the name */
			if (typedef_name[0]) {
				filtAddTdef(typedef_name);
#ifdef DEBUG_KNR
				fprintf(stderr, "KNR: registered typedef '%s'\n", typedef_name);
#endif
			}
			typedef_name[0] = 0;
			typedef_depth = 0;
			state = ST_NORMAL;
		}

		/* Pass through to output */
		if (type == BEGIN)
			brace_depth++;
		else if (type == END)
			brace_depth--;

		emitByType(type, num, fnum, str, slen);
		break;

	case ST_CTRL_COND:
		/*
		 * Inside control structure condition (IF/WHILE/FOR)
		 * Track parens to find end of condition, then check for {
		 */
		if (type == LPAR)
			ctrlParenDep++;
		else if (type == RPAR) {
			ctrlParenDep--;
			if (ctrlParenDep == 0) {
				/* End of condition - emit ) and check next token */
				emitToken(RPAR);
				state = ST_CTRL_PEND;
				return;
			}
		}
		/* Emit the token */
		emitByType(type, num, fnum, str, slen);
		break;

	case ST_CTRL_PEND:
		/*
		 * After condition (or after DO/ELSE), check if next token is {
		 * If not, insert synthetic { and track until statement ends
		 * Exception: ctrl_type == 0 means do-while condition end, no body
		 */
		if (ctrl_type == 0) {
			/* do-while condition ended - just emit and return to normal */
			state = saved_state;
			/* Fall through to emit this token normally */
			emitByType(type, num, fnum, str, slen);
			break;
		}
		if (type == BEGIN) {
			/* Already has braces - pass through */
			emitToken(BEGIN);
			brace_depth++;
			if (ctrl_type == DO) {
				/* DO with braces - track until } then WHILE */
				ctrlBodyDep = 1;
				state = ST_DO_BRACE;
			} else {
				state = saved_state;
			}
		} else if (ctrl_type == ELSE && type == IF) {
			/* else if - don't insert block for else, the if is the body */
			emitToken(IF);
			ctrl_type = IF;
			ctrlParenDep = 0;
			state = ST_CTRL_COND;
		} else {
			/* No braces - insert { and track body */
			emitToken(BEGIN);
			brace_depth++;
			ctrlBodyDep = 0;
			state = ST_CTRL_BODY;
			/* Now process this token as part of the body */
			filter(type, num, fnum, str, slen);
		}
		break;

	case ST_CTRL_BODY:
		/*
		 * Inside synthetic block body - track until statement ends
		 * For DO, we end at WHILE keyword; for others, at ; at depth 0
		 */
		if (type == BEGIN || type == LPAR || type == LBRACK)
			ctrlBodyDep++;
		else if (type == END || type == RPAR || type == RBRACK)
			ctrlBodyDep--;

		/* Check for end of single statement - do this first before nesting */
		if (ctrlBodyDep == 0) {
			if (ctrl_type == DO && type == WHILE) {
				/* DO body ends at WHILE - insert } before it */
				emitToken(END);
				brace_depth--;
				emitToken(WHILE);
				/* Now we need to track the while condition */
				ctrlParenDep = 0;
				state = ST_CTRL_COND;
				ctrl_type = 0;  /* After do-while, no body insertion */
				return;
			}
			if (type == SEMI && ctrl_type != DO) {
				/* Statement ends - emit ; then } */
				/* (DO bodies end at WHILE, not SEMI) */
				emitToken(SEMI);
				emitToken(END);
				brace_depth--;
				/* Pop nested contexts that also end here (not DO) */
				while (ctrl_sp > 0 &&
				       ctrl_stack[ctrl_sp - 1].ctrl_type != DO) {
					ctrl_sp--;
					/* Each stacked body also ends - emit } */
					emitToken(END);
					brace_depth--;
				}
				/* If there's a DO on stack, stay in ST_CTRL_BODY */
				if (ctrl_sp > 0) {
					ctrl_sp--;
					ctrl_type = ctrl_stack[ctrl_sp].ctrl_type;
					ctrlBodyDep = ctrl_stack[ctrl_sp].ctrlBodyDep;
					/* Don't return - continue in ST_CTRL_BODY */
				} else {
					state = saved_state;
					return;
				}
				return;
			}
			/* Check for nested control structures */
			if (type == IF || type == WHILE || type == FOR) {
				/* Nested control with condition - push outer context */
				if (ctrl_sp < CTRL_STACK_SIZE) {
					ctrl_stack[ctrl_sp].ctrl_type = ctrl_type;
					ctrl_stack[ctrl_sp].ctrlBodyDep = ctrlBodyDep;
					ctrl_sp++;
				}
				emitToken(type);
				ctrl_type = type;
				ctrlParenDep = 0;
				state = ST_CTRL_COND;
				return;
			}
			if (type == ELSE && ctrl_type == IF) {
				/* ELSE ends the current if's synthetic body */
				emitToken(END);
				brace_depth--;
				/* Don't pop - stack has parent context */
				emitToken(ELSE);
				ctrl_type = ELSE;
				state = ST_CTRL_PEND;
				return;
			}
			if (type == DO || type == ELSE) {
				/* Nested DO or ELSE - push outer context */
				if (ctrl_sp < CTRL_STACK_SIZE) {
					ctrl_stack[ctrl_sp].ctrl_type = ctrl_type;
					ctrl_stack[ctrl_sp].ctrlBodyDep = ctrlBodyDep;
					ctrl_sp++;
				}
				emitToken(type);
				ctrl_type = type;
				state = ST_CTRL_PEND;
				return;
			}
		}

		/* Emit the token */
		emitByType(type, num, fnum, str, slen);
		break;

	case ST_DO_BRACE:
		/*
		 * Inside braced DO body - track depth until matching }
		 * Then look for WHILE to handle the do-while condition
		 */
		if (type == BEGIN)
			ctrlBodyDep++;
		else if (type == END)
			ctrlBodyDep--;

		/* Emit the token */
		emitByType(type, num, fnum, str, slen);

		/* After matching }, look for WHILE */
		if (type == END && ctrlBodyDep == 0) {
			ctrl_type = 0;  /* No body insertion after do-while */
			state = ST_CTRL_COND;
			ctrlParenDep = 0;
		}
		break;

	case ST_LOCAL_DECL:
		/*
		 * Buffering local declaration, splitting initializers.
		 * Buffer type and declarators, save init expressions separately.
		 *
		 * Token handling:
		 * - Type tokens/SYM (not in init): buffer to decl
		 * - ASSIGN: start capturing init for current var
		 * - COMMA at depth 0: end current declarator/init, buffer comma
		 * - SEMI at depth 0: emit decl, enter ST_LOCAL_POST
		 * - Parens: track depth for complex expressions
		 */
		if (type == LPAR)
			locDeclParen++;
		else if (type == RPAR)
			locDeclParen--;

		if (locDeclParen == 0 && type == SEMI) {
			/* End of declaration */
			if (locInInit)
				locInInit = 0;  /* End any pending init */
			bufLocDecl(SEMI, 0, 0, NULL, 0);
			emitLocDecl();
			locCurName[0] = 0;
			state = ST_LOCAL_POST;
			return;
		}

		if (locDeclParen == 0 && type == COMMA) {
			/* End current declarator, start next */
			if (locInInit)
				locInInit = 0;
			bufLocDecl(COMMA, 0, 0, NULL, 0);
			locCurName[0] = 0;
			return;
		}

		if (locDeclParen == 0 && type == ASSIGN && !locInInit) {
			/* Start of initializer - save var name */
			startLocInit(locCurName);
			return;  /* Don't buffer the = */
		}

		if (locInInit) {
			/* Buffering init expression */
			bufLocInit(type, num, fnum, str, slen);
		} else {
			/* Buffering declaration */
			bufLocDecl(type, num, fnum, str, slen);
			if (type == SYM && !isTypedef(str)) {
				/* Remember current declarator name */
				strncpy(locCurName, str, 15);
				locCurName[15] = 0;
			}
		}
		break;

	case ST_LOCAL_POST:
		/*
		 * After local decl semicolon - check if another decl follows.
		 * If type token, continue buffering. Otherwise, flush inits
		 * and return to normal processing.
		 */
		if (isTypeTok(type, str)) {
			/* Another declaration - continue buffering */
			bufLocDecl(type, num, fnum, str, slen);
			locDeclParen = 0;
			locInInit = 0;
			locCurName[0] = 0;
			state = ST_LOCAL_DECL;
			return;
		}

		/* Not a declaration - flush saved inits */
		flushLocInits();
		state = ST_NORMAL;

		/* Process this token normally (may be control struct, etc) */
		filter(type, num, fnum, str, slen);
		break;

	case ST_LOOP_COND:
		/*
		 * Buffering loop condition (for WHILE and FOR).
		 *
		 * WHILE: buffer condition until RPAR at depth 0, then emit:
		 *   { __W#T: if (!(cond)) { goto __W#B; }
		 *   and push output buffer for body
		 *
		 * FOR: track semicolons to separate init/cond/incr:
		 *   for_part 0: buffering init (until first ;)
		 *   for_part 1: buffering cond (until second ;)
		 *   for_part 2: buffering incr (until RPAR)
		 *   Then emit: { init; __F#T: if (!(cond)) { goto __F#B; }
		 */
		if (type == LPAR) {
			loopParen++;
			if (loopParen == 1)
				return;  /* Skip the opening ( */
		} else if (type == RPAR) {
			loopParen--;
			if (loopParen == 0) {
				/* End of condition - emit prefix */
				/* Set baseline for line tracking before prefix */
				lastEmitLine = lineno;
				/* Emit: { [init;] __X#T: if (!(cond)) { goto __X#B; } */
				emitToken(BEGIN);
				brace_depth++;
				if (loop_stack[loop_sp - 1].type == FOR && num_loop_init > 0) {
					emitLoopInit();
					emitToken(SEMI);
					clearBufArr(loop_init, &num_loop_init);
				}
				emitLoopLabel('T');
				emitCondJump();
				/* Push output buffer for body */
				outbufPush();
				loop_stack[loop_sp - 1].body_depth = 0;
				state = ST_LOOP_BODY;
				return;
			}
		}

		/* FOR: handle semicolons separating init/cond/incr */
		if (loop_stack[loop_sp - 1].type == FOR &&
		    type == SEMI && loopParen == 1) {
			for_part++;
			return;  /* Don't buffer the ; */
		}

		/* Buffer the token to the appropriate buffer */
		if (loop_stack[loop_sp - 1].type == FOR) {
			switch (for_part) {
			case 0:
				bufLoopInit(type, num, fnum, str, slen);
				break;
			case 1:
				bufLoopCond(type, num, fnum, str, slen);
				break;
			case 2:
				bufLoopIncr(type, num, fnum, str, slen);
				break;
			}
		} else {
			bufLoopCond(type, num, fnum, str, slen);
		}
		break;

	case ST_LOOP_BODY:
		/*
		 * Inside loop body (for WHILE and FOR).
		 * Body tokens go to output buffer.
		 * Track brace depth; when we see END at depth 0, body is done.
		 *
		 * On body end:
		 * - Replay buffered body
		 * - WHILE: emit goto __W#T; __W#B:; }
		 * - FOR: emit __F#C: incr; goto __F#T; __F#B:; }
		 */
		if (type == BEGIN) {
			loop_stack[loop_sp - 1].body_depth++;
		} else if (type == END) {
			loop_stack[loop_sp - 1].body_depth--;
			if (loop_stack[loop_sp - 1].body_depth == 0) {
				/* Body complete - emit the END to buffer first */
				emitToken(END);
				/* Replay body */
				outbufReplay();
				/* Emit suffix */
				if (loop_stack[loop_sp - 1].type == WHILE) {
					/* goto __W#T; __W#B:; } */
					emitLoopGoto('T');
					emitLoopLabel('B');
				} else {
					/* __F#C: incr; goto __F#T; __F#B:; } */
					emitLoopLabel('C');
					if (num_loop_incr > 0) {
						/* Reset line tracking - body may have changed it */
						lastEmitLine = -1;
						emitLoopIncr();
						emitToken(SEMI);
					}
					clearBufArr(loop_incr, &num_loop_incr);
					emitLoopGoto('T');
					emitLoopLabel('B');
				}
				emitToken(END);
				brace_depth--;
				/* Restore parent state before popping */
				state = loop_stack[loop_sp - 1].savedState;
				popLoop();
				return;
			}
		}

		/* Handle nested loops, switch, break/continue */
		if (handleNestLoop(type))
			return;
		if (handleSwitch(type))
			return;
		if (handleBrkCont(type))
			return;

		/* Emit token to output buffer (body content) */
		emitByType(type, num, fnum, str, slen);
		break;

	case ST_DO_BODY:
		/*
		 * Inside do body. Body tokens go to output buffer.
		 * Track brace depth; when we see END at depth 0, body is done.
		 * Then wait for WHILE keyword to start condition buffering.
		 *
		 * On body end:
		 * - Replay buffered body
		 * - Emit __D#C: label
		 * - Transition to ST_DO_COND to buffer condition
		 */
		if (type == BEGIN) {
			loop_stack[loop_sp - 1].body_depth++;
		} else if (type == END) {
			loop_stack[loop_sp - 1].body_depth--;
			if (loop_stack[loop_sp - 1].body_depth == 0) {
				/* Body complete - emit END to buffer */
				emitToken(END);
				/* Replay body */
				outbufReplay();
				/* Emit continue label */
				emitLoopLabel('C');
				/* Wait for WHILE keyword */
				state = ST_DO_COND;
				loopParen = -1;  /* -1 = waiting for WHILE */
				return;
			}
		}

		/* Handle nested loops, switch, break/continue */
		if (handleNestLoop(type))
			return;
		if (handleSwitch(type))
			return;
		if (handleBrkCont(type))
			return;

		/* Emit token to output buffer */
		emitByType(type, num, fnum, str, slen);
		break;

	case ST_DO_COND:
		/*
		 * Buffering do-while condition.
		 * First we wait for WHILE keyword (loopParen == -1),
		 * then buffer condition until RPAR at depth 0.
		 *
		 * On condition end:
		 * - Emit: if (cond) { goto __D#T; } __D#B:; }
		 * - Consume the trailing semicolon
		 */
		if (loopParen == -1) {
			/* Waiting for WHILE keyword */
			if (type == WHILE) {
				loopParen = 0;
				num_loop_cond = 0;
			}
			/* Ignore other tokens (shouldn't be any) */
			return;
		}

		if (type == LPAR) {
			loopParen++;
			if (loopParen == 1)
				return;  /* Skip the opening ( */
		} else if (type == RPAR) {
			loopParen--;
			if (loopParen == 0) {
				/* Condition complete */
				/* Emit: if (cond) { goto __D#T; } __D#B:; } */
				if (num_loop_cond > 0) {
					/* Reset line tracking - body may have changed it */
					lastEmitLine = -1;
					emitKeyword(IF);
					emitToken(LPAR);
					emitLoopCond();
					emitToken(RPAR);
					emitToken(BEGIN);
					emitLoopGoto('T');
					emitToken(END);
				}
				clearBufArr(loop_cond, &num_loop_cond);
				emitLoopLabel('B');
				emitToken(END);
				brace_depth--;
				/* Save parent state before popping, store in loopParen for SEMI */
				loopParen = loop_stack[loop_sp - 1].savedState;
				popLoop();
				/* Stay in ST_DO_COND to consume trailing semicolon */
				return;
			}
		} else if (type == SEMI) {
			/* Trailing semicolon after do-while - consume it */
			/* loopParen holds the saved parent state */
			state = loopParen;
			return;
		}

		/* Buffer condition token */
		bufLoopCond(type, num, fnum, str, slen);
		break;
	}
}

/*
 * Initialize the filter
 */
void
filterInit(void)
{
	state = ST_NORMAL;
	brace_depth = 0;
	num_tokens = 0;
	num_params = 0;
	typedefs = NULL;
	typedef_depth = 0;
	typedef_name[0] = 0;
	ctrlParenDep = 0;
	ctrlBodyDep = 0;
	ctrl_type = 0;
	saved_state = 0;
	ctrl_sp = 0;
	/* Local declaration init splitting */
	numLocInits = 0;
	numLocDecl = 0;
	locDeclParen = 0;
	locInInit = 0;
	locCurName[0] = 0;
	/* Loop lowering */
	loop_sp = 0;
	next_loop_num = 1;
	num_loop_cond = 0;
	num_loop_init = 0;
	num_loop_incr = 0;
	loopParen = 0;
	for_part = 0;
	lastEmitLine = 0;
	/* Switch tracking */
	switchStkTop = 0;
	switchLblNum = 0;
	switchParen = -1;
}

/* vim: set tabstop=4 shiftwidth=4 noexpandtab: */
