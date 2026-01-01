/*
 * knr.c - K&R to ANSI function definition normalization
 *
 * Operates as a filter between the lexer's emit calls and the
 * actual .x file output. Tokens flow through this layer which
 * detects K&R patterns and transforms them to ANSI.
 *
 * Architecture:
 *   lexer -> emit*() -> knrFilter() -> actual .x write
 *
 * K&R:   int foo(a, b, c) int a; char *b; long c; { ... }
 * ANSI:  int foo(int a, char *b, long c) { ... }
 *
 * Unspecified parameter types default to int.
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
static int ctrl_paren_depth = 0;	/* Paren depth in condition */
static int ctrl_body_depth = 0;		/* Brace/paren depth in synthetic body */
static unsigned char ctrl_type = 0;	/* IF, WHILE, FOR, DO, or ELSE */
static unsigned char saved_state = 0;	/* State to return to after ctrl */

/* Stack for nested synthetic blocks */
#define CTRL_STACK_SIZE 8
struct ctrl_frame {
	unsigned char ctrl_type;
	int ctrl_body_depth;
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
 * Token buffer - stores tokens during K&R detection
 */
#define MAX_TOKENS 256
struct buftok {
	unsigned char type;
	union {
		long num;
		float fnum;
		char *str;		/* strdup'd for SYM, alloc'd for STRING */
	} v;
};
static struct buftok tokbuf[MAX_TOKENS];
static int num_tokens = 0;

/*
 * Parameter tracking
 */
#define MAX_PARAMS 32
struct param {
	char name[16];
	struct buftok type_toks[32];
	int num_type_toks;
	char has_type;
};
static struct param params[MAX_PARAMS];
static int num_params = 0;
static int paren_depth = 0;	/* for tracking nested parens in decls */

/*
 * Typedef tracking
 */
#define MAX_TYPEDEFS 128
static char *typedefs[MAX_TYPEDEFS];
static int num_typedefs = 0;

/*
 * We use the emit functions from emit.c for actual .x output.
 * These are declared in cpp.h: emitToken, emitKeyword, emitSym, etc.
 *
 * The filter intercepts tokens before they reach emit, buffers when
 * needed, and calls emit when ready to output.
 */

/* Wrappers to call emit.c functions (avoids recursion when filter calls emit) */

static void
realEmitToken(unsigned char tok)
{
	emitToken(tok);
}

static void
realEmitKw(unsigned char kw)
{
	emitKeyword(kw);
}

static void
realEmitSym(char *name)
{
	emitSym(name);
}

static void
realEmitNumber(long val)
{
	emitNumber(val);
}

static void
realEmitFNum(float val)
{
	emitFNumber(val);
}

static void
realEmitString(char *str, int len)
{
	emitString(str, len);
}

/*
 * Add typedef name to tracking table
 */
void
knrAddTypedef(char *name)
{
	if (num_typedefs < MAX_TYPEDEFS) {
		typedefs[num_typedefs++] = strdup(name);
	}
}

/*
 * Check if name is a known typedef
 */
static int
isTypedef(char *name)
{
	int i;
	for (i = 0; i < num_typedefs; i++) {
		if (strcmp(typedefs[i], name) == 0)
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
	if (type >= INT && type <= VOLATILE)
		return 1;
	if (type == SYM && name && isTypedef(name))
		return 1;
	return 0;
}

/*
 * Buffer a token
 */
static void
bufToken(unsigned char type, long num, float fnum, char *str, int slen)
{
	if (num_tokens >= MAX_TOKENS)
		return;

	tokbuf[num_tokens].type = type;
	if (type == SYM) {
		tokbuf[num_tokens].v.str = strdup(str);
	} else if (type == STRING) {
		tokbuf[num_tokens].v.str = malloc(slen + 1);
		memcpy(tokbuf[num_tokens].v.str, str, slen);
		tokbuf[num_tokens].v.str[slen] = 0;
	} else if (type == NUMBER) {
		tokbuf[num_tokens].v.num = num;
	} else if (type == FNUMBER) {
		tokbuf[num_tokens].v.fnum = fnum;
	}
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
		realEmitSym(t->v.str);
		break;
	case NUMBER:
		realEmitNumber(t->v.num);
		break;
	case FNUMBER:
		realEmitFNum(t->v.fnum);
		break;
	case STRING:
		realEmitString(t->v.str, strlen(t->v.str));
		break;
	default:
		realEmitToken(t->type);
		break;
	}
}

/*
 * Free buffered token strings
 */
static void
freeBufTok(struct buftok *t)
{
	if (t->type == SYM || t->type == STRING) {
		if (t->v.str)
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
		if (tokbuf[i].type == BEGIN)
			brace_depth++;
		else if (tokbuf[i].type == END)
			brace_depth--;
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
 * Find parameter by name, return index or -1
 */
static int
findParam(char *name)
{
	int i;
	for (i = 0; i < num_params; i++) {
		if (strcmp(params[i].name, name) == 0)
			return i;
	}
	return -1;
}

/*
 * Emit the transformed ANSI function header
 */
static void
emitAnsiHeader(void)
{
	int i, j, k;

	/* Emit tokens up to LPAR */
	for (i = 0; i < num_tokens; i++) {
		if (tokbuf[i].type == LPAR)
			break;
		emitBufTok(&tokbuf[i]);
	}

	realEmitToken(LPAR);

	/* Emit parameters with types */
	for (j = 0; j < num_params; j++) {
		if (j > 0)
			realEmitToken(COMMA);

		if (params[j].has_type) {
			for (k = 0; k < params[j].num_type_toks; k++) {
				emitBufTok(&params[j].type_toks[k]);
			}
		} else {
			/* Default to int */
			realEmitKw(INT);
			realEmitSym(params[j].name);
		}
	}

	realEmitToken(RPAR);

#ifdef DEBUG_KNR
	fprintf(stderr, "KNR: emitted ANSI header with %d params\n", num_params);
	for (j = 0; j < num_params; j++) {
		fprintf(stderr, "  param[%d] '%s': %d toks:", j, params[j].name, params[j].num_type_toks);
		for (k = 0; k < params[j].num_type_toks; k++)
			fprintf(stderr, " %d", params[j].type_toks[k].type);
		fprintf(stderr, "\n");
	}
#endif

	/* Clean up token buffer */
	clearBuf();

	/* Clean up parameter type tokens */
	for (j = 0; j < num_params; j++) {
		for (k = 0; k < params[j].num_type_toks; k++)
			freeBufTok(&params[j].type_toks[k]);
		params[j].num_type_toks = 0;
	}
	num_params = 0;
}

/*
 * Process a token in ST_DECLS state (parsing K&R declarations)
 * Returns 1 if still in decls, 0 if done
 */
static struct buftok decl_toks[64];
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
			if (findParam(decl_toks[i].v.str) >= 0) {
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
	int i, p, base_end, decl_start;

	/* { ends the declarations */
	if (type == BEGIN) {
		emitAnsiHeader();
		realEmitToken(BEGIN);
		brace_depth++;
		state = ST_NORMAL;
		return 0;
	}

	/* Accumulate tokens for current declaration */
	if (num_decl_toks < 64) {
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
				fprintf(stderr, "KNR: found SYM at i=%d, decl_start=%d, p=%d\n", i, decl_start, p);
#endif
				if (p >= 0 && !params[p].has_type) {
					int k = 0, j;
					/* Copy base type tokens */
					for (j = 0; j < base_end && k < 32; j++) {
						params[p].type_toks[k].type = decl_toks[j].type;
						if (decl_toks[j].type == SYM)
							params[p].type_toks[k].v.str = strdup(decl_toks[j].v.str);
						else
							params[p].type_toks[k].v = decl_toks[j].v;
						k++;
					}
					/* Copy this declarator's tokens (from decl_start to name, inclusive) */
					for (j = decl_start; j <= i && k < 32; j++) {
						params[p].type_toks[k].type = decl_toks[j].type;
						if (decl_toks[j].type == SYM)
							params[p].type_toks[k].v.str = strdup(decl_toks[j].v.str);
						else
							params[p].type_toks[k].v = decl_toks[j].v;
						k++;
					}
					/* Also copy any suffix after name ([] for arrays, () for func ptrs) */
					for (j = i + 1; j < num_decl_toks && k < 32; j++) {
						if (decl_toks[j].type == COMMA)
							break;
						if (decl_toks[j].type == SEMI)
							break;
						params[p].type_toks[k].type = decl_toks[j].type;
						if (decl_toks[j].type == SYM)
							params[p].type_toks[k].v.str = strdup(decl_toks[j].v.str);
						else
							params[p].type_toks[k].v = decl_toks[j].v;
						k++;
					}
					params[p].num_type_toks = k;
					params[p].has_type = 1;
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
knrFilterToken(unsigned char type)
{
	knrFilter(type, 0, 0.0, NULL, 0);
}

void
knrFiltKw(unsigned char kw)
{
	knrFilter(kw, 0, 0.0, NULL, 0);
}

void
knrFilterSym(char *name)
{
	knrFilter(SYM, 0, 0.0, name, strlen(name));
}

void
knrFiltNum(long val)
{
	knrFilter(NUMBER, val, 0.0, NULL, 0);
}

void
knrFiltFNum(float val)
{
	knrFilter(FNUMBER, 0, val, NULL, 0);
}

void
knrFiltStr(char *str, int len)
{
	knrFilter(STRING, 0, 0.0, str, len);
}

/*
 * Main filter function
 *
 * Brace depth is tracked carefully:
 * - Only updated when token actually passes through to output
 * - Not updated when buffering (buffered tokens update when flushed)
 */
void
knrFilter(unsigned char type, long num, float fnum, char *str, int slen)
{
	int cur_depth = brace_depth;  /* Depth BEFORE this token */

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
				realEmitKw(TYPEDEF);
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
		 * IF/WHILE/FOR have conditions in parens - track to find body
		 * DO/ELSE have immediate body - check next token for {
		 */
		if (cur_depth > 0) {
			if (type == IF || type == WHILE || type == FOR) {
				realEmitToken(type);
				ctrl_type = type;
				ctrl_paren_depth = 0;
				saved_state = state;
				state = ST_CTRL_COND;
				return;
			}
			if (type == DO || type == ELSE) {
				realEmitToken(type);
				ctrl_type = type;
				saved_state = state;
				state = ST_CTRL_PEND;
				return;
			}
		}

		/* Pass through - update brace depth */
		if (type == BEGIN)
			brace_depth++;
		else if (type == END)
			brace_depth--;

		if (type == SYM)
			realEmitSym(str);
		else if (type == NUMBER)
			realEmitNumber(num);
		else if (type == FNUMBER)
			realEmitFNum(fnum);
		else if (type == STRING)
			realEmitString(str, slen);
		else
			realEmitToken(type);
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
				if (num_params < MAX_PARAMS) {
					strncpy(params[num_params].name, str, 15);
					params[num_params].name[15] = 0;
					params[num_params].has_type = 0;
					params[num_params].num_type_toks = 0;
					num_params++;
				}
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
			realEmitToken(BEGIN);
			brace_depth++;
			state = ST_NORMAL;
		} else if (type == SEMI) {
			/* Prototype, not definition - emit as-is */
			flushBuf();
			realEmitToken(SEMI);
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

			if (type == SYM)
				realEmitSym(str);
			else if (type == NUMBER)
				realEmitNumber(num);
			else if (type == FNUMBER)
				realEmitFNum(fnum);
			else if (type == STRING)
				realEmitString(str, slen);
			else
				realEmitToken(type);
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
				knrAddTypedef(typedef_name);
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

		if (type == SYM)
			realEmitSym(str);
		else if (type == NUMBER)
			realEmitNumber(num);
		else if (type == FNUMBER)
			realEmitFNum(fnum);
		else if (type == STRING)
			realEmitString(str, slen);
		else
			realEmitToken(type);
		break;

	case ST_CTRL_COND:
		/*
		 * Inside control structure condition (IF/WHILE/FOR)
		 * Track parens to find end of condition, then check for {
		 */
		if (type == LPAR)
			ctrl_paren_depth++;
		else if (type == RPAR) {
			ctrl_paren_depth--;
			if (ctrl_paren_depth == 0) {
				/* End of condition - emit ) and check next token */
				realEmitToken(RPAR);
				state = ST_CTRL_PEND;
				return;
			}
		}
		/* Emit the token */
		if (type == SYM)
			realEmitSym(str);
		else if (type == NUMBER)
			realEmitNumber(num);
		else if (type == FNUMBER)
			realEmitFNum(fnum);
		else if (type == STRING)
			realEmitString(str, slen);
		else
			realEmitToken(type);
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
			if (type == SYM)
				realEmitSym(str);
			else if (type == NUMBER)
				realEmitNumber(num);
			else if (type == FNUMBER)
				realEmitFNum(fnum);
			else if (type == STRING)
				realEmitString(str, slen);
			else
				realEmitToken(type);
			break;
		}
		if (type == BEGIN) {
			/* Already has braces - pass through */
			realEmitToken(BEGIN);
			brace_depth++;
			if (ctrl_type == DO) {
				/* DO with braces - track until } then WHILE */
				ctrl_body_depth = 1;
				state = ST_DO_BRACE;
			} else {
				state = saved_state;
			}
		} else {
			/* No braces - insert { and track body */
			realEmitToken(BEGIN);
			brace_depth++;
			ctrl_body_depth = 0;
			state = ST_CTRL_BODY;
			/* Now process this token as part of the body */
			knrFilter(type, num, fnum, str, slen);
		}
		break;

	case ST_CTRL_BODY:
		/*
		 * Inside synthetic block body - track until statement ends
		 * For DO, we end at WHILE keyword; for others, at ; at depth 0
		 */
		if (type == BEGIN || type == LPAR || type == LBRACK)
			ctrl_body_depth++;
		else if (type == END || type == RPAR || type == RBRACK)
			ctrl_body_depth--;

		/* Check for end of single statement - do this first before nesting */
		if (ctrl_body_depth == 0) {
			if (ctrl_type == DO && type == WHILE) {
				/* DO body ends at WHILE - insert } before it */
				realEmitToken(END);
				brace_depth--;
				realEmitToken(WHILE);
				/* Now we need to track the while condition */
				ctrl_paren_depth = 0;
				state = ST_CTRL_COND;
				ctrl_type = 0;  /* After do-while, no body insertion */
				return;
			}
			if (type == SEMI && ctrl_type != DO) {
				/* Statement ends - emit ; then } */
				/* (DO bodies end at WHILE, not SEMI) */
				realEmitToken(SEMI);
				realEmitToken(END);
				brace_depth--;
				/* Pop nested contexts that also end here (not DO) */
				while (ctrl_sp > 0 &&
				       ctrl_stack[ctrl_sp - 1].ctrl_type != DO) {
					ctrl_sp--;
					/* Each stacked body also ends - emit } */
					realEmitToken(END);
					brace_depth--;
				}
				/* If there's a DO on stack, stay in ST_CTRL_BODY */
				if (ctrl_sp > 0) {
					ctrl_sp--;
					ctrl_type = ctrl_stack[ctrl_sp].ctrl_type;
					ctrl_body_depth = ctrl_stack[ctrl_sp].ctrl_body_depth;
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
					ctrl_stack[ctrl_sp].ctrl_body_depth = ctrl_body_depth;
					ctrl_sp++;
				}
				realEmitToken(type);
				ctrl_type = type;
				ctrl_paren_depth = 0;
				state = ST_CTRL_COND;
				return;
			}
			if (type == DO || type == ELSE) {
				/* Nested DO or ELSE - push outer context */
				if (ctrl_sp < CTRL_STACK_SIZE) {
					ctrl_stack[ctrl_sp].ctrl_type = ctrl_type;
					ctrl_stack[ctrl_sp].ctrl_body_depth = ctrl_body_depth;
					ctrl_sp++;
				}
				realEmitToken(type);
				ctrl_type = type;
				state = ST_CTRL_PEND;
				return;
			}
		}

		/* Emit the token */
		if (type == SYM)
			realEmitSym(str);
		else if (type == NUMBER)
			realEmitNumber(num);
		else if (type == FNUMBER)
			realEmitFNum(fnum);
		else if (type == STRING)
			realEmitString(str, slen);
		else
			realEmitToken(type);
		break;

	case ST_DO_BRACE:
		/*
		 * Inside braced DO body - track depth until matching }
		 * Then look for WHILE to handle the do-while condition
		 */
		if (type == BEGIN)
			ctrl_body_depth++;
		else if (type == END)
			ctrl_body_depth--;

		/* Emit the token */
		if (type == SYM)
			realEmitSym(str);
		else if (type == NUMBER)
			realEmitNumber(num);
		else if (type == FNUMBER)
			realEmitFNum(fnum);
		else if (type == STRING)
			realEmitString(str, slen);
		else
			realEmitToken(type);

		/* After matching }, look for WHILE */
		if (type == END && ctrl_body_depth == 0) {
			ctrl_type = 0;  /* No body insertion after do-while */
			state = ST_CTRL_COND;
			ctrl_paren_depth = 0;
		}
		break;
	}
}

/*
 * Initialize the filter
 */
void
knrInit(void)
{
	state = ST_NORMAL;
	brace_depth = 0;
	num_tokens = 0;
	num_params = 0;
	num_typedefs = 0;
	typedef_depth = 0;
	typedef_name[0] = 0;
	ctrl_paren_depth = 0;
	ctrl_body_depth = 0;
	ctrl_type = 0;
	saved_state = 0;
	ctrl_sp = 0;
}

/* vim: set tabstop=4 shiftwidth=4 noexpandtab: */
