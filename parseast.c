/*
 * parseast.c - Hex-based AST parser for cc2
 *
 * Reads AST from cc1 in hex format and builds trees for code generation.
 * Format: names as <2-hex-len><hex-bytes>, numbers as hex with '.'
 */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>

#include "cc2.h"
#include "astio.h"
#include "regcache.h"
#include "emithelper.h"

/* Forward declarations */
static struct expr *parseExpr(void);
static struct stmt *parseStmt(void);

/* Skip whitespace (space, tab, newline, cr) */
static void
skipWS(void)
{
	while (curchar == '\n' || curchar == ' ' || curchar == '\t' || curchar == '\r')
		nextchar();
}

/* Symbol tracking */
static void addDefSym(const char *name);
void addRefSym(const char *name);

/* Parser state */
unsigned char outFd = 1;
static int labelCounter = 0;

/* Function context globals */
char *fnName;
char *fnParams;
char *fnRettype;
struct stmt *fnBody;
int fnLblCnt;
struct local_var *fnLocals;
int fnFrmSize;
int fnCurLbl;
int fnDESaveCnt;
int fnDInUse;
int fnPendClean;
int fnLoopDep;
int fnDEValid;
int fnTargetDE;
int fnZValid;
int fnCondOnly;
int fnDualCmp;
int fnDualReg;      /* Register for fnDualCmp: R_BC, R_HL, R_DE, or 0 for memory */
int fnCmpFlag;
char fnIXAOfs;
char fnIXHLOfs;
char fnIYHLOfs;
char fnIYHLValid;
char fnABCValid;
char fnAZero;

/* Segment tracking */
#define SEG_NONE 0
#define SEG_TEXT 1
#define SEG_DATA 2
#define SEG_BSS  3
static int currentSeg = SEG_NONE;
static const char *segNames[] = { NULL, "\n.text\n", "\n.data\n", "\n.bss\n" };

static void
switchToSeg(int seg)
{
	if (seg != currentSeg) {
		fdputs(outFd, segNames[seg]);
		currentSeg = seg;
	}
}

/* Out of memory handler */
static void
oom(void)
{
	fdprintf(2, "oom\n");
	exit(1);
}

/* Tree node allocation */
struct expr *
newExpr(unsigned char op)
{
	struct expr *e = malloc(sizeof(struct expr));
	if (!e) oom();
	e->op = op;
	e->left = NULL;
	e->right = NULL;
	e->type_str = 0;
	e->value = 0;
	e->symbol = NULL;
	e->size = 2;
	e->flags = 0;
	e->cleanup_block = NULL;
	e->label = 0;
	e->jump = NULL;
	e->opflags = 0;
	e->cached_var = NULL;
	return e;
}

struct stmt *
newStmt(unsigned char type)
{
	struct stmt *s = malloc(sizeof(struct stmt));
	if (!s) oom();
	s->type = type;
	s->expr = NULL;
	s->expr2 = NULL;
	s->expr3 = NULL;
	s->then_branch = NULL;
	s->else_branch = NULL;
	s->next = NULL;
	s->symbol = NULL;
	s->type_str = 0;
	s->label = 0;
	s->label2 = 0;
	s->asm_block = NULL;
	s->jump = NULL;
	return s;
}

/* Size helpers - uppercase B/S/L = unsigned */
unsigned char
getSizeFTStr(unsigned char c)
{
	if ((c | 0x20) == 'b') return 1;
	if ((c | 0x20) == 's' || c == 'p') return 2;
	if ((c | 0x20) == 'l' || c == 'f') return 4;
	if (c == 'd') return 8;
	return 2;
}

unsigned char
getSignFTStr(unsigned char c)
{
	return (c == 'p' || c == 'B' || c == 'S' || c == 'L') ? E_UNSIGNED : 0;
}

unsigned char
getSizeFromTN(const char *t)
{
	return t ? getSizeFTStr(t[0]) : 2;
}

/* Pattern matchers */
static int
isPowerOf2(long value)
{
	int shift;
	if (value <= 0) return -1;
	if ((value & (value - 1)) != 0) return -1;
	shift = 0;
	while ((value & 1) == 0) { value >>= 1; shift++; }
	return shift;
}

int
isMulByPow2(struct expr *e, struct expr **out_expr)
{
	int shift;
	if (!e || e->op != '*') return -1;
	if (!e->right || e->right->op != 'C') return -1;
	shift = isPowerOf2(e->right->value);
	if (shift < 0) return -1;
	if (out_expr) *out_expr = e->left;
	return shift;
}

int
isStructMem(struct expr *e, char **out_var, long *out_offset)
{
	struct expr *add, *ptr_load, *var, *offset;
	if (!e || e->op != 'M') return 0;
	add = e->left;
	if (!add || add->op != '+') return 0;
	ptr_load = add->left;
	if (!ptr_load || ptr_load->op != 'M' || ptr_load->type_str != 'p') return 0;
	var = ptr_load->left;
	if (!var || var->op != '$' || !var->symbol) return 0;
	offset = add->right;
	if (!offset || offset->op != 'C') return 0;
	if (out_var) *out_var = var->symbol;
	if (out_offset) *out_offset = offset->value;
	return 1;
}

/* Helper: append to statement list */
static void
appendChild(struct stmt *child, struct stmt **first, struct stmt **last)
{
	if (!child) return;
	if (TRACE(T_PARSE)) fdprintf(2, "appendChild: child=%c first=%p last=%p\n", child->type, (void*)*first, (void*)*last);
	if (!*first) *first = child;
	else (*last)->next = child;
	*last = child;
	while ((*last)->next) {
		if (TRACE(T_PARSE)) fdprintf(2, "  walking: last=%c->next=%c\n", (*last)->type, (*last)->next->type);
		*last = (*last)->next;
	}
	if (TRACE(T_PARSE)) fdprintf(2, "  final last=%c\n", (*last)->type);
}

/* Create numeric end-label statement (type 'Y') */
static struct stmt *
mkEndLbl(unsigned char lbl)
{
	struct stmt *s = newStmt('Y');
	s->label = lbl;
	return s;
}

void
freeExpr(struct expr *e)
{
	if (!e) return;
	freeExpr(e->left);
	freeExpr(e->right);
	xfree(e->cleanup_block);
	free(e);
}

void
frStmt(struct stmt *s)
{
	if (!s) return;
	freeExpr(s->expr);
	freeExpr(s->expr2);
	freeExpr(s->expr3);
	frStmt(s->then_branch);
	frStmt(s->else_branch);
	frStmt(s->next);
	xfree(s->symbol);
	xfree(s->asm_block);
	free(s);
}

/* Expression parsing - table-driven
 * All ops have format: (op width operands...)
 * Table encodes: bits 0-1 = arity (0,1,2,3=special), bit 2 = needs label
 */
#define OP_1  1   /* 1 operand */
#define OP_2  2   /* 2 operands */
#define OP_S  3   /* special handler */
#define OP_L  4   /* needs label */

static unsigned char optab[256];
static int optab_init;

static void
initOptab(void)
{
	if (optab_init) return;
	optab_init = 1;
	/* Unary ops */
	optab['M'] = optab['N'] = optab['W'] = optab[AST_SEXT] = OP_1;
	optab['!'] = optab['~'] = optab['\\'] = optab['\''] = OP_1;
	/* Binary ops */
	optab['='] = optab['+'] = optab['-'] = optab['*'] = optab['/'] = OP_2;
	optab['%'] = optab['&'] = optab['|'] = optab['^'] = optab[','] = OP_2;
	optab['<'] = optab['>'] = optab['Q'] = optab['n'] = optab['L'] = OP_2;
	optab['g'] = optab['y'] = optab['w'] = optab[':'] = OP_2;
	optab['P'] = optab['T'] = optab['2'] = optab['1'] = OP_2;
	optab['X'] = optab['0'] = optab['6'] = OP_2;
	optab[AST_SUBEQ] = optab[AST_MODEQ] = optab[AST_ANDEQ] = OP_2;
	/* Logical with label */
	optab['h'] = optab['j'] = OP_2 | OP_L;
	/* Special: call, ternary, copy, inc/dec, bitfield */
	optab['@'] = optab['?'] = optab['Y'] = OP_S;
	optab[AST_PREINC] = optab[AST_POSTINC] = optab[AST_PREDEC] = optab[AST_POSTDEC] = OP_S;
	optab[AST_BFEXTRACT] = optab[AST_BFASSIGN] = OP_S;
}

/* Read type suffix and set size/flags */
static void
readType(struct expr *e)
{
	e->type_str = curchar;
	nextchar();
	e->size = getSizeFTStr(e->type_str);
	e->flags = getSignFTStr(e->type_str);
}

/* Unified expression handler - all ops have width suffix */
static struct expr *
doExprOp(unsigned char op)
{
	struct expr *e = newExpr(op);
	unsigned char info = optab[op];
	int arity = info & 3;

	readType(e);

	/* Allocate label if needed */
	if (info & OP_L) e->label = labelCounter++;

	/* Parse operands */
	if (arity >= 1) e->left = parseExpr();
	if (arity >= 2) e->right = parseExpr();

	/* Strength reduction: multiply by power of 2 */
	if (op == '*') {
		int shift = isMulByPow2(e, NULL);
		if (shift >= 0) {
			struct expr *old_right = e->right;
			e->op = 'y';
			e->right = newExpr('C');
			e->right->value = shift;
			e->right->size = 1;
			freeExpr(old_right);
		}
	}
	return e;
}

/* Special handlers for irregular ops */
static struct expr *
doCall(void)
{
	struct expr *e = newExpr('@'), *w, *prev = NULL;
	int argc, i;
	e->type_str = curchar;  /* Return type */
	e->size = getSizeFTStr(curchar);
	nextchar();
	argc = readHex2();
	e->value = argc;
	e->left = parseExpr();
	for (i = 0; i < argc; i++) {
		w = newExpr(',');
		w->left = parseExpr();
		if (prev) prev->right = w;
		else e->right = w;
		prev = w;
	}
	return e;
}

static struct expr *
doTernary(void)
{
	struct expr *e = newExpr('?'), *c = newExpr(':');
	readType(e);
	e->left = parseExpr();
	c->left = parseExpr();
	c->right = parseExpr();
	e->right = c;
	return e;
}

static struct expr *
doIncDec(unsigned char op)
{
	struct expr *e = newExpr(op);
	readType(e);
	e->left = parseExpr();
	e->value = readHex4();
	if (e->left) e->flags = e->left->flags;
	return e;
}

static struct expr *
doBitfield(unsigned char op)
{
	struct expr *e = newExpr(op);
	int off = readHex2(), wid = readHex2();
	e->value = (off << 16) | wid;
	e->left = parseExpr();
	if (op == AST_BFASSIGN) e->right = parseExpr();
	return e;
}

static struct expr *
doCopy(void)
{
	struct expr *e = newExpr('Y');
	e->value = readHex4();
	e->left = parseExpr();
	e->right = parseExpr();
	return e;
}


/* Parse expression - paren-free format */
static struct expr *
parseExpr(void)
{
	struct expr *e;
	unsigned char op, info;

	skipWS();
	initOptab();

	/* Null expression marker */
	if (curchar == '_') {
		nextchar();
		return NULL;
	}

	/* Symbol reference */
	if (curchar == '$') {
		nextchar();
		e = newExpr('$');
		e->symbol = strdup((char *)readName());
		return e;
	}

	/* Stack offset (rare) */
	if (curchar == 'S') {
		nextchar();
		e = newExpr('S');
		e->value = readHex4();
		return e;
	}

	/* Numeric constant - prefixed with # */
	if (curchar == '#') {
		nextchar();
		e = newExpr('C');
		e->value = readHex8();
		e->size = (e->value >= -32768 && e->value <= 65535) ? 2 : 4;
		return e;
	}

	/* Operator - check optab */
	op = curchar;
	nextchar();
	info = optab[op];

	if ((info & 3) == OP_S) {
		/* Special handlers */
		switch (op) {
		case '@': e = doCall(); break;
		case '?': e = doTernary(); break;
		case 'Y': e = doCopy(); break;
		case AST_PREINC: case AST_POSTINC: case AST_PREDEC: case AST_POSTDEC:
			e = doIncDec(op); break;
		case AST_BFEXTRACT: case AST_BFASSIGN:
			e = doBitfield(op); break;
		default:
			/* Unknown special - return placeholder */
			e = newExpr(op);
			break;
		}
	} else if (info) {
		e = doExprOp(op);
	} else {
		/* Unknown op - return placeholder */
		e = newExpr(op);
	}
	return e;
}

/* Statement handlers - paren-free format */

/*
 * Parse a single statement based on first char
 * New format:
 *   B decl_count. stmt_count. decls... stmts...
 *   I has_else. cond then [else]
 *   E expr
 *   R has_value. [expr]
 *   L hexname
 *   G hexname
 *   S case_count. expr cases...
 *   C stmt_count. value stmts...
 *   O stmt_count. stmts...
 *   A len hexdata
 *   ; (empty)
 *   K (break)
 *   N (continue)
 */
static struct stmt *
parseStmt(void)
{
	struct stmt *s, *first, *last, *child;
	unsigned char op;
	int i;

	skipWS();
	if (!curchar) return NULL;

	op = curchar;
	nextchar();

	switch (op) {
	case 'B':
		/* Block: B decl_count stmt_count decls... stmts... */
		{
			int decl_count = readHex2();
			int stmt_count = readHex2();
			if (TRACE(T_PARSE)) fdprintf(2, "BLOCK: decl=%d stmt=%d\n", decl_count, stmt_count);
			s = newStmt('B');
			first = last = NULL;

			/* Read declarations */
			for (i = 0; i < decl_count; i++) {
				if (curchar == 'd') {
					nextchar();
					child = newStmt('d');
					child->type_str = curchar;
					nextchar();
					child->symbol = strdup((char *)readName());
					appendChild(child, &first, &last);
				} else {
					if (TRACE(T_PARSE)) fdprintf(2, "BLOCK: decl %d/%d expected 'd', got '%c' (0x%x)\n", i, decl_count, curchar, curchar);
					break;  /* Parse error - stop */
				}
			}

			/* Read statements */
			for (i = 0; i < stmt_count; i++) {
				if (TRACE(T_PARSE)) fdprintf(2, "BLOCK: reading stmt %d/%d, curchar='%c' (0x%x)\n", i, stmt_count, curchar, curchar);
				child = parseStmt();
				appendChild(child, &first, &last);
			}
			s->then_branch = first;
		}
		return s;

	case 'I':
		/* If: I has_else cond then [else] */
		{
			int has_else = readHex2();
			if (TRACE(T_PARSE)) fdprintf(2, "IF: has_else=%d\n", has_else);
			s = newStmt('I');
			s->label = labelCounter++;
			s->expr = parseExpr();
			if (TRACE(T_PARSE)) fdprintf(2, "IF: after cond, curchar='%c' (0x%x)\n", curchar, curchar);
			s->then_branch = parseStmt();
			if (TRACE(T_PARSE)) fdprintf(2, "IF: after then, curchar='%c' (0x%x)\n", curchar, curchar);
			if (has_else) {
				s->label2 = labelCounter++;
				s->else_branch = parseStmt();
				s->next = mkEndLbl(s->label2);
			} else {
				s->next = mkEndLbl(s->label);
			}
		}
		return s;

	case 'E':
		/* Expression statement */
		s = newStmt('E');
		if (TRACE(T_PARSE)) fdprintf(2, "E: before parseExpr, curchar='%c' (0x%x)\n", curchar, curchar);
		s->expr = parseExpr();
		if (TRACE(T_PARSE)) fdprintf(2, "E: after parseExpr, curchar='%c' (0x%x)\n", curchar, curchar);
		return s;

	case 'R':
		/* Return: R has_value [expr] */
		{
			int has_value = readHex2();
			if (TRACE(T_PARSE)) fdprintf(2, "RETURN: has_value=%d\n", has_value);
			s = newStmt('R');
			if (has_value) {
				s->expr = parseExpr();
				/* Keep expression's natural size; emit.c handles widening */
			}
		}
		return s;

	case 'L':  /* Label */
	case 'G':  /* Goto */
		s = newStmt(op);
		s->symbol = strdup((char *)readName());
		return s;

	case 'S':
		/* Switch: S has_label [hexlabel] case_count expr cases... */
		{
			int has_label = readHex2();
			char *label_name = NULL;
			int case_count;
			if (has_label)
				label_name = strdup((char *)readName());
			case_count = readHex2();
			if (TRACE(T_PARSE)) fdprintf(2, "SWITCH: has_label=%d case_count=%d\n", has_label, case_count);
			s = newStmt('S');
			s->symbol = label_name;
			s->expr = parseExpr();
			first = last = NULL;
			for (i = 0; i < case_count; i++) {
				child = parseStmt();
				appendChild(child, &first, &last);
			}
			s->then_branch = first;
		}
		return s;

	case 'C':  /* Case: C stmt_count value stmts... */
	case 'O':  /* Default: O stmt_count stmts... */
		{
			int stmt_count = readHex2();
			if (TRACE(T_PARSE)) fdprintf(2, "%s: stmt_count=%d\n", op == 'C' ? "CASE" : "DEFAULT", stmt_count);
			s = newStmt(op);
			if (op == 'C') s->expr = parseExpr();
			first = last = NULL;
			for (i = 0; i < stmt_count; i++) {
				child = parseStmt();
				appendChild(child, &first, &last);
			}
			s->then_branch = first;
		}
		return s;

	case 'A':
		/* Asm: A len hexdata (hex-encoded) */
		s = newStmt('A');
		s->asm_block = (char *)readHexStr();
		return s;

	case ';':  /* Empty statement */
	case 'K':  /* Break */
	case 'N':  /* Continue */
		return newStmt(op);

	case 'W':
		/* While (unlabeled) - not used when labels present */
		s = newStmt('W');
		s->expr = parseExpr();
		s->then_branch = parseStmt();
		return s;

	case 'D':
		/* Do (unlabeled) - not used when labels present */
		s = newStmt('D');
		s->then_branch = parseStmt();
		s->expr = parseExpr();
		return s;

	case 'F':
		/* For (unlabeled) - not used when labels present */
		s = newStmt('F');
		s->expr = parseExpr();   /* init */
		s->expr2 = parseExpr();  /* cond */
		s->expr3 = parseExpr();  /* incr */
		s->then_branch = parseStmt();
		return s;

	default:
		/* Unknown - return empty */
		if (TRACE(T_PARSE)) fdprintf(2, "UNKNOWN STMT: curchar='%c' (0x%x)\n", curchar, curchar);
		return newStmt(';');
	}
}

/* Top-level: function
 * New format: F rettype hexname param_count. d suffix name d suffix name ... body
 */
static void
doFunction(unsigned char rettype)
{
	static char name_buf[256];
	static char params_buf[256];
	static char rettype_buf[2];
	char *p, *param;
	unsigned char ptype;
	int first_param, param_count, i;

	rettype_buf[0] = rettype;
	rettype_buf[1] = '\0';
	fnRettype = rettype_buf;

	strncpy(name_buf, (char *)readName(), sizeof(name_buf) - 1);
	name_buf[sizeof(name_buf) - 1] = '\0';
	fnName = name_buf;
	if (TRACE(T_PARSE)) fdprintf(2, "doFunction: %s\n", fnName);

	switchToSeg(SEG_TEXT);
	addDefSym(fnName);

	/* Parse parameters: param_count d suffix name d suffix name ... */
	param_count = readHex2();
	if (TRACE(T_PARSE)) fdprintf(2, "  param_count=%d\n", param_count);
	p = params_buf;
	params_buf[0] = '\0';
	first_param = 1;
	for (i = 0; i < param_count; i++) {
		skipWS();
		if (curchar != 'd') break;
		nextchar();
		ptype = curchar;
		nextchar();
		param = (char *)readName();
		if (!first_param && p < params_buf + sizeof(params_buf) - 2) {
			*p++ = ','; *p++ = ' ';
		}
		first_param = 0;
		while (*param && p < params_buf + sizeof(params_buf) - 20)
			*p++ = *param++;
		if (p < params_buf + sizeof(params_buf) - 3) {
			*p++ = ':'; *p++ = ptype;
		}
	}
	*p = '\0';
	fnParams = params_buf;
	if (TRACE(T_PARSE)) fdprintf(2, "  params: %s\n", fnParams);

	/* Skip newlines between params and body */
	skipNL();
	if (TRACE(T_PARSE)) fdprintf(2, "  parsing body\n");

	/* Parse body */
	fnBody = parseStmt();
	if (TRACE(T_PARSE)) fdprintf(2, "  body parsed\n");
	fnLblCnt = labelCounter;
	fnLocals = NULL;
	fnFrmSize = 0;
	fnDESaveCnt = 0;
	fnDInUse = 0;
	fnLoopDep = 0;
	fnDEValid = 0;
	fnZValid = 0;
	fnIXAOfs = -1;
	fnIXHLOfs = -1;
	fnIYHLValid = 0;
	fnABCValid = 0;
	fnAZero = 0;
	cacheInvalAll();

	/* Code generation phases */
	if (TRACE(T_PARSE)) fdprintf(2, "  assignFrmOff\n");
	assignFrmOff();
	if (TRACE(T_PARSE)) fdprintf(2, "  analyzeVars\n");
	analyzeVars();
	if (TRACE(T_PARSE)) fdprintf(2, "  allocRegs\n");
	allocRegs();
	if (TRACE(T_PARSE)) fdprintf(2, "  optFrmLayout\n");
	optFrmLayout();
	if (TRACE(T_PARSE)) fdprintf(2, "  setOpFlags\n");
	setOpFlags();
	if (TRACE(T_PARSE)) fdprintf(2, "  dumpFnAst\n");
	dumpFnAst(outFd);
	if (TRACE(T_PARSE)) fdprintf(2, "  scheduleCode\n");
	scheduleCode();
	if (TRACE(T_PARSE)) fdprintf(2, "  dumpScheduled\n");
	dumpScheduled(outFd);
	if (TRACE(T_PARSE)) fdprintf(2, "  specialize\n");
	specialize();
	if (TRACE(T_PARSE)) fdprintf(2, "  generateCode\n");
	generateCode();
	if (TRACE(T_PARSE)) fdprintf(2, "  emitAssembly\n");
	emitAssembly(outFd);
	if (TRACE(T_PARSE)) fdprintf(2, "  complete\n");
}

/* Symbol tracking */
#define MAX_SYMBOLS 256
static char *defSymbols[MAX_SYMBOLS];
static int numDefined = 0;
static char *refSymbols[MAX_SYMBOLS];
static int numReferenced = 0;

static int
findSym(const char *name, char **arr, int cnt)
{
	int i;
	for (i = 0; i < cnt; i++)
		if (strcmp(arr[i], name) == 0) return 1;
	return 0;
}

#define isDefSym(n) findSym(n, defSymbols, numDefined)

static void addDefSym(const char *name) {
	if (!isDefSym(name) && numDefined < MAX_SYMBOLS)
		defSymbols[numDefined++] = strdup(name);
}

void addRefSym(const char *name) {
	if (!findSym(name, refSymbols, numReferenced) && numReferenced < MAX_SYMBOLS)
		refSymbols[numReferenced++] = strdup(name);
}

/* Emit BSS variable if not already defined */
static void
emitBss(const char *name, int size)
{
	if (!isDefSym(name)) {
		addDefSym(name);
		switchToSeg(SEG_BSS);
		fdprintf(outFd, "%s:\n\t.ds %d\n", name, size);
	}
}

/* Top-level: global variable
 * New format: Z $hexname type has_init. [init]
 * Initializer format: [ width count. items...
 */
static void
doGlobal(void)
{
	char name_buf[256];
	char *name;
	unsigned char type_char, elem_type;
	int val, col, first, has_init, init_count;
	long count, elemsize, size;

	/* Skip '$' if present */
	if (curchar == '$') nextchar();
	name = (char *)readName();
	strncpy(name_buf, name, sizeof(name_buf) - 1);
	name_buf[sizeof(name_buf) - 1] = '\0';

	/* Read type info */
	type_char = curchar;
	nextchar();

	if (type_char == 'a') {
		/* Array: a count elemsize has_init [init] */
		count = readHex4();
		elemsize = readHex4();
		size = count * elemsize;
		has_init = readHex2();

		/* Check for initializer */
		if (has_init && curchar == '[' && count >= 0) {
			nextchar();  /* skip '[' */
			elem_type = curchar;
			nextchar();
			init_count = readHex2();

			if (!isDefSym(name_buf) && (elem_type == 'b' || elem_type == 'B')) {
				int i;
				col = 0; first = 1;
				addDefSym(name_buf);
				switchToSeg(SEG_DATA);
				fdprintf(outFd, "%s:\n", name_buf);
				for (i = 0; i < init_count; i++) {
					nextchar();  /* skip # */
					val = (int)readHex8();
					if (!first && col > 70) {
						fdputs(outFd, "\n");
						col = 0;
					}
					if (first || col == 0) {
						fdputs(outFd, "\t.db ");
						col = 12;
						first = 0;
					} else {
						fdputs(outFd, ", ");
						col += 2;
					}
					fdprintf(outFd, "%d", val);
					col += (val < 10) ? 1 : (val < 100) ? 2 : 3;
				}
				fdputs(outFd, "\n");
			} else {
				/* Skip initializer */
				int i;
				for (i = 0; i < init_count; i++) {
					nextchar();  /* skip # */
					readHex8();
				}
			}
			return;
		}

		/* Uninitialized array */
		if (count >= 0)
			emitBss(name_buf, (int)size);
	} else if (type_char == 'p') {
		/* Pointer */
		has_init = readHex2();
		if (has_init) parseExpr();
		emitBss(name_buf, 2);
	} else if (type_char == 'r') {
		/* Struct */
		size = readHex4();
		has_init = readHex2();
		if (has_init) parseExpr();
		emitBss(name_buf, (int)size);
	} else {
		/* Primitive: b/s/l */
		has_init = readHex2();
		if (has_init) parseExpr();
		emitBss(name_buf, getSizeFTStr(type_char));
	}
}

/* Top-level: string literal - these are local/static symbols */
static void
doStrLiteral(void)
{
	static const char *esc = "\n\t\r\"\\";
	static const char *rep[] = { "\\n", "\\t", "\\r", "\\\"", "\\\\" };
	char *name = (char *)readName();
	char *data = (char *)readStr();
	char *orig_data = data, *p;
	unsigned char c;

	addDefSym(name);
	switchToSeg(SEG_DATA);
	fdprintf(outFd, "%s:\n\t.db \"", name);
	while ((c = *data++)) {
		if ((p = strchr(esc, c)))
			fdputs(outFd, rep[p - esc]);
		else if (c >= 32 && c < 127)
			fdprintf(outFd, "%c", c);
		else
			fdprintf(outFd, "\\x%02x", c);
	}
	fdputs(outFd, "\\0\"\n");
	free(orig_data);
}

/* Emit symbol declarations */
static void
emitSymDecls(void)
{
	int i;
	for (i = 0; i < numDefined; i++)
		if (!isLocalSym(defSymbols[i]))
			fdprintf(outFd, "%s %s\n", ASM_GLOBAL, defSymbols[i]);
	for (i = 0; i < numReferenced; i++)
		if (!isLocalSym(refSymbols[i]) && !isDefSym(refSymbols[i]))
			fdprintf(outFd, "%s %s\n", ASM_EXTERN, refSymbols[i]);
}

/* Parse top-level - paren-free format
 * F rettype name params body
 * Z $name type has_init [init]
 * U name data
 */
static void
parseToplvl(void)
{
	unsigned char op;
	unsigned char rettype;

	skipNL();
	if (!curchar) return;

	op = curchar;
	nextchar();

	switch (op) {
	case 'F':
		rettype = curchar;
		nextchar();
		doFunction(rettype);
		break;
	case 'Z':
		doGlobal();
		break;
	case 'U':
		doStrLiteral();
		break;
	case 'A':
		/* Global asm block - output raw text to text segment */
		{
			char *asm_text = (char *)readHexStr();
			switchToSeg(SEG_TEXT);
			fdprintf(outFd, "%s\n", asm_text);
			free(asm_text);
		}
		break;
	default:
		/* Unknown top-level - skip to newline */
		while (curchar && curchar != '\n') nextchar();
		break;
	}
}

/* Main entry point */
int
parseAstFile(int in, int out)
{
	initAstio(in);
	outFd = out;
	nextchar();

	while (curchar) {
		skipNL();
		if (curchar) parseToplvl();
	}

	emitSymDecls();
	return 0;
}
