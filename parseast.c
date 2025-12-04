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
int fnZValid;
int fnCmpFlag;
char fnIXAOfs;
char fnIXHLOfs;
char fnIYHLOfs;
char fnIYHLValid;
char fnABCValid;

/* Segment tracking */
#define SEG_NONE 0
#define SEG_TEXT 1
#define SEG_DATA 2
#define SEG_BSS  3
static int currentSeg = SEG_NONE;

static void
switchToSeg(int seg)
{
	if (seg == currentSeg) return;
	switch (seg) {
	case SEG_TEXT: fdputs(outFd, "\n.text\n"); break;
	case SEG_DATA: fdputs(outFd, "\n.data\n"); break;
	case SEG_BSS:  fdputs(outFd, "\n.bss\n");  break;
	}
	currentSeg = seg;
}

/* Tree node allocation */
struct expr *
newExpr(unsigned char op)
{
	struct expr *e = malloc(sizeof(struct expr));
	if (!e) { fdprintf(2, "parseast: out of memory\n"); exit(1); }
	e->op = op;
	e->left = NULL;
	e->right = NULL;
	e->type_str = 0;
	e->value = 0;
	e->symbol = NULL;
	e->size = 2;
	e->flags = 0;
	e->asm_block = NULL;
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
	if (!s) { fdprintf(2, "parseast: out of memory\n"); exit(1); }
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

/* Size helpers */
unsigned char
getSizeFTStr(unsigned char type_str)
{
	switch (type_str) {
	case 'b': return 1;
	case 's': case 'p': return 2;
	case 'l': case 'f': return 4;
	case 'd': return 8;
	default: return 2;
	}
}

unsigned char
getSignFTStr(unsigned char type_str)
{
	return (type_str == 'p') ? E_UNSIGNED : 0;
}

unsigned char
getSizeFromTN(const char *typename)
{
	if (!typename) return 2;
	if (typename[0] == 'b') return 1;
	if (typename[0] == 's') return 2;
	if (typename[0] == 'l') return 4;
	if (typename[0] == 'p') return 2;
	return 2;
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
	if (!*first) *first = child;
	else (*last)->next = child;
	*last = child;
	while ((*last)->next) *last = (*last)->next;
}

/* Create ASM label statement */
static struct stmt *
createLblAsm(const char *label_name)
{
	struct stmt *s = newStmt('A');
	char buf[128];
	snprintf(buf, sizeof(buf), "%s:", label_name);
	s->asm_block = strdup(buf);
	return s;
}

void
freeExpr(struct expr *e)
{
	if (!e) return;
	freeExpr(e->left);
	freeExpr(e->right);
	if (e->asm_block) free(e->asm_block);
	if (e->cleanup_block) free(e->cleanup_block);
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
	if (s->symbol) free(s->symbol);
	if (s->asm_block) free(s->asm_block);
	free(s);
}

/* Expression handlers */

static struct expr *
doConst(void)
{
	struct expr *e = newExpr('C');
	e->value = readNum();
	e->size = (e->value >= -32768 && e->value <= 65535) ? 2 : 4;
	return e;
}

static struct expr *
doSymbol(void)
{
	struct expr *e = newExpr('$');
	e->symbol = strdup((char *)readName());
	return e;
}

static struct expr *
doDeref(void)
{
	struct expr *e = newExpr('M');
	unsigned char width = curchar;
	nextchar();
	e->type_str = width;
	e->size = getSizeFTStr(width);
	e->flags = getSignFTStr(width);
	e->left = parseExpr();
	if (curchar == ')') nextchar();
	return e;
}

static struct expr *
doAssign(void)
{
	struct expr *e = newExpr('=');
	unsigned char width = curchar;
	nextchar();
	e->type_str = width;
	e->size = getSizeFTStr(width);
	e->flags = getSignFTStr(width);
	e->left = parseExpr();
	e->right = parseExpr();
	if (e->right && e->right->op == 'C') e->right->size = e->size;
	if (curchar == ')') nextchar();
	return e;
}

static struct expr *
doCompAsn(unsigned char op)
{
	struct expr *e = newExpr(op);
	unsigned char width = curchar;
	nextchar();
	e->type_str = width;
	e->size = getSizeFTStr(width);
	e->flags = getSignFTStr(width);
	e->left = parseExpr();
	e->right = parseExpr();
	if (e->right && e->right->op == 'C') e->right->size = e->size;
	if (curchar == ')') nextchar();
	return e;
}

static struct expr *
doBinaryOp(unsigned char op)
{
	struct expr *e = newExpr(op);
	e->left = parseExpr();
	e->right = parseExpr();
	if (e->left && e->right) {
		if (e->left->op == 'C' && e->right->op != 'C')
			e->left->size = e->right->size;
		else if (e->right->op == 'C' && e->left->op != 'C')
			e->right->size = e->left->size;
		e->size = (e->left->size > e->right->size) ? e->left->size : e->right->size;
		e->flags = (e->left->flags | e->right->flags) & E_UNSIGNED;
	}
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
	if (curchar == ')') nextchar();
	return e;
}

static struct expr *
doLand(void)
{
	struct expr *e = newExpr('j');
	e->label = labelCounter++;
	e->left = parseExpr();
	e->right = parseExpr();
	if (curchar == ')') nextchar();
	return e;
}

static struct expr *
doLor(void)
{
	struct expr *e = newExpr('h');
	e->label = labelCounter++;
	e->left = parseExpr();
	e->right = parseExpr();
	if (curchar == ')') nextchar();
	return e;
}

static struct expr *
doUnaryOp(unsigned char op)
{
	struct expr *e = newExpr(op);
	e->left = parseExpr();
	if (e->left) e->size = e->left->size;
	if (op == 0xb6) e->flags = E_UNSIGNED;
	else if (op == 0xab) e->flags = 0;
	else if (e->left) e->flags = e->left->flags;
	if (curchar == ')') nextchar();
	return e;
}

/* Handle conversion ops: N (NARROW), W (WIDEN), 0xab (SEXT) with :s suffix */
static struct expr *
doConvOp(unsigned char op)
{
	struct expr *e = newExpr(op);
	unsigned char size_suffix;
	/* expect :s suffix */
	if (curchar == ':') {
		nextchar();
		size_suffix = curchar;
		nextchar();
		e->type_str = size_suffix;
		e->size = getSizeFTStr(size_suffix);
	}
	e->left = parseExpr();
	if (e->left) e->flags = e->left->flags;
	if (curchar == ')') nextchar();
	return e;
}

static struct expr *
doIncDec(unsigned char op)
{
	struct expr *e = newExpr(op);
	unsigned char size = curchar;
	nextchar();
	e->type_str = size;
	e->size = getSizeFTStr(size);
	e->left = parseExpr();
	e->value = readNum();
	if (e->left) e->flags = e->left->flags;
	if (curchar == ')') nextchar();
	return e;
}

static struct expr *
doBfextract(void)
{
	struct expr *e = newExpr(0xa7);
	int offset = (int)readNum();
	int width = (int)readNum();
	e->value = (offset << 16) | width;
	e->left = parseExpr();
	if (curchar == ')') nextchar();
	return e;
}

static struct expr *
doBfassign(void)
{
	struct expr *e = newExpr(0xdd);
	int offset = (int)readNum();
	int width = (int)readNum();
	e->value = (offset << 16) | width;
	e->left = parseExpr();
	e->right = parseExpr();
	if (curchar == ')') nextchar();
	return e;
}

static struct expr *
doCopy(void)
{
	struct expr *e = newExpr(0xbb);
	e->value = readNum();
	e->left = parseExpr();
	e->right = parseExpr();
	if (curchar == ')') nextchar();
	return e;
}

static struct expr *
doCall(void)
{
	struct expr *e = newExpr('@');
	struct expr *args[32];
	struct expr *wrapper, *prev;
	int arg_count = 0;
	int i;
	e->left = parseExpr();
	while (curchar != ')' && curchar != 0) {
		if (curchar == '\n' || curchar == ' ' || curchar == '\t' || curchar == '\r') {
			nextchar();
			continue;
		}
		if (arg_count >= 32) break;
		args[arg_count++] = parseExpr();
	}
	e->value = arg_count;
	prev = NULL;
	for (i = 0; i < arg_count; i++) {
		wrapper = newExpr(',');
		wrapper->left = args[i];
		wrapper->right = NULL;
		if (prev) prev->right = wrapper;
		else e->right = wrapper;
		prev = wrapper;
	}
	if (curchar == ')') nextchar();
	return e;
}

static struct expr *
doTernary(void)
{
	struct expr *e = newExpr('?');
	struct expr *colon;
	e->left = parseExpr();
	if (curchar == '(') {
		nextchar();
		if (curchar == ':') {
			nextchar();
			colon = newExpr(':');
			colon->left = parseExpr();
			colon->right = parseExpr();
			if (curchar == ')') nextchar();
			e->right = colon;
		}
	}
	if (curchar == ')') nextchar();
	return e;
}

static struct expr *
doColon(void)
{
	struct expr *e = newExpr(':');
	e->left = parseExpr();
	e->right = parseExpr();
	if (curchar == ')') nextchar();
	return e;
}

/* Parse expression */
static struct expr *
parseExpr(void)
{
	struct expr *e;
	unsigned char op;

	if (curchar == '(') {
		nextchar();
		if (curchar == ')') { nextchar(); return NULL; }
		op = curchar;
		nextchar();
		switch (op) {
		case 'M': e = doDeref(); break;
		case '=': e = doAssign(); break;
		case '@': e = doCall(); break;
		case '?': e = doTernary(); break;
		case ':': e = doColon(); break;
		case 'Y': e = doCopy(); break;
		case '+': case '-': case '*': case '/': case '%':
		case '&': case '|': case '^': case '<': case '>':
		case 'Q': case 'n': case 'L': case 'g': case 'y': case 'w':
			e = doBinaryOp(op); break;
		case 'h': e = doLor(); break;
		case 'j': e = doLand(); break;
		case 'P': case 'T': case '2': case '1': case 'X': case '0': case '6':
		case 0xdf: case 0xfe: case 0xc6:
			e = doCompAsn(op); break;
		case '!': case '~': case '\\': case '\'':
			e = doUnaryOp(op); break;
		case 'N': case 'W': case 0xab:
			e = doConvOp(op); break;
		case 0xcf: case 0xef: case 0xd6: case 0xf6:
			e = doIncDec(op); break;
		case 0xa7: e = doBfextract(); break;
		case 0xdd: e = doBfassign(); break;
		default:
			e = newExpr(op);
			while (curchar && curchar != ')') nextchar();
			if (curchar == ')') nextchar();
			break;
		}
	} else if (curchar == '$') {
		nextchar();
		e = doSymbol();
	} else if (curchar == 'S') {
		nextchar();
		e = newExpr('S');
		e->value = readNum();
	} else if ((curchar >= '0' && curchar <= '9') ||
		   (curchar >= 'a' && curchar <= 'f') || curchar == '-') {
		e = doConst();
	} else {
		e = NULL;
	}
	return e;
}

/* Statement handlers */
static struct stmt *doBlock(void);
static struct stmt *doIf(void);
static struct stmt *doReturn(void);
static struct stmt *doExprStmt(void);
static struct stmt *doAsm(void);
static struct stmt *doLabel(void);
static struct stmt *doGoto(void);
static struct stmt *doSwitch(void);

static struct stmt *
doBlock(void)
{
	struct stmt *s = newStmt('B');
	struct stmt *first = NULL, *last = NULL, *child;
	unsigned char op;
	char *name;
	unsigned char type_suffix;

	while (curchar != ')' && curchar) {
		if (curchar == '\n' || curchar == ' ' || curchar == '\t' || curchar == '\r') {
			nextchar();
			continue;
		}
		if (curchar == '(') {
			nextchar();
			op = curchar;
			nextchar();
			if (op == 'd') {
				type_suffix = curchar;
				nextchar();
				name = (char *)readName();
				child = newStmt('d');
				child->symbol = strdup(name);
				child->type_str = type_suffix;
				if (curchar == ')') nextchar();
			} else {
				switch (op) {
				case 'B': child = doBlock(); break;
				case 'I': child = doIf(); break;
				case 'R': child = doReturn(); break;
				case 'E': child = doExprStmt(); break;
				case ';': child = newStmt(';'); if (curchar == ')') nextchar(); break;
				case 'A': child = doAsm(); break;
				case 'L': child = doLabel(); break;
				case 'G': child = doGoto(); break;
				case 'S': child = doSwitch(); break;
				case 'C': {
					child = newStmt('C');
					child->expr = parseExpr();
					if (curchar == '(') { nextchar(); if (curchar == ')') nextchar(); }
					if (curchar == ')') nextchar();
					break;
				}
				case 'O': {
					child = newStmt('O');
					if (curchar == '(') { nextchar(); if (curchar == ')') nextchar(); }
					if (curchar == ')') nextchar();
					break;
				}
				case 'K': child = newStmt('K'); if (curchar == ')') nextchar(); break;
				case 'N': child = newStmt('N'); if (curchar == ')') nextchar(); break;
				default:
					while (curchar && curchar != ')') nextchar();
					if (curchar == ')') nextchar();
					child = NULL;
					break;
				}
			}
			appendChild(child, &first, &last);
		} else {
			/* Skip unexpected character */
			nextchar();
		}
	}
	if (curchar == ')') nextchar();
	s->then_branch = first;
	return s;
}

static struct stmt *
doIf(void)
{
	struct stmt *s = newStmt('I');
	char label_buf[64];
	s->label = labelCounter++;
	s->expr = parseExpr();
	s->then_branch = parseStmt();
	if (curchar != ')') {
		s->label2 = labelCounter++;
		s->else_branch = parseStmt();
		snprintf(label_buf, sizeof(label_buf), "_if_end_%d", s->label2);
	} else {
		snprintf(label_buf, sizeof(label_buf), "_if_end_%d", s->label);
	}
	s->next = createLblAsm(label_buf);
	if (curchar == ')') nextchar();
	return s;
}

static struct stmt *
doReturn(void)
{
	struct stmt *s = newStmt('R');
	if (curchar != ')') {
		s->expr = parseExpr();
		if (s->expr && s->expr->op == 'C') {
			s->expr->size = fnRettype[0] == 'b' ? 1 : fnRettype[0] == 'l' ? 4 : 2;
		}
	}
	if (curchar == ')') nextchar();
	return s;
}

static struct stmt *
doExprStmt(void)
{
	struct stmt *s = newStmt('E');
	s->expr = parseExpr();
	if (curchar == ')') nextchar();
	return s;
}

static struct stmt *
doAsm(void)
{
	struct stmt *s = newStmt('A');
	char *asm_text = (char *)readStr();
	s->asm_block = asm_text;
	if (curchar == ')') nextchar();
	return s;
}

static struct stmt *
doLabel(void)
{
	struct stmt *s = newStmt('L');
	s->symbol = strdup((char *)readName());
	if (curchar == ')') nextchar();
	return s;
}

static struct stmt *
doGoto(void)
{
	struct stmt *s = newStmt('G');
	s->symbol = strdup((char *)readName());
	if (curchar == ')') nextchar();
	return s;
}

static struct stmt *
doSwitch(void)
{
	struct stmt *s = newStmt('S');
	struct stmt *first = NULL, *last = NULL, *child;
	unsigned char clause_type;

	s->expr = parseExpr();
	while (curchar != ')' && curchar) {
		if (curchar == '\n' || curchar == ' ' || curchar == '\t' || curchar == '\r') {
			nextchar();
			continue;
		}
		if (curchar == '(') {
			nextchar();
			clause_type = curchar;
			nextchar();
			switch (clause_type) {
			case 'C':
				child = newStmt('C');
				child->expr = parseExpr();
				if (curchar == '(') { nextchar(); if (curchar == ')') nextchar(); }
				if (curchar == ')') nextchar();
				break;
			case 'O':
				child = newStmt('O');
				if (curchar == '(') { nextchar(); if (curchar == ')') nextchar(); }
				if (curchar == ')') nextchar();
				break;
			case 'R': child = doReturn(); break;
			case 'G': child = doGoto(); break;
			case 'B': child = doBlock(); break;
			case 'K': child = newStmt('K'); if (curchar == ')') nextchar(); break;
			case 'E': child = doExprStmt(); break;
			case 'I': child = doIf(); break;
			case 'L': child = doLabel(); break;
			case 'A': child = doAsm(); break;
			case 'N': child = newStmt('N'); if (curchar == ')') nextchar(); break;
			case ';': child = newStmt(';'); if (curchar == ')') nextchar(); break;
			default:
				while (curchar && curchar != ')') nextchar();
				if (curchar == ')') nextchar();
				child = NULL;
				break;
			}
			appendChild(child, &first, &last);
		}
	}
	if (curchar == ')') nextchar();
	s->then_branch = first;
	return s;
}

/* Parse statement */
static struct stmt *
parseStmt(void)
{
	unsigned char op;
	if (curchar != '(') return NULL;
	nextchar();
	if (curchar == ')') { nextchar(); return NULL; }
	op = curchar;
	nextchar();
	switch (op) {
	case 'B': return doBlock();
	case 'I': return doIf();
	case 'R': return doReturn();
	case 'E': return doExprStmt();
	case ';': { struct stmt *s = newStmt(';'); if (curchar == ')') nextchar(); return s; }
	case 'A': return doAsm();
	case 'L': return doLabel();
	case 'G': return doGoto();
	case 'S': return doSwitch();
	default:
		while (curchar && curchar != ')') nextchar();
		if (curchar == ')') nextchar();
		return NULL;
	}
}

/* Top-level: function */
static void
doFunction(unsigned char rettype)
{
	static char name_buf[256];
	static char params_buf[256];
	static char rettype_buf[2];
	char *p, *param;
	unsigned char ptype;
	int first_param;

	rettype_buf[0] = rettype;
	rettype_buf[1] = '\0';
	fnRettype = rettype_buf;

	strncpy(name_buf, (char *)readName(), sizeof(name_buf) - 1);
	name_buf[sizeof(name_buf) - 1] = '\0';
	fnName = name_buf;

	switchToSeg(SEG_TEXT);
	addDefSym(fnName);

	/* Parse parameters: ((d suffix hexname) ...) */
	if (curchar == '(') nextchar();
	p = params_buf;
	params_buf[0] = '\0';
	first_param = 1;
	while (curchar != ')' && curchar) {
		if (curchar == '\n' || curchar == ' ' || curchar == '\t' || curchar == '\r') {
			nextchar();
			continue;
		}
		if (curchar == '(') {
			nextchar();
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
			if (curchar == ')') nextchar();
		}
	}
	*p = '\0';
	fnParams = params_buf;
	if (curchar == ')') nextchar();

	/* Skip newlines between params and body */
	skipNL();

	/* Parse body */
	fnBody = parseStmt();
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
	cacheInvalAll();

	if (curchar == ')') nextchar();

	/* Code generation phases */
	assignFrmOff();
	analyzeVars();
	optFrmLayout();
	allocRegs();
	setOpFlags();
	dumpFnAst(outFd);
	specialize();
	generateCode();
	emitAssembly(outFd);
}

/* Symbol tracking */
#define MAX_SYMBOLS 512
static char *defSymbols[MAX_SYMBOLS];
static int numDefined = 0;
static char *refSymbols[MAX_SYMBOLS];
static int numReferenced = 0;

static int
addSymTo(const char *name, char **arr, int *cnt, int max)
{
	int i;
	for (i = 0; i < *cnt; i++)
		if (strcmp(arr[i], name) == 0) return 1;
	if (*cnt < max) arr[(*cnt)++] = strdup(name);
	return 0;
}

static int isDefSym(const char *name) {
	int i;
	for (i = 0; i < numDefined; i++)
		if (strcmp(defSymbols[i], name) == 0) return 1;
	return 0;
}

static void addDefSym(const char *name) {
	addSymTo(name, defSymbols, &numDefined, MAX_SYMBOLS);
}

void addRefSym(const char *name) {
	addSymTo(name, refSymbols, &numReferenced, MAX_SYMBOLS);
}

/* Top-level: global variable */
static void
doGlobal(void)
{
	char name_buf[256];
	char *name;
	unsigned char type_char, elem_type;
	int isDefined, depth, val, col, first;
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
		/* Array: a count. elemsize. [init] */
		count = readNum();
		elemsize = readNum();
		size = count * elemsize;

		/* Check for initializer */
		if (curchar == '(' && count >= 0) {
			nextchar();
			if (curchar == '[') {
				nextchar();
				elem_type = curchar;
				nextchar();

				isDefined = isDefSym(name_buf);
				if (!isDefined && elem_type == 'b') {
					col = 0; first = 1;
					addDefSym(name_buf);
					switchToSeg(SEG_DATA);
					fdprintf(outFd, "%s:\n", name_buf);
					while (curchar != ')' && curchar) {
						val = (int)readNum();
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
					while (curchar != ')' && curchar) readNum();
				}
				if (curchar == ')') nextchar();
				if (curchar == ')') nextchar();
				if (curchar == ')') nextchar();
				return;
			}
			/* Skip other initializer formats */
			depth = 1;
			while (depth > 0 && curchar) {
				if (curchar == '(') depth++;
				else if (curchar == ')') depth--;
				nextchar();
			}
		}

		/* Uninitialized array */
		if (count >= 0) {
			isDefined = isDefSym(name_buf);
			if (!isDefined) {
				addDefSym(name_buf);
				switchToSeg(SEG_BSS);
				fdprintf(outFd, "%s:\n\t.ds %d\n", name_buf, size);
			}
		}
	} else if (type_char == 'p') {
		/* Pointer */
		isDefined = isDefSym(name_buf);
		if (!isDefined) {
			addDefSym(name_buf);
			switchToSeg(SEG_BSS);
			fdprintf(outFd, "%s:\n\t.ds 2\n", name_buf);
		}
	} else if (type_char == 'r') {
		/* Struct */
		long size = readNum();
		isDefined = isDefSym(name_buf);
		if (!isDefined) {
			addDefSym(name_buf);
			switchToSeg(SEG_BSS);
			fdprintf(outFd, "%s:\n\t.ds %ld\n", name_buf, size);
		}
	} else {
		/* Primitive: b/s/l */
		int size = getSizeFTStr(type_char);
		isDefined = isDefSym(name_buf);
		if (!isDefined) {
			addDefSym(name_buf);
			switchToSeg(SEG_BSS);
			fdprintf(outFd, "%s:\n\t.ds %d\n", name_buf, size);
		}
	}

	/* Skip any trailing content */
	while (curchar && curchar != ')' && curchar != '\n') {
		if (curchar == '(') {
			int depth = 1;
			nextchar();
			while (depth > 0 && curchar) {
				if (curchar == '(') depth++;
				else if (curchar == ')') depth--;
				nextchar();
			}
		} else {
			nextchar();
		}
	}
	if (curchar == ')') nextchar();
}

/* Top-level: string literal */
static void
doStrLiteral(void)
{
	char *name = (char *)readName();
	char *data = (char *)readStr();
	char *orig_data = data;
	char str_label[128];

	switchToSeg(SEG_DATA);
	fdprintf(outFd, "_%s:\n\t.db \"", name);

	/* Emit escaped string */
	while (*data) {
		unsigned char c = *data++;
		switch (c) {
		case '\n': fdputs(outFd, "\\n"); break;
		case '\t': fdputs(outFd, "\\t"); break;
		case '\r': fdputs(outFd, "\\r"); break;
		case '"':  fdputs(outFd, "\\\""); break;
		case '\\': fdputs(outFd, "\\\\"); break;
		default:
			if (c >= 32 && c < 127) fdprintf(outFd, "%c", c);
			else fdprintf(outFd, "\\x%02x", c);
			break;
		}
	}
	fdputs(outFd, "\\0\"\n");

	snprintf(str_label, sizeof(str_label), "_%s", name);
	addDefSym(str_label);
	free(orig_data);

	if (curchar == ')') nextchar();
}

/* Emit symbol declarations */
static void
emitSymDecls(void)
{
	int i;
	for (i = 0; i < numDefined; i++) {
		if (!isLocalSym(defSymbols[i])) {
			fdputs(outFd, ASM_GLOBAL " ");
			fdputs(outFd, defSymbols[i]);
			fdputs(outFd, "\n");
		}
	}
	for (i = 0; i < numReferenced; i++) {
		if (isLocalSym(refSymbols[i])) continue;
		if (isDefSym(refSymbols[i])) continue;
		fdputs(outFd, ASM_EXTERN " ");
		fdputs(outFd, refSymbols[i]);
		fdputs(outFd, "\n");
	}
}

/* Parse top-level */
static void
parseToplvl(void)
{
	unsigned char op;
	unsigned char rettype;

	skipNL();
	if (curchar != '(') {
		/* consume unexpected char to avoid infinite loop */
		if (curchar) nextchar();
		return;
	}
	nextchar();
	op = curchar;
	nextchar();

	switch (op) {
	case 'f':
		rettype = curchar;
		nextchar();
		doFunction(rettype);
		break;
	case 'Z':
		doGlobal();
		break;
	case 's':
		doStrLiteral();
		break;
	default:
		while (curchar && curchar != ')') nextchar();
		if (curchar == ')') nextchar();
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
		if (curchar == ';') {
			skipLine();
			continue;
		}
		if (curchar) parseToplvl();
	}

	emitSymDecls();
	return 0;
}
