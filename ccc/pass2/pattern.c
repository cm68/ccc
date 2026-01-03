/*
 * pattern.c - Table-driven pattern builder for code generation
 *
 * Builds flattened expression strings from AST for pattern matching.
 * Format: each operator is 2 chars (op + size), $ and # add 2 hex digits for index.
 *
 * Example: x = y + 5 becomes "=s$s02+s$s00#s01"
 *   operands[0] = "_y", operands[1] = 5, operands[2] = "_x"
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cc2.h"
#include "../../cpp/lexeme.h"

static char hexdigit[16] = "0123456789abcdef";

/*
 * Initialize a pattern
 */
void patInit(struct pattern *p)
{
	p->str[0] = '\0';
	p->len = 0;
	p->nops = 0;
}

/*
 * Append a character to pattern string
 */
static void patAppend(struct pattern *p, char c)
{
	if (p->len < MAX_PATTERN - 1) {
		p->str[p->len++] = c;
		p->str[p->len] = '\0';
	}
}

/*
 * Append a 2-digit hex index
 */
static void patAppendIdx(struct pattern *p, int idx)
{
	patAppend(p, hexdigit[(idx >> 4) & 0xf]);
	patAppend(p, hexdigit[idx & 0xf]);
}

/*
 * Add an operand, return its index
 */
static int patAddOp(struct pattern *p, void *val)
{
	if (p->nops >= MAX_OPERANDS)
		return 0;
	p->ops[p->nops] = val;
	return p->nops++;
}

/*
 * Build a symbol reference: $<size><index>
 */
void patSym(struct pattern *p, char size, char *name)
{
	int idx = patAddOp(p, name ? name : "(null)");
	patAppend(p, '$');
	patAppend(p, size);
	patAppendIdx(p, idx);
}

/*
 * Build a constant: #<size><index>
 */
void patConst(struct pattern *p, char size, long val)
{
	int idx = patAddOp(p, (void *)val);
	patAppend(p, '#');
	patAppend(p, size);
	patAppendIdx(p, idx);
}

/*
 * Build a local variable reference: V<size><index>
 * Operand is the IY offset
 */
void patLocal(struct pattern *p, char size, int offset)
{
	int idx = patAddOp(p, (void *)(long)offset);
	patAppend(p, 'V');
	patAppend(p, size);
	patAppendIdx(p, idx);
}

/*
 * Build a register variable reference: R<size><index>
 * Operand is the register number (R_B, R_C, R_BC, R_IX)
 */
void patReg(struct pattern *p, char size, int reg)
{
	int idx = patAddOp(p, (void *)(long)reg);
	patAppend(p, 'R');
	patAppend(p, size);
	patAppendIdx(p, idx);
}

/*
 * Merge operands from child pattern into parent
 * Returns the index offset to add to child's operand references
 */
static int patMergeOps(struct pattern *p, struct pattern *child)
{
	int offset = p->nops;
	int i;

	for (i = 0; i < child->nops && p->nops < MAX_OPERANDS; i++)
		p->ops[p->nops++] = child->ops[i];

	return offset;
}

/*
 * Append child pattern string, adjusting operand indices
 */
static void patAppendChild(struct pattern *p, struct pattern *child, int offset)
{
	int i = 0;

	while (i < child->len) {
		char c = child->str[i++];
		patAppend(p, c);

		/* After $ # V R, copy size then adjust index */
		if (c == '$' || c == '#' || c == 'V' || c == 'R') {
			/* Copy size */
			if (i < child->len)
				patAppend(p, child->str[i++]);
			/* Read old index, write adjusted index */
			if (i + 1 < child->len) {
				int hi = child->str[i];
				int lo = child->str[i + 1];
				int idx;

				hi = (hi >= 'a') ? hi - 'a' + 10 : hi - '0';
				lo = (lo >= 'a') ? lo - 'a' + 10 : lo - '0';
				idx = (hi << 4) | lo;
				idx += offset;
				patAppendIdx(p, idx);
				i += 2;
			}
		}
	}
}

/*
 * Build a unary operator: <op><size><child>
 */
void patUnary(struct pattern *p, char op, char size, struct pattern *child)
{
	struct pattern result;
	int offset;

	patInit(&result);
	patAppend(&result, op);
	patAppend(&result, size);

	offset = patMergeOps(&result, child);
	patAppendChild(&result, child, offset);

	*p = result;
}

/*
 * Build a binary operator: <op><size><left><right>
 */
static int patBinCnt = 0;

void patBinary(struct pattern *p, char op, char size,
	       struct pattern *left, struct pattern *right)
{
	struct pattern result;
	int loff, roff;

	patBinCnt++;
	if ((patBinCnt % 100) == 0)
		fprintf(stderr, "patBinary: cnt=%d op=%c left=%d right=%d\n",
			patBinCnt, op, left->len, right->len);

	patInit(&result);
	patAppend(&result, op);
	patAppend(&result, size);

	loff = patMergeOps(&result, left);
	roff = patMergeOps(&result, right);

	patAppendChild(&result, left, loff);
	patAppendChild(&result, right, roff);

	*p = result;
}

/*
 * Build a call: @<retsize><nargs><func><args...>
 */
void patCall(struct pattern *p, char retsize, int nargs,
	     struct pattern *func, struct pattern *args[], int narg)
{
	struct pattern result;
	int foff, i;
	int argoffs[16];

	patInit(&result);
	patAppend(&result, '@');
	patAppend(&result, retsize);
	patAppendIdx(&result, nargs);

	foff = patMergeOps(&result, func);
	for (i = 0; i < narg && i < 16; i++)
		argoffs[i] = patMergeOps(&result, args[i]);

	patAppendChild(&result, func, foff);
	for (i = 0; i < narg && i < 16; i++)
		patAppendChild(&result, args[i], argoffs[i]);

	*p = result;
}

/*
 * Debug: print pattern
 */
void patPrint(struct pattern *p)
{
	int i;

	printf("pattern: \"%s\" (%d ops)\n", p->str, p->nops);
	for (i = 0; i < p->nops; i++) {
		printf("  [%02x] = %p\n", i, p->ops[i]);
	}
}

/*
 * Get operand by index from pattern string position
 * pos points to first hex digit of index
 */
void *patGetOp(struct pattern *p, char *pos)
{
	int hi = pos[0];
	int lo = pos[1];
	int idx;

	hi = (hi >= 'a') ? hi - 'a' + 10 : hi - '0';
	lo = (lo >= 'a') ? lo - 'a' + 10 : lo - '0';
	idx = (hi << 4) | lo;

	if (idx < p->nops)
		return p->ops[idx];
	return 0;
}

/*
 * Get symbol name from pattern at position
 */
char *patGetSym(struct pattern *p, char *pos)
{
	return (char *)patGetOp(p, pos);
}

/*
 * Get constant value from pattern at position
 */
long patGetConst(struct pattern *p, char *pos)
{
	return (long)patGetOp(p, pos);
}

/*
 * Get local offset from pattern at position
 */
int patGetLocal(struct pattern *p, char *pos)
{
	return (int)(long)patGetOp(p, pos);
}

/*
 * Get register from pattern at position
 */
int patGetReg(struct pattern *p, char *pos)
{
	return (int)(long)patGetOp(p, pos);
}

/*
 * Convert expression tree to pattern (recursive)
 */
static int patDepth = 0;

void patFromExpr(struct pattern *p, struct expr *e)
{
	struct pattern left, right;
	struct pattern func;
	struct pattern *args[16];
	int i;

	patDepth++;

	if (!e) {
		patInit(p);
		patDepth--;
		return;
	}

	if (patDepth > 100) {
		fprintf(stderr, "patFromExpr: depth %d, op=%c\n", patDepth, e->op);
	}

	switch (e->op) {
	case AST_CONST:	/* constant */
		patInit(p);
		patConst(p, e->type, e->v.l);
		break;

	case SYM:	/* global symbol */
		patInit(p);
		/* globals have type 0, use 'S' (pointer/address) as default */
		patSym(p, e->type ? e->type : 'S', e->sym);
		break;

	case LOCALVAR:	/* local variable */
		patInit(p);
		patLocal(p, e->type, e->offset);
		break;

	case REGVAR:	/* register variable */
		patInit(p);
		patReg(p, e->type, e->aux);
		break;

	case CALL:	/* function call */
		patFromExpr(&func, e->left);
		/* args are wrapped in ARGNODE nodes linked via right pointers */
		i = 0;
		{
			struct expr *arg = e->right;
			while (arg && arg->op == ARGNODE && i < 16) {
				args[i] = malloc(sizeof(struct pattern));
				patFromExpr(args[i], arg->left);
				i++;
				arg = arg->right;
			}
		}
		patCall(p, e->type ? e->type : 'v', e->aux, &func, args, i);
		while (--i >= 0)
			free(args[i]);
		break;

	case DEREF:	/* memory dereference (unary) */
	case BANG:	/* logical not */
	case TWIDDLE:	/* bitwise not */
	case NEG:	/* unary minus */
	case NARROW:	/* narrow */
	case WIDEN:	/* widen */
	case SEXT:	/* sign extend */
		patFromExpr(&left, e->left);
		patUnary(p, e->op, e->type, &left);
		break;

	case PREINC:	/* pre-increment */
	case POSTINC:	/* post-increment */
	case PREDEC:	/* pre-decrement */
	case POSTDEC:	/* post-decrement */
		/* encode as unary with increment in aux operand */
		patFromExpr(&left, e->left);
		patUnary(p, e->op, e->type, &left);
		break;

	default:	/* binary operators */
		patFromExpr(&left, e->left);
		patFromExpr(&right, e->right);
		patBinary(p, e->op, e->type, &left, &right);
		break;
	}
	patDepth--;
}

/*
 * Emit pattern as assembly comment with operand array
 */
void patEmitComment(struct pattern *p)
{
	char buf[MAX_PATTERN + 256];
	char *out = buf;
	int i;

	/* Build: pattern [op0, op1, ...] */
	out += sprintf(out, "%s [", p->str);
	for (i = 0; i < p->nops; i++) {
		if (i > 0)
			*out++ = ',';
		out += sprintf(out, "%lx", (unsigned long)p->ops[i]);
	}
	*out++ = ']';
	*out = '\0';

	emit("; PAT: %s", buf);
}
