/*
 * rewrite.c - table-driven expression tree rewriting
 *
 * Compact pattern language:
 *   Operators: + * - / % & | ^ < > D V L I N P _ 0 =
 *   Pattern:   op(left,right) or op(child) or op
 *   Examples:  L          matches LOCALVAR
 *              +(D(V),N)  matches PLUS(DEREF(REGVAR),NUMBER)
 *              *(_,P)     matches STAR(any,POW2)
 */
#include "pass2.h"
#include "expr.h"
#include "opcodes.h"
#include <stdlib.h>

#ifdef DEBUG
#include "debug.h"
#include <stdio.h>
#endif

/* Special pattern values */
#define P_ANY    0
#define P_NULL   255
#define P_NUM    254
#define P_POW2   253

/* Replacement flags */
#define RF_POW2  0x01    /* transform constant through log2 */
#define RF_IXIY  0x02    /* require reg is IX or IY */

/*
 * Map single char to opcode (or special pattern value)
 */
static unsigned char
chartopc(char c)
{
	switch (c) {
	case '+': return PLUS;
	case '*': return STAR;
	case '-': return MINUS;
	case '/': return DIV;
	case '%': return MOD;
	case '&': return AND;
	case '|': return OR;
	case '^': return XOR;
	case '<': return LSHIFT;
	case '>': return RSHIFT;
	case '=': return ASSIGN;
	case 'D': return DEREF;
	case 'V': return REGVAR;
	case 'L': return LOCALVAR;
	case 'I': return INDEX;
	case 'N': return P_NUM;
	case 'P': return P_POW2;
	case '_': return P_ANY;
	case '0': return P_NULL;
	}
	return P_ANY;
}

/*
 * Check if n is power of 2, return exponent or -1
 */
static int
ispow2(unsigned long n)
{
	int i;
	if (n == 0) return -1;
	for (i = 0; i < 32; i++)
		if (n == (1UL << i))
			return i;
	return -1;
}

/*
 * Match pattern byte against expression
 */
static int
opmatch(unsigned char pat, Expr *e)
{
	if (pat == P_ANY) return 1;
	if (pat == P_NULL) return e == NULL;
	if (pat == P_NUM) return e && e->op == NUMBER;
	if (pat == P_POW2) return e && e->op == NUMBER && ispow2(e->u.val) > 0;
	return e && e->op == pat;
}

/*
 * Parse and match pattern string against expression
 * Returns pointer past matched pattern, or NULL if no match
 * Pattern: op or op(left) or op(left,right)
 */
static char *
pmatch(char *p, Expr *e)
{
	unsigned char op;

	if (!p || !*p) return NULL;

	op = chartopc(*p++);
	if (!opmatch(op, e))
		return NULL;

	/* Check for children */
	if (*p == '(') {
		p++;
		/* Match left child */
		p = pmatch(p, e ? e->left : NULL);
		if (!p) return NULL;

		if (*p == ',') {
			p++;
			/* Match right child */
			p = pmatch(p, e ? e->right : NULL);
			if (!p) return NULL;
		}
		if (*p != ')') return NULL;
		p++;
	}
	return p;
}

/*
 * Get node by path string: L=left, R=right, LL=left->left, etc.
 */
static Expr *
getpath(Expr *e, char **pp)
{
	char *p = *pp;
	while (*p == 'L' || *p == 'R') {
		if (!e) break;
		if (*p == 'L') e = e->left;
		else e = e->right;
		p++;
	}
	*pp = p;
	return e;
}

/*
 * Rewrite rule
 */
struct rule {
	char *pat;      /* pattern string */
	char *rep;      /* replacement: I=INDEX, <=LSHIFT, etc */
	char *lsrc;     /* left child source path */
	char *rsrc;     /* right child source path */
	char *dsrc;     /* data source path (for reg/off) */
	unsigned char flags;
};

static struct rule rules[] = {
	/* LOCALVAR -> INDEX */
	{"L", "I", "", "", "", 0},

	/* PLUS(DEREF(REGVAR), NUM) -> INDEX [normalized: const on right] */
	{"+(D(V),N)", "I", "", "", "LL", RF_IXIY},

	/* STAR(any, POW2) -> LSHIFT [normalized: const on right] */
	{"*(_,P)", "<", "L", "R", "", RF_POW2},

	{NULL, NULL, NULL, NULL, NULL, 0}
};

/*
 * Try to apply a rule
 */
static Expr *
tryrule(struct rule *rp, Expr *e)
{
	Expr *n, *src, *num, *lc, *rc;
	char *p;
	char reg, off;
	int shift;
	unsigned char newop;

	/* Match pattern */
	if (!pmatch(rp->pat, e))
		return NULL;

	/* Check IX/IY constraint */
	if (rp->flags & RF_IXIY) {
		p = rp->dsrc;
		src = getpath(e, &p);
		if (!src || (src->u.var.reg != R_IX && src->u.var.reg != R_IY))
			return NULL;
	}

#ifdef DEBUG
	if (VERBOSE(V_REWRITE))
		fprintf(stderr, "rewrite: %s -> %s\n", rp->pat, rp->rep);
#endif

	newop = chartopc(rp->rep[0]);

	/* Get replacement children */
	p = rp->lsrc;
	lc = (*p) ? getpath(e, &p) : NULL;
	p = rp->rsrc;
	rc = (*p) ? getpath(e, &p) : NULL;

	/* Handle INDEX specially */
	if (newop == INDEX) {
		if (e->op == LOCALVAR) {
			reg = e->u.var.reg ? e->u.var.reg : R_IY;
			off = e->u.var.off;
		} else {
			p = rp->dsrc;
			src = getpath(e, &p);
			reg = src ? src->u.var.reg : R_IY;
			num = e->right;
			off = num ? (char)num->u.val : 0;
		}
		n = mkindex(e->width, reg, off);
		freeexpr(e);
		return n;
	}

	/* Reuse node, change op */
	e->op = newop;
	if (lc != e->left || rc != e->right) {
		/* Detach children we're keeping */
		if (lc == e->left) e->left = NULL;
		if (lc == e->right) e->right = NULL;
		if (rc == e->left) e->left = NULL;
		if (rc == e->right) e->right = NULL;
		/* Free unused subtrees */
		freeexpr(e->left);
		freeexpr(e->right);
		e->left = lc;
		e->right = rc;
	}

	/* Transform POW2 to shift amount */
	if ((rp->flags & RF_POW2) && e->right && e->right->op == NUMBER) {
		shift = ispow2(e->right->u.val);
		if (shift > 0)
			e->right->u.val = shift;
	}

	return e;
}

/*
 * Check if op is commutative
 */
static int
iscommut(unsigned char op)
{
	switch (op) {
	case PLUS: case STAR: case AND: case OR: case XOR:
	case EQ: case NEQ: case LAND: case LOR:
		return 1;
	}
	return 0;
}

/*
 * Normalize: put constants on right for commutative ops
 */
static void
normalize(Expr *e)
{
	Expr *t;
	if (!e || !e->left || !e->right) return;
	if (!iscommut(e->op)) return;
	if (e->left->op == NUMBER && e->right->op != NUMBER) {
		t = e->left;
		e->left = e->right;
		e->right = t;
	}
}

/*
 * Rewrite single node
 */
static Expr *
rewrite1(Expr *e)
{
	struct rule *rp;
	Expr *n;

	if (!e) return NULL;

	e->left = rewrite1(e->left);
	e->right = rewrite1(e->right);

	normalize(e);

	for (rp = rules; rp->pat; rp++) {
		n = tryrule(rp, e);
		if (n) return n;
	}
	return e;
}

/*
 * Public entry point
 */
Expr *
rewrite(Expr *e)
{
	Expr *r;
#ifdef DEBUG
	if (VERBOSE(V_REWRITE)) {
		out("; --- before rewrite ---\n");
		dumpexpr(e);
	}
#endif
	r = rewrite1(e);
#ifdef DEBUG
	if (VERBOSE(V_REWRITE)) {
		out("; --- after rewrite ---\n");
		dumpexpr(r);
	}
#endif
	return r;
}

/* vim: set tabstop=4 shiftwidth=4 noexpandtab: */
