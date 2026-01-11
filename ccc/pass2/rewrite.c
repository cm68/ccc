/*
 * rewrite.c - expression tree rewriting
 *
 * Bottom-up tree transformation to simplify patterns before codegen.
 */
#include "pass2.h"
#include "expr.h"
#include "opcodes.h"
#include <stdlib.h>

#ifdef DEBUG
#include "debug.h"
#include <stdio.h>
#endif

static int depth;

static Expr *rewrite1(Expr *e);

/*
 * rewrite - transform expression tree bottom-up
 *
 * Current transformations:
 *   LOCALVAR(reg, off) → INDEX(reg, off)
 *   ADD(DEREF(REGVAR reg), const) → INDEX(reg, const)  [reg = IX or IY]
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
	depth = 0;
	r = rewrite1(e);
#ifdef DEBUG
	if (VERBOSE(V_REWRITE)) {
		out("; --- after rewrite ---\n");
		dumpexpr(r);
	}
#endif
	return r;
}

static Expr *
rewrite1(Expr *e)
{
	Expr *l, *r, *n;

	if (!e)
		return NULL;

	/* rewrite children first (bottom-up) */
	depth++;
	e->left = rewrite1(e->left);
	e->right = rewrite1(e->right);
	depth--;

	l = e->left;
	r = e->right;

	switch (e->op) {
	case LOCALVAR:
		/* LOCALVAR → INDEX */
		n = mkindex(e->width, e->u.var.reg ? e->u.var.reg : R_IY,
		            e->u.var.off);
		free(e);
#ifdef DEBUG
		if (VERBOSE(V_REWRITE))
			fprintf(stderr, "rewrite: LOCALVAR -> INDEX\n");
#endif
		return n;

	case PLUS:
		/* ADD(DEREF(REGVAR reg), const) → INDEX(reg, const) */
		if (l && l->op == DEREF &&
		    l->left && l->left->op == REGVAR &&
		    (l->left->u.var.reg == R_IX || l->left->u.var.reg == R_IY) &&
		    r && r->op == NUMBER) {
			n = mkindex(e->width, l->left->u.var.reg, (char)r->u.val);
			freeexpr(e);
#ifdef DEBUG
			if (VERBOSE(V_REWRITE))
				fprintf(stderr, "rewrite: ADD(DEREF(REGVAR),const) -> INDEX\n");
#endif
			return n;
		}
		/* ADD(const, DEREF(REGVAR reg)) → INDEX(reg, const) */
		if (l && l->op == NUMBER &&
		    r && r->op == DEREF &&
		    r->left && r->left->op == REGVAR &&
		    (r->left->u.var.reg == R_IX || r->left->u.var.reg == R_IY)) {
			n = mkindex(e->width, r->left->u.var.reg, (char)l->u.val);
			freeexpr(e);
#ifdef DEBUG
			if (VERBOSE(V_REWRITE))
				fprintf(stderr, "rewrite: ADD(const,DEREF(REGVAR)) -> INDEX\n");
#endif
			return n;
		}
		break;
	}

	return e;
}

/* vim: set tabstop=4 shiftwidth=4 noexpandtab: */
