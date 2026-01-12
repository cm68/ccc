/*
 * expr.c - expression tree builder and dumper
 */
#include "pass2.h"
#include "expr.h"
#include "opcodes.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#ifdef DEBUG
#include "debug.h"
#endif

static Expr *
alloc(void)
{
	Expr *e = malloc(sizeof(Expr));
	if (!e) {
		out("!OOM\n");
		exit(1);
	}
	e->op = 0;
	e->width = 0;
	e->dest = DEST_NONE;
	e->left = NULL;
	e->right = NULL;
	e->u.val = 0;
	return e;
}

Expr *
mkconst(char width, long val)
{
	Expr *e = alloc();
	e->op = NUMBER;
	e->width = width;
	e->u.val = val;
	return e;
}

Expr *
mksym(char *name)
{
	Expr *e = alloc();
	e->op = SYM;
	e->width = 'p';
	e->u.name = strdup(name);
	return e;
}

Expr *
mklocalvar(char width, char reg, char off)
{
	Expr *e = alloc();
	e->op = LOCALVAR;
	e->width = width;
	e->u.var.reg = reg;
	e->u.var.off = off;
	return e;
}

Expr *
mkregvar(char width, char reg)
{
	Expr *e = alloc();
	e->op = REGVAR;
	e->width = width;
	e->u.var.reg = reg;
	return e;
}

Expr *
mkindex(char width, char reg, char off)
{
	Expr *e = alloc();
	e->op = INDEX;
	e->width = width;
	e->u.var.reg = reg;
	e->u.var.off = off;
	return e;
}

Expr *
mkinhl(char width, Expr *child)
{
	Expr *e = alloc();
	e->op = INHL;
	e->width = width;
	e->left = child;
	return e;
}

Expr *
mkinde(char width, Expr *child)
{
	Expr *e = alloc();
	e->op = INDE;
	e->width = width;
	e->left = child;
	return e;
}

Expr *
mkina(char width, Expr *child)
{
	Expr *e = alloc();
	e->op = INA;
	e->width = width;
	e->left = child;
	return e;
}

Expr *
mkunary(int op, char width, Expr *child)
{
	Expr *e = alloc();
	e->op = op;
	e->width = width;
	e->left = child;
	return e;
}

Expr *
mkbinary(int op, char width, Expr *left, Expr *right)
{
	Expr *e = alloc();
	e->op = op;
	e->width = width;
	e->left = left;
	e->right = right;
	return e;
}

Expr *
mkcall(char width, int argc, Expr *func, Expr *args)
{
	Expr *e = alloc();
	e->op = CALL;
	e->width = width;
	e->u.call.argc = argc;
	e->left = func;
	e->right = args;
	return e;
}

Expr *
mkincdec(int op, char width, Expr *child, int amt)
{
	Expr *e = alloc();
	e->op = op;
	e->width = width;
	e->left = child;
	e->u.incdec.amt = amt;
	return e;
}

Expr *
mkbfext(char off, char wid, Expr *addr)
{
	Expr *e = alloc();
	e->op = BFEXTRACT;
	e->width = 's';
	e->u.bf.off = off;
	e->u.bf.wid = wid;
	e->left = addr;
	return e;
}

Expr *
mkbfass(char off, char wid, Expr *addr, Expr *val)
{
	Expr *e = alloc();
	e->op = BFASSIGN;
	e->width = 's';
	e->u.bf.off = off;
	e->u.bf.wid = wid;
	e->left = addr;
	e->right = val;
	return e;
}

Expr *
mksymref(char *name, short off)
{
	Expr *e = alloc();
	e->op = SYMREF;
	e->width = 'p';
	e->u.symref.name = strdup(name);
	e->u.symref.off = off;
	return e;
}

Expr *
mkcode(char width, char reg)
{
	Expr *e = alloc();
	e->op = CODE;
	e->width = width;
	e->u.var.reg = reg;
	return e;
}

void
setdest(Expr *e, char dest)
{
	if (!e) return;
	e->dest = dest;

	/* Propagate flag context to logical/comparison ops */
	if (dest == DEST_FLAGS) {
		switch (e->op) {
		case LAND: case LOR:
			setdest(e->left, DEST_FLAGS);
			setdest(e->right, DEST_FLAGS);
			break;
		case BANG:
			setdest(e->left, DEST_FLAGS);
			break;
		}
	}
}

Expr *
dupexpr(Expr *e)
{
	Expr *n;
	if (!e)
		return NULL;
	n = malloc(sizeof(Expr));
	if (!n) {
		out("!OOM\n");
		exit(1);
	}
	*n = *e;
	n->left = dupexpr(e->left);
	n->right = dupexpr(e->right);
	if (e->op == SYM || e->op == SYMREF)
		n->u.name = strdup(e->u.name);
	return n;
}

void
freeexpr(Expr *e)
{
	if (!e)
		return;
	freeexpr(e->left);
	freeexpr(e->right);
	if (e->op == SYM || e->op == SYMREF)
		free(e->u.name);
	free(e);
}

/* check if op is binary */
static int
isbinary(int op)
{
	switch (op) {
	case PLUS: case MINUS: case TIMES: case STAR:
	case DIV: case MOD: case AND: case OR: case XOR:
	case LSHIFT: case RSHIFT: case URSHIFT:
	case EQ: case NEQ: case LT: case LE: case GT: case GE:
	case LAND: case LOR: case ASSIGN:
	case PLUSEQ: case SUBEQ: case MULTEQ: case DIVEQ: case MODEQ:
	case ANDEQ: case OREQ: case XOREQ: case LSHIFTEQ: case RSHIFTEQ:
	case COMMA:
		return 1;
	}
	return 0;
}

Expr *
readexpr(void)
{
	static char buf[64];
	unsigned char op, t, n, i;
	unsigned long v;
	Expr *e, *args, *arg;

	op = read1();

#ifdef DEBUG
	if (VERBOSE(V_EXPR))
		fprintf(stderr, "expr: op=%d '%c'\n", op, op);
#endif

	switch (op) {
	case AST_EMPTY:
#ifdef DEBUG
		if (VERBOSE(V_EXPR))
			fprintf(stderr, "  NULL expr\n");
#endif
		return NULL;

	case NUMBER:
		t = read1();
		v = read4();
#ifdef DEBUG
		if (VERBOSE(V_EXPR))
			fprintf(stderr, "  CONST type=%c val=%lu\n", t, v);
#endif
		return mkconst(t, (long)v);

	case SYM:
		readS(buf);
#ifdef DEBUG
		if (VERBOSE(V_EXPR))
			fprintf(stderr, "  SYM %s\n", buf);
#endif
		return mksym(buf);

	case LOCALVAR:
		t = read1();
		n = read1();
#ifdef DEBUG
		if (VERBOSE(V_EXPR))
			fprintf(stderr, "  LOCALVAR type=%c off=%d\n", t, n);
#endif
		return mklocalvar(t, 0, n);

	case REGVAR:
		t = read1();
		n = read1();
#ifdef DEBUG
		if (VERBOSE(V_EXPR))
			fprintf(stderr, "  REGVAR type=%c reg=%d\n", t, n);
#endif
		return mkregvar(t, n);

	case CALL:
		t = read1();
		n = read1();
#ifdef DEBUG
		if (VERBOSE(V_EXPR))
			fprintf(stderr, "  CALL type=%c argc=%d\n", t, n);
#endif
		e = readexpr();
		args = NULL;
		for (i = 0; i < n; i++) {
			arg = readexpr();
			if (!args)
				args = arg;
			else
				args = mkbinary(COMMA, arg->width, args, arg);
		}
		return mkcall(t, n, e, args);

	case PREINC:
	case POSTINC:
	case PREDEC:
	case POSTDEC:
		t = read1();
		e = readexpr();
		v = read2();
#ifdef DEBUG
		if (VERBOSE(V_EXPR))
			fprintf(stderr, "  INCDEC op=%d type=%c amt=%lu\n", op, t, v);
#endif
		return mkincdec(op, t, e, v);

	case BFEXTRACT:
		t = read1();
		n = read1();
#ifdef DEBUG
		if (VERBOSE(V_EXPR))
			fprintf(stderr, "  BFEXTRACT off=%d wid=%d\n", t, n);
#endif
		e = readexpr();
		return mkbfext(t, n, e);

	case BFASSIGN:
		t = read1();
		n = read1();
#ifdef DEBUG
		if (VERBOSE(V_EXPR))
			fprintf(stderr, "  BFASSIGN off=%d wid=%d\n", t, n);
#endif
		e = readexpr();
		return mkbfass(t, n, e, readexpr());

	case QUES:
		t = read1();
#ifdef DEBUG
		if (VERBOSE(V_EXPR))
			fprintf(stderr, "  TERNARY type=%c\n", t);
#endif
		e = alloc();
		e->op = QUES;
		e->width = t;
		e->left = readexpr();
		e->right = mkbinary(TERNBRANCH, t, readexpr(), readexpr());
		return e;

	case BEGIN:
		n = read1();
#ifdef DEBUG
		if (VERBOSE(V_EXPR))
			fprintf(stderr, "  INITLIST (BEGIN) n=%d\n", n);
#endif
		e = alloc();
		e->op = INITLIST;
		e->width = 's';
		args = NULL;
		for (i = 0; i < n; i++) {
			arg = readexpr();
			if (!args)
				args = arg;
			else
				args = mkbinary(COMMA, 's', args, arg);
		}
		e->left = args;
		read1();
		return e;

	case LBRACK:
		t = read1();
		n = read1();
#ifdef DEBUG
		if (VERBOSE(V_EXPR))
			fprintf(stderr, "  INITLIST (LBRACK) type=%c n=%d\n", t, n);
#endif
		e = alloc();
		e->op = INITLIST;
		e->width = t;
		args = NULL;
		for (i = 0; i < n; i++) {
			arg = readexpr();
			if (!args)
				args = arg;
			else
				args = mkbinary(COMMA, t, args, arg);
		}
		e->left = args;
		read1();
		return e;

	default:
		t = read1();
#ifdef DEBUG
		if (VERBOSE(V_EXPR))
			fprintf(stderr, "  OP %d type=%c binary=%d\n", op, t, isbinary(op));
#endif
		e = readexpr();
		if (isbinary(op))
			return mkbinary(op, t, e, readexpr());
		return mkunary(op, t, e);
	}
}

#ifdef DEBUG

#include "../format.h"

static char *
destName(int d)
{
	switch (d) {
	case DEST_FLAGS: return "/f";
	case DEST_VALUE: return "/v";
	default: return "";
	}
}

static void
dumpnode(Expr *e)
{
	char buf[64];

	if (!e) {
		out("_");
		return;
	}

	switch (e->op) {
	case NUMBER:
		sprintf(buf, "%ld:%s", e->u.val, widthName(e->width));
		out(buf);
		return;
	case SYM:
		out("$");
		out(e->u.name);
		return;
	case LOCALVAR:
		sprintf(buf, "(LOCALVAR:%s %s%+d)", widthName(e->width),
		        regName(e->u.var.reg ? e->u.var.reg : R_IY),
		        (int)(signed char)e->u.var.off);
		out(buf);
		return;
	case REGVAR:
		sprintf(buf, "(REGVAR:%s %s)", widthName(e->width),
		        regName(e->u.var.reg));
		out(buf);
		return;
	case INDEX:
		sprintf(buf, "(INDEX:%s %s%+d)", widthName(e->width),
		        regName(e->u.var.reg),
		        (int)(signed char)e->u.var.off);
		out(buf);
		return;
	case CALL:
		sprintf(buf, "(CALL:%s/%d ", widthName(e->width), e->u.call.argc);
		out(buf);
		dumpnode(e->left);
		/* args are in right as COMMA chain */
		if (e->right) {
			out(" ");
			dumpnode(e->right);
		}
		out(")");
		return;
	case PREINC:
	case POSTINC:
	case PREDEC:
	case POSTDEC:
		sprintf(buf, "(%s:%s/%d ", opName(e->op), widthName(e->width),
		        e->u.incdec.amt);
		out(buf);
		dumpnode(e->left);
		out(")");
		return;
	case BFEXTRACT:
		sprintf(buf, "(BFEXT %d:%d ", e->u.bf.off, e->u.bf.wid);
		out(buf);
		dumpnode(e->left);
		out(")");
		return;
	case BFASSIGN:
		sprintf(buf, "(BFSET %d:%d ", e->u.bf.off, e->u.bf.wid);
		out(buf);
		dumpnode(e->left);
		out(" ");
		dumpnode(e->right);
		out(")");
		return;
	case SYMREF:
		sprintf(buf, "(SYMREF %s%+d)", e->u.symref.name, e->u.symref.off);
		out(buf);
		return;
	case CODE:
		sprintf(buf, "(CODE:%s%s @%s)", widthName(e->width),
		        destName(e->dest), regName(e->u.var.reg));
		out(buf);
		return;
	case QUES:
		out("(?:");
		out(widthName(e->width));
		out(" ");
		dumpnode(e->left);
		out(" ");
		dumpnode(e->right->left);
		out(" ");
		dumpnode(e->right->right);
		out(")");
		return;
	}

	/* regular unary/binary ops */
	out("(");
	out(opName(e->op));
	out(":");
	out(widthName(e->width));
	out(destName(e->dest));
	out(" ");
	dumpnode(e->left);
	if (e->right) {
		out(" ");
		dumpnode(e->right);
	}
	out(")");
}

void
dumpexpr(Expr *e)
{
	out("; ");
	dumpnode(e);
	out("\n");
}

#endif

/* vim: set tabstop=4 shiftwidth=4 noexpandtab: */
