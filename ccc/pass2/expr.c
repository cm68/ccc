/*
 * expr.c - expression tree builder and dumper
 */
#include "cc2.h"
#include "expr.h"
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
	e->op = AST_CONST;
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

void
setdest(Expr *e, char dest)
{
	e->dest = dest;
}

void
freeexpr(Expr *e)
{
	if (!e)
		return;
	freeexpr(e->left);
	freeexpr(e->right);
	if (e->op == SYM)
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
	case '_':
#ifdef DEBUG
		if (VERBOSE(V_EXPR))
			fprintf(stderr, "  NULL expr\n");
#endif
		return NULL;

	case AST_CONST:
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

static char *
opname(int op)
{
	switch (op) {
	case AST_CONST: return "CONST";
	case SYM:       return "SYM";
	case LOCALVAR:  return "LOCAL";
	case REGVAR:    return "REG";
	case DEREF:     return "DEREF";
	case ASSIGN:    return "ASSIGN";
	case PLUS:      return "ADD";
	case MINUS:     return "SUB";
	case TIMES:     return "MUL";
	case STAR:      return "MUL";
	case DIV:       return "DIV";
	case MOD:       return "MOD";
	case AND:       return "AND";
	case OR:        return "OR";
	case XOR:       return "XOR";
	case LSHIFT:    return "SHL";
	case RSHIFT:    return "SHR";
	case URSHIFT:   return "USHR";
	case LT:        return "LT";
	case GT:        return "GT";
	case EQ:        return "EQ";
	case NEQ:       return "NE";
	case LE:        return "LE";
	case GE:        return "GE";
	case LOR:       return "LOR";
	case LAND:      return "LAND";
	case BANG:      return "NOT";
	case NOT:       return "LNOT";
	case TWIDDLE:   return "BNOT";
	case NEG:       return "NEG";
	case NARROW:    return "NARROW";
	case WIDEN:     return "WIDEN";
	case SEXT:      return "SEXT";
	case PREINC:    return "PREINC";
	case POSTINC:   return "POSTINC";
	case PREDEC:    return "PREDEC";
	case POSTDEC:   return "POSTDEC";
	case CALL:      return "CALL";
	case QUES:      return "TERNARY";
	case TERNBRANCH:return "TERNBR";
	case COMMA:     return "COMMA";
	case BFEXTRACT: return "BFEXT";
	case BFASSIGN:  return "BFASS";
	case INITLIST:  return "INIT";
	case PLUSEQ:    return "ADDEQ";
	case SUBEQ:     return "SUBEQ";
	case MULTEQ:    return "MULEQ";
	case DIVEQ:     return "DIVEQ";
	case MODEQ:     return "MODEQ";
	case ANDEQ:     return "ANDEQ";
	case OREQ:      return "OREQ";
	case XOREQ:     return "XOREQ";
	case LSHIFTEQ:  return "SHLEQ";
	case RSHIFTEQ:  return "SHREQ";
	default:        return "???";
	}
}

static char *
destname(char dest)
{
	switch (dest) {
	case DEST_NONE:  return "none";
	case DEST_FLAGS: return "flag";
	case DEST_VALUE: return "val";
	default:         return "???";
	}
}

static void
dumpnode(Expr *e, int depth)
{
	int i;

	if (!e)
		return;

	for (i = 0; i < depth; i++)
		out("  ");

	out("; ");
	out(opname(e->op));
	outc('/');
	outc(e->width);
	out(" d=");
	out(destname(e->dest));

	switch (e->op) {
	case AST_CONST:
		out(" v=");
		outd(e->u.val);
		break;
	case SYM:
		out(" n=");
		out(e->u.name);
		break;
	case LOCALVAR:
		out(" off=");
		outd(e->u.var.off);
		break;
	case REGVAR:
		out(" r=");
		outd(e->u.var.reg);
		break;
	case CALL:
		out(" argc=");
		outd(e->u.call.argc);
		break;
	case PREINC:
	case POSTINC:
	case PREDEC:
	case POSTDEC:
		out(" amt=");
		outd(e->u.incdec.amt);
		break;
	case BFEXTRACT:
	case BFASSIGN:
		out(" o=");
		outd(e->u.bf.off);
		out(" w=");
		outd(e->u.bf.wid);
		break;
	}

	outc('\n');

	dumpnode(e->left, depth + 1);
	dumpnode(e->right, depth + 1);
}

void
dumpexpr(Expr *e)
{
	out("; --- expr ---\n");
	dumpnode(e, 0);
}

#endif

/* vim: set tabstop=4 shiftwidth=4 noexpandtab: */
