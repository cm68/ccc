/*
 * parseast.c - AST parser with expression tree building
 */
#include "pass2.h"
#include "expr.h"
#include <stdio.h>

#ifdef DEBUG
#include "debug.h"

static char *
stmtname(int op)
{
	switch (op) {
	case 'B': return "BLOCK";
	case 'I': return "IF";
	case 'R': return "RETURN";
	case 'E': return "EXPR";
	case 'L': return "LABEL";
	case 'G': return "GOTO";
	case 'S': return "SWITCH";
	case 'C': return "CASE";
	case 'O': return "DEFAULT";
	case 'F': return "FUNC";
	default:  return "???";
	}
}
#endif

static char buf[64];

static void
parseStmt(void)
{
	unsigned char op = read1();
	unsigned char n, i;
	Expr *e;

#ifdef DEBUG
	if (VERBOSE(V_STMT))
		fprintf(stderr, "stmt op=%s\n", stmtname(op));
	out("; stmt "); out(stmtname(op)); outc('\n');
#endif
	switch (op) {
	case 'B':
		read1();
		n = read1();
#ifdef DEBUG
		if (VERBOSE(V_STMT))
			fprintf(stderr, "  BLOCK n=%d\n", n);
		out("; BLOCK n="); outd(n); outc('\n');
#endif
		for (i = 0; i < n; i++)
			parseStmt();
		return;
	case 'I':
		n = read1();
#ifdef DEBUG
		if (VERBOSE(V_STMT))
			fprintf(stderr, "  IF nlbl=%d\n", n);
		out("; IF nlbl="); outd(n); outc('\n');
#endif
		e = readexpr();
		if (e) {
			setdest(e, DEST_FLAGS);
#ifdef DEBUG
			dumpexpr(e);
#endif
			freeexpr(e);
		}
		parseStmt();
		if (read1())
			parseStmt();
		return;
	case 'R':
		n = read1();
#ifdef DEBUG
		if (VERBOSE(V_STMT))
			fprintf(stderr, "  RETURN hasval=%d\n", n);
		out("; RETURN hasval="); outd(n); outc('\n');
#endif
		if (n) {
			e = readexpr();
			if (e) {
				setdest(e, DEST_VALUE);
#ifdef DEBUG
				dumpexpr(e);
#endif
				freeexpr(e);
			}
		}
		return;
	case 'E':
#ifdef DEBUG
		if (VERBOSE(V_STMT))
			fprintf(stderr, "  EXPR\n");
		out("; EXPR\n");
#endif
		e = readexpr();
		if (e) {
			setdest(e, DEST_NONE);
#ifdef DEBUG
			dumpexpr(e);
#endif
			freeexpr(e);
		}
		return;
	case 'L':
		readS(buf);
#ifdef DEBUG
		if (VERBOSE(V_STMT))
			fprintf(stderr, "  LABEL %s\n", buf);
		out("; LABEL "); out(buf); outc('\n');
#endif
		return;
	case 'G':
		readS(buf);
#ifdef DEBUG
		if (VERBOSE(V_STMT))
			fprintf(stderr, "  GOTO %s\n", buf);
		out("; GOTO "); out(buf); outc('\n');
#endif
		return;
	case 'S':
		read1();
		n = read1();
#ifdef DEBUG
		if (VERBOSE(V_STMT))
			fprintf(stderr, "  SWITCH n=%d\n", n);
		out("; SWITCH n="); outd(n); outc('\n');
#endif
		e = readexpr();
		if (e) {
			setdest(e, DEST_VALUE);
#ifdef DEBUG
			dumpexpr(e);
#endif
			freeexpr(e);
		}
		for (i = 0; i < n; i++)
			parseStmt();
		return;
	case 'C':
		n = read1();
#ifdef DEBUG
		if (VERBOSE(V_STMT))
			fprintf(stderr, "  CASE n=%d\n", n);
		out("; CASE n="); outd(n); outc('\n');
#endif
		e = readexpr();
		if (e) {
#ifdef DEBUG
			dumpexpr(e);
#endif
			freeexpr(e);
		}
		for (i = 0; i < n; i++)
			parseStmt();
		return;
	case 'O':
		n = read1();
#ifdef DEBUG
		if (VERBOSE(V_STMT))
			fprintf(stderr, "  DEFAULT n=%d\n", n);
		out("; DEFAULT n="); outd(n); outc('\n');
#endif
		for (i = 0; i < n; i++)
			parseStmt();
		return;
	}
}

void
parse(void)
{
	unsigned char op, t, n, i;

#ifdef DEBUG
	if (VERBOSE(V_PARSE))
		fprintf(stderr, "parse: starting\n");
#endif
	while ((op = read1()) != E_O_F) {
#ifdef DEBUG
		if (VERBOSE(V_PARSE))
			fprintf(stderr, "parse: top op=%s\n", stmtname(op));
		out("; top "); out(stmtname(op)); outc('\n');
#endif
		switch (op) {
		case 'F':
			t = read1();
			readS(buf);
#ifdef DEBUG
			if (VERBOSE(V_PARSE))
				fprintf(stderr, "parse: FUNC %s type=%c\n", buf, t);
			out("; FUNC "); out(buf); outc(':'); outc(t); outc('\n');
#endif
			n = read1();
			i = read1();
			read1();
#ifdef DEBUG
			if (VERBOSE(V_PARSE))
				fprintf(stderr, "parse: params=%d locals=%d\n", n, i);
			out("; params="); outd(n); out(" locals="); outd(i); outc('\n');
#endif
			n += i;
			while (n--) {
				read1();
				t = read1();
				readS(buf);
#ifdef DEBUG
				if (VERBOSE(V_PARSE))
					fprintf(stderr, "parse: decl %s type=%c\n", buf, t);
				out("; decl "); out(buf); outc(':'); outc(t); outc('\n');
#endif
				read1();
				read1();
			}
			parseStmt();
			break;
		}
	}
#ifdef DEBUG
	if (VERBOSE(V_PARSE))
		fprintf(stderr, "parse: EOF\n");
	out("; EOF\n");
#endif
}

/* vim: set tabstop=4 shiftwidth=4 noexpandtab: */
