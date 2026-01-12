/*
 * parseast.c - AST parser with expression tree building
 */
#include "pass2.h"
#include "expr.h"
#include "../cpp/lexeme.h"
#include <stdio.h>
#include <string.h>

#ifdef DEBUG
#include "debug.h"

static char *
stmtname(int op)
{
	switch (op) {
	case AST_BLOCK: return "BLOCK";
	case IF: return "IF";
	case RETURN: return "RETURN";
	case LABEL: return "LABEL";
	case GOTO: return "GOTO";
	case SWITCH: return "SWITCH";
	case CASE: return "CASE";
	case DEFAULT: return "DEFAULT";
	case AST_FUNC: return "FUNC";
	default:  return "EXPR";
	}
}
#endif

static char buf[64];

/* Current function state */
static char funcname[16];	/* function name */
static short framesize;		/* bytes of local stack frame */
static unsigned char regsused;	/* bitmask of callee-save regs */
static short bcoff, ixoff;	/* IY-relative offsets for saved regs */

/* Param staging: move from stack to register */
#define MAXSTAGE 8
static struct {
	unsigned char reg;	/* target register */
	unsigned char off;	/* stack offset from IY */
	unsigned char width;	/* b/B/s/S = size */
} stage[MAXSTAGE];
static unsigned char nstage;

/* Register bitmasks for callee-save tracking */
#define REGBIT(r) (1 << (r))
#define USES_BC (REGBIT(R_B) | REGBIT(R_C) | REGBIT(R_BC))

static void
emitprolog(void)
{
	/* Emit function label: S prefix = static (one :), else global (::) */
	out(funcname);
	if (funcname[0] == 'S')
		out(":\n");
	else
		out("::\n");

	bcoff = ixoff = 0;

	{
		short off;
		/* Set up frame pointer */
		out("\tpush\tiy\n");
		out("\tld\tiy,0\n");
		out("\tadd\tiy,sp\n");
		/* Allocate locals if any */
		if (framesize > 0) {
			out("\tld\thl,-");
			outd(framesize);
			outc('\n');
			out("\tadd\thl,sp\n");
			out("\tld\tsp,hl\n");
		}
		/* Push callee-saves below locals, track offsets */
		off = -framesize;
		if (regsused & USES_BC) {
			out("\tpush\tbc\n");
			off -= 2;
			bcoff = off;
			if (bcoff < -128)
				out("\t.error frame too large for BC restore\n");
		}
		if (regsused & REGBIT(R_IX)) {
			out("\tpush\tix\n");
			off -= 2;
			ixoff = off;
			if (ixoff < -128)
				out("\t.error frame too large for IX restore\n");
		}
	}

	/* Stage params from stack to registers */
	while (nstage--) {
		unsigned char r = stage[nstage].reg;
		unsigned char off = stage[nstage].off;
		unsigned char w = stage[nstage].width;

		if (ISBYTE(w)) {
			/* Byte: ld r,(iy+off) */
			out("\tld\t");
			switch (r) {
			case R_B: outc('b'); break;
			case R_C: outc('c'); break;
			}
			out(",(iy+");
			outd(off);
			out(")\n");
		} else {
			/* Word: load low then high */
			switch (r) {
			case R_BC:
				out("\tld\tc,(iy+");
				outd(off);
				out(")\n\tld\tb,(iy+");
				outd(off + 1);
				out(")\n");
				break;
			case R_IX:
				out("\tld\tl,(iy+");
				outd(off);
				out(")\n\tld\th,(iy+");
				outd(off + 1);
				out(")\n\tpush\thl\n\tpop\tix\n");
				break;
			}
		}
	}
}

static void
emitepilog(void)
{
	/* Emit return label: Xfuncname (local, same length as func) */
	outc('X');
	out(funcname + 1);
	out(":\n");

	/* Restore callee-saves via IY-relative loads (preserves HL) */
	if (regsused & REGBIT(R_IX)) {
		out("\tld\te,(iy");
		outd(ixoff);
		out(")\n\tld\td,(iy");
		outd(ixoff + 1);
		out(")\n\tpush\tde\n\tpop\tix\n");
	}
	if (regsused & USES_BC) {
		out("\tld\tc,(iy");
		outd(bcoff);
		out(")\n\tld\tb,(iy");
		outd(bcoff + 1);
		out(")\n");
	}
	/* Restore frame pointer */
	out("\tld\tsp,iy\n");
	out("\tpop\tiy\n");

	out("\tret\n");
}

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
	case AST_BLOCK:
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
	case IF:
		n = read1();
#ifdef DEBUG
		if (VERBOSE(V_STMT))
			fprintf(stderr, "  IF nlbl=%d\n", n);
		out("; IF nlbl="); outd(n); outc('\n');
#endif
		e = readexpr();
		if (e) {
			setdest(e, DEST_FLAGS);
			e = rewrite(e);
#ifdef DEBUG
			dumpexpr(e);
#endif
			freeexpr(e);
		}
		parseStmt();
		if (read1())
			parseStmt();
		return;
	case RETURN:
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
				e = rewrite(e);
#ifdef DEBUG
				dumpexpr(e);
#endif
				freeexpr(e);
			}
		}
		/* Jump to function epilogue */
		out("\tjp\tX");
		out(funcname + 1);
		outc('\n');
		return;
	case LABEL:
		readS(buf);
#ifdef DEBUG
		if (VERBOSE(V_STMT))
			fprintf(stderr, "  LABEL %s\n", buf);
		out("; LABEL "); out(buf); outc('\n');
#endif
		return;
	case GOTO:
		readS(buf);
#ifdef DEBUG
		if (VERBOSE(V_STMT))
			fprintf(stderr, "  GOTO %s\n", buf);
		out("; GOTO "); out(buf); outc('\n');
#endif
		return;
	case SWITCH:
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
			e = rewrite(e);
#ifdef DEBUG
			dumpexpr(e);
#endif
			freeexpr(e);
		}
		for (i = 0; i < n; i++)
			parseStmt();
		return;
	case CASE:
		n = read1();
#ifdef DEBUG
		if (VERBOSE(V_STMT))
			fprintf(stderr, "  CASE n=%d\n", n);
		out("; CASE n="); outd(n); outc('\n');
#endif
		e = readexpr();
		e = rewrite(e);
		if (e) {
#ifdef DEBUG
			dumpexpr(e);
#endif
			freeexpr(e);
		}
		for (i = 0; i < n; i++)
			parseStmt();
		return;
	case DEFAULT:
		n = read1();
#ifdef DEBUG
		if (VERBOSE(V_STMT))
			fprintf(stderr, "  DEFAULT n=%d\n", n);
		out("; DEFAULT n="); outd(n); outc('\n');
#endif
		for (i = 0; i < n; i++)
			parseStmt();
		return;
	case SEMI:
		/* Empty statement (bare semicolon) - no-op */
#ifdef DEBUG
		if (VERBOSE(V_STMT))
			fprintf(stderr, "  SEMI (empty)\n");
		out("; SEMI\n");
#endif
		return;
	default:
		/* Expression statement - op byte is start of expression */
		unread1(op);
#ifdef DEBUG
		if (VERBOSE(V_STMT))
			fprintf(stderr, "  EXPR\n");
		out("; EXPR\n");
#endif
		e = readexpr();
		if (e) {
			setdest(e, DEST_NONE);
			e = rewrite(e);
#ifdef DEBUG
			dumpexpr(e);
#endif
			freeexpr(e);
		}
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
		case AST_FUNC:
			t = read1();
			readS(funcname);
#ifdef DEBUG
			if (VERBOSE(V_PARSE))
				fprintf(stderr, "parse: FUNC %s type=%c\n", funcname, t);
			out("; FUNC "); out(funcname); outc(':'); outc(t); outc('\n');
#endif
			n = read1();		/* param count */
			i = read1();		/* local count */
			framesize = read1();	/* frame size */
			regsused = 0;
			nstage = 0;
#ifdef DEBUG
			if (VERBOSE(V_PARSE))
				fprintf(stderr, "parse: params=%d locals=%d frame=%d\n",
					n, i, framesize);
			out("; params="); outd(n); out(" locals="); outd(i);
			out(" frame="); outd(framesize); outc('\n');
#endif
			/* Scan params: may need staging to registers */
			while (n--) {
				unsigned char reg, off;
				read1();	/* AST_DECL */
				t = read1();
				readS(buf);
				reg = read1();
				off = read1();
#ifdef DEBUG
				if (VERBOSE(V_PARSE))
					fprintf(stderr, "parse: param %s t=%c r=%d o=%d\n",
						buf, t, reg, off);
				out("; param "); out(buf); outc(':'); outc(t);
				out(" r="); outd(reg); out(" o="); outd(off); outc('\n');
#endif
				if (reg) {
					regsused |= REGBIT(reg);
					stage[nstage].reg = reg;
					stage[nstage].off = off;
					stage[nstage].width = t;
					nstage++;
				}
			}
			/* Scan locals: just track register usage */
			while (i--) {
				unsigned char reg;
				read1();	/* AST_DECL */
				t = read1();
				readS(buf);
				reg = read1();
				read1();	/* offset */
#ifdef DEBUG
				if (VERBOSE(V_PARSE))
					fprintf(stderr, "parse: local %s t=%c r=%d\n",
						buf, t, reg);
				out("; local "); out(buf); outc(':'); outc(t);
				out(" r="); outd(reg); outc('\n');
#endif
				if (reg)
					regsused |= REGBIT(reg);
			}
			emitprolog();
			parseStmt();
			emitepilog();
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
