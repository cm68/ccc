/*
 * parseast.c - AST parser with expression tree building
 */
#include "pass2.h"
#include "expr.h"
#include "opcodes.h"
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
	case ASM: return "ASM";
	case AST_FUNC: return "FUNC";
	default:  return "EXPR";
	}
}
#endif

static char buf[64];

/* Label generation */
static int labelcnt;		/* per-function label counter */
static int fnindex;		/* function index for unique labels */

/* Current function state */
/*
 * Function name, as it goes into the assembly - so with the leading
 * underscore, which pass1 has already added.  A 15-character C
 * identifier becomes 16 here, and 16 is what the old size held with no
 * room for the terminator.  The object format's own limit is the
 * assembler's to complain about, and it does.
 */
static char funcname[20];
static short framesize;		/* bytes of local stack frame */
static short savebase;		/* scalar area size: save slots below it */
static unsigned char regsused;	/* bitmask of callee-save regs */
static short bcoff, ixoff;	/* IY-relative offsets for saved regs */

/*
 * Switch dispatch.
 *
 * The stream is sequential - SWITCH, then each CASE with its value and
 * its body - so the values are not all known before the first body has
 * to be emitted.  So the control value is worked out, the bodies are
 * jumped over, and the comparisons go after them, where every case
 * label is known.  Nothing falls into that block: the last body jumps
 * past it, and the bodies were jumped over rather than run, so the
 * control value is still in the register when the comparisons are
 * reached.
 *
 * Case values are bytes in this compiler, so the chain is a cp against
 * A.  A control expression need not be one - a state machine over an
 * int is the usual shape - so a word control tests its high byte once
 * before comparing the low one, and any value that does not fit a byte
 * cannot match and goes to the default.
 */
#define MAXSWNEST 8		/* nested switches */
/*
 * Case values are bytes, so a switch cannot have more than 256 of
 * them and this cannot overflow.  wsnm's disassembler is the one that
 * goes anywhere near it.
 */
#define MAXSWCASE 256
/*
 * The case values live OUTSIDE the context struct, on purpose: a
 * struct type's size is a byte in pass1, so "long val[256]" inside
 * the struct silently contributed nothing and swstk came out 48
 * bytes for what should have been 8K - every case value recorded
 * past the tenth stomped the statics that follow, which is how the
 * twelfth case of any switch stopped existing in the self-hosted
 * build.  A bare array is sized on the wide path and survives; the
 * struct keeps a pointer into it.
 */
static struct swctx {
	int id;			/* label number for this switch */
	int ncase;
	int hasdef;
	unsigned char *val;	/* this nesting level's slice of swvals */
} swstk[MAXSWNEST];
/*
 * Bytes, not longs: case values are bytes in this compiler - the
 * dispatch masks to eight bits anyway - and the long version of
 * this pool, once it actually existed, was 8K of bss that left the
 * self-hosted c1 under a thousand bytes of heap.
 */
static unsigned char swvals[MAXSWNEST * MAXSWCASE];
static int swtop;

/*
 * _Kn_f_m: case m of switch n in function f.  _Dn_f: the dispatch,
 * _Nn_f: nothing matched, _Fn_f: the default, _Xn_f: past it all.
 *
 * The function index is not decoration.  labelcnt starts again at zero
 * in every function, so without it the first switch of one function
 * and the first of the next are both _D0.
 */
static void
swlabel(char k, int id, int n)
{
	outf("_%c%d_%d", k, id, fnindex);
	if (n >= 0)
		outf("_%d", n);
}

/*
 * Emit the dispatch for a switch whose cases have all been seen.
 *
 * Three shapes, and which is smallest is a matter of counting rather
 * than taste.  With n cases spanning span values:
 *
 *	chain	5n		cp v / jp z,L per case
 *	swtab	4 + 3n		call, count byte, value + label per case
 *	swidx	5 + 2*span	call, lo, span, a label per slot
 *
 * So the chain wins to n=2, and above that it is swidx when the values
 * are dense enough to beat the pair table - 2*span < 3n-1, which is a
 * little over two thirds - and swtab when they are not.  Over the
 * tree's own 85 switches that is 4175 bytes of dispatch down to 2705.
 *
 * The two live at opposite ends and both are needed: every switch with
 * more than about twenty cases here is sparse (the largest, 125 cases
 * in wsnm.c, spans the whole byte at 48%) and would want a 517 byte
 * index against a 379 byte pair table, while the dense ones are nearly
 * all small - and for those swidx is not just smaller but constant
 * time instead of a scan.
 *
 * A count or span of 256 would store as a zero byte, so anything that
 * large stays on the chain.  MAXSWCASE bounds it at 256 and a switch
 * that big is theoretical; correctness is worth more than the bytes.
 */
static void
swdispatch(struct swctx *sw)
{
	int i, j, n, lo, hi, span;

	n = sw->ncase;
	if (n == 0)
		return;

	lo = hi = (int)(sw->val[0] & 0xff);
	for (i = 1; i < n; i++) {
		j = (int)(sw->val[i] & 0xff);
		if (j < lo) lo = j;
		if (j > hi) hi = j;
	}
	span = hi - lo + 1;

	if (n >= 3 && span <= 255 && 2 * span < 3 * n - 1) {
		/* dense: bias and index, gaps pointing at no-match */
		outf("\tcall swidx\n\t.db %d\n\t.db %d\n", lo, span);
		for (i = 0; i < span; i++) {
			out("\t.dw ");
			for (j = 0; j < n; j++)
				if ((int)(sw->val[j] & 0xff) == lo + i)
					break;
			if (j < n)
				swlabel('K', sw->id, j);
			else
				swlabel('N', sw->id, -1);
			outc('\n');
		}
		return;
	}
	if (n >= 3 && n <= 255) {
		/*
		 * Sparse: values together so the scan is one cpir, labels
		 * after them and backwards, which is what lets the helper
		 * find the slot from what cpir leaves in HL and BC.
		 */
		outf("\tcall swtab\n\t.db %d\n", n);
		for (i = 0; i < n; i++)
			outf("\t.db %d\n", (int)(sw->val[i] & 0xff));
		for (i = n - 1; i >= 0; i--) {
			out("\t.dw ");
			swlabel('K', sw->id, i);
			outc('\n');
		}
		return;
	}
	for (i = 0; i < n; i++) {
		outf("\tcp %d\n\tjp z,", (int)(sw->val[i] & 0xff));
		swlabel('K', sw->id, i);
		outc('\n');
	}
}

/* Param staging: move from stack to register */
#define MAXSTAGE 8
struct stgent {
	unsigned char reg;	/* target register */
	unsigned char off;	/* stack offset from IY */
	unsigned char width;	/* b/B/s/S = size */
};
static struct stgent stage[MAXSTAGE];
static unsigned char nstage;

/* Register bitmasks for callee-save tracking */
#define REGBIT(r) (1 << (r))
#define USES_BC (REGBIT(R_B) | REGBIT(R_C) | REGBIT(R_BC))

/*
 * Does this function keep a VARIABLE in BC?  Every 32-bit runtime
 * helper takes its second operand off the stack with a pop bc, so a
 * call to one destroys whatever was there, and the $[ $] guards in
 * the rule table save it across those.  Only a variable needs that:
 * scratch does not care what a helper leaves behind.
 */
int
bcinuse(void)
{
	return (regsused & USES_BC) != 0;
}

/*
 * Must this function hand the caller's BC back?  Always.
 *
 * The register-variable homes are callee-saved, so a function that
 * keeps a variable in BC saves it - and that used to be the whole
 * test.  It is not enough, because the code generator also uses BC
 * as SCRATCH in functions that have no variable there at all: "ld
 * bc,4" for an offset, "ld c,l / ld b,h" to move a pair.  366 of the
 * tree's functions do it, and none of them were saving anything.
 *
 * While callers saved BC around every call that did not matter.  Now
 * that they do not, it is the difference between a caller's variable
 * surviving a call and not: cpp lost the "out" parameter of filtbrace
 * that way, wrote a token through the null, and landed on the syscall
 * trap in page zero.
 *
 * The prologue is emitted before the body, so pass2 cannot know
 * whether the scratch will be used - and since nearly every function
 * uses it, the answer that costs least to be sure of is always.
 */
static int
savesbc(void)
{
	return 1;
}

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
		short off, rest;
		char *h;

		/*
		 * Frame pointer, then the scalar area, then the callee
		 * saves just under it - so they stay inside the 7-bit
		 * (iy+d) window - and the rest last, where big arrays live
		 * and are addressed with 16-bit arithmetic rather than
		 * (iy+d).
		 *
		 * All but the last of that is one call: eleven bytes of
		 * prologue become five, and the only thing particular to
		 * the function - how big the scalar area is - rides in the
		 * word after the call.
		 *
		 * A function with neither a save nor a scalar area wants
		 * plain fenter, and it is CALLED rather than written out.
		 * The bare sequence is eight bytes - push iy is two, ld
		 * iy,0 is four, add iy,sp is two - against three for the
		 * call, and leaving it to the peephole to substitute meant
		 * paying the eight in every build that does not run it.
		 */
		h = savesbc() ?
		      ((regsused & REGBIT(R_IX)) ? "fentbx" : "fentb") :
		      ((regsused & REGBIT(R_IX)) ? "fentx" : "fentn");

		if (!savesbc() && !(regsused & REGBIT(R_IX)) && savebase == 0)
			out("\tcall\tfenter\n");
		else
			outf("\tcall\t%s\n\t.dw\t%d\n", h, -savebase);

		off = -savebase;
		if (savesbc()) {
			off -= 2;
			bcoff = off;
			if (bcoff < -128)
				out("\t.error scalar frame too large for BC restore\n");
		}
		if (regsused & REGBIT(R_IX)) {
			off -= 2;
			ixoff = off;
			if (ixoff < -128)
				out("\t.error scalar frame too large for IX restore\n");
		}
		/* rest = arrays plus any unused save-slot bytes
		 * (off is -savebase-pushed, so this is
		 * framesize - savebase - pushed) */
		rest = framesize + off;
		if (rest > 0)
			outf("\tld\thl,-%d\n\tadd\thl,sp\n\tld\tsp,hl\n",
			    rest);
	}

	/* Stage params from stack to registers.  The walk runs from the
	 * top down to keep the emitted order what it always was; the
	 * static count itself stays intact (it is reset per function). */
	{
	register struct stgent *sp = stage + nstage;
	unsigned char ns = nstage + 1;

	while (--ns) {
		unsigned char r, off, w;

		sp--;
		r = sp->reg;
		off = sp->off;
		w = sp->width;

		if (ISBYTE(w)) {
			/* Byte: ld r,(iy+off) */
			out("\tld\t");
			switch (r) {
			case R_B: outc('b'); break;
			case R_C: outc('c'); break;
			}
			outf(",(iy+%d)\n", off);
		} else {
			/* Word: load low then high */
			switch (r) {
			case R_BC:
				outf("\tld\tc,(iy+%d)\n\tld\tb,(iy+%d)\n",
				    off, off + 1);
				break;
			case R_IX:
				outf("\tld\tl,(iy+%d)\n\tld\th,(iy+%d)\n\tpush\thl\n\tpop\tix\n",
				    off, off + 1);
				break;
			}
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

	/*
	 * Restore callee-saves without touching the return value.
	 *
	 * This used to be written out here: twelve bytes for IX, which
	 * has to come back through A because HL is the return value and
	 * DE is the rest of it when the value is long - a long-returning
	 * function that restored IX through DE handed back the saved
	 * IX's address as its low word, and "int a[5]" reserved .ds
	 * <heap pointer> bytes - and six more for BC.  Two thousand
	 * bytes of it in c1 alone.
	 *
	 * The helpers in csv.s do it once.  The saves sit together just
	 * under the scalar area, so all the caller has to say is where
	 * the lower of them is; the helper points the stack there and
	 * pops.  Five bytes against twenty-one, and the unwind is the
	 * same code, so there is no jp fexit after it.
	 */
	if (savesbc() || (regsused & REGBIT(R_IX))) {
		char *h;
		short off;

		if (!savesbc()) {
			h = "fexx";
			off = ixoff;
		} else if (!(regsused & REGBIT(R_IX))) {
			h = "fexb";
			off = bcoff;
		} else {
			h = "fexbx";
			off = ixoff;	/* pushed last, so the lower */
		}
		outf("\tcall\t%s\n\t.dw\t%d\n", h, off);
		return;
	}

	/*
	 * Nothing to restore: just the unwind, and jumped to for the
	 * same reason the entry is called.  Written out it is five bytes
	 * - ld sp,iy and pop iy are two each - against three, and the
	 * peephole that used to make the substitution only runs under
	 * -O.
	 */
	out("\tjp\tfexit\n");
}

static void
parseStmt(void)
{
	unsigned char op = read1();
	unsigned char n;
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
		++n;
		while (--n)
			parseStmt();
		return;
	case IF: {
		int lbl, hasel;
		n = read1();		/* nlabels for short-circuit */
		/*
		 * Two, not one.  An if with an else emits "no<lbl>" for the
		 * false branch and "no<lbl+1>" to jump over the else, but
		 * only one was reserved - and whether there is an else is
		 * not known until the then-body has been read, by which time
		 * any if inside it has already taken the number.
		 *
		 * So "no<lbl+1>" was defined twice and every jump to it went
		 * to whichever the assembler kept.  In an else-if chain the
		 * body of a branch was simply skipped: cpp built this way
		 * read its own "-o" and did nothing with the name after it.
		 *
		 * One number wasted per if without an else is nothing; they
		 * are per function and start again at zero.
		 */
		lbl = labelcnt;
		labelcnt += 2 + n;	/* lbl, lbl+1, and the short-circuits */
#ifdef DEBUG
		if (VERBOSE(V_STMT))
			fprintf(stderr, "  IF nlbl=%d lbl=%d\n", n, lbl);
		out("; IF nlbl="); outd(n); outc('\n');
#endif
		e = readexpr();
		if (e) {
#ifdef DEBUG
			dumpexpr(e);
#endif
			/*
			 * The condition branch-chains: && and || become jumps
			 * straight to the false label, with no nought-or-one
			 * materialised and retested in between.
			 */
			fmtstr(buf, "no%d_%d", lbl, fnindex);
			condfalse(e, buf);
		}
		parseStmt();		/* then-body */
		hasel = read1();
		if (hasel) {
			/* Jump over else */
			outf("\tjp no%d_%d\n", lbl + 1, fnindex);
		}
		/* Emit false label */
		outf("no%d_%d:\n", lbl, fnindex);
		if (hasel) {
			parseStmt();	/* else-body */
			/* Emit end label */
			outf("no%d_%d:\n", lbl + 1, fnindex);
		}
		return;
	}
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
				Expr *hl, *assign;
				char w = e->width;

				/*
				 * Wrap in ASSIGN to HL for return value.
				 *
				 * A byte comes back in HL like everything
				 * else, so it has to be widened first, and
				 * a signed one has to carry its sign: the
				 * assignment used to be byte-wide, took the
				 * store rule that zeroes H, and a function
				 * returning -1 as a char handed back 0x00ff.
				 * Callers read the sign out of H and saw a
				 * positive number.
				 */
				if (ISBYTE(w)) {
					/*
					 * A constant is already whatever
					 * width it is asked to be, and there
					 * is no rule for converting one -
					 * "return 0" would have become a
					 * widening of a literal and emitted
					 * nothing.
					 */
					if (e->op == NUMBER)
						e->width = 's';
					else
						e = mkunary(ISSIGNED(w) ?
						    SEXT : WIDEN, 's', e);
					w = 's';
				}
				hl = mkcode(w, R_HL);
				hl->op = INHL;
				assign = mkbinary(ASSIGN, w, hl, e);
				setdest(assign, DEST_VALUE);
				assign = rewrite(assign);
#ifdef DEBUG
				dumpexpr(assign);
#endif
				freeexpr(assign);
			}
		}
		/* Jump to function epilogue */
		out("\tjp\tX");
		out(funcname + 1);
		outc('\n');
		return;
	case LABEL:
		readS(buf, sizeof(buf));
#ifdef DEBUG
		if (VERBOSE(V_STMT))
			fprintf(stderr, "  LABEL %s\n", buf);
#endif
		out(buf);
		out(":\n");
		return;
	case GOTO:
		readS(buf, sizeof(buf));
#ifdef DEBUG
		if (VERBOSE(V_STMT))
			fprintf(stderr, "  GOTO %s\n", buf);
#endif
		out("\tjp ");
		out(buf);
		outc('\n');
		return;
	case SWITCH: {
		struct swctx *sw;
		int isbyte;

		read1();
		n = read1();
#ifdef DEBUG
		if (VERBOSE(V_STMT))
			fprintf(stderr, "  SWITCH n=%d\n", n);
		out("; SWITCH n="); outd(n); outc('\n');
#endif
		if (swtop >= MAXSWNEST) {
			outf("\t.error switches nested deeper than %d\n",
			    MAXSWNEST);
			swtop = MAXSWNEST - 1;
		}
		sw = &swstk[swtop++];
		sw->id = labelcnt++;
		sw->ncase = 0;
		sw->hasdef = 0;
		sw->val = swvals + (swtop - 1) * MAXSWCASE;

		/* work out the control value, then jump over the bodies to
		 * the comparisons - which is what keeps it live */
		isbyte = 0;
		e = readexpr();
		if (e) {
			setdest(e, DEST_VALUE);
			e = rewrite(e);
#ifdef DEBUG
			dumpexpr(e);
#endif
			isbyte = e && e->op == INA;
			freeexpr(e);
		}
		if (isbyte) {
			/* already in A, and a byte can never fail the high
			 * byte test that a word needs */
			;
		} else {
			out("\tld a,h\n\tor a\n\tjp nz,");
			swlabel('N', sw->id, -1);
			out("\n\tld a,l\n");
		}
		out("\tjp ");
		swlabel('D', sw->id, -1);
		outc('\n');

		++n;
		while (--n)
			parseStmt();

		/* the last body must not fall into the comparisons */
		out("\tjp ");
		swlabel('X', sw->id, -1);
		outc('\n');

		swlabel('D', sw->id, -1);
		out(":\n");
		swdispatch(sw);
		/* no case matched, and a word control that did not fit a
		 * byte arrives here too.  Both helpers fall out of their
		 * table onto this label rather than storing its address */
		swlabel('N', sw->id, -1);
		out(":\n");
		if (sw->hasdef) {
			out("\tjp ");
			swlabel('F', sw->id, -1);
			outc('\n');
		}
		swlabel('X', sw->id, -1);
		out(":\n");
		swtop--;
		return;
	}
	case CASE: {
		struct swctx *sw = swtop ? &swstk[swtop - 1] : 0;

		n = read1();
#ifdef DEBUG
		if (VERBOSE(V_STMT))
			fprintf(stderr, "  CASE n=%d\n", n);
		out("; CASE n="); outd(n); outc('\n');
#endif
		/* the value is a constant - pass1 folded it - so take it
		 * rather than emitting code for it */
		e = readexpr();
		if (sw && e) {
			if (sw->ncase >= MAXSWCASE) {
				outf("\t.error more than %d cases in one switch\n",
				    MAXSWCASE);
			} else {
				sw->val[sw->ncase] = e->u.val;
				swlabel('K', sw->id, sw->ncase);
				out(":\n");
				sw->ncase++;
			}
		}
		if (e)
			freeexpr(e);
		++n;
		while (--n)
			parseStmt();
		return;
	}
	case DEFAULT: {
		struct swctx *sw = swtop ? &swstk[swtop - 1] : 0;

		n = read1();
#ifdef DEBUG
		if (VERBOSE(V_STMT))
			fprintf(stderr, "  DEFAULT n=%d\n", n);
		out("; DEFAULT n="); outd(n); outc('\n');
#endif
		if (sw) {
			sw->hasdef = 1;
			swlabel('F', sw->id, -1);
			out(":\n");
		}
		++n;
		while (--n)
			parseStmt();
		return;
	}
	case ASM: {
		/* Inline asm - copy the text through verbatim */
		unsigned short len = read2();
#ifdef DEBUG
		if (VERBOSE(V_STMT))
			fprintf(stderr, "  ASM len=%d\n", len);
#endif
		outc('\n');
		outc('\t');
		while (len--)
			outc(read1());
		outc('\n');
		return;
	}
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
			readS(funcname, sizeof(funcname));
			labelcnt = 0;
			fnindex++;
#ifdef DEBUG
			if (VERBOSE(V_PARSE))
				fprintf(stderr, "parse: FUNC %s type=%c\n", funcname, t);
			out("; FUNC "); out(funcname); outc(':'); outc(t); outc('\n');
#endif
			n = read1();		/* param count */
			i = read1();		/* local count */
			framesize = read2();	/* frame size */
			savebase = read1();	/* scalar area size */
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
			++n;
			while (--n) {
				unsigned char reg, off;
				read1();	/* AST_DECL */
				t = read1();
				readS(buf, sizeof(buf));
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
					struct stgent *tp = &stage[nstage++];
					regsused |= REGBIT(reg);
					tp->reg = reg;
					tp->off = off;
					tp->width = t;
				}
			}
			/* Scan locals: just track register usage */
			++i;
			while (--i) {
				unsigned char reg;
				read1();	/* AST_DECL */
				t = read1();
				readS(buf, sizeof(buf));
				reg = read1();
				read2();	/* offset */
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
