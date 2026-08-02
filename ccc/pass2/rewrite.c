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
#include "rules.h"
#include "../cpp/lexeme.h"
#include <stdlib.h>

/* Label counter for short-circuit jumps */
static int labelcnt;

#ifdef DEBUG
#include <stdio.h>

/*
 * Which rules have fired.
 *
 * A rule that matches nothing is worse than absent: it reads as
 * coverage that does not exist.  One sat in this table for a long time
 * emitting bit n,(iy+d), correct and unreachable, because an AND
 * reduced its left operand before any rule could see it - and every
 * test passed the whole time, because the code that ran instead was
 * right, only longer.
 *
 * Set CCC_RULEHITS to a file name and each run appends what it used.
 * Debug build only: the counters are host-side bookkeeping and the
 * Z80 build has never seen them.
 */
#define MAXRULES 1024
static unsigned long rulehits[MAXRULES];

void
rulehit(int i)
{
	if (i >= 0 && i < MAXRULES)
		rulehits[i]++;
}

void
dumphits(void)
{
	char *path = getenv("CCC_RULEHITS");
	FILE *f;
	int i;

	if (!path || !(f = fopen(path, "a")))
		return;
	for (i = 0; rules[i].pat && i < MAXRULES; i++)
		fprintf(f, "%d\t%lu\t%s\n", i, rulehits[i], rules[i].pat);
	fclose(f);
}
#endif

/* Forward declarations */
static char *pmatch(char *p, Expr *e);
static Expr *rewrite1(Expr *e);
static unsigned char baseop(unsigned char op);
static int islocdesc(Expr *e);

/*
 * Check if expression matches any preserve pattern.
 * Returns 1 if should be preserved (not reduced).
 */
static int
shouldpres(Expr *e)
{
	char **pp;
	if (!e) return 0;
	for (pp = preserve; *pp; pp++) {
		if (pmatch(*pp, e))
			return 1;
	}
	return 0;
}

/*
 * Sethi-Ullman labeling: compute registers needed for each node
 * With only HL and DE available:
 *   0 = already in register (INHL, INDE, some REGVAR)
 *   1 = needs one register
 *   2 = needs both HL and DE
 *   3+ = needs spill to stack
 */
void
label(Expr *e)
{
	unsigned char l, r;

	if (!e) return;

	/* Label children first (post-order) */
	label(e->left);
	label(e->right);

	/*
	 * What the children cost is wanted by most of what follows, and
	 * every case used to fetch it again through the pointer it had
	 * just finished testing.  Once, here.  A child that is not there
	 * costs one: the operand still has to be loaded from somewhere.
	 */
	l = e->left ? e->left->regs : 1;
	r = e->right ? e->right->regs : 1;

	switch (e->op) {
	/* Already in register: 0 */
	case INHL:
	case INDE:
	case INA:
	case INBC:
	case INE:
	case CODE:
		e->regs = 0;
		return;

	/* REGVAR: 0 if HL/DE, 1 if BC/IX (needs move) */
	case REGVAR:
		if (e->u.var.reg == R_HL || e->u.var.reg == R_DE)
			e->regs = 0;
		else
			e->regs = 1;
		return;

	/* LOCALVAR past the (iy+d) window (big-array bases): the
	 * address arithmetic needs HL and DE, not a free (iy+d)
	 * operand, so cost it like a computed subexpression */
	case LOCALVAR:
		if (e->u.var.off < -126 || e->u.var.off > 124) {
			e->regs = 2;
			return;
		}
		e->regs = 1;
		return;

	/* Leaves that need loading: 1 */
	case NUMBER:
	case SYM:
	case SYMREF:
	case INDEX:
		e->regs = 1;
		return;

	/* DEREF: depends on address complexity */
	case DEREF:
		e->regs = l > 1 ? l : 1;
		return;

	/* ASSIGN: lvalue doesn't consume reg, only rvalue */
	case ASSIGN:
		e->regs = r ? r : 1;
		return;

	/*
	 * CALL: the result comes back in HL and nowhere else - the ABI
	 * leaves no choice - so a call cannot be evaluated into DE the
	 * way a loadable operand can.  Costing it two makes two of them
	 * add up to three, which takes the spill path and puts the first
	 * result on the stack instead of letting the second call
	 * overwrite it.
	 */
	case CALL:
		e->regs = 2;
		return;

	/* ARGNODE: each arg independent, pushed to stack */
	case ARGNODE:
		e->regs = l;
		return;

	/* Short-circuit: sides evaluated separately */
	case LAND:
	case LOR:
		e->regs = l > r ? l : r;
		return;

	/* Ternary: condition, then, else all separate */
	case QUES:
	case TERNBRANCH:
		e->regs = l > r ? l : r;
		return;

	/* Unary ops: same as child, min 1 */
	case BANG:
	case NEG:
	case NOT:
		e->regs = l ? l : 1;
		return;

	/*
	 * A step whose rules can only put the answer in HL costs two, the
	 * same as a call and for the same reason.  This number is what
	 * chooses which side is worked out first, and an operand that
	 * cannot be held in DE has to go before whatever is going to sit
	 * there - saying it costs one is how the spill gets avoided by
	 * accident instead of on purpose.
	 *
	 * Costed one, "buf[pos++]" put the array's address in HL and then
	 * the step on top of it, and "i <= ++j" over a register variable
	 * did the same.
	 *
	 * A frame slot is the exception, and the table says so: those
	 * templates write through $t and $T and so land wherever they
	 * were asked to.  Everything else - a global, a register variable
	 * - names l and h outright.
	 */
	case PREINC:
	case POSTINC:
	case PREDEC:
	case POSTDEC:
		e->regs = l;
		if (e->left && e->left->op != LOCALVAR &&
		    e->left->op != INDEX && e->regs < 2)
			e->regs = 2;
		if (!e->regs)
			e->regs = 1;
		return;

	/* Binary ops: Sethi-Ullman formula */
	default:
		if (l == r)
			e->regs = l + 1;
		else
			e->regs = l > r ? l : r;
		/*
		 * A compound assignment holds the location while it works
		 * out the value, so it needs two whatever its operands cost
		 * - the expansion puts the address in HL and the value in
		 * DE, and the side-effecting form keeps the address on the
		 * stack and uses both on the way back.  Scoring it as one
		 * would let a parent believe a register survives it.
		 */
		if (baseop(e->op) && e->regs < 2)
			e->regs = 2;
		return;
	}
}

/*
 * Register assignment: top-down pass to set target registers
 * Most ops are HL-centric on Z80, so:
 *   - Binary ops: left→HL, right→DE
 *   - Unary ops: child inherits parent's target
 *   - Result may need move if parent wants different reg
 */
void
assign(Expr *e, unsigned char tgt)
{
	if (!e) return;

	e->tgt = tgt;

	switch (e->op) {
	/* Already in register: no children */
	case INHL:
	case INDE:
	case INA:
	case INBC:
	case INE:
	case CODE:
	case NUMBER:
	case SYM:
	case SYMREF:
	case LOCALVAR:
	case INDEX:
	case REGVAR:
		return;

	/* Unary: child inherits target */
	case DEREF:
	case BANG:
	case NEG:
	case NOT:
	case PREINC:
	case POSTINC:
	case PREDEC:
	case POSTDEC:
		assign(e->left, tgt);
		return;

	/* ASSIGN: lvalue doesn't need target, rvalue→tgt */
	case ASSIGN:
		/*
		 * An lvalue that names a place needs no register.  One that
		 * dereferences is an address the tree has to work out, and
		 * the store rules want it in HL - saying no target left
		 * whatever computed it with nowhere to go, and a rule whose
		 * destination follows the target then produced a node with
		 * no register at all.  "*p++ = 0" stepped the pointer, left
		 * the old value in HL, and reported it as being nowhere.
		 *
		 * An assignment's left is a location, so "*p" as an lvalue is
		 * just p and "*p++" is just the step - there is no DEREF to
		 * look for, only the question of whether the location has to
		 * be worked out before it can be stored through.
		 */
		assign(e->left, islocdesc(e->left) ? 0 : R_HL);
		assign(e->right, tgt);
		return;

	/* CALL: args go to stack, result in HL */
	case CALL:
		assign(e->left, 0);  /* function address */
		/* args handled specially */
		return;

	/* ARGNODE: each arg evaluated to HL, then pushed */
	case ARGNODE:
		assign(e->left, R_HL);
		if (e->right)
			assign(e->right, R_HL);
		return;

	/* Short-circuit: each side independent, wants flags */
	case LAND:
	case LOR:
		assign(e->left, R_HL);
		assign(e->right, R_HL);
		return;

	/* Ternary: condition→flags, branches→tgt */
	case QUES:
		assign(e->left, R_HL);  /* condition */
		if (e->right) {
			assign(e->right->left, tgt);   /* then */
			assign(e->right->right, tgt);  /* else */
		}
		return;

	case TERNBRANCH:
		assign(e->left, tgt);
		assign(e->right, tgt);
		return;

	/* Binary ops: left→HL, right→DE (Z80 is HL-centric) */
	default:
		if (e->regs >= 3 && e->right && e->right->op == NUMBER) {
			/*
			 * A constant costs no register - the rule that names
			 * it writes it into the instruction - so however dear
			 * the other side is, there is nothing here to spill
			 * for.  Sending it to HL with the left made the spill
			 * path push the left operand, rewrite a bare constant
			 * to nothing at all, and pop back a copy of what it
			 * had just pushed: "(v & m) != 0" compared the result
			 * against itself and was always equal.
			 */
			assign(e->left, R_HL);
			assign(e->right, R_DE);
		} else if (e->regs >= 3) {
			/* Need spill: both children compute to HL */
			assign(e->left, R_HL);
			assign(e->right, R_HL);
		} else if (e->regs == 2) {
			/* Need both registers */
			assign(e->left, R_HL);
			assign(e->right, R_DE);
			/*
			 * A byte comparison keeps its right operand whole so
			 * rules like Q(A,N):F can still see a literal.  Test
			 * the operand width, not the node's: a comparison
			 * yields ubyte whatever it compared, so keying off
			 * e->width preserved the right operand of every
			 * comparison and stranded the ones that needed
			 * reducing - a REGVAR never became INBC.
			 */
			if (e->left && ISBYTE(e->left->width) &&
			    (e->op == EQ || e->op == NEQ || e->op == LT ||
			     e->op == GT || e->op == LE || e->op == GE) &&
			    e->right && shouldpres(e->right)) {
				e->right->nored = 1;
			}
		} else {
			/* Only need one, propagate target */
			assign(e->left, tgt);
			assign(e->right, tgt);
		}
		/* For ADD with NUMBER, preserve NUMBER for address rules */
		/* +(V,N)->I, +(S,N)->O, +(O,N)->O */
		if (e->op == PLUS && e->right && e->right->op == NUMBER &&
		    e->left && (e->left->op == REGVAR || e->left->op == SYM ||
		                e->left->op == SYMREF)) {
			e->right->nored = 1;
		}
		return;
	}
}

#ifdef DEBUG
#include "debug.h"
#include <stdio.h>
#endif

/* Special pattern values */
#define P_ANY    0
#define P_NULL   255
#define P_NUM    254
#define P_POW2   253
#define P_ZERO   252
#define P_SMALL  251    /* 1-4: can use inc/dec */
#define P_MUL3   250    /* constant 3 */
#define P_MUL5   249    /* constant 5 */
#define P_MUL6   248    /* constant 6 */
#define P_MUL7   247    /* constant 7 */
#define P_MUL9   246    /* constant 9 */
#define P_MUL10  245    /* constant 10 */
#define P_MUL11  244    /* constant 11 */
#define P_MUL12  243    /* constant 12 */
#define P_MUL14  242    /* constant 14 */
#define P_MUL15  241    /* constant 15 */
#define P_MUL20  240    /* constant 20 */
#define P_MUL24  239    /* constant 24 */
#define P_MUL40  238    /* constant 40 */
#define P_EIGHT  237    /* constant 8: a shift by a whole byte */

/*
 * Map single char to opcode (or special pattern value)
 */
/*
 * op_table is filled from op_map by initOpTab() at startup: designated
 * initializers are C99 and zc3/ccc can't parse them.
 */
static unsigned char op_table[256];

static struct opmap {
	char c;
	unsigned char op;
} op_map[] = {
	{'+', PLUS}, {'*', STAR}, {'-', MINUS}, {'/', DIV}, {'%', MOD},
	{'&', AND}, {'|', OR}, {'^', XOR}, {'<', LSHIFT}, {'>', RSHIFT},
	{'=', ASSIGN}, {'D', DEREF}, {'V', REGVAR}, {'L', LOCALVAR},
	{'I', INDEX}, {'H', INHL}, {'E', INDE}, {'A', INA}, {'K', INE},
	{'B', INBC}, {'O', SYMREF}, {'Q', EQ}, {'U', NEQ}, {'T', LT},
	{'G', GT}, {'W', LE}, {'Y', GE}, {'N', P_NUM}, {'P', P_POW2},
	{'X', SEXT}, {'J', WIDEN}, {'R', NARROW}, {';', COMMA},
	{'Z', P_ZERO}, {'M', P_SMALL}, {'S', SYM}, {'i', PREINC},
	{'j', POSTINC}, {'k', PREDEC}, {'m', POSTDEC}, {'a', ARGNODE},
	{'C', CODE}, {'o', OREQ}, {'g', NEG}, {'~', NOT}, {'!', BANG},
	{'_', P_ANY}, {'0', P_NULL}, {'3', P_MUL3}, {'5', P_MUL5},
	{'6', P_MUL6}, {'7', P_MUL7}, {'9', P_MUL9}, {'x', P_MUL10},
	{'e', P_MUL11}, {'w', P_MUL12}, {'f', P_MUL14}, {'n', P_MUL15},
	{'y', P_MUL20}, {'q', P_MUL24}, {'z', P_MUL40},
	{'8', P_EIGHT}
};

void
initOpTab(void)
{
	int i;

	for (i = 0; i < sizeof(op_map) / sizeof(op_map[0]); i++)
		op_table[(unsigned char)op_map[i].c] = op_map[i].op;
}

static unsigned char
chartopc(char c)
{
	return op_table[(unsigned char)c];
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
		if (n == ((unsigned long)1 << i))
			return i;
	return -1;
}

/*
 * Match pattern byte against expression
 */
static int
opmatch(unsigned char pat, Expr *e)
{
	/* indexed by pat-238: P_MUL40 (238) up through P_MUL3 (250) */
	static unsigned char multab[] = {
		40, 24, 20, 15, 14, 12, 11, 10, 9, 7, 6, 5, 3
	};

	if (pat == P_ANY) return 1;
	if (pat == P_NULL) return e == NULL;
	if (pat == P_NUM) return e && e->op == NUMBER;
	if (pat == P_POW2) return e && e->op == NUMBER && ispow2(e->u.val) > 0;
	if (pat == P_ZERO) return e && e->op == NUMBER && e->u.val == 0;
	if (pat == P_SMALL) return e && e->op == NUMBER && e->u.val >= 1 && e->u.val <= 4;
	if (pat == P_EIGHT) return e && e->op == NUMBER && e->u.val == 8;
	if (pat >= P_MUL40 && pat <= P_MUL3)
		return e && e->op == NUMBER && e->u.val == multab[pat-238];
	return e && e->op == pat;
}

/*
 * Parse and match pattern string against expression
 * Returns pointer past matched pattern, or NULL if no match
 * Pattern: op or op(left) or op(left,right)
 * Width suffix: :b :s :l :p :f (or :_ for any)
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

	/* Check width/dest suffix: :w or :F or :V or :wF (width + dest) */
	if (*p == ':') {
		p++;
		/* check width first (b/B/s/S/l/L/p/f or _ for any) */
		/* case-insensitive: b matches B, s matches S, etc. */
		if (*p != 'F' && *p != 'V' && *p != 'S' &&
		    *p != '\0' && *p != ')' && *p != ',') {
			if (*p != '_' && e && (e->width | 0x20) != (*p | 0x20))
				return NULL;
			p++;
		}
		/* check flag context (F) */
		if (*p == 'F') {
			if (!e || e->dest != DEST_FLAGS)
				return NULL;
			p++;
		}
		/* check value context (V) */
		if (*p == 'V') {
			if (!e || e->dest != DEST_VALUE)
				return NULL;
			p++;
		}
		/*
		 * Statement context (S): the result is thrown away.  Worth
		 * distinguishing where producing it costs something - a long
		 * step returns the value from before the step, so wanting the
		 * one after means reading it back.
		 */
		if (*p == 'S') {
			if (!e || e->dest != DEST_NONE)
				return NULL;
			p++;
		}
	}
	return p;
}

/*
 * Get node by path string: L=left, R=right, LL=left->left, etc.
 */
static Expr *
getpath(Expr *e, unsigned char p)
{
	if (p == P_NONE) return e;
	if (p == P_L) return e ? e->left : NULL;
	if (p == P_R) return e ? e->right : NULL;
	if (p == P_LL) return (e && e->left) ? e->left->left : NULL;
	return NULL;
}

/*
 * Emit index register name
 */
static char *
idxregname(unsigned char reg)
{
	switch (reg) {
	case R_IX: return "ix";
	case R_IY: return "iy";
	}
	return "??";
}

/*
 * Interpolate asm template, emitting to output
 * $X where X is path (L, R, LL, LR, etc) interpolates that node
 * Modifiers after path:
 *   l - low byte of number
 *   h - high byte of number
 *   + - increment index offset by 1
 * Special:
 *   $t - target low register (l or e based on e->tgt)
 *   $u - target high register (h or d based on e->tgt)
 *   $T - target register pair (hl or de)
 *   %(text) - repeat text N times where N is right operand value
 */
/*
 * Put the shared sequences back.  A byte with the high bit set is an
 * index into fragtab; everything else is itself.
 *
 * This runs before any interpolation rather than during it, because
 * the repeat construct copies its span literally and would otherwise
 * hand a raw index straight to the output.  Expanding first means
 * nothing else in here has to know fragments exist.
 */
static void
expandtpl(char *tpl, char *buf)
{
	unsigned char *s = (unsigned char *)tpl;
	char *d = buf, *f;
	char *end = buf + TPLMAX - 1;

	while (*s) {
		if (*s & 0x80) {
			for (f = fragtab[*s & 0x7f]; *f && d < end; f++)
				*d++ = *f;
			s++;
		} else if (d < end) {
			*d++ = *s++;
		} else {
			break;
		}
	}
	*d = 0;
}

static void
emitasm(char *tpl, Expr *e)
{
	char expbuf[TPLMAX];
	char *p;
	char path[8];
	int i, offadj;
	char mod;
	Expr *n;
	long val;
	int cnt;
	char *start;

	expandtpl(tpl, expbuf);
	p = expbuf;

	while (*p) {
		/* %(text) - repeat text N times where N is right operand */
		if (*p == '%' && *(p+1) == '(') {
			p += 2;
			start = p;
			/* find closing paren */
			while (*p && *p != ')') p++;
			/* get count from right operand */
			cnt = (e->right && e->right->op == NUMBER) ?
			      (int)e->right->u.val : 1;
			/* emit the enclosed text cnt times */
			for (i = 0; i < cnt; i++) {
				char *q;
				for (q = start; q < p; q++)
					outc(*q);
			}
			if (*p == ')') p++;
			continue;
		}
		if (*p == '$') {
			p++;
			/* $$ escapes the assembler's own $, the address of
			 * the current instruction */
			if (*p == '$') {
				outc('$');
				p++;
				continue;
			}
			/*
			 * $[ and $] bracket a call to one of the 16-bit
			 * helpers, which take their second operand off the
			 * stack with a pop bc and do not put it back.  A
			 * register variable living in BC has to be saved
			 * across that, and only here is it known whether
			 * there is one - the table cannot say.
			 *
			 * Without it "t = a * a" in a function with a
			 * register variable quietly destroyed the variable,
			 * and when the variable was a loop subscript doing
			 * the multiplying, the loop did not end.
			 */
			if (*p == '[') {
				if (bcinuse())
					out("\tpush bc\n");
				p++;
				continue;
			}
			if (*p == ']') {
				if (bcinuse())
					out("\tpop bc\n");
				p++;
				continue;
			}
			/* Target register substitution */
			if (*p == 't') {
				outc(e->tgt == R_DE ? 'e' : 'l');
				p++;
				continue;
			}
			if (*p == 'u') {
				outc(e->tgt == R_DE ? 'd' : 'h');
				p++;
				continue;
			}
			if (*p == 'T') {
				out(e->tgt == R_DE ? "de" : "hl");
				p++;
				continue;
			}
			/* collect path chars */
			for (i = 0; i < 7 && (*p == 'L' || *p == 'R'); i++)
				path[i] = *p++;
			path[i] = 0;
			/* check for modifier */
			mod = 0;
			offadj = 0;
			if (*p == 'l' || *p == 'h' ||
			    *p == '2' || *p == '3' ||
			    *p == 'o' || *p == 'r') {
				mod = *p++;
			}
			while (*p == '+') {
				offadj++;
				p++;
			}
			/* Special: $RL (right child name) */
			if (path[0] == 'R' && path[1] == 'L') {
				n = (e && e->right) ? e->right->left : NULL;
			} else {
				/* navigate to node */
				n = e;
				for (i = 0; path[i] && n; i++) {
					if (path[i] == 'L') n = n->left;
					else n = n->right;
				}
			}
			/* emit based on node type */
			if (n) {
				if (n->op == NUMBER) {
					val = n->u.val;
					/* l h 2 3 select the four bytes */
					if (mod == 'l') val = val & 0xff;
					else if (mod == 'h') val = (val >> 8) & 0xff;
					else if (mod == '2') val = (val >> 16) & 0xff;
					else if (mod == '3') val = (val >> 24) & 0xff;
					outd(val);
				} else if (n->op == SYMREF) {
					/* honour $L+ here too - without it a
					 * template reaching for the second
					 * word of a long silently addressed
					 * the first one again */
					out(n->u.symref.name);
					val = n->u.symref.off + offadj;
					if (val != 0) {
						if (val > 0)
							outc('+');
						outd(val);
					}
				} else if (n->op == INDEX) {
					/* o and r split it into the offset and
					 * the register, for templates that
					 * have to do the arithmetic themselves
					 * rather than let (ix+d) do it */
					if (mod == 'o') {
						outd(n->u.var.off + offadj);
					} else if (mod == 'r') {
						out(idxregname(n->u.var.reg));
					} else {
						out(idxregname(n->u.var.reg));
						val = n->u.var.off + offadj;
						if (val >= 0) outc('+');
						outd(val);
					}
				} else if (n->op == LOCALVAR) {
					/* raw frame offset, for address
					 * arithmetic templates */
					outd(n->u.var.off + offadj);
				} else {
					/* template navigated to a node the
					 * emitter can't print - make the
					 * assembler flag it loudly */
					outf("?op%d?", n->op);
				}
			} else {
				out("?null?");
			}
		} else {
			outc(*p++);
		}
	}
}


/*
 * Nearly every rewrite that has emitted its own code ends the same way:
 * make a node standing for the answer in HL, give it the destination
 * the original was asked for, and free what it replaced.  Eleven copies
 * of four statements became this.
 *
 * op is what to call the answer: INHL where the rewrite names the
 * register outright, CODE where it leaves the typing to the pass below
 * that turns CODE into INHL/INDE/INA by its register.
 *
 * The seven rewrites that build their node in a branch - a different
 * register in each arm - keep their own tails.  Sharing just the last
 * three statements with them was tried and cost two bytes: passing two
 * pointers to a function is dearer than the stores it saves.
 */
static Expr *
donehl(Expr *e, unsigned char op)
{
	Expr *n = mkcode(e->width, R_HL);

	n->op = op;
	n->dest = e->dest;
	freeexpr(e);
	return n;
}

/*
 * The register each RF_REG value demands, indexed by the field shifted
 * down.  Slot 1 is RF_IXIY, which accepts either index register and is
 * tested by hand; slot 0 is no requirement at all and never reached.
 */
static char regwant[8] = {
	0, 0, R_BC, R_DE, R_HL, R_IX, R_C, R_B
};

/*
 * Try to apply a rule
 */
static Expr *
tryrule(struct rule *rp, Expr *e)
{
	Expr *n, *src, *num, *lc, *rc;
	char reg;
	int off;
	int shift, changed;
	unsigned char newop, oldop;

	/* Match pattern */
	if (!pmatch(rp->pat, e))
		return NULL;

	/*
	 * Check the register constraint.  A rule names at most one, so
	 * this is a value to compare rather than a set of bits to test.
	 * RF_REG is a three bit field, so the register a rule demands is
	 * a lookup rather than a ladder of seven comparisons - each of
	 * which used to reload src->u.var.reg from memory.  RF_IXIY is
	 * the one that accepts two, and stays written out.
	 */
	if (rp->flags & RF_REG) {
		unsigned char want = rp->flags & RF_REG;
		char have;

		src = getpath(e, RP_D(rp));
		if (!src)
			return NULL;
		have = src->u.var.reg;
		if (want == RF_IXIY) {
			if (have != R_IX && have != R_IY)
				return NULL;
		} else if (have != regwant[want >> 5])
			return NULL;
	}

	/* Sign-bit tests are only valid on a signed operand */
	if ((rp->flags & RF_SIGNL) && (!e->left || !ISSIGNED(e->left->width)))
		return NULL;

	/*
	 * Some forms only apply when the result is wanted in DE - a byte
	 * heading for E as the right operand of a binary op, rather than
	 * for A as the left.
	 */
	if ((rp->flags & RF_TDE) && e->tgt != R_DE)
		return NULL;

#ifdef DEBUG
	if (VERBOSE(V_RULES))
		fprintf(stderr, "rewrite: %s -> %c\n", rp->pat, rp->rep);
#endif

	oldop = e->op;
	newop = rp->rep;
	changed = 0;

	/* Get replacement children */
	lc = RP_L(rp) ? getpath(e, RP_L(rp)) : NULL;
	rc = RP_R(rp) ? getpath(e, RP_R(rp)) : NULL;

	/* Handle NEQ -> BANG(EQ) - caller must rewrite result */
	if (rp->flags & RF_NOTEQ) {
		Expr *eq = mkbinary(EQ, e->width, e->left, e->right);
		eq->dest = e->dest;
		e->op = BANG;
		e->dest = DEST_FLAGS;
		e->left = eq;
		e->right = NULL;
		return e;  /* tagged for re-rewrite */
	}

	/* Handle INDEX specially */
	if (newop == INDEX) {
		if (e->op == LOCALVAR) {
			reg = e->u.var.reg ? e->u.var.reg : R_IY;
			off = e->u.var.off;
		} else {
			src = getpath(e, RP_D(rp));
			reg = src ? src->u.var.reg : R_IY;
			num = e->right;
			off = num ? (short)num->u.val : 0;
			/* If source is INDEX, combine offsets */
			if (src && src->op == INDEX)
				off += src->u.var.off;
		}
		/*
		 * (iy+d) displacements are 7-bit signed; leave headroom
		 * for +3 word/long adjustments.  Out-of-window accesses
		 * (big arrays) must go through address arithmetic, so
		 * refuse the INDEX form and let other rules apply.
		 */
		if (off < -126 || off > 124)
			return NULL;
		n = mkindex(e->width, reg, off);
		freeexpr(e);
		return n;
	}

	/* INDEX -> CODE: the effective address as a value.  add hl only
	 * takes bc/de/hl/sp, so the index register goes through the
	 * stack and the displacement is added the ordinary way.
	 *
	 * Only for a node that was HANDED a target: an operand a parent
	 * will dereference carries none, and converting it would steal
	 * every (reg+d) form before the addressing rules saw it. */
	if (newop == CODE && oldop == INDEX) {
		if (!e->tgt)
			return NULL;
		off = e->u.var.off;
		reg = e->u.var.reg ? e->u.var.reg : R_IY;
		if (e->tgt == R_DE) {
			/* sibling value lives in HL - preserve it */
			outf("\tpush hl\n\tpush %s\n\tpop hl\n\tld de,%d\n\tadd hl,de\n\tex de,hl\n\tpop hl\n",
			    idxregname(reg), off);
			n = mkcode(e->width, R_DE);
		} else {
			outf("\tpush %s\n\tpop hl\n", idxregname(reg));
			if (off)
				outf("\tld de,%d\n\tadd hl,de\n", off);
			n = mkcode(e->width, R_HL);
		}
		n->dest = e->dest;
		freeexpr(e);
		return n;
	}

	/* Far LOCALVAR -> CODE: form the frame address with 16-bit
	 * arithmetic (big-array bases sit past the (iy+d) window) */
	if (newop == CODE && oldop == LOCALVAR) {
		off = e->u.var.off;
		if (e->tgt == R_DE) {
			/* sibling value lives in HL - preserve it */
			outf("\tpush hl\n\tpush iy\n\tpop hl\n\tld de,%d\n\tadd hl,de\n\tex de,hl\n\tpop hl\n",
			    off);
			n = mkcode(e->width, R_DE);
		} else {
			outf("\tpush iy\n\tpop hl\n\tld de,%d\n\tadd hl,de\n",
			    off);
			n = mkcode(e->width, R_HL);
		}
		n->dest = e->dest;
		freeexpr(e);
		return n;
	}

	/* Handle NUMBER -> CODE: emit load instruction */
	if (newop == CODE && oldop == NUMBER) {
		long val = e->u.val;
		char w = e->width;
		if (w == 'b' || w == 'B') {
			/* Byte: load into A, or E if target is DE (for RHS of compare) */
			if (e->tgt == R_DE) {
				outf("\tld e,%d\n", (int)val);
				n = mkcode('b', R_E);
			} else {
				outf("\tld a,%d\n", (int)val);
				n = mkcode('b', R_A);
			}
		} else if (w == 'l' || w == 'L') {
			/* Long: load into HLDE (DE=low, HL=high) */
			outf("\tld de,%d\n\tld hl,%d\n",
			    (int)(val & 0xffff), (int)((val >> 16) & 0xffff));
			n = mkcode(w, R_HL);
		} else if (e->tgt == R_DE) {
			/* a word constant honours its target too - as the
			 * right operand of a binary op it belongs in DE, and
			 * putting it in HL would land on the left one */
			outf("\tld de,%d\n", (int)val);
			n = mkcode(e->width, R_DE);
		} else {
			outf("\tld hl,%d\n", (int)val);
			n = mkcode(e->width, R_HL);
		}
		n->dest = e->dest;
		freeexpr(e);
		return n;
	}

	/* Handle SYMREF: SYM, SYM+NUMBER, or SYMREF+NUMBER */
	if (newop == SYMREF) {
		char *name;
		short soff;
		if (e->op == SYM) {
			/* bare SYM -> SYMREF+0 */
			name = e->u.name;
			soff = 0;
		} else if (e->left->op == SYMREF) {
			/* SYMREF + NUMBER -> combine offsets */
			name = e->left->u.symref.name;
			soff = e->left->u.symref.off;
			if (e->right)
				soff += (short)e->right->u.val;
		} else {
			/* SYM + NUMBER */
			name = e->left->u.name;
			soff = e->right ? (short)e->right->u.val : 0;
		}
		n = mksymref(name, soff);
		freeexpr(e);
		return n;
	}

	/* Reuse node, change op */
	if (newop != oldop)
		changed = 1;
	e->op = newop;
	if (lc != e->left || rc != e->right) {
		changed = 1;
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
		if (shift > 0) {
			e->right->u.val = shift;
			changed = 1;
		}
	}

	/* Increment constant by 1 (for GT->GE, LE->LT transforms) */
	if ((rp->flags & RF_INC1) && e->right && e->right->op == NUMBER) {
		e->right->u.val++;
		changed = 1;
	}

	/* Emit assembly and create CODE node if template present */
	if (rp->asmtpl) {
		unsigned char dest;
		emitasm(rp->asmtpl, e);
		/* Use rule's destval, or target register if destval is 0 */
		dest = rp->destval ? rp->destval : e->tgt;
		n = mkcode(e->width, dest);
		n->dest = e->dest;
		freeexpr(e);
		return n;
	}

	return changed ? e : NULL;
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
 * For relational ops, swap operands AND flip operator
 */
static void
normalize(Expr *e)
{
	Expr *t;
	if (!e || !e->left || !e->right) return;
	/* Commutative ops: just swap operands */
	if (iscommut(e->op)) {
		if (e->left->op == NUMBER && e->right->op != NUMBER) {
			t = e->left;
			e->left = e->right;
			e->right = t;
		}
		return;
	}
	/* Relational ops: swap operands AND flip operator */
	/* 0 < x becomes x > 0, 0 > x becomes x < 0, etc */
	if (e->left->op == NUMBER && e->right->op != NUMBER) {
		switch (e->op) {
		case LT:
			t = e->left; e->left = e->right; e->right = t;
			e->op = GT;
			break;
		case GT:
			t = e->left; e->left = e->right; e->right = t;
			e->op = LT;
			break;
		case LE:
			t = e->left; e->left = e->right; e->right = t;
			e->op = GE;
			break;
		case GE:
			t = e->left; e->left = e->right; e->right = t;
			e->op = LE;
			break;
		}
	}
}

/*
 * Normalize the whole tree before it is labeled.  step() normalizes
 * each node too, but by the time it runs the children have already
 * been reduced into concrete registers, so an operand swap there
 * leaves the operand sitting in the wrong one.
 */
static void
normtree(Expr *e)
{
	if (!e) return;
	normalize(e);
	/*
	 * A byte tested against a byte-range mask is a byte test.  C
	 * promoted the operand and emitOperand said so with a WIDEN or
	 * SEXT, but in flag context the promotion buys nothing: a mask
	 * under 256 zeroes the high byte whatever the extension put
	 * there - zeroes for WIDEN, the sign for SEXT, either way
	 * nothing survives the AND.  Dropping the widening is what lets
	 * the byte rules see the test at all, and one of those rules is
	 * the bit instruction.  "flags & TF_X" is the most repeated
	 * expression in both compilers, and every one of them was
	 * eleven instructions of word arithmetic.
	 */
	if (e->op == AND && e->dest == DEST_FLAGS &&
	    e->right && e->right->op == NUMBER &&
	    (e->right->u.val & ~0xffL) == 0 &&
	    e->left && (e->left->op == WIDEN || e->left->op == SEXT) &&
	    e->left->left && ISBYTE(e->left->left->width)) {
		Expr *w = e->left;

		e->left = w->left;
		w->left = NULL;
		freeexpr(w);
		e->width = e->left->width;
		e->right->width = e->left->width;
	}
	/*
	 * What an assignment stores is wanted as a value whatever is done
	 * with the assignment itself, and "i = k = 5" needs the inner one
	 * to know it: a store rule that writes straight to memory leaves
	 * nothing for the outer assignment to copy, and only the value
	 * context tells it to pay for a register.
	 */
	if (e->op == ASSIGN && e->right && e->right->dest == DEST_NONE)
		e->right->dest = DEST_VALUE;
	/*
	 * And the left, when it is not simply a place.  An lvalue that
	 * has to be worked out is an address, and an address is a value:
	 * "*p++ = 7" wants the pointer from before the step.  Read as
	 * discarded, the step took the form that does not bother
	 * producing one - which for a step of more than one is a
	 * compound assignment that never undoes itself - and the store
	 * went through the pointer after the step instead of before.
	 */
	if (e->op == ASSIGN && e->left && !islocdesc(e->left) &&
	    e->left->dest == DEST_NONE)
		e->left->dest = DEST_VALUE;
	/*
	 * A conversion is transparent: what it converts is wanted exactly
	 * as much as the conversion is.  Without this the destination
	 * stopped at the widening and everything under it read as
	 * discarded - so "r = g++" took the form of the step that throws
	 * its value away, and then widened the address it had left in HL.
	 */
	if ((e->op == SEXT || e->op == WIDEN) && e->left &&
	    e->left->dest == DEST_NONE)
		e->left->dest = e->dest;
	/*
	 * A comma's left really is discarded - that is what it is for -
	 * but its right is the value, and is wanted exactly as much as the
	 * comma itself.  Left out of the rule below along with the left,
	 * it read as discarded: a constant or a frame slot there never
	 * paid for a register, and the comma rules, which do nothing but
	 * say where the value ended up, had nothing to name.
	 */
	if (e->op == COMMA && e->right && e->right->dest == DEST_NONE)
		e->right->dest = e->dest;
	/*
	 * The two above are the same rule found twice, one operator at a
	 * time, so here it is once: what an operator operates on is
	 * wanted as a value.  A node's destination starts as DEST_NONE
	 * and only a statement root is ever told otherwise, so an operand
	 * nobody had spoken for read as discarded - and a rule asking for
	 * statement context with :S would match it.  "uc++ != 5" took the
	 * statement form of the step, which increments the byte in memory
	 * and leaves HL holding its address, and then compared the
	 * address against five.
	 *
	 * Not an assignment, whose left side is a place rather than a
	 * value and whose right side is handled above.  Not a comma,
	 * whose left side really is discarded - that is what it is for.
	 * And not the compound assignments, which are an assignment
	 * wearing an operator's clothes.
	 */
	if (e->op != ASSIGN && e->op != COMMA && !baseop(e->op) &&
	    e->op != PREINC && e->op != POSTINC &&
	    e->op != PREDEC && e->op != POSTDEC) {
		if (e->left && e->left->dest == DEST_NONE)
			e->left->dest = DEST_VALUE;
		if (e->right && e->right->dest == DEST_NONE)
			e->right->dest = DEST_VALUE;
	}
	normtree(e->left);
	normtree(e->right);
}

/*
 * Flip flag code: Z<->NZ, C<->NC
 */
static unsigned char
flipflag(unsigned char f)
{
	switch (f) {
	case F_Z:  return F_NZ;
	case F_NZ: return F_Z;
	case F_C:  return F_NC;
	case F_NC: return F_C;
	case F_M:  return F_P;
	case F_P:  return F_M;
	}
	return f;
}

static int
isflag(unsigned char r)
{
	return r >= F_Z && r <= F_P;
}

/*
 * Turn a condition into the number 0 or 1 in A, for when a comparison
 * was wanted as a value rather than as a branch.
 *
 * Carry is nearly free: ld a,0 leaves the flags alone, so adc a,a adds
 * the carry into a cleared A - three bytes, and one more for a ccf to
 * take the inverse.  The others need a branch over an inc, and jr can
 * only test NZ/Z/NC/C, so sign has to go through jp.
 */
static void
matflag(unsigned char r)
{
	switch (r) {
	case F_C:
		out("\tld a,0\n\tadc a,a\n");
		return;
	case F_NC:
		out("\tccf\n\tld a,0\n\tadc a,a\n");
		return;
	/* jr is 2 bytes, so $+3 clears the inc; jp is 3, so $+4 does */
	case F_Z:
		out("\tld a,0\n\tjr nz,$+3\n\tinc a\n");
		return;
	case F_NZ:
		out("\tld a,0\n\tjr z,$+3\n\tinc a\n");
		return;
	case F_M:
		out("\tld a,0\n\tjp p,$+4\n\tinc a\n");
		return;
	case F_P:
		out("\tld a,0\n\tjp m,$+4\n\tinc a\n");
		return;
	}
}

/*
 * Apply one rewrite step to node (not children)
 * Returns new node if changed, NULL if no change
 */
static Expr *
step(Expr *e)
{
	struct rule *rp;
	Expr *n;

	if (!e) return NULL;

	normalize(e);

	/* BANG(CODE) in flag context: flip the flag */
	if (e->op == BANG && e->dest == DEST_FLAGS &&
	    e->left && e->left->op == CODE) {
		n = e->left;
		n->u.var.reg = flipflag(n->u.var.reg);
		n->dest = e->dest;
		e->left = NULL;
		freeexpr(e);
		return n;
	}

	/*
	 * EQ(x, 0) in flag context: testing x is cheaper than comparing
	 * against zero, but the test is true when x is nonzero, so this
	 * is !x - the BANG below flips the flag once x has been reduced.
	 */
	if (e->op == EQ && e->dest == DEST_FLAGS &&
	    e->right && e->right->op == NUMBER && e->right->u.val == 0) {
		n = e->left;
		n->dest = DEST_FLAGS;
		e->left = NULL;
		freeexpr(e);
		n = mkunary(BANG, 'b', n);
		n->dest = DEST_FLAGS;
		return n;
	}

	/*
	 * Shift by a count only known at runtime.  The Z80 has no
	 * variable shift, so it loops - add hl,hl shifts left by one,
	 * srl/sra h with rr l shifts right, arithmetic or logical
	 * depending on the sign of the value.
	 *
	 * The count arrives in A already, or in E as a byte, or in DE as
	 * a word of which only the low byte can matter; E is the low half
	 * of DE, so the last two are the same load.  The zero guard is
	 * not optional: C defines "x << 0" as x, and the loop body would
	 * otherwise run once.
	 */
	if ((e->op == LSHIFT || e->op == RSHIFT) &&
	    e->left && (e->left->op == INHL || e->left->op == INBC ||
	     (e->left->op == NUMBER && !ISBYTE(e->width))) &&
	    e->right &&
	    (e->right->op == INA || e->right->op == INE ||
	     e->right->op == INDE)) {
		/*
		 * The value shifts in HL, so a register variable comes over
		 * first - the same move the constant-count rules make with
		 * T_BC_HL.  Without it "v <<= n" on a variable the allocator
		 * had put in BC matched nothing here and had no rule either,
		 * and emitted nothing at all.
		 *
		 * A constant left never reduces - it stays a NUMBER for the
		 * ",N)" rules - so "1 << i" reached this point with nothing
		 * in HL and matched nothing, silently.  ispow2 in this very
		 * file is built from that shape, so the self-hosted c1
		 * answered "not a power of two" to everything and multiplied
		 * by 2 with a helper call.
		 */
		if (e->left->op == INBC)
			out("\tld l,c\n\tld h,b\n");
		else if (e->left->op == NUMBER)
			outf("\tld hl,%d\n", (int)e->left->u.val);
		if (e->right->op != INA)
			out("\tld a,e\n");
		/*
		 * $ is the address of the instruction it appears in, so the
		 * displacements below are counted from each jr itself:
		 *
		 *   J+0  jr z    2   skip the loop entirely
		 *   J+2  body    1 for add hl,hl, 4 for the CB-prefixed pair
		 *   J+3  dec a   1
		 *   J+4  jr nz   2   back to the body at J+2
		 *   J+6  ...
		 *
		 * Recount both if the body ever changes size.
		 */
		out("\tor a\n");
		if (e->op == LSHIFT)
			out("\tjr z,$+6\n\tadd hl,hl\n");
		else if (ISSIGNED(e->width))
			out("\tjr z,$+9\n\tsra h\n\trr l\n");
		else
			out("\tjr z,$+9\n\tsrl h\n\trr l\n");
		out("\tdec a\n");
		out(e->op == LSHIFT ? "\tjr nz,$-2\n" : "\tjr nz,$-5\n");

		return donehl(e, INHL);
	}

	/* CALL is handled up in rewrite1() - args must be pushed one at a
	 * time, before the children are batch-rewritten. */

	/* Long unary operations */
	if ((e->width == 'l' || e->width == 'L') && e->left) {
		/* Long complement: ~HLDE */
		if (e->op == NOT && e->left->op == INHL) {
			out("\tcall lcom\n");
			return donehl(e, CODE);
		}
		/* Long negation: -HLDE (two's complement) */
		if (e->op == NEG && e->left->op == INHL) {
			/* Negate DE, then HL with borrow */
			out("\txor a\n\tsub e\n\tld e,a\n"
			    "\tld a,0\n\tsbc a,d\n\tld d,a\n"
			    "\tld a,0\n\tsbc a,l\n\tld l,a\n"
			    "\tld a,0\n\tsbc a,h\n\tld h,a\n");
			return donehl(e, CODE);
		}
		/*
		 * Long shift by a count worked out at runtime.  The three
		 * forms - left, right signed, right unsigned - ask the same
		 * question of the tree and differ only in which helper they
		 * call.  The enclosing test has already established that the
		 * width is long, so 'l' against 'L' is the sign.  The count
		 * arrives in A and the helpers want it in B.
		 */
		if ((e->op == LSHIFT || e->op == RSHIFT) &&
		    e->left->op == INHL && e->right && e->right->op == INA) {
			out("\tld b,a\n");
			out(e->op == LSHIFT ? "\tcall lllsh\n" :
			    e->width == 'l' ? "\tcall alrsh\n" : "\tcall llrsh\n");
			return donehl(e, CODE);
		}
	}

	/* CODE -> INHL/INDE/INBC/INA/INE: convert to typed register nodes */
	if (e->op == CODE) {
		unsigned char reg = e->u.var.reg;
		if (reg == R_HL) e->op = INHL;
		else if (reg == R_DE) e->op = INDE;
		else if (reg == R_BC) e->op = INBC;
		else if (reg == R_A) e->op = INA;
		else if (reg == R_E) e->op = INE;
		else if (isflag(reg) && e->dest != DEST_FLAGS) {
			/* the condition was wanted as a number, not a jump */
			matflag(reg);
			e->op = INA;
			e->width = 'B';
			e->u.var.reg = R_A;
		}
		else goto no_regconv;
		return e;
	}
no_regconv:

	for (rp = rules; rp->pat; rp++) {
		n = tryrule(rp, e);
		if (n) {
#ifdef DEBUG
			rulehit(rp - rules);
#endif
			return n;
		}
	}
	return NULL;  /* no change */
}

/*
 * Rewrite node: depth-first, fixed-point at each level
 * ARGNODE handled specially: right chain processed after push
 */
/*
 * Does this node name a location outright, rather than work out an
 * address that a store then has to go through?
 */
static int
islocdesc(Expr *e)
{
	if (!e)
		return 0;
	switch (e->op) {
	case REGVAR:
	case LOCALVAR:
	case INDEX:
	case SYM:
	case SYMREF:
		return 1;
	}
	return 0;
}

/*
 * Did this subtree actually reduce?  A tree that did is a single
 * register node with nothing under it; anything else means a rule was
 * missing somewhere below and no code came out for it.  The root
 * check at the end of rewrite() asks the same question - this is that
 * test, named, so the places that have to ask it earlier can.
 */
static int
reduced(Expr *e)
{
	if (!e)
		return 0;
	if (e->left || e->right)
		return 0;
	switch (e->op) {
	case CODE:
	case INHL:
	case INDE:
	case INBC:
	case INA:
	case INE:
		return 1;
	}
	return 0;
}

/*
 * A register standing on the left of an assignment as the destination
 * itself, rather than as somewhere to store through - which is how
 * RETURN and the call-argument wrapper ask for a value in a given
 * register.  Only meaningful before the lvalue is reduced; afterwards
 * the same node means an address that was worked out.
 */
static int
isdestreg(Expr *e)
{
	if (!e)
		return 0;
	switch (e->op) {
	case INHL:
	case INDE:
	case INBC:
	case INA:
	case INE:
	case CODE:
		return 1;
	}
	return 0;
}

/*
 * Base operator behind a compound assignment: += is +, and so on.
 * Returns 0 for anything that is not a compound assignment.
 */
static unsigned char
baseop(unsigned char op)
{
	switch (op) {
	case PLUSEQ:   return PLUS;
	case SUBEQ:    return MINUS;
	case MULTEQ:   return STAR;
	case DIVEQ:    return DIV;
	case MODEQ:    return MOD;
	case RSHIFTEQ: return RSHIFT;
	case LSHIFTEQ: return LSHIFT;
	case ANDEQ:    return AND;
	case OREQ:     return OR;
	case XOREQ:    return XOR;
	}
	return 0;
}

/*
 * Does evaluating this tree do anything besides produce a value?
 */
static int
sideeffect(Expr *e)
{
	if (!e)
		return 0;
	switch (e->op) {
	case CALL:
	case ASSIGN:
	case PREINC:
	case POSTINC:
	case PREDEC:
	case POSTDEC:
		return 1;
	}
	if (baseop(e->op))
		return 1;
	return sideeffect(e->left) || sideeffect(e->right);
}

/*
 * Can this lvalue be named twice?  The expansion below reads the
 * location and then writes it, so the location expression appears
 * twice.  This runs before anything has been emitted for the subtree,
 * so a second copy costs at most some recomputed address arithmetic -
 * but a second call, or a second POSTINC, would be a real bug, which
 * is what "evaluate the lvalue once" forbids.
 *
 * pass1 keeps the DEREF on an lvalue, so a bare REGVAR here really is
 * the variable ("i += 5" with i in BC) and DEREF(REGVAR) is the memory
 * it points at ("*p += 10").  Both are fine to name twice.
 */
static int
dupableloc(Expr *e)
{
	return e && !sideeffect(e);
}

/*
 * The same location read rather than written.  A location and the
 * value in it are different expressions, and the difference is one
 * load for every level of indirection:
 *
 *   REGVAR      the register is the storage - it already is the value
 *   DEREF(a)    the memory at a, so the value is a load from a's
 *               value - and a is a location in its own right, so
 *               this has to recur
 *   everything else (SYMREF, LOCALVAR, or an address the tree works
 *               out for itself) names a location, so reading it takes
 *               a load
 *
 * The recursion is the whole point.  "*p" is DEREF(REGVAR) when p is
 * in a register and DEREF(LOCALVAR) when it is in the frame; the
 * first already reads memory, while the second reads only the frame
 * slot and still owes the load the pointer stands for.  Treating the
 * two alike read the pointer and used it as the value.
 */
static Expr *
locvalue(Expr *e, char w)
{
	if (e->op == REGVAR)
		return e;
	if (e->op == DEREF) {
		/* what it points through is an address: word sized */
		e->left = locvalue(e->left, 's');
		return e;
	}
	return mkunary(DEREF, w, e);
}

/*
 * Lower "x OP= y" to "x = x OP y".  This has to run before the children
 * are reduced: once the lvalue has become a register, copying it would
 * re-emit whatever code produced it.
 */
static Expr *
lowercompound(Expr *e)
{
	unsigned char op = baseop(e->op);
	char w;
	Expr *loc, *val, *rhs;

	if (!op || !dupableloc(e->left))
		return NULL;

	w = e->width;
	loc = e->left;
	rhs = e->right;
	e->left = e->right = NULL;
	freeexpr(e);

	val = locvalue(dupexpr(loc), w);

	return mkbinary(ASSIGN, w, loc, mkbinary(op, w, val, rhs));
}

/*
 * One arm of a ternary, landed in HL.  Wrapping it in ASSIGN(INHL, v)
 * reuses the whole =(H,...) rule set - which is what makes a constant
 * arm emit anything at all, and what puts both arms in one register so
 * the expression has a value wherever the branch went.
 */
static void
branchval(Expr *v)
{
	Expr *hl, *asn;

	if (!v)
		return;
	hl = mkcode(v->width, R_HL);
	hl->op = INHL;
	asn = mkbinary(ASSIGN, v->width, hl, v);
	setdest(asn, DEST_VALUE);
	freeexpr(rewrite(asn));
}

/*
 * The condition code that branches when a condition is false, which is
 * the inverse of the flag the condition produced.  A comparison leaves
 * one of Z/NZ/C/NC/M/P; anything else came back as a value and has to
 * be tested for zero first, which this emits.
 *
 * Both the if statement and the ternary need this.  The ternary used
 * to assume Z meant false, which is only true of a value that has just
 * been tested - a comparison leaves its answer somewhere else, and
 * loading a register leaves the flags alone entirely.
 */
/*
 * A constant operand sets no flags, and u.var.reg on a NUMBER reads
 * the value's own bytes out of the union - so the switch below picked
 * a condition code out of the constant and emitted no test at all.
 * "x && 1" branched on whatever the last instruction happened to
 * leave.  Only && and || and the ternary reach here with a bare
 * NUMBER: an if() goes through rewrite(), which folds a constant
 * condition at the root.
 */
static int
constflag(Expr *e)
{
	if (!e || e->op != NUMBER)
		return 0;
	out("\txor a\n");
	if (e->u.val)
		out("\tinc a\n");
	return 1;
}

/*
 * A condition that never reduced still reaches the cc functions, and
 * the union field they read is garbage there - the branch goes on
 * whatever flags were lying around.  "p > q" between two register
 * homes did exactly that, silently.  Say so in the output: it is
 * only a comment, but it is what the differential greps for.
 */
static void
ccguard(Expr *e)
{
	unsigned char op;

	if (!e)
		return;
	op = e->op;
	if (op == CODE || op == REGVAR || op == NUMBER ||
	    op == INA || op == INHL || op == INDE ||
	    op == INBC || op == INE)
		return;
	out("; XXXXXX unreduced condition\n");
}

char *
falsecc(Expr *e)
{
	if (constflag(e))
		return "z";
	ccguard(e);
	switch (e ? e->u.var.reg : 0) {
	case F_Z:  return "nz";
	case F_NZ: return "z";
	case F_C:  return "nc";
	case F_NC: return "c";
	case F_M:  return "p";
	case F_P:  return "m";
	case R_A:  out("\tor a\n"); return "z";
	case R_HL: out("\tld a,l\n\tor h\n"); return "z";
	case R_DE: out("\tld a,e\n\tor d\n"); return "z";
	case R_BC: out("\tld a,c\n\tor b\n"); return "z";
	}
	return "z";
}

/*
 * The other half: the condition that branches when a condition is
 * true.  Same shape, same zero test where the answer came back as a
 * value rather than a flag.
 */
char *
truecc(Expr *e)
{
	if (constflag(e))
		return "nz";
	ccguard(e);
	switch (e ? e->u.var.reg : 0) {
	case F_Z:  return "z";
	case F_NZ: return "nz";
	case F_C:  return "c";
	case F_NC: return "nc";
	case F_M:  return "m";
	case F_P:  return "p";
	case R_A:  out("\tor a\n"); return "nz";
	case R_HL: out("\tld a,l\n\tor h\n"); return "nz";
	case R_DE: out("\tld a,e\n\tor d\n"); return "nz";
	case R_BC: out("\tld a,c\n\tor b\n"); return "nz";
	}
	return "nz";
}

/*
 * Both operands are bytes and both have to be worked out.  A is the
 * only byte register the ALU works in, so there is no second one to
 * assign and reducing the right operand lands it on top of the left.
 * Nothing downstream notices: the tree reads (A) op (A), no rule
 * matches a register against itself, and the code that came out
 * applied the operator to the right operand and whatever was left
 * over.
 *
 * So the left waits on the stack while the right is worked out, and
 * the right comes back to E - the second operand every byte rule
 * already expects.  Q(A,K), +(A,K) and the rest of that family were
 * there all along with nothing able to reach them.
 *
 * Three bytes, and push af rather than a spare register because there
 * is none: B and C may hold a register variable, and D and E are where
 * the answer is going.  The flags ride along and come back, which
 * costs nothing - the operator sets its own.
 */
static void
bytepair(Expr *e)
{
	char w;

	out("\tpush af\n");
	e->right = rewrite1(e->right);
	if (e->right && e->right->op == INA) {
		w = e->right->width;
		out("\tld e,a\n");
		freeexpr(e->right);
		e->right = mkcode(w, R_E);
		e->right->op = INE;
	}
	out("\tpop af\n");
}

/*
 * A reduced operand does not always land where it was asked to: a byte
 * operation can only end in A and a call only in HL, whatever target
 * they were given.  Move it, for the cases where something else is
 * about to want that register.
 *
 * Only safe while the other operand has not been evaluated yet - the
 * HL form goes through ex de,hl, which would trample it.
 */
static Expr *
movetotgt(Expr *e, unsigned char tgt)
{
	if (!e || tgt != R_DE)
		return e;
	if (e->op == INA) {
		out("\tld e,a\n");
		e->op = INE;
		e->u.var.reg = R_E;
	} else if (e->op == INHL) {
		out("\tex de,hl\n");
		e->op = INDE;
		e->u.var.reg = R_DE;
	}
	return e;
}

/*
 * Compound assignment whose lvalue has side effects, so the expansion
 * in lowercompound() cannot be used - naming "*p++" twice would
 * increment p twice.  The address is worked out once and waits on the
 * stack while the value is read, updated and written back, the stack
 * standing in for the temporary the expression tree has no way to
 * spell.
 *
 * Returns NULL if the address did not reduce to HL, leaving the node
 * to be flagged rather than guessed at.
 */
static Expr *
docompound(Expr *e)
{
	unsigned char op = baseop(e->op);
	char w = e->width;
	int isbyte = ISBYTE(w);
	Expr *addr, *val, *rhs, *sum, *n;

	if (!op || !e->left || !e->right)
		return NULL;

	/*
	 * The address, once - the side effects happen here and only here.
	 * Its value is wanted, and has to say so: a postfix step inside
	 * it yields the value from before the step, and anything deciding
	 * that from the destination would otherwise read "discarded" and
	 * be free to hand back the one after.
	 */
	setdest(e->left, DEST_VALUE);
	addr = rewrite1(e->left);
	e->left = NULL;
	if (!addr || (addr->op != INHL &&
	    !(addr->op == CODE && addr->u.var.reg == R_HL))) {
		e->left = addr;
		return NULL;
	}
	freeexpr(addr);
	out("\tpush hl\n");

	/* read through it */
	if (isbyte)
		out("\tld a,(hl)\n");
	else
		out("\tld a,(hl)\n\tinc hl\n\tld h,(hl)\n\tld l,a\n");

	/* apply the operator to what was there */
	val = mkcode(w, isbyte ? R_A : R_HL);
	val->op = isbyte ? INA : INHL;
	rhs = e->right;
	e->right = NULL;
	sum = mkbinary(op, w, val, rhs);
	setdest(sum, DEST_VALUE);
	/*
	 * The left is already a byte in A.  Working the right side out
	 * lands byte loads in A too - on top of the value - and the
	 * fresh nodes here carry no labels, so the register machinery
	 * that usually notices never runs.  bytepair() is exactly this
	 * situation's tool: park A on the stack, reduce the right into
	 * E, take A back.
	 */
	if (isbyte && rhs && rhs->op != NUMBER && !reduced(rhs))
		bytepair(sum);
	sum = rewrite(sum);
	/*
	 * The store below asserts the answer landed in A or HL.  When no
	 * rule reduced the operator that assertion stored garbage - the
	 * count of a shift, whatever a half-finished reduction left in
	 * the register - and said nothing.  The else-if bitmap bug rode
	 * exactly that silence one level up; this path gets the marker
	 * it always owed.
	 */
	if (!sum || (isbyte ? sum->op != INA : sum->op != INHL)) {
		out("; XXXXXX incomplete: compound rhs");
#ifdef DEBUG
		if (sum) {
			out(" ");
			dumpexpr(sum);
		}
#endif
		outc('\n');
	}

	/* store it back through the address that was waiting */
	if (isbyte) {
		out("\tpop hl\n\tld (hl),a\n");
		n = mkcode(w, R_A);
		n->op = INA;
	} else {
		out("\tpop de\n\tex de,hl\n\tld (hl),e\n\tinc hl\n\tld (hl),d\n\tex de,hl\n");
		n = mkcode(w, R_HL);
		n->op = INHL;
	}
	freeexpr(sum);
	n->dest = e->dest;
	freeexpr(e);
	return n;
}

/*
 * Push one call argument, returning the stack bytes it consumed.
 * Wrapping the value in ASSIGN(INHL, value) reuses the whole =(H,...)
 * rule set to land it in HL, the same trick RETURN uses.  Scalars are
 * widened to a word first: C promotes char arguments to int.
 */
static int
pusharg(Expr *a)
{
	Expr *hl, *asn;
	char w;

	if (!a)
		return 0;
	w = a->width;
	if (w != 'l' && w != 'L')
		w = 's';

	hl = mkcode(w, R_HL);
	hl->op = INHL;
	asn = mkbinary(ASSIGN, w, hl, a);
	setdest(asn, DEST_VALUE);
	freeexpr(rewrite(asn));

	if (w == 's') {
		out("\tpush hl\n");
		return 2;
	}
	/* longs live in HL:DE, high word first */
	out("\tpush hl\n\tpush de\n");
	return 4;
}

/* a 32-bit integer, which the helpers handle */
#define ISLONGINT(t) ((t) == T_LONG || (t) == T_ULONG)

/*
 * Did this reduce to a 32-bit value sitting in HL:DE?  A rule hands one
 * back as a register node, or as a CODE that has not been converted
 * yet; either way the high word is in HL.
 */
static int
islongreg(Expr *e)
{
	if (!e)
		return 0;
	if (e->op == INHL)
		return 1;
	return e->op == CODE && e->u.var.reg == R_HL;
}

static char *longhelper(unsigned char op, int sign);

/*
 * Is this one of the operators the long path above is responsible for?
 * Only those, because only those have no rules of their own to fall
 * back on - assignment and the conversions are handled in the table
 * and must be left alone to reach it.
 *
 * A comparison is a byte wide whatever it compared, so it is the
 * operand that says whether this is 32-bit work.
 */
static int
islongop(Expr *e)
{
	if (!e || !e->left || !e->right)
		return 0;
	switch (e->op) {
	case PLUS: case MINUS: case STAR: case DIV: case MOD:
	case AND: case OR: case XOR: case LSHIFT: case RSHIFT:
		return ISLONGINT(e->width);
	case EQ: case NEQ: case LT: case GT: case LE: case GE:
		return ISLONGINT(e->left->width);
	}
	return 0;
}

/*
 * The two halves of a 32-bit constant, high word first.  A constant
 * operand never reduces - it stays a NUMBER so the ",N)" rules can see
 * it - so the long path has to place one itself.
 */
static void
loadlongc(long v)
{
	outf("\tld hl,%d\n\tld de,%d\n",
	    (int)((v >> 16) & 0xffff), (int)(v & 0xffff));
}

static void
pushlongc(long v)
{
	outf("\tld hl,%d\n\tpush hl\n\tld hl,%d\n\tpush hl\n",
	    (int)((v >> 16) & 0xffff), (int)(v & 0xffff));
}

/*
 * Will this operand end up as a 32-bit value in HL:DE?  Asked before
 * any code is emitted, because the first operand of a helper call goes
 * on the stack and there is no backing out after that.
 *
 * A constant is placed directly.  Anything else has to be a shape the
 * long rules cover, or something that reduces to one - a nested long
 * operator, or a widening of something narrower.
 */
static int
longable(Expr *e)
{
	if (!e)
		return 0;
	if (e->op == NUMBER || islongreg(e))
		return 1;
	if (!ISLONGINT(e->width))
		return 0;
	switch (e->op) {
	case DEREF:
	case SEXT:
	case WIDEN:
	case CALL:
		return 1;
	case NOT:
		/* complement has a rule of its own */
		return 1;
	case LSHIFT:
	case RSHIFT:
		/* handled here too, but by their own path - the count is not
		 * a long and longhelper has no name for them */
		return e->left && e->right;
	}
	/* a nested long operator, which recurses through this same path */
	return longhelper(e->op, 1) != NULL && e->left && e->right;
}

/*
 * The runtime helper for a 32-bit operator, or NULL if there is none.
 *
 * The library names these by what the operation means rather than what
 * it is: the "a" forms are arithmetic and treat the top bit as a sign,
 * the "ll" forms are logical and treat it as a value.  Add, subtract
 * and the bitwise operators are the same either way and the library
 * points both names at one routine; divide, remainder, right shift and
 * comparison genuinely differ.
 */
static char *
longhelper(unsigned char op, int sign)
{
	switch (op) {
	case PLUS:   return "ladd";
	case MINUS:  return sign ? "alsub" : "llsub";
	case STAR:   return sign ? "almul" : "llmul";
	case DIV:    return sign ? "aldiv" : "lldiv";
	case MOD:    return sign ? "almod" : "llmod";
	case AND:    return sign ? "aland" : "lland";
	case OR:     return sign ? "alor"  : "llor";
	case XOR:    return sign ? "alxor" : "llxor";
	case EQ: case NEQ: case LT: case GT: case LE: case GE:
		return sign ? "arelop" : "lrelop";
	}
	return NULL;
}

/*
 * The flag a comparison helper leaves the answer in.
 *
 * arelop reports a signed comparison the way a subtraction would, in
 * the sign flag; lrelop reports an unsigned one in carry, where the
 * 16-bit code looks for it.  Neither gives "greater" or "at or below"
 * as one flag, so those are had by swapping the operands, which the
 * caller does by handing them over the other way round.
 */
static unsigned char
longflag(unsigned char op, int sign)
{
	switch (op) {
	case EQ:  return F_Z;
	case NEQ: return F_NZ;
	case LT: case GT:  return sign ? F_M : F_C;
	case GE: case LE:  return sign ? F_P : F_NC;
	}
	return 0;
}

/*
 * Widen an operand to a long, if it is not one already.  Signed sources
 * sign-extend and unsigned ones do not, which is the whole difference
 * between the SEXT and WIDEN rules at long width.  A constant is left
 * as it is: the callers below load and push one directly, at whatever
 * width it was written.
 */
static Expr *
tolong(Expr *e, char w)
{
	Expr *n;

	if (!e || e->op == NUMBER || ISLONGINT(e->width))
		return e;
	n = mkunary(ISSIGNED(e->width) ? SEXT : WIDEN, w, e);
	n->dest = DEST_VALUE;
	return n;
}

/*
 * A binary operator on 32-bit values.  Both operands want HL:DE and
 * there is only one of those, so the right one is worked out first and
 * pushed - which is also what the helpers expect: left in HL:DE, right
 * on the stack with its high word pushed first.
 *
 * "Greater than" and "at or below" have no flag of their own, so they
 * are passed over as "less than" and "at or above" with the operands
 * the other way round.  Evaluation order is already unspecified here,
 * and the reorder elsewhere in this file relies on the same thing.
 */
static Expr *
dolongbin(Expr *e)
{
	unsigned char op = e->op;
	int iscmp = (longflag(op, 1) != 0);
	Expr *opnd = iscmp ? e->left : e;
	int swap = (op == GT || op == LE);
	int savebc = bcinuse();
	int sign;
	char *fn;
	Expr *l, *r, *n;

	if (!e->left || !e->right)
		return NULL;
	/*
	 * A comparison carries the width of what it answers, not of what
	 * it compares, so the operands are where to look - and either one
	 * of them may be the long.  Reading only the left meant "s < l"
	 * was judged by s and declined here, while "l < s" got as far as
	 * the gate below and was declined there instead.
	 */
	if (iscmp && !ISLONGINT(opnd->width) && ISLONGINT(e->right->width))
		opnd = e->right;
	if (!opnd || !ISLONGINT(opnd->width))
		return NULL;
	sign = ISSIGNED(opnd->width);

	l = swap ? e->right : e->left;
	r = swap ? e->left : e->right;

	/*
	 * A shift is not like the rest: the count is a plain int and goes
	 * in B rather than on the stack.  Work it out first and park it,
	 * because reducing the value may call a helper of its own and
	 * those use BC.
	 */
	if (op == LSHIFT || op == RSHIFT) {
		if (!longable(l))
			return NULL;
		e->left = e->right = NULL;
		if (savebc)
			out("\tpush bc\n");
		if (r->op == NUMBER) {
			if (l->op == NUMBER)
				loadlongc(l->u.val);
			else
				l = rewrite1(l);
			outf("\tld b,%d\n", (int)(r->u.val & 0xff));
		} else {
			/*
			 * The count is an ordinary int and would have been aimed
			 * at DE, being the right operand of a binary node.  It
			 * has to come back in HL: that is where this parks it,
			 * and DE belongs to the value.
			 */
			assign(r, R_HL);
			r = rewrite1(r);
			out("\tpush hl\n");
			if (l->op == NUMBER)
				loadlongc(l->u.val);
			else
				l = rewrite1(l);
			/* the count came back in HL, so its low byte is in C */
			out("\tpop bc\n\tld b,c\n");
		}
		freeexpr(l);
		freeexpr(r);
		outf("\tcall %s\n",
		    op == LSHIFT ? "allsh" : sign ? "alrsh" : "lushr");
		if (savebc)
			out("\tpop bc\n");
		return donehl(e, INHL);
	}

	fn = longhelper(op, sign);
	if (!fn)
		return NULL;

	/*
	 * A long against something narrower.  C converts the narrow side,
	 * and the rules for that conversion are already in the table - what
	 * was missing is that nobody put the conversion in the tree, so the
	 * gate below saw an operand that could not be made to land in HL:DE
	 * and declined.  A constant is left alone: pushlongc and loadlongc
	 * below take one at whatever width it is written.
	 */
	l = tolong(l, opnd->width);
	r = tolong(r, opnd->width);

	/*
	 * Both operands have to be shapes that end up in HL:DE, and that
	 * has to be settled before anything is emitted - once the right
	 * operand has been pushed there is no way back.
	 */
	if (!longable(l) || !longable(r))
		return NULL;

	e->left = e->right = NULL;

	/*
	 * The helper takes its right operand off the stack with a pop bc,
	 * so it destroys a register variable living there.  Save it
	 * underneath the operand: the helper consumes exactly the two
	 * words it was passed, so the copy is on top again when it
	 * returns.  Only worth it where BC actually holds something.
	 */
	if (savebc)
		out("\tpush bc\n");

	/*
	 * Both want HL:DE, so neither can keep the target it was given as
	 * one side of a binary node - the right operand would have been
	 * aimed at DE, which is half of where it has to land.
	 */
	if (r->op == NUMBER) {
		pushlongc(r->u.val);
	} else {
		assign(r, R_HL);
		r = rewrite1(r);
		out("\tpush hl\n\tpush de\n");
	}
	freeexpr(r);

	/* then the left, which the helper wants where it lands */
	if (l->op == NUMBER) {
		loadlongc(l->u.val);
	} else {
		assign(l, R_HL);
		l = rewrite1(l);
	}
	freeexpr(l);

	outf("\tcall %s\n", fn);
	if (savebc)
		out("\tpop bc\n");

	if (iscmp) {
		/*
		 * A comparison answers in a flag, and this returns straight
		 * to the caller rather than through the loop that would
		 * otherwise turn one into a number where a number is what
		 * was asked for.  So do it here: everything downstream then
		 * sees an ordinary byte, and a long comparison can be
		 * assigned and widened like any other.
		 */
		unsigned char f = longflag(op, sign);

		if (e->dest == DEST_FLAGS) {
			n = mkcode(e->width, f);
		} else {
			matflag(f);
			n = mkcode('B', R_A);
			n->op = INA;
		}
	} else {
		n = mkcode(e->width, R_HL);
		n->op = INHL;
	}
	n->dest = e->dest;
	freeexpr(e);
	return n;
}

/*
 * Emit a call: arguments pushed right-to-left, then the call, then the
 * caller drops the arguments.  The arg chain is already built
 * last-to-first, which is push order.
 */
static Expr *
docall(Expr *e)
{
	Expr *a, *next, *fn;
	int nbytes = 0;
	int i;
	int direct;
	int savebc, argwrites;

	/*
	 * A name resolves to a SYMREF and emits nothing, so it can be
	 * settled now.  Anything else is a pointer whose value has to be
	 * loaded, and loading it here would only see it trampled by the
	 * argument pushes - so it waits.
	 */
	fn = e->left;
	direct = fn && (fn->op == SYM || fn->op == SYMREF);
	if (direct) {
		e->left = fn = rewrite1(fn);
		if (!fn || fn->op != SYMREF)
			return NULL;
	}

	/*
	 * BC does not survive a call.
	 *
	 * ccc's own prologue saves it, so ccc calling ccc is consistent -
	 * but the runtime library is built by zc3, which uses BC as
	 * scratch and never saves it.  csv and ncsv save IX and IY and
	 * nothing else, zc3's output for a function that uses BC has no
	 * push of it anywhere, and fputc.s pops its argument straight
	 * into BC and returns with it still there.  So a register
	 * variable in BC is gone across any call into the library:
	 *
	 *	for (i = 0; i < argc; i++)	i came back holding the
	 *		printf("%s\n", argv[i]);   last character printed
	 *
	 * The helpers had this noticed once and got $[ and $]; ordinary
	 * calls are the commoner case and never did.
	 *
	 * Saved before the arguments, so that dropping them afterwards
	 * uncovers it.  But an argument may be what changes the variable
	 * - "one(i += 4)" - and then the value to keep is the one the
	 * arguments left, not the one saved before them.  So where any
	 * argument writes anything, the saved copy is refreshed just
	 * before the call.  Most calls do not, and pay nothing for it.
	 */
	savebc = bcinuse();
	argwrites = 0;
	if (savebc) {
		for (a = e->right; a && a->op == ARGNODE; a = a->right)
			if (sideeffect(a->left)) {
				argwrites = 1;
				break;
			}
		out("\tpush bc\n");
	}

	for (a = e->right; a && a->op == ARGNODE; a = next) {
		Expr *v = a->left;
		next = a->right;
		a->left = a->right = NULL;
		freeexpr(a);
		nbytes += pusharg(v);
	}
	e->right = NULL;

	/* the arguments may have moved it - the saved copy sits under
	 * them, nbytes down */
	if (savebc && argwrites) {
		outf("\tld hl,%d\n\tadd hl,sp\n\tld (hl),c\n\tinc hl\n\tld (hl),b\n",
		    nbytes);
	}

	if (direct) {
		outf("\tcall %s\n", fn->u.symref.name);
	} else {
		/*
		 * Through a pointer.  The Z80 can jump to the address in HL
		 * but not call it, and the whole difference is the return
		 * address - so borrow one: call a trampoline that is
		 * nothing but jp (hl).  The call pushes the return address,
		 * the jump hands over, and the function's own ret comes
		 * back here.  One byte of library and an ordinary call at
		 * every site.
		 */
		Expr *hl, *asn;

		e->left = NULL;
		/* land the address in HL the way everything else does -
		 * wrapping it in an assignment to HL reuses the whole
		 * =(H,...) rule set, which knows every place it might be */
		hl = mkcode('s', R_HL);
		hl->op = INHL;
		asn = mkbinary(ASSIGN, 's', hl, fn);
		setdest(asn, DEST_VALUE);
		freeexpr(rewrite(asn));
		out("\tcall tramp\n");
	}

	/*
	 * Drop the arguments.  inc sp costs a byte apiece and touches no
	 * register; past a few words the HL form is smaller, but it has
	 * to shuffle the result through DE.
	 */
	if (nbytes > 0 && nbytes <= 8) {
		for (i = 0; i < nbytes; i++)
			out("\tinc sp\n");
	} else if (nbytes > 8) {
		outf("\tex de,hl\n\tld hl,%d\n\tadd hl,sp\n\tld sp,hl\n\tex de,hl\n",
		    nbytes);
	}

	if (savebc)
		out("\tpop bc\n");

	/* Result is in HL.  Hand back an INHL rather than a bare CODE:
	 * we return straight to the caller, skipping the step() loop that
	 * would otherwise do the CODE -> IN* conversion for us. */
	return donehl(e, INHL);
}

/*
 * Load a SYMREF's address into HL and hand back the register node.
 * A SYMREF is left unreduced so the store and load rules can use it
 * as an address; the two places that need its VALUE staged share
 * this.
 */
static Expr *
symtohl(Expr *s)
{
	Expr *n;
	char w = s->width;

	out("\tld hl,");
	out(s->u.symref.name);
	if (s->u.symref.off) {
		if (s->u.symref.off > 0)
			outc('+');
		outd(s->u.symref.off);
	}
	outc('\n');
	freeexpr(s);
	n = mkcode(w, R_HL);
	n->op = INHL;
	return n;
}

/*
 * An INDEX being used as a value rather than a location: "a + 2"
 * with a in a register.  It reduces to itself and emits nothing -
 * that is what lets the (reg+d) rules read through it - so a path
 * that stages values through HL has to form the number itself, or
 * the pop below it stores whatever HL last held.  add hl takes only
 * bc/de/hl/sp, so the register goes through the stack; DE is free
 * everywhere this is called, the sibling being on the stack.
 */
static Expr *
idxtohl(Expr *s)
{
	Expr *n;
	char w = s->width;

	outf("\tpush %s\n\tpop hl\n", idxregname(s->u.var.reg));
	if (s->u.var.off)
		outf("\tld de,%d\n\tadd hl,de\n", s->u.var.off);
	freeexpr(s);
	n = mkcode(w, R_HL);
	n->op = INHL;
	return n;
}

/*
 * A reduced operand whose VALUE the caller is about to stage through
 * HL - push it, pass it, keep it while something else runs.  Half a
 * dozen node kinds reduce to themselves by design so the rules can
 * read them in place, and every staging path that assumed "reduced
 * means it is in HL" has been bitten by one of them: SYMREF (an
 * address), INDEX (a register-relative address), and the register
 * homes.  This is the one place that knows the whole list.
 */
static Expr *
valtohl(Expr *e)
{
	Expr *n;
	char w;

	if (!e)
		return e;
	if (e->op == SYMREF)
		return symtohl(e);
	if (e->op == INDEX)
		return idxtohl(e);
	if (e->op == INBC || e->op == INDE ||
	    (e->op == REGVAR &&
	     (e->u.var.reg == R_BC || e->u.var.reg == R_IX))) {
		if (e->op == INDE)
			out("\tld l,e\n\tld h,d\n");
		else if (e->op == REGVAR && e->u.var.reg == R_IX)
			out("\tpush ix\n\tpop hl\n");
		else
			out("\tld l,c\n\tld h,b\n");
		w = e->width;
		freeexpr(e);
		n = mkcode(w, R_HL);
		n->op = INHL;
		return n;
	}
	return e;
}

static Expr *
rewrite1(Expr *e)
{
	Expr *n, *next;
	char lw, rw;

	if (!e) return NULL;

	/*
	 * A comma is its right operand, once the left has been emitted for
	 * whatever it does.  Collapse it rather than matching it: no rule
	 * reduces a bare constant - a constant only ever becomes a load as
	 * part of some parent rule that names it - so a comma whose right
	 * was a constant or a frame slot had nothing to match, and the
	 * ";(_,H)" family could only ever catch the cases that happened to
	 * reduce to a register on their own.  Collapsing hands the right
	 * operand to whatever encloses the comma, which has the rules for
	 * it already.
	 */
	if (e->op == COMMA && e->left && e->right) {
		Expr *val;

		e->left->dest = DEST_NONE;
		freeexpr(rewrite1(e->left));
		val = e->right;
		val->dest = e->dest;
		val->tgt = e->tgt;
		e->left = e->right = NULL;
		freeexpr(e);
		return rewrite1(val);
	}

	/* CALL: args have to be pushed one at a time, not rewritten as a
	 * batch - each one lands in HL and would clobber the last. */
	if (e->op == CALL) {
		n = docall(e);
		if (n)
			return n;
	}

	/*
	 * A compare whose left operand is a bare symbol.  The address is
	 * the value, so load it up front and let the (H,x) forms carry
	 * every context - the table only ever grew flag-context rules
	 * for the symbol-on-the-left shapes, and "n = s > buf" sat
	 * unreduced in value context.  Only for a register right-hand
	 * side: anything bigger may pass through HL itself on the way.
	 */
	if ((e->op == LT || e->op == GE || e->op == LE || e->op == GT ||
	     e->op == EQ || e->op == NEQ) && e->left && e->right) {
		char lsym = e->left->op == SYM || e->left->op == SYMREF;
		char rsym = e->right->op == SYM || e->right->op == SYMREF;
		char linreg = e->left->op == REGVAR ||
		    e->left->op == INBC || e->left->op == INDE;
		char rinreg = e->right->op == REGVAR ||
		    e->right->op == INBC || e->right->op == INDE;

		if (rsym && linreg) {
			/* mirror it: a<b is b>a, and equality commutes */
			Expr *t = e->left;
			e->left = e->right;
			e->right = t;
			if (e->op == LT) e->op = GT;
			else if (e->op == GT) e->op = LT;
			else if (e->op == LE) e->op = GE;
			else if (e->op == GE) e->op = LE;
			lsym = 1;
			rinreg = linreg;
		}
		if (lsym && (rinreg || rsym)) {
			/* a bare SYM reduces to SYMREF first, emitting nothing */
			e->left = rewrite1(e->left);
			if (e->left && e->left->op == SYMREF)
				e->left = symtohl(e->left);
		}
	}

	/*
	 * A 32-bit binary operator, before the children are reduced: both
	 * operands want HL:DE, so the ordinary depth-first walk would put
	 * the second one on top of the first.
	 */
	if (e->left && e->right && !baseop(e->op)) {
		n = dolongbin(e);
		if (n)
			return n;
		/*
		 * If that declined, stop here.  The arithmetic rules carry no
		 * width in their patterns, so a 32-bit operator falls through
		 * into them and is quietly done sixteen bits wide - "a << 31"
		 * became seven add hl,hl and a comparison against the low
		 * half of the constant.  Refusing leaves a marker instead.
		 */
		if (islongop(e))
			return e;
	}

	/*
	 * A step of one is what inc and dec do; anything else is a
	 * pointer to something wider, and "p++" means "p += sizeof".
	 * The rules only ever emitted the single step, so stepping a
	 * short pointer moved it one byte and landed between elements.
	 *
	 * It is a compound assignment, so make it one and let that path
	 * have it - including the part that names a side-effecting
	 * location only once.  A prefix yields the new value either way;
	 * a postfix yields the old one, which this does not produce, so
	 * that is only converted where the value is being thrown away.
	 */
	if (e->left && e->u.incdec.amt != 1 &&
	    (e->op == PREINC || e->op == PREDEC ||
	     ((e->op == POSTINC || e->op == POSTDEC) &&
	      e->dest == DEST_NONE))) {
		long amt = e->u.incdec.amt;
		unsigned char nop =
		    (e->op == PREINC || e->op == POSTINC) ? PLUSEQ : SUBEQ;

		e->op = nop;
		e->right = mkconst(e->width, amt);
		label(e);
		assign(e, e->tgt ? e->tgt : R_HL);
	}

	/*
	 * The same step, postfix, where the value is wanted: that is the
	 * value from before, so it has to be kept while the location is
	 * updated.  The stack is the only temporary there is.
	 */
	if (e->left && e->u.incdec.amt != 1 &&
	    (e->op == POSTINC || e->op == POSTDEC) &&
	    dupableloc(e->left)) {
		long amt = e->u.incdec.amt;
		unsigned char nop = (e->op == POSTINC) ? PLUSEQ : SUBEQ;
		Expr *loc = e->left;
		Expr *val, *step;

		val = locvalue(dupexpr(loc), e->width);
		setdest(val, DEST_VALUE);
		/* the value must actually BE in HL before the push - a
		 * register home or an address form reduces to itself */
		freeexpr(valtohl(rewrite(val)));
		out("\tpush hl\n");

		step = mkbinary(nop, e->width, loc, mkconst(e->width, amt));
		setdest(step, DEST_NONE);
		freeexpr(rewrite(step));
		out("\tpop hl\n");

		e->left = NULL;
		return donehl(e, INHL);
	}

	/* x OP= y -> x = x OP y, before the children are reduced */
	if (baseop(e->op)) {
		unsigned char tgt = e->tgt ? e->tgt : R_HL;
		unsigned char dst = e->dest;

		n = lowercompound(e);
		if (n) {
			setdest(n, dst);
			label(n);
			assign(n, tgt);
			return rewrite1(n);
		}
		/*
		 * The lvalue has side effects, so it cannot be named twice
		 * the way the expansion above does - "*p++ += 5" has to
		 * increment p once.  Work the address out once and keep it
		 * on the stack, which serves as the temporary the tree has
		 * no way to spell, then read, update and store through it.
		 */
		n = docompound(e);
		if (n)
			return n;
	}

	/* ARGNODE: evaluate left, push, then process right chain */
	if (e->op == ARGNODE) {
		e->left = rewrite1(e->left);
		next = e->right;
		e->right = NULL;  /* detach chain before step */
		/* Fixed-point on this ARGNODE */
		for (;;) {
			n = step(e);
			if (!n)
				break;
			n->left = rewrite1(n->left);
			e = n;
		}
		/* Now process next argument */
		if (next)
			rewrite1(next);
		return e;
	}

	/*
	 * The short-circuit operators, in flag context.
	 *
	 * Both used to assume their operands answered in Z - jump on z
	 * for "&&", on nz for "||" - and then to claim the result was in
	 * NZ.  Neither holds.  A signed comparison answers in the sign
	 * flag and an unsigned one in carry, so the branch went on a flag
	 * that meant nothing; and where an operand short-circuits, the
	 * flags arriving at the label are that operand's, in whatever
	 * encoding it used, not the NZ the result claimed.
	 *
	 * So the branch asks each operand which flag it answers in, and
	 * both paths are made to arrive with the same one: zero for
	 * false, non-zero for true, which is what the result then names.
	 */
	if (e->op == LAND || e->op == LOR) {
		int out_lbl = labelcnt++;
		int end_lbl = labelcnt++;
		int isand = (e->op == LAND);
		char *cc;

		e->left->dest = DEST_FLAGS;
		e->left = rewrite1(e->left);
		cc = isand ? falsecc(e->left) : truecc(e->left);
		outf("\tjp %s,_L%d\n", cc, out_lbl);

		e->right->dest = DEST_FLAGS;
		e->right = rewrite1(e->right);
		cc = isand ? falsecc(e->right) : truecc(e->right);
		outf("\tjp %s,_L%d\n", cc, out_lbl);

		/* both operands agreed with the expression: force the
		 * answer "&&" wants when true, "||" wants when false */
		out(isand ? "\txor a\n\tinc a\n" : "\txor a\n");
		outf("\tjp _L%d\n_L%d:\n", end_lbl, out_lbl);
		out(isand ? "\txor a\n" : "\txor a\n\tinc a\n");

		outf("_L%d:\n", end_lbl);

		/*
		 * Both paths leave a definite nought or one in A, so this
		 * serves either context: a flag for a branch, and the value
		 * itself where a number was wanted - which had no rule at
		 * all and reduced to nothing.
		 */
		if (e->dest == DEST_FLAGS) {
			n = mkcode(e->width, F_NZ);
			n->dest = DEST_FLAGS;
		} else {
			n = mkcode('B', R_A);
			n->op = INA;
			n->dest = e->dest;
		}
		freeexpr(e);
		return n;
	}

	/*
	 * QUES (ternary): cond ? then : else.
	 *
	 * Both arms have to leave their value in the same place for the
	 * expression to have one, and returning whichever node the then
	 * arm happened to reduce to did not arrange that.  Worse, a
	 * constant arm reduces to itself - a bare NUMBER matches no rule,
	 * because as an operand it has to stay a NUMBER for the ",N)"
	 * rules - so "x ? 1 : 0" emitted two empty branches and handed
	 * back the constant 1 as though it were the answer.
	 *
	 * Landing each arm in HL through ASSIGN(INHL, arm) settles both:
	 * it reuses the whole =(H,...) rule set, which knows how to put a
	 * constant there, and it puts both arms in the same register.
	 * RETURN and argument pushing land a value the same way.
	 */
	if (e->op == QUES && e->right && e->right->op == TERNBRANCH) {
		int lbl = labelcnt++;
		Expr *tb = e->right;
		unsigned char dest = e->dest;
		unsigned char tgt = e->tgt ? e->tgt : R_HL;
		char *cc;

		/* the condition, and the branch that skips the then arm.
		 * falsecc may emit the zero test it needs, so it has to run
		 * before any of the jump is written */
		e->left->dest = DEST_FLAGS;
		e->left = rewrite1(e->left);
		cc = falsecc(e->left);
		outf("\tjp %s,_T%d\n", cc, lbl);

		branchval(tb->left);
		outf("\tjp _E%d\n_T%d:\n", lbl, lbl);
		branchval(tb->right);
		outf("_E%d:\n", lbl);

		e->left = NULL;
		tb->left = NULL;
		tb->right = NULL;
		n = mkcode(e->width, R_HL);
		n->op = INHL;
		n->dest = dest;
		freeexpr(e);
		return movetotgt(n, tgt);
	}

	/* Handle long (32-bit) binary operations */
	/* Long values use HLDE (HL=high, DE=low), helpers take 2nd arg on stack */
	if ((e->width == 'l' || e->width == 'L') && e->left && e->right) {
		char *helper = NULL;
		int iscompare = 0;
		Expr *tmp;

		/* GT and LE need operand swap: a>b becomes b<a, a<=b becomes b>=a */
		if (e->op == GT || e->op == LE) {
			tmp = e->left;
			e->left = e->right;
			e->right = tmp;
		}

		switch (e->op) {
		case PLUS:   helper = "ladd"; break;
		case MINUS:  helper = "alsub"; break;
		case STAR:   helper = "almul"; break;
		case DIV:    helper = (e->width == 'l') ? "aldiv" : "lldiv"; break;
		case MOD:    helper = (e->width == 'l') ? "almod" : "llmod"; break;
		case AND:    helper = "lland"; break;
		case OR:     helper = "llor"; break;
		case XOR:    helper = "llxor"; break;
		case EQ: case NEQ: case LT: case GT: case LE: case GE:
			helper = "lrelop";
			iscompare = 1;
			break;
		}

		if (helper) {
			/* Evaluate right operand first (result in HLDE) */
			e->right = rewrite1(e->right);
			/* Push right operand: low word first, then high */
			out("\tpush de\n\tpush hl\n");
			/* Evaluate left operand (result in HLDE) */
			e->left = rewrite1(e->left);
			/* Call helper */
			outf("\tcall %s\n", helper);
			/* For comparisons, result is in flags */
			if (iscompare) {
				unsigned char flag;
				switch (e->op) {
				case EQ:  flag = F_Z; break;
				case NEQ: flag = F_NZ; break;
				case LT:  flag = F_C; break;   /* swapped GT uses this */
				case GE:  flag = F_NC; break;  /* swapped LE uses this */
				case GT:  flag = F_C; break;   /* after swap, use LT flag */
				case LE:  flag = F_NC; break;  /* after swap, use GE flag */
				default:  flag = F_NZ; break;
				}
				n = mkcode('b', flag);
				n->dest = DEST_FLAGS;
			} else {
				/* Result in HLDE, report as HL (high word) */
				n = mkcode(e->width, R_HL);
				n->dest = e->dest;
			}
			freeexpr(e);
			return n;
		}
	}

	/*
	 * Handle spill for expressions needing > 2 registers.
	 * Exclude ASSIGN - left side is target, not operand - and a
	 * constant right operand, which needs no register and which
	 * nothing would materialise if it were pushed and popped around.
	 */
	if (e->regs >= 3 && e->left && e->right && e->op != ASSIGN &&
	    e->right->op != NUMBER) {
		/* Evaluate left subtree (result in HL) */
		e->left = rewrite1(e->left);
		/*
		 * A SYMREF is left unreduced on purpose, so the rules can
		 * take it as an address rather than loading it.  That means
		 * it emits no code, and the push below spills whatever the
		 * last statement left in HL - which the other operand is
		 * then added to, and the sum read as a pointer.
		 */
		/*
		 * The value has to actually BE in HL before the push: a
		 * register variable, a SYMREF or an INDEX reduces to
		 * itself by design - the rules read them in place.
		 * Without this, "p += 1 + (w ? 2 : 1) + len" with p in BC
		 * pushed whatever the condition had left in HL and
		 * marched p off into it - which is how cpp's define store
		 * walked garbage and wrote six interned names' ids over
		 * with it.
		 */
		e->left = valtohl(e->left);
		/*
		 * Unless it is a byte, which lands in A whatever target it
		 * was given.  Pushing HL then would spill the address the
		 * value was read through, and the operator would be applied
		 * to two addresses.
		 */
		if (e->left->op == INA) {
			bytepair(e);
		} else {
		/* Spill left result to stack */
		out("\tpush hl\n");
		/* Evaluate right subtree (result in HL) */
		e->right = valtohl(rewrite1(e->right));
		/* Pop left result, exchange so left in HL, right in DE */
		out("\tpop de\n\tex de,hl\n");
		/*
		 * Now left in HL, right in DE - convert children to register
		 * nodes, each keeping the width it had.  Taking the parent's
		 * made every operand of a comparison unsigned, because a
		 * comparison yields ubyte whatever it compared: the signed
		 * rules ask about the operand's width and stopped matching,
		 * so "f(x) < g(y)" on two negative shorts read the carry and
		 * answered an unsigned question.
		 */
		lw = e->left->width;
		rw = e->right->width;
		freeexpr(e->left);
		freeexpr(e->right);
		e->left = mkcode(lw, R_HL);
		e->left->op = INHL;
		e->right = mkcode(rw, R_DE);
		e->right->op = INDE;
		}
		/* Fall through to step() to apply operation */
	} else {
		/* Rewrite children first (depth-first) */
		/* Skip children marked nored (preserve for parent rules) */
		if (e->op == ASSIGN && e->left && e->right &&
		    !islocdesc(e->left) && !isdestreg(e->left) &&
		    e->left->op != DEREF &&
		    !islocdesc(e->right) && e->right->op != NUMBER) {
			/*
			 * Storing through an address the tree has to work out,
			 * to a value it also has to work out - "arr[i] += n".
			 * Both want HL, so the address waits on the stack
			 * while the value is computed and comes back with the
			 * value beside it in DE, which is what the
			 * =(D(H),E) store rule expects.
			 *
			 * Only when the value really needs a register: a
			 * constant stores straight through the address, and
			 * spilling for that would just cost bytes.
			 *
			 * And only when the address really needs one, which
			 * is not known until it has been reduced.  "arr[2]"
			 * folds to a symbol and an offset and emits nothing
			 * at all, so there would be nothing in HL to push:
			 * the push would spill whatever the last statement
			 * happened to leave there and the store would go to
			 * that address.  A descriptor needs no register and
			 * no temporary, so it is simply used where it is.
			 */
			Expr *addr = rewrite1(e->left);
			if (islocdesc(addr)) {
				e->left = addr;
				e->right = rewrite1(e->right);
				goto children_done;
			}
			/*
			 * The address is only worth pushing if it is
			 * actually in HL.  If the subtree did not reduce -
			 * a rule missing somewhere below it - then nothing
			 * put it there, and the push spills whatever the
			 * last statement happened to leave: the store then
			 * goes to that address and writes over something
			 * unrelated.  The comment above says exactly this
			 * about "arr[2]", and islocdesc was the whole test
			 * for it, which only covers the shapes that reduce
			 * to a descriptor rather than the ones that do not
			 * reduce at all.
			 *
			 * That is not a case the root check can catch.  By
			 * the time it looks, the address has been replaced
			 * by the register nodes the store rule wants and
			 * the tree reads as fully reduced - which is how
			 *
			 *	parms[m->parmcount++] = permdup(s);
			 *
			 * in cpp's macdefine came to store through a stale
			 * HL, land on the input buffer, and eat a character
			 * out of the next line of every source that defines
			 * a function-like macro with an empty body.
			 *
			 * The right operand still has to be evaluated: it
			 * is a call in the case that found this, and its
			 * side effects are not optional.  Only the store is
			 * dropped, and the marker says so.
			 */
			if (!reduced(addr)) {
				e->right = rewrite1(e->right);
				out("; XXXXXX incomplete: ");
#ifdef DEBUG
				dumpexpr(addr);
#endif
				outc('\n');
				freeexpr(addr);
				freeexpr(e->right);
				e->left = e->right = NULL;
				return donehl(e, INHL);
			}
			out("\tpush hl\n");
			e->right = rewrite1(e->right);
			/*
			 * An address-as-value or register-homed right
			 * reduces to itself and emits nothing; without
			 * this the pop below stored the slot's own
			 * address - "paths[np++] = a + 2" filed the slot
			 * into itself.
			 */
			e->right = valtohl(e->right);
			/*
			 * Where the value came back decides how to get the
			 * address out from under it.  A byte operation ends in
			 * A, which the address does not disturb, so the address
			 * comes straight back to HL.  A word is in HL itself and
			 * has to move aside.  Assuming the second made "p[i] +=
			 * n" store the low half of the address.
			 */
			if (ISLONGINT(e->width)) {
				/*
				 * A 32-bit value fills HL:DE, so there is no
				 * register left for the address and no room to
				 * shuffle it into one.  It is already on the
				 * stack, which is where lstde wants it.
				 */
				out("\tcall lstde\n");
				freeexpr(addr);
				freeexpr(e->right);
				e->left = e->right = NULL;
				return donehl(e, INHL);
			}
			if (e->right && e->right->op == INA) {
				out("\tpop hl\n");
				freeexpr(addr);
				freeexpr(e->right);
				e->right = mkcode(e->width, R_A);
				e->right->op = INA;
			} else {
				out("\tpop de\n\tex de,hl\n");
				freeexpr(addr);
				freeexpr(e->right);
				e->right = mkcode(e->width, R_DE);
				e->right->op = INDE;
			}
			addr = mkcode(e->width, R_HL);
			addr->op = INHL;
			e->left = mkunary(DEREF, e->width, addr);
			goto children_done;
		} else if (e->op == AND && e->dest == DEST_FLAGS &&
			   e->left && e->left->op == DEREF &&
			   ISBYTE(e->left->width) &&
			   e->right && e->right->op == NUMBER &&
			   ispow2(e->right->u.val) > 0) {
			/*
			 * One bit of a byte in memory, asked for as a
			 * condition.  The Z80 tests it where it lies - bit
			 * 4,(ix+3) - but only if the rule can still see
			 * where that is, and reducing the left operand the
			 * usual way loads it into A first and leaves the
			 * rule nothing to match.  So the address underneath
			 * is reduced and the DEREF left standing, which is
			 * what the ASSIGN lvalue below does for the same
			 * reason.
			 *
			 * Where the address lands decides whether there is a
			 * rule to match at all, so the DEREF is only left
			 * standing for the three that have one.  Anything
			 * else is reduced the ordinary way - a global among
			 * them, whose "ld a,(nn)" is the shortest the Z80
			 * has and which bit cannot address regardless.
			 * Leaving it standing with nothing to match would
			 * emit no code at all.
			 */
			n = rewrite1(e->left->left);
			e->left->left = n;
			if (!(n->op == INDEX || n->op == INHL ||
			      (n->op == REGVAR && n->u.var.reg == R_IX)))
				e->left = rewrite1(e->left);
			goto children_done;
		} else if (e->left && e->left->op == REGVAR &&
			   ISBYTE(e->width) &&
			   (e->op == ASSIGN ||
			    e->op == PREINC || e->op == PREDEC ||
			    e->op == POSTINC || e->op == POSTDEC)) {
			/*
			 * A byte register variable being assigned to, or
			 * stepped.  Leave it alone: reducing it turns it into
			 * A, the ?(V,..):b rules that would have written b or
			 * c never match, and the A forms match instead - which
			 * work on a copy and stop there.  "c = -1" came out as
			 * "ld a,b" and "ld a,-1", and c kept whatever it had;
			 * "while (--n)" decremented A, tested that, and never
			 * touched n, so it did not terminate.
			 *
			 * Only bytes.  A word register variable reduces to
			 * INBC and has a working family of its own.
			 */
			;
		} else if (e->op == ASSIGN && e->left && e->left->op == DEREF) {
			/*
			 * An assignment's lvalue is a location, not a value.
			 * Reduce the address underneath but leave the DEREF
			 * standing, so the =(D(..),..) store rules can still
			 * see it - reducing it here would apply a load rule
			 * and quietly turn the store into a fetch.
			 */
			e->left->left = rewrite1(e->left->left);
		} else if ((e->op == PREINC || e->op == PREDEC ||
			    e->op == POSTINC || e->op == POSTDEC) &&
			   e->left && e->left->op == DEREF &&
			   e->left->left && e->left->left->op == REGVAR) {
			/*
			 * Stepping through a pointer kept in a register.
			 *
			 * Every other pointer works already, because the
			 * ordinary reduction of DEREF(pointer variable) loads
			 * the pointer, and a loaded pointer in HL is exactly
			 * what "step through an address in HL" wants.  A
			 * register variable has nothing to load: the reduction
			 * applies a load rule anyway and fetches what p points
			 * at, so the step ran on the value - and at short
			 * width on that value used as an address again, making
			 * "(*p)++" step what *p pointed at.
			 *
			 * Keeping the DEREF and reducing only underneath is
			 * what the assignment above does, for the same reason:
			 * dropping it leaves a bare INBC, and "i(B)" - step BC
			 * itself - is a real rule that would then match, which
			 * is the "no way to tell *p = x from p = x" that the
			 * pass1 side of this warns about.  The ?(D(B)) rules
			 * name the shape that is left.
			 */
			e->left->left = rewrite1(e->left->left);
		} else if (e->op == ASSIGN && e->left &&
			   !islocdesc(e->left) && !isdestreg(e->left)) {
			/*
			 * An lvalue that is neither a location descriptor nor
			 * a destination register is an address the tree works
			 * out - an array element, say.  Reduce it, and unless
			 * it folded into a descriptor of its own (a constant
			 * subscript becomes a SYMREF) give it the DEREF the
			 * store rules expect, so such a store needs no rules
			 * of its own.
			 */
			Expr *addr = rewrite1(e->left);
			if (islocdesc(addr))
				e->left = addr;
			else
				e->left = mkunary(DEREF, e->width, addr);
		} else if (e->op != COMMA && e->left && e->right &&
			   e->left->regs <= 1 && e->right->regs > e->left->regs &&
			   !e->left->nored && !e->right->nored) {
			/*
			 * Sethi-Ullman: work out the costlier side first, so
			 * the cheaper one can follow without spilling.  Each
			 * still ends up in the register it was assigned, so a
			 * non-commutative operator is unaffected - only the
			 * order of evaluation changes, which C leaves open.
			 *
			 * Held to a left side costing one register, because
			 * that is the case that cannot disturb DE while the
			 * right operand is sitting in it.  And never for the
			 * comma, whose order is the whole point of it.
			 */
			unsigned char rtgt = e->right->tgt;

			e->right = rewrite1(e->right);
			e->right = movetotgt(e->right, rtgt);
			e->left = rewrite1(e->left);
			goto children_done;
		} else if (!e->left || !e->left->nored) {
			e->left = rewrite1(e->left);
		}
		if (!e->right || !e->right->nored) {
			if (e->left && e->left->op == INA && e->right &&
			    e->op != ASSIGN && e->right->regs > 0 &&
			    e->right->op != NUMBER && ISBYTE(e->right->width))
				bytepair(e);
			else
				e->right = rewrite1(e->right);
		}
	children_done: ;
		/*
		 * A long stored into something narrower.  The value is in
		 * HL:DE with HL the high word, and every narrowing store
		 * rule takes the low half of HL - which is the third byte
		 * of the long, not the first.  There are ten of those rules
		 * at byte width alone, so the conversion belongs here once
		 * rather than in each of them: bring the low word into HL
		 * and they are all correct as written.
		 *
		 * "buf[1] = val & 0xff" was right and "buf[2] = (val >> 8)
		 * & 0xff" was not, which is what made it look like a shift
		 * bug - the first reads the low byte straight out of memory
		 * and never goes through a register pair at all.  cpp emits
		 * every number this way, so every constant above 255 lost
		 * its high bytes: 0644 arrived as 164 and 256 as 0.
		 */
		if (e->op == ASSIGN && !ISLONG(e->width) &&
		    e->right && e->right->op == INHL &&
		    ISLONG(e->right->width)) {
			out("\tex de,hl\n");
			e->right->width = T_SHORT;
		}
	}

	/* Fixed-point: keep rewriting until no change */
	for (;;) {
		unsigned char tgt = e->tgt;
		/* Re-label and re-assign after transformations */
		label(e);
		assign(e, tgt);
		n = step(e);
		if (!n)
			break;
		/* Transformation may create new children - rewrite them */
		n->left = rewrite1(n->left);
		n->right = rewrite1(n->right);
		e = n;
	}
	return e;
}

/*
 * Public entry point
 */
/*
 * A constant that survived as the whole expression.  This is only
 * reachable at the root - as an operand a NUMBER has to stay a NUMBER
 * for the ",N)" rules to match it, which is why there is no plain "N"
 * rule in the table.  What it has to produce depends on where the
 * value was wanted: nothing at all for a statement, a flag for a
 * condition, HL for a value.
 */
static Expr *
constresult(Expr *e)
{
	long v = e->u.val;
	Expr *n;

	switch (e->dest) {
	case DEST_NONE:
		/* a constant statement does nothing */
		n = mkcode(e->width, 0);
		break;
	case DEST_FLAGS:
		/*
		 * Z means false.  xor a clears A and sets Z; inc a then
		 * makes it nonzero for a true constant.
		 */
		out("\txor a\n");
		if (v)
			out("\tinc a\n");
		n = mkcode(e->width, F_NZ);
		break;
	default:
		outf("\tld hl,%d\n", (int)v);
		n = mkcode(e->width, R_HL);
		n->op = INHL;
		break;
	}
	n->dest = e->dest;
	freeexpr(e);
	return n;
}

/*
 * Branch-chained conditions.  An if's condition wants a jump taken
 * when the expression is false; the old path rewrote && and || to
 * a nought-or-one in A and then tested A - six bytes of join and a
 * retest per operator, six hundred xor a's across this pass alone.
 * Here the short-circuit IS the branch: every conjunct jumps
 * straight to the consumer's label, and nothing materialises.
 *
 * condgo(e, lbl, wf): emit code that jumps to lbl when e is false
 * (wf=1) or true (wf=0), consuming e.  Leaves go through the
 * ordinary flag-context rewrite and one conditional jump.
 */
static void
condleaf(Expr *e, char *lbl, int wf)
{
	char *cc;

	e->dest = DEST_FLAGS;
	label(e);
	assign(e, R_HL);
	e = rewrite1(e);
	cc = wf ? falsecc(e) : truecc(e);
	outf("\tjp %s,%s\n", cc, lbl);
	freeexpr(e);
}

static void
condgo(Expr *e, char *lbl, int wf)
{
	Expr *l, *r;
	int op;
	char sc[12];

	op = e->op;
	if (op == LAND || op == LOR) {
		l = e->left;
		r = e->right;
		e->left = e->right = NULL;
		freeexpr(e);
		if ((op == LAND) == (wf != 0)) {
			/* every operand agrees with the jump: chain them */
			condgo(l, lbl, wf);
			condgo(r, lbl, wf);
		} else {
			/* the left short-circuits PAST the test instead */
			fmtstr(sc, "_C%d", labelcnt++);
			condgo(l, sc, !wf);
			condgo(r, lbl, wf);
			outf("%s:\n", sc);
		}
		return;
	}
	if (op == BANG) {
		l = e->left;
		e->left = NULL;
		freeexpr(e);
		condgo(l, lbl, !wf);
		return;
	}
	condleaf(e, lbl, wf);
}

void
condfalse(Expr *e, char *lbl)
{
	normtree(e);
	condgo(e, lbl, 1);
}

Expr *
rewrite(Expr *e)
{
	Expr *r;

#ifdef DEBUG
	if (VERBOSE(V_REWRITE)) {
		out("; --- raw ---\n");
		dumpexpr(e);
	}
#endif

	/* Canonicalize operand order before anything is labeled */
	normtree(e);

	/* Label nodes with register requirements */
	label(e);

	/* Assign target registers based on labels */
	assign(e, R_HL);  /* root expression targets HL */

#ifdef DEBUG
	if (VERBOSE(V_REWRITE)) {
		out("; --- labeled ---\n");
		dumpexpr(e);
	}
#endif

	r = rewrite1(e);

	/* A constant is only left standing when it is the whole thing */
	if (r && r->op == NUMBER)
		r = constresult(r);

	/*
	 * Check if code generation is incomplete.
	 *
	 * A tree that reduced is a single register node with nothing
	 * under it, so children left standing mean a rule was missing
	 * somewhere below - and only the root used to be looked at.  A
	 * parent that still matched hid it: "arr[i] = i * a" with i in a
	 * register has no rule for multiplying BC by DE, so the multiply
	 * and its left operand emitted nothing, the store above them
	 * matched anyway, and the wrong value went into the array with
	 * nothing said.
	 */
	if (r && !reduced(r)) {
		out("; XXXXXX incomplete: ");
#ifdef DEBUG
		dumpexpr(r);
#endif
	}

#ifdef DEBUG
	if (VERBOSE(V_REWRITE)) {
		out("; --- rewritten ---\n");
		dumpexpr(r);
	}
#endif
	return r;
}

/* vim: set tabstop=4 shiftwidth=4 noexpandtab: */
