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

/* Forward declarations */
static char *pmatch(char *p, Expr *e);
static Expr *rewrite1(Expr *e);
static unsigned char baseop(unsigned char op);

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
		l = e->left ? e->left->regs : 1;
		e->regs = l > 1 ? l : 1;
		return;

	/* ASSIGN: lvalue doesn't consume reg, only rvalue */
	case ASSIGN:
		e->regs = e->right ? e->right->regs : 1;
		if (e->regs < 1) e->regs = 1;
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
		e->regs = e->left ? e->left->regs : 1;
		return;

	/* Short-circuit: sides evaluated separately */
	case LAND:
	case LOR:
		l = e->left ? e->left->regs : 1;
		r = e->right ? e->right->regs : 1;
		e->regs = l > r ? l : r;
		return;

	/* Ternary: condition, then, else all separate */
	case QUES:
	case TERNBRANCH:
		l = e->left ? e->left->regs : 1;
		r = e->right ? e->right->regs : 1;
		e->regs = l > r ? l : r;
		return;

	/* Unary ops: same as child, min 1 */
	case BANG:
	case NEG:
	case NOT:
	case PREINC:
	case POSTINC:
	case PREDEC:
	case POSTDEC:
		e->regs = e->left ? e->left->regs : 1;
		if (e->regs < 1) e->regs = 1;
		return;

	/* Binary ops: Sethi-Ullman formula */
	default:
		l = e->left ? e->left->regs : 1;
		r = e->right ? e->right->regs : 1;
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
		assign(e->left, 0);  /* lvalue, no target */
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
		if (e->regs >= 3) {
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
	{'X', SEXT}, {'J', WIDEN}, {';', COMMA},
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
		if (n == (1 << i))
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
static void
emitasm(char *tpl, Expr *e)
{
	char *p = tpl;
	char path[8];
	int i, offadj;
	char mod;
	Expr *n;
	long val;
	int cnt;
	char *start;

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
					out("?op");
					outd(n->op);
					out("?");
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

	/* Check register constraints */
	if (rp->flags & (RF_IXIY | RF_BC | RF_C | RF_B | RF_DE | RF_HL | RF_IX)) {
		src = getpath(e, rp->dsrc);
		if (!src)
			return NULL;
		if ((rp->flags & RF_IXIY) &&
		    src->u.var.reg != R_IX && src->u.var.reg != R_IY)
			return NULL;
		if ((rp->flags & RF_BC) && src->u.var.reg != R_BC)
			return NULL;
		if ((rp->flags & RF_C) && src->u.var.reg != R_C)
			return NULL;
		if ((rp->flags & RF_B) && src->u.var.reg != R_B)
			return NULL;
		if ((rp->flags & RF_DE) && src->u.var.reg != R_DE)
			return NULL;
		if ((rp->flags & RF_HL) && src->u.var.reg != R_HL)
			return NULL;
		if ((rp->flags & RF_IX) && src->u.var.reg != R_IX)
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
	lc = rp->lsrc ? getpath(e, rp->lsrc) : NULL;
	rc = rp->rsrc ? getpath(e, rp->rsrc) : NULL;

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
			src = getpath(e, rp->dsrc);
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

	/* Far LOCALVAR -> CODE: form the frame address with 16-bit
	 * arithmetic (big-array bases sit past the (iy+d) window) */
	if (newop == CODE && oldop == LOCALVAR) {
		off = e->u.var.off;
		if (e->tgt == R_DE) {
			/* sibling value lives in HL - preserve it */
			out("\tpush hl\n\tpush iy\n\tpop hl\n\tld de,");
			outd(off);
			out("\n\tadd hl,de\n\tex de,hl\n\tpop hl\n");
			n = mkcode(e->width, R_DE);
		} else {
			out("\tpush iy\n\tpop hl\n\tld de,");
			outd(off);
			out("\n\tadd hl,de\n");
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
				out("\tld e,");
				outd(val);
				out("\n");
				n = mkcode('b', R_E);
			} else {
				out("\tld a,");
				outd(val);
				out("\n");
				n = mkcode('b', R_A);
			}
		} else if (w == 'l' || w == 'L' || w == 'f') {
			/* Long/float: load into HLDE (DE=low, HL=high) */
			out("\tld de,");
			outd(val & 0xffff);
			out("\n\tld hl,");
			outd((val >> 16) & 0xffff);
			out("\n");
			n = mkcode(w, R_HL);
		} else if (e->tgt == R_DE) {
			/* a word constant honours its target too - as the
			 * right operand of a binary op it belongs in DE, and
			 * putting it in HL would land on the left one */
			out("\tld de,");
			outd(val);
			out("\n");
			n = mkcode(e->width, R_DE);
		} else {
			out("\tld hl,");
			outd(val);
			out("\n");
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
	 * What an assignment stores is wanted as a value whatever is done
	 * with the assignment itself, and "i = k = 5" needs the inner one
	 * to know it: a store rule that writes straight to memory leaves
	 * nothing for the outer assignment to copy, and only the value
	 * context tells it to pay for a register.
	 */
	if (e->op == ASSIGN && e->right && e->right->dest == DEST_NONE)
		e->right->dest = DEST_VALUE;
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
	    e->left && e->left->op == INHL && e->right &&
	    (e->right->op == INA || e->right->op == INE ||
	     e->right->op == INDE)) {
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

		n = mkcode(e->width, R_HL);
		n->op = INHL;
		n->dest = e->dest;
		freeexpr(e);
		return n;
	}

	/* CALL is handled up in rewrite1() - args must be pushed one at a
	 * time, before the children are batch-rewritten. */

	/* Long unary operations */
	if ((e->width == 'l' || e->width == 'L') && e->left) {
		/* Long complement: ~HLDE */
		if (e->op == NOT && e->left->op == INHL) {
			out("\tcall lcom\n");
			n = mkcode(e->width, R_HL);
			n->dest = e->dest;
			freeexpr(e);
			return n;
		}
		/* Long negation: -HLDE (two's complement) */
		if (e->op == NEG && e->left->op == INHL) {
			/* Negate DE, then HL with borrow */
			out("\txor a\n\tsub e\n\tld e,a\n");
			out("\tld a,0\n\tsbc a,d\n\tld d,a\n");
			out("\tld a,0\n\tsbc a,l\n\tld l,a\n");
			out("\tld a,0\n\tsbc a,h\n\tld h,a\n");
			n = mkcode(e->width, R_HL);
			n->dest = e->dest;
			freeexpr(e);
			return n;
		}
		/* Long left shift: HLDE << B */
		if (e->op == LSHIFT && e->left->op == INHL &&
		    e->right && e->right->op == INA) {
			out("\tld b,a\n");
			out("\tcall lllsh\n");
			n = mkcode(e->width, R_HL);
			n->dest = e->dest;
			freeexpr(e);
			return n;
		}
		/* Long right shift (signed): HLDE >> B */
		if (e->op == RSHIFT && e->width == 'l' &&
		    e->left->op == INHL && e->right && e->right->op == INA) {
			out("\tld b,a\n");
			out("\tcall alrsh\n");
			n = mkcode(e->width, R_HL);
			n->dest = e->dest;
			freeexpr(e);
			return n;
		}
		/* Long right shift (unsigned): HLDE >> B */
		if (e->op == RSHIFT && e->width == 'L' &&
		    e->left->op == INHL && e->right && e->right->op == INA) {
			out("\tld b,a\n");
			out("\tcall llrsh\n");
			n = mkcode(e->width, R_HL);
			n->dest = e->dest;
			freeexpr(e);
			return n;
		}
	}

	/* Float unary operations */
	if (e->width == 'f' && e->left) {
		/* Float negation: flip sign bit (bit 7 of H) */
		if (e->op == NEG && e->left->op == INHL) {
			out("\tld a,h\n\txor 80h\n\tld h,a\n");
			n = mkcode('f', R_HL);
			n->dest = e->dest;
			freeexpr(e);
			return n;
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
		if (n)
			return n;
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
char *
falsecc(Expr *e)
{
	switch (e ? e->u.var.reg : 0) {
	case F_Z:  return "nz";
	case F_NZ: return "z";
	case F_C:  return "nc";
	case F_NC: return "c";
	case F_M:  return "p";
	case F_P:  return "m";
	case R_A:  out("\tor a\n"); return "z";
	case R_HL: out("\tld a,l\n\tor a,h\n"); return "z";
	case R_DE: out("\tld a,e\n\tor a,d\n"); return "z";
	case R_BC: out("\tld a,c\n\tor a,b\n"); return "z";
	}
	return "z";
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

	/* the address, once - the side effects happen here and only here */
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
	sum = rewrite(sum);

	/* store it back through the address that was waiting */
	if (isbyte) {
		out("\tpop hl\n\tld (hl),a\n");
		n = mkcode(w, R_A);
		n->op = INA;
	} else {
		out("\tpop de\n\tex de,hl\n");
		out("\tld (hl),e\n\tinc hl\n\tld (hl),d\n\tex de,hl\n");
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
	if (w != 'l' && w != 'L' && w != 'f')
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
	/* long/float live in HL:DE, high word first */
	out("\tpush hl\n\tpush de\n");
	return 4;
}

/* a 32-bit integer, which the helpers handle - float is a separate set */
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
	out("\tld hl,");
	outd((int)((v >> 16) & 0xffff));
	out("\n\tld de,");
	outd((int)(v & 0xffff));
	outc('\n');
}

static void
pushlongc(long v)
{
	out("\tld hl,");
	outd((int)((v >> 16) & 0xffff));
	out("\n\tpush hl\n\tld hl,");
	outd((int)(v & 0xffff));
	out("\n\tpush hl\n");
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
	int sign;
	char *fn;
	Expr *l, *r, *n;

	if (!e->left || !e->right)
		return NULL;
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
		if (r->op == NUMBER) {
			if (l->op == NUMBER)
				loadlongc(l->u.val);
			else
				l = rewrite1(l);
			out("\tld b,");
			outd((int)(r->u.val & 0xff));
			outc('\n');
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
		out("\tcall ");
		out(op == LSHIFT ? "allsh" : sign ? "alrsh" : "lushr");
		outc('\n');
		n = mkcode(e->width, R_HL);
		n->op = INHL;
		n->dest = e->dest;
		freeexpr(e);
		return n;
	}

	fn = longhelper(op, sign);
	if (!fn)
		return NULL;

	/*
	 * Both operands have to be shapes that end up in HL:DE, and that
	 * has to be settled before anything is emitted - once the right
	 * operand has been pushed there is no way back.
	 */
	if (!longable(l) || !longable(r))
		return NULL;

	e->left = e->right = NULL;

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

	out("\tcall ");
	out(fn);
	outc('\n');

	if (iscmp) {
		n = mkcode(e->width, longflag(op, sign));
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
	Expr *a, *next, *n;
	int nbytes = 0;
	int i;

	/* Resolve the callee (emits nothing); SYM becomes SYMREF. */
	e->left = rewrite1(e->left);
	if (!e->left || e->left->op != SYMREF)
		return NULL;		/* indirect call - not handled yet */

	for (a = e->right; a && a->op == ARGNODE; a = next) {
		Expr *v = a->left;
		next = a->right;
		a->left = a->right = NULL;
		freeexpr(a);
		nbytes += pusharg(v);
	}
	e->right = NULL;

	out("\tcall ");
	out(e->left->u.symref.name);
	outc('\n');

	/*
	 * Drop the arguments.  inc sp costs a byte apiece and touches no
	 * register; past a few words the HL form is smaller, but it has
	 * to shuffle the result through DE.
	 */
	if (nbytes > 0 && nbytes <= 8) {
		for (i = 0; i < nbytes; i++)
			out("\tinc sp\n");
	} else if (nbytes > 8) {
		out("\tex de,hl\n\tld hl,");
		outd(nbytes);
		out("\n\tadd hl,sp\n\tld sp,hl\n\tex de,hl\n");
	}

	/* Result is in HL.  Hand back an INHL rather than a bare CODE:
	 * we return straight to the caller, skipping the step() loop that
	 * would otherwise do the CODE -> IN* conversion for us. */
	n = mkcode(e->width, R_HL);
	n->op = INHL;
	n->dest = e->dest;
	freeexpr(e);
	return n;
}

static Expr *
rewrite1(Expr *e)
{
	Expr *n, *next;

	if (!e) return NULL;

	/* CALL: args have to be pushed one at a time, not rewritten as a
	 * batch - each one lands in HL and would clobber the last. */
	if (e->op == CALL) {
		n = docall(e);
		if (n)
			return n;
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

	/* LAND in flag context: short-circuit AND */
	if (e->op == LAND && e->dest == DEST_FLAGS) {
		int lbl = labelcnt++;
		/* Evaluate left operand */
		e->left->dest = DEST_FLAGS;
		e->left = rewrite1(e->left);
		/* If left is false (Z), jump to false label */
		out("\tjp z,_L");
		outd(lbl);
		out("\n");
		/* Evaluate right operand */
		e->right->dest = DEST_FLAGS;
		e->right = rewrite1(e->right);
		/* Emit false label */
		out("_L");
		outd(lbl);
		out(":\n");
		/* Result is Z flag from right (or jumped with Z set) */
		n = mkcode(e->width, F_NZ);
		n->dest = DEST_FLAGS;
		freeexpr(e);
		return n;
	}

	/* LOR in flag context: short-circuit OR */
	if (e->op == LOR && e->dest == DEST_FLAGS) {
		int lbl = labelcnt++;
		/* Evaluate left operand */
		e->left->dest = DEST_FLAGS;
		e->left = rewrite1(e->left);
		/* If left is true (NZ), jump to true label */
		out("\tjp nz,_L");
		outd(lbl);
		out("\n");
		/* Evaluate right operand */
		e->right->dest = DEST_FLAGS;
		e->right = rewrite1(e->right);
		/* Emit true label */
		out("_L");
		outd(lbl);
		out(":\n");
		/* Result is NZ if either was true */
		n = mkcode(e->width, F_NZ);
		n->dest = DEST_FLAGS;
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
		out("\tjp ");
		out(cc);
		out(",_T");
		outd(lbl);
		outc('\n');

		branchval(tb->left);
		out("\tjp _E");
		outd(lbl);
		outc('\n');

		out("_T");
		outd(lbl);
		out(":\n");
		branchval(tb->right);
		out("_E");
		outd(lbl);
		out(":\n");

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
			out("\tpush de\n");
			out("\tpush hl\n");
			/* Evaluate left operand (result in HLDE) */
			e->left = rewrite1(e->left);
			/* Call helper */
			out("\tcall ");
			out(helper);
			out("\n");
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

	/* Handle float (32-bit) binary operations */
	/* Same HLDE convention as longs, different helpers */
	if (e->width == 'f' && e->left && e->right) {
		char *helper = NULL;
		int iscompare = 0;
		Expr *tmp;

		/* GT and LE need operand swap */
		if (e->op == GT || e->op == LE) {
			tmp = e->left;
			e->left = e->right;
			e->right = tmp;
		}

		switch (e->op) {
		case PLUS:   helper = "fladd"; break;
		case MINUS:  helper = "flsub"; break;
		case STAR:   helper = "flmul"; break;
		case DIV:    helper = "fldiv"; break;
		case EQ: case NEQ: case LT: case GT: case LE: case GE:
			helper = "frelop";
			iscompare = 1;
			break;
		}

		if (helper) {
			/* Evaluate right operand first (result in HLDE) */
			e->right = rewrite1(e->right);
			/* Push right operand: low word first, then high */
			out("\tpush de\n");
			out("\tpush hl\n");
			/* Evaluate left operand (result in HLDE) */
			e->left = rewrite1(e->left);
			/* Call helper */
			out("\tcall ");
			out(helper);
			out("\n");
			/* For comparisons, result is in flags */
			if (iscompare) {
				unsigned char flag;
				switch (e->op) {
				case EQ:  flag = F_Z; break;
				case NEQ: flag = F_NZ; break;
				case LT:  flag = F_C; break;
				case GE:  flag = F_NC; break;
				case GT:  flag = F_C; break;   /* after swap */
				case LE:  flag = F_NC; break;  /* after swap */
				default:  flag = F_NZ; break;
				}
				n = mkcode('b', flag);
				n->dest = DEST_FLAGS;
			} else {
				/* Result in HLDE */
				n = mkcode('f', R_HL);
				n->dest = e->dest;
			}
			freeexpr(e);
			return n;
		}
	}

	/* Handle spill for expressions needing > 2 registers */
	/* Exclude ASSIGN - left side is target, not operand */
	if (e->regs >= 3 && e->left && e->right && e->op != ASSIGN) {
		/* Evaluate left subtree (result in HL) */
		e->left = rewrite1(e->left);
		/* Spill left result to stack */
		out("\tpush hl\n");
		/* Evaluate right subtree (result in HL) */
		e->right = rewrite1(e->right);
		/* Pop left result, exchange so left in HL, right in DE */
		out("\tpop de\n");
		out("\tex de,hl\n");
		/* Now left in HL, right in DE - convert children to register nodes */
		freeexpr(e->left);
		freeexpr(e->right);
		e->left = mkcode(e->width, R_HL);
		e->left->op = INHL;
		e->right = mkcode(e->width, R_DE);
		e->right->op = INDE;
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
			out("\tpush hl\n");
			e->right = rewrite1(e->right);
			out("\tpop de\n\tex de,hl\n");
			freeexpr(addr);
			freeexpr(e->right);
			addr = mkcode(e->width, R_HL);
			addr->op = INHL;
			e->left = mkunary(DEREF, e->width, addr);
			e->right = mkcode(e->width, R_DE);
			e->right->op = INDE;
			goto children_done;
		} else if (e->op == ASSIGN && e->left && e->left->op == DEREF) {
			/*
			 * An assignment's lvalue is a location, not a value.
			 * Reduce the address underneath but leave the DEREF
			 * standing, so the =(D(..),..) store rules can still
			 * see it - reducing it here would apply a load rule
			 * and quietly turn the store into a fetch.
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
		if (!e->right || !e->right->nored)
			e->right = rewrite1(e->right);
	children_done: ;
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
		out("\tld hl,");
		outd((int)v);
		outc('\n');
		n = mkcode(e->width, R_HL);
		n->op = INHL;
		break;
	}
	n->dest = e->dest;
	freeexpr(e);
	return n;
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

	/* Check if code generation is incomplete */
	if (r && r->op != CODE && r->op != INHL && r->op != INDE &&
	    r->op != INBC && r->op != INA && r->op != INE) {
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
