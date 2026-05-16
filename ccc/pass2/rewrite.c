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

	/* Leaves that need loading: 1 */
	case NUMBER:
	case SYM:
	case SYMREF:
	case LOCALVAR:
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

	/* CALL: args pushed separately, result in HL */
	case CALL:
		e->regs = 1;
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
			/* For byte comparisons, mark RHS as nored if preservable */
			if ((e->width == 'b' || e->width == 'B') &&
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

/*
 * Map single char to opcode (or special pattern value)
 */
static const unsigned char op_table[256] = {
	['+'] = PLUS, ['*'] = STAR, ['-'] = MINUS, ['/'] = DIV, ['%'] = MOD,
	['&'] = AND, ['|'] = OR, ['^'] = XOR, ['<'] = LSHIFT, ['>'] = RSHIFT,
	['='] = ASSIGN, ['D'] = DEREF, ['V'] = REGVAR, ['L'] = LOCALVAR,
	['I'] = INDEX, ['H'] = INHL, ['E'] = INDE, ['A'] = INA, ['K'] = INE,
	['B'] = INBC, ['O'] = SYMREF, ['Q'] = EQ, ['U'] = NEQ, ['T'] = LT,
	['G'] = GT, ['W'] = LE, ['Y'] = GE, ['N'] = P_NUM, ['P'] = P_POW2,
	['Z'] = P_ZERO, ['M'] = P_SMALL, ['S'] = SYM, ['i'] = PREINC,
	['j'] = POSTINC, ['k'] = PREDEC, ['m'] = POSTDEC, ['a'] = ARGNODE,
	['C'] = CODE, ['o'] = OREQ, ['g'] = NEG, ['~'] = NOT, ['!'] = BANG,
	['_'] = P_ANY, ['0'] = P_NULL, ['3'] = P_MUL3, ['5'] = P_MUL5,
	['6'] = P_MUL6, ['7'] = P_MUL7, ['9'] = P_MUL9, ['x'] = P_MUL10,
	['e'] = P_MUL11, ['w'] = P_MUL12, ['f'] = P_MUL14, ['n'] = P_MUL15,
	['y'] = P_MUL20, ['q'] = P_MUL24, ['z'] = P_MUL40
};

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
	static const unsigned char multab[] = {
		[P_MUL3-238] = 3, [P_MUL5-238] = 5, [P_MUL6-238] = 6,
		[P_MUL7-238] = 7, [P_MUL9-238] = 9, [P_MUL10-238] = 10,
		[P_MUL11-238] = 11, [P_MUL12-238] = 12, [P_MUL14-238] = 14,
		[P_MUL15-238] = 15, [P_MUL20-238] = 20, [P_MUL24-238] = 24,
		[P_MUL40-238] = 40
	};

	if (pat == P_ANY) return 1;
	if (pat == P_NULL) return e == NULL;
	if (pat == P_NUM) return e && e->op == NUMBER;
	if (pat == P_POW2) return e && e->op == NUMBER && ispow2(e->u.val) > 0;
	if (pat == P_ZERO) return e && e->op == NUMBER && e->u.val == 0;
	if (pat == P_SMALL) return e && e->op == NUMBER && e->u.val >= 1 && e->u.val <= 4;
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
		if (*p != 'F' && *p != 'V' && *p != '\0' && *p != ')' && *p != ',') {
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
			if (*p == 'l' || *p == 'h') {
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
					if (mod == 'l') val = val & 0xff;
					else if (mod == 'h') val = (val >> 8) & 0xff;
					outd(val);
				} else if (n->op == SYMREF) {
					out(n->u.symref.name);
					if (n->u.symref.off != 0) {
						if (n->u.symref.off > 0)
							outc('+');
						outd(n->u.symref.off);
					}
				} else if (n->op == INDEX) {
					out(idxregname(n->u.var.reg));
					val = (char)n->u.var.off + offadj;
					if (val >= 0) outc('+');
					outd(val);
				}
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
	char reg, off;
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
			off = num ? (char)num->u.val : 0;
			/* If source is INDEX, combine offsets */
			if (src && src->op == INDEX)
				off += src->u.var.off;
		}
		n = mkindex(e->width, reg, off);
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
	}
	return f;
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

	/* EQ(x, 0) in flag context: just test x for zero */
	if (e->op == EQ && e->dest == DEST_FLAGS &&
	    e->right && e->right->op == NUMBER && e->right->u.val == 0) {
		n = e->left;
		n->dest = DEST_FLAGS;
		e->left = NULL;
		freeexpr(e);
		return n;
	}

	/* CALL(SYMREF, args...): emit call, result in HL */
	if (e->op == CALL && e->left && e->left->op == SYMREF) {
		out("\tcall ");
		out(e->left->u.symref.name);
		out("\n");
		n = mkcode(e->width, R_HL);
		n->dest = e->dest;
		freeexpr(e);
		return n;
	}


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
static Expr *
rewrite1(Expr *e)
{
	Expr *n, *next;

	if (!e) return NULL;

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

	/* QUES (ternary): cond ? then : else */
	if (e->op == QUES && e->right && e->right->op == TERNBRANCH) {
		int lbl = labelcnt++;
		Expr *tb = e->right;
		unsigned char dest = e->dest;
		/* Evaluate condition in flag context */
		e->left->dest = DEST_FLAGS;
		e->left = rewrite1(e->left);
		/* If false, jump to else */
		out("\tjp z,_T");
		outd(lbl);
		out("\n");
		/* Evaluate then-expression */
		tb->left->dest = dest;
		tb->left = rewrite1(tb->left);
		n = tb->left;
		/* Jump over else */
		out("\tjp _E");
		outd(lbl);
		out("\n");
		/* Emit else label */
		out("_T");
		outd(lbl);
		out(":\n");
		/* Evaluate else-expression */
		tb->right->dest = dest;
		tb->right = rewrite1(tb->right);
		/* Emit end label */
		out("_E");
		outd(lbl);
		out(":\n");
		/* Result is from whichever branch was taken */
		/* Return the then result node (both branches should produce same type) */
		e->left = NULL;
		tb->left = NULL;
		tb->right = NULL;
		freeexpr(e);
		return n;
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
		if (!e->left || !e->left->nored)
			e->left = rewrite1(e->left);
		if (!e->right || !e->right->nored)
			e->right = rewrite1(e->right);
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
