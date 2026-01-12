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
#define RF_NOTEQ 0x04    /* NEQ->BANG(EQ): wrap children in EQ node */
#define RF_INC1  0x08    /* increment right constant by 1 */

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
	case 'H': return INHL;
	case 'E': return INDE;
	case 'A': return INA;
	case 'O': return SYMREF;
	case 'Q': return EQ;
	case 'U': return NEQ;
	case 'T': return LT;
	case 'G': return GT;
	case 'W': return LE;
	case 'Y': return GE;
	case 'N': return P_NUM;
	case 'P': return P_POW2;
	case 'S': return SYM;
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

	/* Check width/dest suffix: :w or :F or :wF (width + flags) */
	if (*p == ':') {
		p++;
		/* check width first (b/B/s/S/l/L/p/f or _ for any) */
		if (*p != 'F' && *p != '\0' && *p != ')' && *p != ',') {
			if (*p != '_' && e && e->width != *p)
				return NULL;
			p++;
		}
		/* check flag context (F) */
		if (*p == 'F') {
			if (!e || e->dest != DEST_FLAGS)
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
	char *asmtpl;   /* asm template: $L/$R/$LL/etc for interpolation */
	unsigned char destval; /* result location: R_HL, R_A, etc (0=none) */
};

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

	while (*p) {
		if (*p == '$') {
			p++;
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
			if (*p == '+') {
				offadj = 1;
				p++;
			}
			/* navigate to node */
			n = e;
			for (i = 0; path[i] && n; i++) {
				if (path[i] == 'L') n = n->left;
				else n = n->right;
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
					val = (signed char)n->u.var.off + offadj;
					if (val >= 0) outc('+');
					outd(val);
				}
			}
		} else {
			outc(*p++);
		}
	}
}

static struct rule rules[] = {
	/* LOCALVAR -> INDEX */
	{"L", "I", "", "", "", 0, NULL, 0},

	/* PLUS(DEREF(REGVAR), NUM) -> INDEX [normalized: const on right] */
	{"+(D(V),N)", "I", "", "", "LL", RF_IXIY, NULL, 0},

	/* STAR(any, POW2) -> LSHIFT [normalized: const on right] */
	{"*(_,P)", "<", "L", "R", "", RF_POW2, NULL, 0},

	/* byte store to indexed: ld (ix+d), n */
	{"=(I,N):b", "=", "L", "R", "", 0, "\tld ($L),$R\n", 0},

	/* short store to indexed: ld (ix+d), low; ld (ix+d+1), hi */
	{"=(I,N):s", "=", "L", "R", "", 0, "\tld ($L),$Rl\n\tld ($L+),$Rh\n", 0},

	/* byte store to indexed: ld (ix+d), a */
	{"=(I,A)", "=", "L", "R", "", 0, NULL, 0},

	/* byte store to (hl): ld (hl), n */
	{"=(H,N)", "=", "L", "R", "", 0, NULL, 0},

	/* byte store to (hl): ld (hl), a */
	{"=(H,A)", "=", "L", "R", "", 0, NULL, 0},

	/* byte load from (hl): ld a, (hl) */
	{"D(H)", "D", "L", "", "", 0, NULL, 0},

	/* byte deref indexed for flags: ld a,(ix+d); or a -> Z */
	{"D(I):bF", "D", "L", "", "", 0, "\tld a,($L)\n\tor a\n", F_Z},

	/* short deref indexed for flags: or low,hi -> Z */
	{"D(I):sF", "D", "L", "", "", 0, "\tld a,($L)\n\tor a,($L+)\n", F_Z},

	/* byte load from indexed for value: ld a, (ix+d) */
	{"D(I):b", "D", "L", "", "", 0, NULL, 0},

	/* 16-bit add: add hl, de */
	{"+(H,E)", "+", "L", "R", "", 0, NULL, 0},

	/* byte add immediate: add a, n */
	{"+(A,N)", "+", "L", "R", "", 0, NULL, 0},

	/* byte sub immediate: sub n */
	{"-(A,N)", "-", "L", "R", "", 0, NULL, 0},

	/* compare equal: cp n (Z flag) - value already in A */
	{"Q(A,N):F", "Q", "L", "R", "", 0, NULL, 0},

	/* compare equal: ld a,(sym); cp n (Z flag) */
	{"Q(D(O),N):F", "Q", "L", "R", "", 0, "\tld a,($LL)\n\tcp $R\n", F_Z},

	/* compare equal byte indexed: ld a,(ix+d); cp n (Z flag) */
	{"Q(D(I),N):bF", "Q", "L", "R", "", 0, "\tld a,($LL)\n\tcp $R\n", F_Z},

	/* compare less than: cp n (C flag) - value already in A */
	{"T(A,N):F", "T", "L", "R", "", 0, NULL, 0},

	/* compare less than: ld a,(sym); cp n (C flag) */
	{"T(D(O),N):F", "T", "L", "R", "", 0, "\tld a,($LL)\n\tcp $R\n", F_C},

	/* compare less than byte indexed: ld a,(ix+d); cp n (C flag) */
	{"T(D(I),N):bF", "T", "L", "R", "", 0, "\tld a,($LL)\n\tcp $R\n", F_C},

	/* NEQ -> BANG(EQ): normalize for conditional jumps */
	{"U(_,_)", "!", "L", "R", "", RF_NOTEQ, NULL, 0},

	/* GE: cp n, jp nc (cheap - direct flag) - value already in A */
	{"Y(A,N):F", "Y", "L", "R", "", 0, NULL, 0},

	/* GE: ld a,(sym); cp n (NC flag) */
	{"Y(D(O),N):F", "Y", "L", "R", "", 0, "\tld a,($LL)\n\tcp $R\n", F_NC},

	/* GE byte indexed: ld a,(ix+d); cp n (NC flag) */
	{"Y(D(I),N):bF", "Y", "L", "R", "", 0, "\tld a,($LL)\n\tcp $R\n", F_NC},

	/* GT(a,n) -> GE(a,n+1): a > n iff a >= n+1 */
	{"G(_,N)", "Y", "L", "R", "", RF_INC1, NULL, 0},

	/* LE(a,n) -> LT(a,n+1): a <= n iff a < n+1 */
	{"W(_,N)", "T", "L", "R", "", RF_INC1, NULL, 0},

	/* SYM + NUMBER -> SYMREF (linker-resolvable) */
	{"+(S,N)", "O", "", "", "", 0, NULL, 0},

	/* SYMREF + NUMBER -> SYMREF with combined offset */
	{"+(O,N)", "O", "", "", "", 0, NULL, 0},

	/* bare SYM -> SYMREF with offset 0 */
	{"S", "O", "", "", "", 0, NULL, 0},

	{NULL, NULL, NULL, NULL, NULL, 0, NULL, 0}
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

	/* Handle NEQ -> BANG(EQ) */
	if (rp->flags & RF_NOTEQ) {
		Expr *eq = mkbinary(EQ, e->width, e->left, e->right);
		eq->dest = e->dest;
		e->op = BANG;
		e->left = eq;
		e->right = NULL;
		return e;
	}

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

	/* Increment constant by 1 (for GT->GE, LE->LT transforms) */
	if ((rp->flags & RF_INC1) && e->right && e->right->op == NUMBER) {
		e->right->u.val++;
	}

	/* Emit assembly and create CODE node if template present */
	if (rp->asmtpl) {
		emitasm(rp->asmtpl, e);
		n = mkcode(e->width, rp->destval);
		n->dest = e->dest;
		freeexpr(e);
		return n;
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
