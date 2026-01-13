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
#include "../cpp/lexeme.h"
#include <stdlib.h>

/* Label counter for short-circuit jumps */
static int labelcnt;

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
		} else {
			/* Only need one, propagate target */
			assign(e->left, tgt);
			assign(e->right, tgt);
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

/* Replacement flags */
#define RF_POW2  0x01    /* transform constant through log2 */
#define RF_IXIY  0x02    /* require reg is IX or IY */
#define RF_NOTEQ 0x04    /* NEQ->BANG(EQ): wrap children in EQ node */
#define RF_INC1  0x08    /* increment right constant by 1 */
#define RF_BC    0x10    /* require reg is BC */
#define RF_DE    0x20    /* require reg is DE */
#define RF_HL    0x40    /* require reg is HL */
#define RF_IX    0x80    /* require reg is IX */
#define RF_C     0x100   /* require reg is C (low byte of BC) */
#define RF_B     0x200   /* require reg is B (high byte of BC) */

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
	case 'B': return INBC;
	case 'O': return SYMREF;
	case 'Q': return EQ;
	case 'U': return NEQ;
	case 'T': return LT;
	case 'G': return GT;
	case 'W': return LE;
	case 'Y': return GE;
	case 'N': return P_NUM;
	case 'P': return P_POW2;
	case 'Z': return P_ZERO;
	case 'M': return P_SMALL;
	case 'S': return SYM;
	case 'i': return PREINC;
	case 'j': return POSTINC;
	case 'k': return PREDEC;
	case 'm': return POSTDEC;
	case 'a': return ARGNODE;
	case 'C': return CODE;
	case 'o': return OREQ;
	case 'g': return NEG;
	case '~': return NOT;
	case '!': return BANG;
	case '_': return P_ANY;
	case '0': return P_NULL;
	case '3': return P_MUL3;
	case '5': return P_MUL5;
	case '6': return P_MUL6;
	case '7': return P_MUL7;
	case '9': return P_MUL9;
	case 'x': return P_MUL10;
	case 'e': return P_MUL11;
	case 'w': return P_MUL12;
	case 'f': return P_MUL14;
	case 'n': return P_MUL15;
	case 'y': return P_MUL20;
	case 'q': return P_MUL24;
	case 'z': return P_MUL40;
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
	if (pat == P_ANY) return 1;
	if (pat == P_NULL) return e == NULL;
	if (pat == P_NUM) return e && e->op == NUMBER;
	if (pat == P_POW2) return e && e->op == NUMBER && ispow2(e->u.val) > 0;
	if (pat == P_ZERO) return e && e->op == NUMBER && e->u.val == 0;
	if (pat == P_SMALL) return e && e->op == NUMBER && e->u.val >= 1 && e->u.val <= 4;
	if (pat == P_MUL3) return e && e->op == NUMBER && e->u.val == 3;
	if (pat == P_MUL5) return e && e->op == NUMBER && e->u.val == 5;
	if (pat == P_MUL6) return e && e->op == NUMBER && e->u.val == 6;
	if (pat == P_MUL7) return e && e->op == NUMBER && e->u.val == 7;
	if (pat == P_MUL9) return e && e->op == NUMBER && e->u.val == 9;
	if (pat == P_MUL10) return e && e->op == NUMBER && e->u.val == 10;
	if (pat == P_MUL11) return e && e->op == NUMBER && e->u.val == 11;
	if (pat == P_MUL12) return e && e->op == NUMBER && e->u.val == 12;
	if (pat == P_MUL14) return e && e->op == NUMBER && e->u.val == 14;
	if (pat == P_MUL15) return e && e->op == NUMBER && e->u.val == 15;
	if (pat == P_MUL20) return e && e->op == NUMBER && e->u.val == 20;
	if (pat == P_MUL24) return e && e->op == NUMBER && e->u.val == 24;
	if (pat == P_MUL40) return e && e->op == NUMBER && e->u.val == 40;
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
	unsigned short flags;
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

static struct rule rules[] = {
	/* LOCALVAR -> INDEX */
	{"L", "I", "", "", "", 0, NULL, 0},

	/* REGVAR -> IN* (value is in register) */
	{"V", "B", "", "", "", RF_BC, NULL, 0},
	{"V", "E", "", "", "", RF_DE, NULL, 0},
	{"V", "H", "", "", "", RF_HL, NULL, 0},

	/* REGVAR IX in flag context: test for zero */
	{"V:F", "V", "", "", "", RF_IX, "\tld a,ixl\n\tor a,ixh\n", F_NZ},

	/* INBC in flag context: test for zero */
	{"B:F", "B", "", "", "", 0, "\tld a,c\n\tor a,b\n", F_NZ},

	/* REGVAR byte C in flag context */
	{"V:bF", "V", "", "", "", RF_C, "\tld a,c\n\tor a\n", F_NZ},

	/* REGVAR byte B in flag context */
	{"V:bF", "V", "", "", "", RF_B, "\tld a,b\n\tor a\n", F_NZ},

	/* assign constant to REGVAR C */
	{"=(V,N):b", "=", "L", "R", "L", RF_C, "\tld c,$R\n", R_A},

	/* assign constant to REGVAR B */
	{"=(V,N):b", "=", "L", "R", "L", RF_B, "\tld b,$R\n", R_A},

	/* assign A to REGVAR C */
	{"=(V,A):b", "=", "L", "R", "L", RF_C, "\tld c,a\n", R_A},

	/* assign A to REGVAR B */
	{"=(V,A):b", "=", "L", "R", "L", RF_B, "\tld b,a\n", R_A},

	/* assign HL (low byte) to REGVAR C */
	{"=(V,H):b", "=", "L", "R", "L", RF_C, "\tld c,l\n", R_HL},

	/* assign HL (low byte) to REGVAR B */
	{"=(V,H):b", "=", "L", "R", "L", RF_B, "\tld b,l\n", R_HL},

	/* load REGVAR C to HL (zero-extended) */
	{"=(H,V):b", "=", "L", "R", "R", RF_C, "\tld l,c\n\tld h,0\n", R_HL},

	/* load REGVAR B to HL (zero-extended) */
	{"=(H,V):b", "=", "L", "R", "R", RF_B, "\tld l,b\n\tld h,0\n", R_HL},

	/* REGVAR C -> INA (value in C, byte context) */
	{"V:b", "A", "", "", "", RF_C, "\tld a,c\n", R_A},

	/* REGVAR B -> INA (value in B, byte context) */
	{"V:b", "A", "", "", "", RF_B, "\tld a,b\n", R_A},

	/* INHL in flag context: test for zero */
	{"H:F", "H", "", "", "", 0, "\tld a,l\n\tor a,h\n", F_NZ},

	/* INDE in flag context: test for zero */
	{"E:F", "E", "", "", "", 0, "\tld a,e\n\tor a,d\n", F_NZ},

	/* INA in flag context: test for zero */
	{"A:F", "A", "", "", "", 0, "\tor a\n", F_NZ},

	/* copy IX to HL (must use push/pop) */
	{"=(H,V)", "=", "L", "R", "R", RF_IX, "\tpush ix\n\tpop hl\n", R_HL},

	/* copy IX to BC */
	{"=(B,V)", "=", "L", "R", "R", RF_IX, "\tld c,ixl\n\tld b,ixh\n", R_BC},

	/* copy IX to DE */
	{"=(E,V)", "=", "L", "R", "R", RF_IX, "\tld e,ixl\n\tld d,ixh\n", R_DE},

	/* PLUS(REGVAR IX, NUM) -> INDEX (ix+offset addressing) */
	{"+(V,N)", "I", "", "", "L", RF_IX, NULL, 0},

	/* PLUS(DEREF(REGVAR), NUM) -> INDEX [normalized: const on right] */
	{"+(D(V),N)", "I", "", "", "LL", RF_IXIY, NULL, 0},

	/* PLUS(INDEX, NUM) -> INDEX (combine offsets) */
	{"+(I,N)", "I", "", "", "L", 0, NULL, 0},

	/* STAR(any, POW2) -> LSHIFT [normalized: const on right] */
	{"*(_,P)", "<", "L", "R", "", RF_POW2, NULL, 0},

	/* STAR by small constants with few set bits */
	/* hl*3 = hl + hl*2 */
	{"*(H,3)", "*", "L", "", "", 0, "\tld d,h\n\tld e,l\n\tadd hl,hl\n\tadd hl,de\n", R_HL},
	/* hl*5 = hl + hl*4 */
	{"*(H,5)", "*", "L", "", "", 0, "\tld d,h\n\tld e,l\n\tadd hl,hl\n\tadd hl,hl\n\tadd hl,de\n", R_HL},
	/* hl*6 = (hl*3)*2 */
	{"*(H,6)", "*", "L", "", "", 0, "\tld d,h\n\tld e,l\n\tadd hl,hl\n\tadd hl,de\n\tadd hl,hl\n", R_HL},
	/* hl*7 = hl + (hl*3)*2 */
	{"*(H,7)", "*", "L", "", "", 0, "\tld d,h\n\tld e,l\n\tadd hl,hl\n\tadd hl,de\n\tadd hl,hl\n\tadd hl,de\n", R_HL},
	/* hl*9 = hl + hl*8 */
	{"*(H,9)", "*", "L", "", "", 0, "\tld d,h\n\tld e,l\n\tadd hl,hl\n\tadd hl,hl\n\tadd hl,hl\n\tadd hl,de\n", R_HL},
	/* hl*10 = (hl*5)*2 */
	{"*(H,x)", "*", "L", "", "", 0, "\tld d,h\n\tld e,l\n\tadd hl,hl\n\tadd hl,hl\n\tadd hl,de\n\tadd hl,hl\n", R_HL},
	/* hl*11 = hl*10 + hl */
	{"*(H,e)", "*", "L", "", "", 0, "\tld d,h\n\tld e,l\n\tadd hl,hl\n\tadd hl,hl\n\tadd hl,de\n\tadd hl,hl\n\tadd hl,de\n", R_HL},
	/* hl*12 = (hl*3)*4 */
	{"*(H,w)", "*", "L", "", "", 0, "\tld d,h\n\tld e,l\n\tadd hl,hl\n\tadd hl,de\n\tadd hl,hl\n\tadd hl,hl\n", R_HL},
	/* hl*14 = (hl*7)*2 */
	{"*(H,f)", "*", "L", "", "", 0, "\tld d,h\n\tld e,l\n\tadd hl,hl\n\tadd hl,de\n\tadd hl,hl\n\tadd hl,de\n\tadd hl,hl\n", R_HL},
	/* hl*15 = hl*14 + hl */
	{"*(H,n)", "*", "L", "", "", 0, "\tld d,h\n\tld e,l\n\tadd hl,hl\n\tadd hl,de\n\tadd hl,hl\n\tadd hl,de\n\tadd hl,hl\n\tadd hl,de\n", R_HL},
	/* hl*20 = (hl*5)*4 */
	{"*(H,y)", "*", "L", "", "", 0, "\tld d,h\n\tld e,l\n\tadd hl,hl\n\tadd hl,hl\n\tadd hl,de\n\tadd hl,hl\n\tadd hl,hl\n", R_HL},
	/* hl*24 = (hl*3)*8 */
	{"*(H,q)", "*", "L", "", "", 0, "\tld d,h\n\tld e,l\n\tadd hl,hl\n\tadd hl,de\n\tadd hl,hl\n\tadd hl,hl\n\tadd hl,hl\n", R_HL},
	/* hl*40 = (hl*5)*8 */
	{"*(H,z)", "*", "L", "", "", 0, "\tld d,h\n\tld e,l\n\tadd hl,hl\n\tadd hl,hl\n\tadd hl,de\n\tadd hl,hl\n\tadd hl,hl\n\tadd hl,hl\n", R_HL},

	/* general HL*DE: call runtime multiply */
	{"*(H,E)", "*", "L", "R", "", 0, "\tcall __mul16\n", R_HL},

	/* general HL/DE: call runtime divide */
	{"/(H,E)", "/", "L", "R", "", 0, "\tcall __div16\n", R_HL},

	/* general HL%DE: call runtime modulo */
	{"%(H,E)", "%", "L", "R", "", 0, "\tcall __mod16\n", R_HL},

	/* byte store to indexed: ld (ix+d), n */
	{"=(I,N):b", "=", "L", "R", "", 0, "\tld ($L),$R\n", 0},

	/* short store to indexed: ld (ix+d), low; ld (ix+d+1), hi */
	{"=(I,N):s", "=", "L", "R", "", 0, "\tld ($L),$Rl\n\tld ($L+),$Rh\n", 0},

	/* short store HL to indexed */
	{"=(I,H):s", "=", "L", "R", "", 0, "\tld ($L),l\n\tld ($L+),h\n", 0},

	/* short copy INDEX to INDEX: load then store */
	{"=(I,I):s", "=", "L", "R", "", 0, "\tld l,($R)\n\tld h,($R+)\n\tld ($L),l\n\tld ($L+),h\n", R_HL},

	/* short store DE to indexed */
	{"=(I,E):s", "=", "L", "R", "", 0, "\tld ($L),e\n\tld ($L+),d\n", 0},

	/* byte store to symref: ld (sym), a */
	{"=(O,A):b", "=", "L", "R", "", 0, "\tld ($L),a\n", R_A},

	/* byte store constant to symref */
	{"=(O,N):b", "=", "L", "R", "", 0, "\tld a,$R\n\tld ($L),a\n", R_A},

	/* short store HL to symref */
	{"=(O,H):s", "=", "L", "R", "", 0, "\tld ($L),hl\n", R_HL},

	/* short store constant to symref */
	{"=(O,N):s", "=", "L", "R", "", 0, "\tld hl,$R\n\tld ($L),hl\n", R_HL},

	/* short store BC to indexed */
	{"=(I,B):s", "=", "L", "R", "", 0, "\tld ($L),c\n\tld ($L+),b\n", 0},

	/* load constant to register variable */
	{"=(V,N)", "=", "L", "R", "L", RF_IX, "\tld ix,$R\n", R_IX},
	{"=(V,N)", "=", "L", "R", "L", RF_BC, "\tld bc,$R\n", R_BC},
	{"=(V,N)", "=", "L", "R", "L", RF_DE, "\tld de,$R\n", R_DE},
	{"=(V,N)", "=", "L", "R", "L", RF_HL, "\tld hl,$R\n", R_HL},

	/* load constant to register (already converted) */
	{"=(B,N)", "=", "L", "R", "", 0, "\tld bc,$R\n", R_BC},
	{"=(E,N)", "=", "L", "R", "", 0, "\tld de,$R\n", R_DE},
	{"=(H,N)", "=", "L", "R", "", 0, "\tld hl,$R\n", R_HL},

	/* assign to IX register variable */
	{"=(V,H)", "=", "L", "R", "L", RF_IX, "\tpush hl\n\tpop ix\n", R_IX},
	{"=(V,E)", "=", "L", "R", "L", RF_IX, "\tpush de\n\tpop ix\n", R_IX},
	{"=(V,B)", "=", "L", "R", "L", RF_IX, "\tpush bc\n\tpop ix\n", R_IX},

	/* register-to-register moves */
	{"=(B,H)", "=", "L", "R", "", 0, "\tld c,l\n\tld b,h\n", R_BC},
	{"=(E,H)", "=", "L", "R", "", 0, "\tex de,hl\n", R_DE},
	{"=(H,E)", "=", "L", "R", "", 0, "\tex de,hl\n", R_HL},
	{"=(H,B)", "=", "L", "R", "", 0, "\tld l,c\n\tld h,b\n", R_HL},
	{"=(B,E)", "=", "L", "R", "", 0, "\tld c,e\n\tld b,d\n", R_BC},
	{"=(E,B)", "=", "L", "R", "", 0, "\tld e,c\n\tld d,b\n", R_DE},
	{"=(B,B)", "=", "L", "R", "", 0, "", R_BC},  /* nop */
	{"=(E,E)", "=", "L", "R", "", 0, "", R_DE},  /* nop */
	{"=(H,H)", "=", "L", "R", "", 0, "", R_HL},  /* nop */

	/* assign register to CODE - result already in place */
	{"=(C,H)", "=", "L", "R", "", 0, "", R_HL},  /* nop */
	{"=(C,E)", "=", "L", "R", "", 0, "", R_DE},  /* nop */
	{"=(C,B)", "=", "L", "R", "", 0, "", R_BC},  /* nop */
	{"=(C,A)", "=", "L", "R", "", 0, "", R_A},   /* nop */

	/* assign A (zero-extended) to BC: ld c,a; ld b,0 */
	{"=(B,A)", "=", "L", "R", "", 0, "\tld c,a\n\tld b,0\n", R_BC},

	/* assign A (zero-extended) to HL: ld l,a; ld h,0 */
	{"=(H,A)", "=", "L", "R", "", 0, "\tld l,a\n\tld h,0\n", R_HL},

	/* assign A (zero-extended) to DE: ld e,a; ld d,0 */
	{"=(E,A)", "=", "L", "R", "", 0, "\tld e,a\n\tld d,0\n", R_DE},

	/* BC + constant -> HL (for struct member access via BC pointer) */
	{"+(B,N)", "+", "L", "R", "", 0, "\tld l,c\n\tld h,b\n\tld de,$R\n\tadd hl,de\n", R_HL},

	/* BC + small constant -> HL (more efficient for 1-4) */
	{"+(B,M)", "+", "L", "R", "", 0, "\tld l,c\n\tld h,b\n%(\tinc hl\n)", R_HL},

	/* DE + constant -> HL */
	{"+(E,N)", "+", "L", "R", "", 0, "\tex de,hl\n\tld de,$R\n\tadd hl,de\n", R_HL},

	/* DE + small constant -> HL (more efficient for 1-4) */
	{"+(E,M)", "+", "L", "R", "", 0, "\tex de,hl\n%(\tinc hl\n)", R_HL},

	/* NEG BC: negate BC register (result in HL) */
	{"g(B)", "g", "L", "", "", 0, "\tld a,0\n\tsub c\n\tld l,a\n\tld a,0\n\tsbc a,b\n\tld h,a\n", R_HL},

	/* NEG HL: negate HL register */
	{"g(H)", "g", "L", "", "", 0, "\txor a\n\tsub l\n\tld l,a\n\tld a,0\n\tsbc a,h\n\tld h,a\n", R_HL},

	/* NEG DE: negate DE register (result in HL) */
	{"g(E)", "g", "L", "", "", 0, "\tld a,0\n\tsub e\n\tld l,a\n\tld a,0\n\tsbc a,d\n\tld h,a\n", R_HL},

	/* PREINC BC: ++bc (result in HL) */
	{"i(B)", "i", "L", "", "", 0, "\tinc bc\n\tld l,c\n\tld h,b\n", R_HL},

	/* PREDEC BC: --bc (result in HL) */
	{"k(B)", "k", "L", "", "", 0, "\tdec bc\n\tld l,c\n\tld h,b\n", R_HL},

	/* PREINC indexed short: ++(ix+d) */
	{"i(I):s", "i", "L", "", "", 0, "\tld l,($L)\n\tld h,($L+)\n\tinc hl\n\tld ($L),l\n\tld ($L+),h\n", R_HL},

	/* PREDEC indexed short: --(ix+d) */
	{"k(I):s", "k", "L", "", "", 0, "\tld l,($L)\n\tld h,($L+)\n\tdec hl\n\tld ($L),l\n\tld ($L+),h\n", R_HL},

	/* PREINC indexed byte: ++(ix+d) - result in A */
	{"i(I):b", "i", "L", "", "", 0, "\tld a,($L)\n\tinc a\n\tld ($L),a\n", R_A},

	/* PREDEC indexed byte: --(ix+d) - result in A */
	{"k(I):b", "k", "L", "", "", 0, "\tld a,($L)\n\tdec a\n\tld ($L),a\n", R_A},

	/* byte store to indexed: ld (ix+d), a */
	{"=(I,A)", "=", "L", "R", "", 0, "\tld ($L),a\n", R_A},

	/* byte store to (hl): ld (hl), n */
	{"=(H,N)", "=", "L", "R", "", 0, NULL, 0},

	/* byte assign A to HL: ld l,a (for byte returns) */
	{"=(H,A):b", "=", "L", "R", "", 0, "\tld l,a\n", R_HL},

	/* byte store REGVAR B to (HL) - store low byte of BC */
	{"=(H,V):b", "=", "L", "R", "R", RF_BC, "\tld (hl),c\n", 0},

	/* byte load from (hl): ld a, (hl) */
	{"D(H):b", "D", "L", "", "", 0, "\tld a,(hl)\n", R_A},

	/* byte load from (bc): move to hl, then load */
	{"D(B):b", "D", "L", "", "", 0, "\tld l,c\n\tld h,b\n\tld a,(hl)\n", R_A},

	/* short load from (bc): move to hl, load */
	{"D(B):s", "D", "L", "", "", 0, "\tld l,c\n\tld h,b\n\tld a,(hl)\n\tinc hl\n\tld h,(hl)\n\tld l,a\n", R_HL},

	/* byte load from (de): move to hl, then load */
	{"D(E):b", "D", "L", "", "", 0, "\tex de,hl\n\tld a,(hl)\n", R_A},

	/* short load from (de): move to hl, load */
	{"D(E):s", "D", "L", "", "", 0, "\tex de,hl\n\tld e,(hl)\n\tinc hl\n\tld d,(hl)\n\tex de,hl\n", R_HL},

	/* byte store A to *bc (indirect through BC) */
	{"=(D(B),A):b", "=", "L", "R", "", 0, "\tld l,c\n\tld h,b\n\tld (hl),a\n", 0},

	/* byte store A to *de (indirect through DE) */
	{"=(D(E),A):b", "=", "L", "R", "", 0, "\tex de,hl\n\tld (hl),a\n\tex de,hl\n", 0},

	/* short store HL to *bc (indirect through BC) */
	{"=(D(B),H):s", "=", "L", "R", "", 0, "\tpush hl\n\tld l,c\n\tld h,b\n\tpop de\n\tld (hl),e\n\tinc hl\n\tld (hl),d\n", 0},

	/* short store HL to *de (indirect through DE) */
	{"=(D(E),H):s", "=", "L", "R", "", 0, "\tex de,hl\n\tpush de\n\tld (hl),e\n\tinc hl\n\tld (hl),d\n\tpop hl\n", 0},

	/* byte store constant to *hl */
	{"=(D(H),N):b", "=", "L", "R", "", 0, "\tld (hl),$R\n", 0},

	/* short store DE to *hl */
	{"=(D(H),E):s", "=", "L", "R", "", 0, "\tld (hl),e\n\tinc hl\n\tld (hl),d\n", 0},

	/* byte store constant to *bc */
	{"=(D(B),N):b", "=", "L", "R", "", 0, "\tld l,c\n\tld h,b\n\tld (hl),$R\n", 0},

	/* short store constant to *hl */
	{"=(D(H),N):s", "=", "L", "R", "", 0, "\tld (hl),$Rl\n\tinc hl\n\tld (hl),$Rh\n", 0},

	/* short store BC to *hl */
	{"=(D(H),B):s", "=", "L", "R", "", 0, "\tld (hl),c\n\tinc hl\n\tld (hl),b\n", 0},

	/* pointer deref for flags (test if pointer is null) */
	{"D(H):pF", "D", "L", "", "", 0, "\tld a,(hl)\n\tor a,(hl)\n", F_NZ},

	/* short load from (hl) to BC: ld c,(hl); inc hl; ld b,(hl) */
	{"=(B,D(H)):s", "=", "L", "R", "", 0, "\tld c,(hl)\n\tinc hl\n\tld b,(hl)\n", R_BC},

	/* load indexed address into BC: copy IX+offset to BC */
	{"=(B,I)", "=", "L", "R", "", 0, "\tld c,($R)\n\tld b,($R+)\n", R_BC},

	/* load SYMREF address into BC */
	{"=(B,O)", "=", "L", "R", "", 0, "\tld bc,$R\n", R_BC},

	/* short load from symref into BC: must go via A */
	{"=(B,D(O)):s", "=", "L", "R", "", 0, "\tld a,($RL)\n\tld c,a\n\tld a,($RL+)\n\tld b,a\n", R_BC},

	/* short load from symref into DE */
	{"=(E,D(O)):s", "=", "L", "R", "", 0, "\tld de,($RL)\n", R_DE},

	/* short load from symref into HL */
	{"=(H,D(O)):s", "=", "L", "R", "", 0, "\tld hl,($RL)\n", R_HL},

	/* short load from (hl) to DE: ld e,(hl); inc hl; ld d,(hl) */
	{"=(E,D(H)):s", "=", "L", "R", "", 0, "\tld e,(hl)\n\tinc hl\n\tld d,(hl)\n", R_DE},

	/* short load from (hl) to HL: need temp */
	{"=(H,D(H)):s", "=", "L", "R", "", 0, "\tld a,(hl)\n\tinc hl\n\tld h,(hl)\n\tld l,a\n", R_HL},

	/* short store DEREF(HL) to indexed */
	{"=(I,D(H)):s", "=", "L", "R", "", 0, "\tld a,(hl)\n\tld ($L),a\n\tinc hl\n\tld a,(hl)\n\tld ($L+),a\n", 0},

	/* byte or-equals on (hl): ld a,(hl); or N; ld (hl),a */
	{"o(H,N):b", "o", "L", "R", "", 0, "\tld a,(hl)\n\tor $R\n\tld (hl),a\n", R_A},

	/* byte deref indexed for flags: ld a,(ix+d); or a -> Z */
	{"D(I):bF", "D", "L", "", "", 0, "\tld a,($L)\n\tor a\n", F_Z},

	/* short deref indexed for flags: or low,hi -> Z */
	{"D(I):sF", "D", "L", "", "", 0, "\tld a,($L)\n\tor a,($L+)\n", F_Z},

	/* short load from indexed for value: ld t,(ix+d); ld u,(ix+d+1) */
	{"D(I):s", "D", "L", "", "", 0, "\tld $t,($L)\n\tld $u,($L+)\n", 0},

	/* byte load from indexed for value: ld a, (ix+d) */
	{"D(I):b", "D", "L", "", "", 0, NULL, 0},

	/* byte load from symref: ld a, (sym) */
	{"D(O):b", "D", "L", "", "", 0, "\tld a,($L)\n", R_A},

	/* short load from symref: ld hl, (sym) */
	{"D(O):s", "D", "L", "", "", 0, "\tld hl,($L)\n", R_HL},

	/* 16-bit add: add hl, de */
	{"+(H,E)", "+", "L", "R", "", 0, "\tadd hl,de\n", R_HL},

	/* small increment/decrement: use inc/dec instructions */
	{"+(H,M)", "+", "L", "R", "", 0, "%(\tinc hl\n)", R_HL},
	{"-(H,M)", "-", "L", "R", "", 0, "%(\tdec hl\n)", R_HL},
	{"+(A,M)", "+", "L", "R", "", 0, "%(\tinc a\n)", R_A},
	{"-(A,M)", "-", "L", "R", "", 0, "%(\tdec a\n)", R_A},

	/* add constant to HL using DE */
	{"+(H,N)", "+", "L", "R", "", 0, "\tld de,$R\n\tadd hl,de\n", R_HL},

	/* byte add immediate: add a, n */
	{"+(A,N)", "+", "L", "R", "", 0, NULL, 0},

	/* byte add indexed + constant: ld a,(ix+d); add a,n */
	{"+(D(I),N):b", "+", "L", "R", "", 0, "\tld a,($LL)\n\tadd a,$R\n", R_A},

	/* byte sub immediate: sub n */
	{"-(A,N)", "-", "L", "R", "", 0, NULL, 0},

	/* byte sub indexed - constant: ld a,(ix+d); sub n */
	{"-(D(I),N):b", "-", "L", "R", "", 0, "\tld a,($LL)\n\tsub $R\n", R_A},

	/* 16-bit subtract: HL - DE */
	{"-(H,E)", "-", "L", "R", "", 0, "\tor a\n\tsbc hl,de\n", R_HL},

	/* 16-bit subtract constant: HL - N */
	{"-(H,N)", "-", "L", "R", "", 0, "\tld de,$R\n\tor a\n\tsbc hl,de\n", R_HL},

	/* 16-bit left shift by constant: use add hl,hl */
	{"<(H,N)", "<", "L", "R", "", 0, "%(\tadd hl,hl\n)", R_HL},

	/* byte left shift: sla a N times */
	{"<(A,N):b", "<", "L", "R", "", 0, "%(\tsla a\n)", R_A},

	/* byte right shift (logical): srl a N times */
	{">(A,N):b", ">", "L", "R", "", 0, "%(\tsrl a\n)", R_A},

	/* 16-bit right shift by small constant (1-4): srl h; rr l repeated */
	{">(H,M)", ">", "L", "R", "", 0, "%(\tsrl h\n\trr l\n)", R_HL},

	/* assign indexed byte to A */
	{"=(A,D(I)):b", "=", "L", "R", "", 0, "\tld a,($RL)\n", R_A},

	/* assign symref byte to A */
	{"=(A,D(O)):b", "=", "L", "R", "", 0, "\tld a,($RL)\n", R_A},

	/* assign A to A: nop */
	{"=(A,A)", "=", "L", "R", "", 0, "", R_A},

	/* byte bit test indexed: bit n,(ix+d) - Z=0 if bit set */
	{"&(D(I),P):bF", "&", "L", "R", "", RF_POW2, "\tbit $R,($LL)\n", F_NZ},

	/* byte AND indexed: ld a,(ix+d); and n */
	{"&(D(I),N):b", "&", "L", "R", "", 0, "\tld a,($LL)\n\tand $R\n", R_A},

	/* byte OR indexed: ld a,(ix+d); or n */
	{"|(D(I),N):b", "|", "L", "R", "", 0, "\tld a,($LL)\n\tor $R\n", R_A},

	/* byte XOR indexed: ld a,(ix+d); xor n */
	{"^(D(I),N):b", "^", "L", "R", "", 0, "\tld a,($LL)\n\txor $R\n", R_A},

	/* byte AND A with constant */
	{"&(A,N):b", "&", "L", "R", "", 0, "\tand $R\n", R_A},

	/* byte OR A with constant */
	{"|(A,N):b", "|", "L", "R", "", 0, "\tor $R\n", R_A},

	/* byte XOR A with constant */
	{"^(A,N):b", "^", "L", "R", "", 0, "\txor $R\n", R_A},

	/* 16-bit AND: HL & DE */
	{"&(H,E)", "&", "L", "R", "", 0, "\tld a,l\n\tand e\n\tld l,a\n\tld a,h\n\tand d\n\tld h,a\n", R_HL},

	/* 16-bit OR: HL | DE */
	{"|(H,E)", "|", "L", "R", "", 0, "\tld a,l\n\tor e\n\tld l,a\n\tld a,h\n\tor d\n\tld h,a\n", R_HL},

	/* 16-bit XOR: HL ^ DE */
	{"^(H,E)", "^", "L", "R", "", 0, "\tld a,l\n\txor e\n\tld l,a\n\tld a,h\n\txor d\n\tld h,a\n", R_HL},

	/* 16-bit AND with constant */
	{"&(H,N)", "&", "L", "R", "", 0, "\tld a,l\n\tand $Rl\n\tld l,a\n\tld a,h\n\tand $Rh\n\tld h,a\n", R_HL},

	/* 16-bit OR with constant */
	{"|(H,N)", "|", "L", "R", "", 0, "\tld a,l\n\tor $Rl\n\tld l,a\n\tld a,h\n\tor $Rh\n\tld h,a\n", R_HL},

	/* 16-bit XOR with constant */
	{"^(H,N)", "^", "L", "R", "", 0, "\tld a,l\n\txor $Rl\n\tld l,a\n\tld a,h\n\txor $Rh\n\tld h,a\n", R_HL},

	/* bitwise NOT on A */
	{"~(A):b", "~", "L", "", "", 0, "\tcpl\n", R_A},

	/* compare equal: cp n (Z flag) - value already in A */
	{"Q(A,N):F", "Q", "L", "R", "", 0, "\tcp $R\n", F_Z},

	/* compare equal: ld a,(sym); cp n (Z flag) */
	{"Q(D(O),N):F", "Q", "L", "R", "", 0, "\tld a,($LL)\n\tcp $R\n", F_Z},

	/* compare equal byte indexed: ld a,(ix+d); cp n (Z flag) */
	{"Q(D(I),N):bF", "Q", "L", "R", "", 0, "\tld a,($LL)\n\tcp $R\n", F_Z},

	/* compare less than: cp n (C flag) - value already in A */
	{"T(A,N):F", "T", "L", "R", "", 0, "\tcp $R\n", F_C},

	/* compare 0 < A: just test A for nonzero */
	{"T(Z,A):F", "T", "L", "R", "", 0, "\tor a\n", F_NZ},

	/* compare less than: ld a,(sym); cp n (C flag) */
	{"T(D(O),N):F", "T", "L", "R", "", 0, "\tld a,($LL)\n\tcp $R\n", F_C},

	/* compare less than byte indexed: ld a,(ix+d); cp n (C flag) */
	{"T(D(I),N):bF", "T", "L", "R", "", 0, "\tld a,($LL)\n\tcp $R\n", F_C},

	/* compare equal HL with constant: sub and test for zero */
	{"Q(H,N):F", "Q", "L", "R", "", 0, "\tld a,l\n\tsub $Rl\n\tld a,h\n\tsbc a,$Rh\n\tor a\n", F_Z},

	/* compare equal BC with constant */
	{"Q(B,N):F", "Q", "L", "R", "", 0, "\tld a,c\n\tsub $Rl\n\tld a,b\n\tsbc a,$Rh\n\tor a\n", F_Z},

	/* compare less than HL < constant: unsigned 16-bit compare */
	{"T(H,N):F", "T", "L", "R", "", 0, "\tld a,l\n\tsub $Rl\n\tld a,h\n\tsbc a,$Rh\n", F_C},

	/* compare less than BC < constant */
	{"T(B,N):F", "T", "L", "R", "", 0, "\tld a,c\n\tsub $Rl\n\tld a,b\n\tsbc a,$Rh\n", F_C},

	/* compare GE HL >= constant: unsigned 16-bit compare */
	{"Y(H,N):F", "Y", "L", "R", "", 0, "\tld a,l\n\tsub $Rl\n\tld a,h\n\tsbc a,$Rh\n", F_NC},

	/* compare GE BC >= constant */
	{"Y(B,N):F", "Y", "L", "R", "", 0, "\tld a,c\n\tsub $Rl\n\tld a,b\n\tsbc a,$Rh\n", F_NC},

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

	/* short pre-increment through (hl): load, inc, store, return new */
	{"i(H):s", "i", "L", "", "", 0, "\tld e,(hl)\n\tinc hl\n\tld d,(hl)\n\tinc de\n\tld (hl),d\n\tdec hl\n\tld (hl),e\n\tex de,hl\n", R_HL},

	/* short post-increment through (hl): load, store inc'd, return old */
	{"j(H):s", "j", "L", "", "", 0, "\tld e,(hl)\n\tinc hl\n\tld d,(hl)\n\tpush de\n\tinc de\n\tld (hl),d\n\tdec hl\n\tld (hl),e\n\tpop hl\n", R_HL},

	/* short pre-decrement through (hl) */
	{"k(H):s", "k", "L", "", "", 0, "\tld e,(hl)\n\tinc hl\n\tld d,(hl)\n\tdec de\n\tld (hl),d\n\tdec hl\n\tld (hl),e\n\tex de,hl\n", R_HL},

	/* short post-decrement through (hl) */
	{"m(H):s", "m", "L", "", "", 0, "\tld e,(hl)\n\tinc hl\n\tld d,(hl)\n\tpush de\n\tdec de\n\tld (hl),d\n\tdec hl\n\tld (hl),e\n\tpop hl\n", R_HL},

	/* short post-increment symref */
	{"j(O):s", "j", "L", "", "", 0, "\tld hl,($L)\n\tinc hl\n\tld ($L),hl\n\tdec hl\n", R_HL},

	/* short pre-increment symref */
	{"i(O):s", "i", "L", "", "", 0, "\tld hl,($L)\n\tinc hl\n\tld ($L),hl\n", R_HL},

	/* short pre-decrement symref */
	{"k(O):s", "k", "L", "", "", 0, "\tld hl,($L)\n\tdec hl\n\tld ($L),hl\n", R_HL},

	/* short post-decrement symref */
	{"m(O):s", "m", "L", "", "", 0, "\tld hl,($L)\n\tdec hl\n\tld ($L),hl\n\tinc hl\n", R_HL},

	/* byte pre-increment through (hl) */
	{"i(H):b", "i", "L", "", "", 0, "\tinc (hl)\n\tld a,(hl)\n", R_A},

	/* byte post-increment through (hl) */
	{"j(H):b", "j", "L", "", "", 0, "\tld a,(hl)\n\tinc (hl)\n", R_A},

	/* byte pre-increment indexed: inc then load new value */
	{"i(I):b", "i", "L", "", "", 0, "\tinc ($L)\n\tld a,($L)\n", R_A},

	/* byte post-increment indexed: load old value then inc */
	{"j(I):b", "j", "L", "", "", 0, "\tld a,($L)\n\tinc ($L)\n", R_A},

	/* byte pre-decrement indexed: dec then load new value */
	{"k(I):b", "k", "L", "", "", 0, "\tdec ($L)\n\tld a,($L)\n", R_A},

	/* byte post-decrement indexed: load old value then dec */
	{"m(I):b", "m", "L", "", "", 0, "\tld a,($L)\n\tdec ($L)\n", R_A},

	/* byte pre-increment symref: inc then load new value */
	{"i(O):b", "i", "L", "", "", 0, "\tld hl,$L\n\tinc (hl)\n\tld a,(hl)\n", R_A},

	/* byte post-increment symref: load old value then inc */
	{"j(O):b", "j", "L", "", "", 0, "\tld hl,$L\n\tld a,(hl)\n\tinc (hl)\n", R_A},

	/* byte pre-decrement symref: dec then load new value */
	{"k(O):b", "k", "L", "", "", 0, "\tld hl,$L\n\tdec (hl)\n\tld a,(hl)\n", R_A},

	/* byte post-decrement symref: load old value then dec */
	{"m(O):b", "m", "L", "", "", 0, "\tld hl,$L\n\tld a,(hl)\n\tdec (hl)\n", R_A},

	/* SYM + NUMBER -> SYMREF (linker-resolvable) */
	{"+(S,N)", "O", "", "", "", 0, NULL, 0},

	/* SYMREF + NUMBER -> SYMREF with combined offset */
	{"+(O,N)", "O", "", "", "", 0, NULL, 0},

	/* bare SYM -> SYMREF with offset 0 */
	{"S", "O", "", "", "", 0, NULL, 0},

	/* NUMBER in value context: load into register */
	{"N:bV", "C", "", "", "", 0, NULL, R_A},
	{"N:sV", "C", "", "", "", 0, NULL, R_HL},

	/* NUMBER without context: still load into register */
	{"N:b", "C", "", "", "", 0, NULL, R_A},
	{"N:s", "C", "", "", "", 0, NULL, R_HL},
	{"N:p", "C", "", "", "", 0, NULL, R_HL},

	/* ARGNODE: push register pairs */
	{"a(H)", "a", "L", "", "", 0, "\tpush hl\n", 0},
	{"a(E)", "a", "L", "", "", 0, "\tpush de\n", 0},
	{"a(B)", "a", "L", "", "", 0, "\tpush bc\n", 0},

	/* ARGNODE: push constant */
	{"a(N)", "a", "L", "", "", 0, "\tld hl,$L\n\tpush hl\n", 0},

	/* ARGNODE: push symbol address */
	{"a(O)", "a", "L", "", "", 0, "\tld hl,$L\n\tpush hl\n", 0},

	/* ARGNODE: push register variable */
	{"a(V)", "a", "L", "", "L", RF_BC, "\tpush bc\n", 0},
	{"a(V)", "a", "L", "", "L", RF_DE, "\tpush de\n", 0},
	{"a(V)", "a", "L", "", "L", RF_HL, "\tpush hl\n", 0},
	{"a(V)", "a", "L", "", "L", RF_IX, "\tpush ix\n", 0},

	/* ARGNODE: push byte A (extend to 16-bit, push) */
	{"a(A)", "a", "L", "", "", 0, "\tld l,a\n\tld h,0\n\tpush hl\n", 0},

	/* ARGNODE: push byte index value (extend to 16-bit) */
	{"a(D(I)):b", "a", "L", "", "", 0, "\tld l,($LL)\n\tld h,0\n\tpush hl\n", 0},

	/* ARGNODE: push short index value */
	{"a(D(I)):s", "a", "L", "", "", 0, "\tld l,($LL)\n\tld h,($LL+)\n\tpush hl\n", 0},

	/* ARGNODE: push symref deref short */
	{"a(D(O)):s", "a", "L", "", "", 0, "\tld hl,($LL)\n\tpush hl\n", 0},

	/* ARGNODE: push symref deref byte */
	{"a(D(O)):b", "a", "L", "", "", 0, "\tld a,($LL)\n\tld l,a\n\tld h,0\n\tpush hl\n", 0},

	/* Store HL to indexed (pointer width) */
	{"=(I,H):p", "=", "L", "R", "", 0, "\tld ($L),l\n\tld ($L+),h\n", 0},

	/* Store HL to symref (pointer width) */
	{"=(O,H):p", "=", "L", "R", "", 0, "\tld ($L),hl\n", R_HL},

	/* Load symref pointer to HL */
	{"D(O):p", "D", "L", "", "", 0, "\tld hl,($L)\n", R_HL},

	/* Load indexed pointer to HL */
	{"D(I):p", "D", "L", "", "", 0, "\tld l,($L)\n\tld h,($L+)\n", R_HL},

	/* assign constant to A */
	{"=(A,N):b", "=", "L", "R", "", 0, "\tld a,$R\n", R_A},

	/* short assign BC to symref */
	{"=(O,B):s", "=", "L", "R", "", 0, "\tld a,c\n\tld ($L),a\n\tld a,b\n\tld ($L+),a\n", 0},

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
	int shift, changed;
	unsigned char newop, oldop;

	/* Match pattern */
	if (!pmatch(rp->pat, e))
		return NULL;

	/* Check register constraints */
	if (rp->flags & (RF_IXIY | RF_BC | RF_C | RF_B | RF_DE | RF_HL | RF_IX)) {
		p = rp->dsrc;
		src = getpath(e, &p);
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
		fprintf(stderr, "rewrite: %s -> %s\n", rp->pat, rp->rep);
#endif

	oldop = e->op;
	newop = chartopc(rp->rep[0]);
	changed = 0;

	/* Get replacement children */
	p = rp->lsrc;
	lc = (*p) ? getpath(e, &p) : NULL;
	p = rp->rsrc;
	rc = (*p) ? getpath(e, &p) : NULL;

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
			p = rp->dsrc;
			src = getpath(e, &p);
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
			out("\tld a,");
			outd(val);
			out("\n");
			n = mkcode('b', R_A);
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


	/* CODE -> INHL/INDE/INBC/INA: convert to typed register nodes */
	if (e->op == CODE) {
		unsigned char reg = e->u.var.reg;
		if (reg == R_HL) e->op = INHL;
		else if (reg == R_DE) e->op = INDE;
		else if (reg == R_BC) e->op = INBC;
		else if (reg == R_A) e->op = INA;
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
		e->left = rewrite1(e->left);
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
	    r->op != INBC && r->op != INA) {
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
