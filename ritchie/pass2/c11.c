/*
 *  C compiler
 *  Z80 target version
 */

#include "c1.h"
#include "z80.h"

degree(t)
register union tree *t;
{
	register union tree *t1;

	if (t == NULL || t->t.op == 0)
		return (0);
	if (t->t.op == CON)
		return (-3);
	if (t->t.op == AMPER)
		return (-2);
	if (t->t.op == ITOL) {
		if ((t1 = isconstant(t)) && (t1->c.value >= 0 || uns(t1)))
			return (-2);
		if (uns(t1 = t->t.tr1) && opdope[t1->t.op] & LEAF)
			return (-1);
	}
	if ((opdope[t->t.op] & LEAF) != 0) {
		if (t->t.type == CHAR || t->t.type == UNCHAR || t->t.type == FLOAT)
			return (1);
		return (0);
	}
	return (t->t.degree);
}

/*
 * Z80 register names for output
 */
char *regname[] = { "hl", "de", "bc", "ix", "iy", "a", "sp" };

pname(p, flag)
register union tree *p;
{
	register i;

loop:
	switch (p->t.op) {

	case LCON:
		/*
		 * Long constant - high or low word 
		 */
		printf("%o", flag <= 10 ? UNS(p->l.lvalue >> 16) :
			   UNS(p->l.lvalue));
		return;

	case SFCON:
	case CON:
		/*
		 * Integer constant 
		 */
		psoct(p->c.value);
		return;

	case FCON:
		/*
		 * Float constant label 
		 */
		printf("L%d", (p->c.value > 0 ? p->c.value : -p->c.value));
		return;

	case NAME:
		i = p->n.offset;
		if (flag > 10)
			i += 2;
		switch (p->n.class) {

		case SOFFS:
		case XOFFS:
			/*
			 * Static/extern with offset from frame 
			 */
			pbase(p);
			if (i)
				printf("+%d", i);
			return;

		case OFFS:
			/*
			 * Stack frame offset via IY 
			 */
			printf("(iy%+d)", i);
			return;

		case EXTERN:
		case STATIC:
			pbase(p);
			if (i)
				printf("+%d", i);
			return;

		case REG:
			printf("%s", regname[p->n.nloc]);
			return;

		}
		error("Compiler error: pname");
		return;

	case AMPER:
		/*
		 * Address-of: handled by caller loading address 
		 */
		p = p->t.tr1;
		if (p->t.op == NAME && p->n.class == REG)
			error("Illegal use of register");
		goto loop;

	case AUTOI:
		/*
		 * Auto-increment (hl)+ - Z80 doesn't have this 
		 */
		printf("(%s)", regname[p->n.nloc]);
		return;

	case AUTOD:
		/*
		 * Auto-decrement -(hl) - Z80 doesn't have this 
		 */
		printf("(%s)", regname[p->n.nloc]);
		return;

	case STAR:
		/*
		 * Indirection 
		 */
		p = p->t.tr1;
		printf("(");
		pname(p, flag);
		printf(")");
		return;

	}
	error("compiler error: bad pname");
}

pbase(p)
register union tree *p;
{

	if (p->n.class == SOFFS || p->n.class == STATIC)
		printf("L%d", p->n.nloc);
	else
		printf("%s", p->x.name);
}

xdcalc(p, nrleft)
register union tree *p;
{
	register d;

	if (p == NULL)
		return (0);
	d = dcalc(p, nrleft);
	if (d < 20 && (p->t.type == CHAR || p->t.type == UNCHAR)) {
		if (nrleft >= 1)
			d = 20;
		else
			d = 24;
	}
	return (d);
}

dcalc(p, nrleft)
register union tree *p;
{
	register union tree *p1;

	if (p == NULL)
		return (0);
	switch (p->t.op) {

	case NAME:
		if (p->n.class == REG && p->n.type != CHAR && p->n.type != UNCHAR)
			return (9);

	case AMPER:
	case FCON:
	case LCON:
	case AUTOI:
	case AUTOD:
		return (12);

	case CON:
	case SFCON:
		if (p->c.value == 0)
			return (4);
		if (p->c.value == 1)
			return (5);
		if (p->c.value > 0)
			return (8);
		return (12);

	case STAR:
		p1 = p->t.tr1;
		if (p1->t.op == NAME || p1->t.op == CON || p1->t.op == AUTOI
			|| p1->t.op == AUTOD)
			if (p->t.type != LONG && p->t.type != UNLONG)
				return (12);
	}
	if (p->t.type == LONG || p->t.type == UNLONG)
		nrleft--;
	return (p->t.degree <= nrleft ? 20 : 24);
}

notcompat(p, ast, deg, op)
register union tree *p;
{
	unsigned register at, st;

	at = p->t.type;
	/*
	 * an e or n UNCHAR is to be considered an UNSIGNED,
	 * as long as it is not pointed to.
	 */
	if (at == UNCHAR && deg < DEG_INDIR && deg >= 20)
		at = UNSIGN;
	st = ast;
	if (st == 0)				/* word, byte */
		return (at != CHAR && at != INT && at != UNSIGN && at < PTR);
	if (st == 1)				/* word */
		return (at != INT && at != UNSIGN && at < PTR);
	if (st == 9 && (at & XTYPE))
		return (0);
	st -= 2;
	if ((at & (~(TYPE + XTYPE))) != 0)
		at = PTR;
	if ((at & (~TYPE)) != 0)
		at = at & TYPE | PTR;
	if (st == FLOAT && at == DOUBLE)
		at = FLOAT;
	if (p->t.op == NAME && p->n.class == REG && op == ASSIGN && st == CHAR)
		return (0);
	return (st != at);
}

prins(op, c, itable, lbl)
struct instab *itable;
{
	extern char jmijne[];
	register struct instab *insp;
	register char *ip;
	register int skip;

	skip = 0;
	for (insp = itable; insp->iop != 0; insp++) {
		if (insp->iop == op) {
			ip = c ? insp->str2 : insp->str1;
			if (!ip)
				break;
			if (ip != jmijne) {
				printf(ip, lbl);
			} else {
				skip = isn++;
				printf(ip, skip);
			}
			return (skip);
		}
	}
	error("No match' for op %d", op);
	return (skip);
}

collcon(p)
register union tree *p;
{
	register op;

	if (p == NULL)
		return (0);
	if (p->t.op == STAR) {
		if (p->t.type == LONG + PTR || p->t.type == UNLONG + PTR) /* avoid 
																   *x(r); *x+2(r) */
			return (0);
		p = p->t.tr1;
	}
	if (p->t.op == PLUS) {
		op = p->t.tr2->t.op;
		if (op == CON || op == AMPER)
			return (1);
	}
	return (0);
}

isfloat(t)
register union tree *t;
{

	if ((opdope[t->t.op] & RELAT) != 0)
		t = t->t.tr1;
	if (t->t.type == FLOAT || t->t.type == DOUBLE) {
		nfloat = 1;
		return ('f');
	}
	return (0);
}

/* Type lengths: INT,CHAR,UNSIGN,UNCHAR=2, LONG,UNLONG=4, FLOAT,DOUBLE=8 */
static char arltab[] = { 2, 2, 8, 8, 0, 0, 4, 2, 2, 4, 0 };

arlength(t)
{
	if (t >= PTR)
		return (2);
	return arltab[t];
}

/*
 * Strings for switch code.
 */

/*
 * Modified Memorial day May 80 to uniquely identify switch tables
 * (as on Vax) so a shell script can optionally include them in RO code.
 * This is useful in overlays to reduce the size of data space load.
 * wfj 5/80 
 */
/*
 * Z80 direct switch - compare, bounds check, jump table 
 */
char dirsw[] = { "\
\tld de,%o\n\
\tor a\n\
\tsbc hl,de\n\
\tjp c,L%d\n\
\tadd hl,hl\n\
\tld de,L%d\n\
\tadd hl,de\n\
\tld e,(hl)\n\
\tinc hl\n\
\tld d,(hl)\n\
\tex de,hl\n\
\tjp (hl)\n\
\t.data\n\
L%d:\
" };

/*
 * Z80 hash switch - modulo and jump table 
 */
char hashsw[] = { "\
\tex de,hl\n\
\tld hl,%o\n\
\tcall umod16\n\
\tadd hl,hl\n\
\tld de,L%d\n\
\tadd hl,de\n\
\tld e,(hl)\n\
\tinc hl\n\
\tld d,(hl)\n\
\tex de,hl\n\
\tjp (hl)\n\
\t.data\n\
L%d:\
" };

/*
 * If the unsigned casts below won't compile,
 * try using the calls to lrem and ldiv.
 */

pswitch(afp, alp, deflab)
struct swtab *afp, *alp;
{
	int ncase, i, j, tabs, worst, best, range;
	register struct swtab *swp, *fp, *lp;
	int *poctab;

	fp = afp;
	lp = alp;
	if (fp == lp) {
		printf("\tjp\tL%d\n", deflab);
		return;
	}
	isn++;
	if (sort(fp, lp))
		return;
	ncase = lp - fp;
	lp--;
	range = lp->swval - fp->swval;
	/*
	 * direct switch 
	 */
	if (range > 0 && range <= 3 * ncase) {
		if (fp->swval)
			printf("\tld\tde,%o\n\tor\ta\n\tsbc\thl,de\n", UNS(fp->swval));
		printf(dirsw, UNS(range), deflab, isn, isn);
		isn++;
		for (i = fp->swval;; i++) {
			if (i == fp->swval) {
				printf("\t.dw\tL%d\n", fp->swlab);
				if (fp == lp)
					break;
				fp++;
			} else
				printf("\t.dw\tL%d\n", deflab);
		}
		printf("\t.text\n");
		return;
	}
	/*
	 * simple switch 
	 */
	if (ncase < 10) {
		for (fp = afp; fp <= lp; fp++)
			breq(fp->swval, fp->swlab);
		printf("\tjp\tL%d\n", deflab);
		return;
	}
	/*
	 * hash switch 
	 */
	best = 077777;
	poctab = (int *) getblk(((ncase + 2) / 2) * sizeof(*poctab));
	for (i = ncase / 4; i <= ncase / 2; i++) {
		for (j = 0; j < i; j++)
			poctab[j] = 0;
		for (swp = fp; swp <= lp; swp++)
			poctab[(unsigned) swp->swval % i]++;
		worst = 0;
		for (j = 0; j < i; j++)
			if (poctab[j] > worst)
				worst = poctab[j];
		if (i * worst < best) {
			tabs = i;
			best = i * worst;
		}
	}
	i = isn++;
	printf(hashsw, UNS(tabs), i, i);
	isn++;
	for (i = 0; i < tabs; i++)
		printf("\t.dw\tL%d\n", isn + i);
	printf("\t.text\n");
	for (i = 0; i < tabs; i++) {
		printf("L%d:", isn++);
		for (swp = fp; swp <= lp; swp++) {
			if ((unsigned) swp->swval % tabs == i) {
				breq((int) ((unsigned) swp->swval / tabs), swp->swlab);
			}
		}
		printf("\tjp\tL%d\n", deflab);
	}
}

breq(v, l)
{
	if (v == 0)
		printf("\tld\ta,h\n\tor\tl\n");
	else
		printf("\tld\tde,%o\n\tor\ta\n\tsbc\thl,de\n\tadd\thl,de\n",
			   UNS(v));
	printf("\tjp\tz,L%d\n", l);
}

sort(afp, alp)
struct swtab *afp, *alp;
{
	register struct swtab *cp, *fp, *lp;
	int intch, t;

	fp = afp;
	lp = alp;
	while (fp < --lp) {
		intch = 0;
		for (cp = fp; cp < lp; cp++) {
			if (cp->swval == cp[1].swval) {
				error("Duplicate case (%d)", cp->swval);
				return (1);
			}
			if (cp->swval > cp[1].swval) {
				intch++;
				t = cp->swval;
				cp->swval = cp[1].swval;
				cp[1].swval = t;
				t = cp->swlab;
				cp->swlab = cp[1].swlab;
				cp[1].swlab = t;
			}
		}
		if (intch == 0)
			break;
	}
	return (0);
}

ispow2(tree)
register union tree *tree;
{
	register int d;

	if (!isfloat(tree) && tree->t.tr2->t.op == CON) {
		d = tree->t.tr2->c.value;
		if (d > 1 && (d & (d - 1)) == 0)
			return (d);
	}
	return (0);
}

union tree *
pow2(tree)
register union tree *tree;
{
	register int d, i;

	if (d = ispow2(tree)) {
		for (i = 0; (d >>= 1) != 0; i++);
		tree->t.tr2->c.value = i;
		switch (tree->t.op) {

		case TIMES:
			tree->t.op = LSHIFT;
			break;

		case ASTIMES:
			tree->t.op = ASLSH;
			break;

		case PTOI:
			if (i == 1 && tree->t.tr1->t.op == MINUS
				&& !isconstant(tree->t.tr1->t.tr2)) {
				tree->t.op = PTOI1;
				tree->t.tr1 = tnode(LTOI, INT, tree->t.tr1, TNULL);
				return (optim(tree));
			}
			tree->t.op = LLSHIFT;
			tree->t.tr2->c.value = -i;
			i = tree->t.type;
			tree->t.type = LONG;
			tree = tnode(LTOI, i, tree, TNULL);
			break;

		case DIVIDE:
			tree->t.op = ULSH;
			tree->t.tr2->c.value = -i;
			break;

		case ASDIV:
			tree->t.op = ASULSH;
			tree->t.tr2->c.value = -i;
			break;

		case MOD:
			tree->t.op = AND;
			tree->t.tr2->c.value = (1 << i) - 1;
			break;

		case ASMOD:
			tree->t.op = ASAND;
			tree->t.tr2->c.value = (1 << i) - 1;
			break;

		default:
			error("pow2 botch");
		}
		tree = optim(tree);
	}
	return (tree);
}

cbranch(atree, lbl, cond, reg)
union tree *atree;
int lbl;
char cond, reg;
{
	int l1;
	char op;
	register union tree *tree;

again:
	if ((tree = atree) == NULL)
		return;
	switch (tree->t.op) {

	case LOGAND:
		if (cond) {
			cbranch(tree->t.tr1, l1 = isn++, 0, reg);
			cbranch(tree->t.tr2, lbl, 1, reg);
			label(l1);
		} else {
			cbranch(tree->t.tr1, lbl, 0, reg);
			cbranch(tree->t.tr2, lbl, 0, reg);
		}
		return;

	case LOGOR:
		if (cond) {
			cbranch(tree->t.tr1, lbl, 1, reg);
			cbranch(tree->t.tr2, lbl, 1, reg);
		} else {
			cbranch(tree->t.tr1, l1 = isn++, 1, reg);
			cbranch(tree->t.tr2, lbl, 0, reg);
			label(l1);
		}
		return;

	case EXCLA:
		cbranch(tree->t.tr1, lbl, !cond, reg);
		return;

	case SEQNC:
		rcexpr(tree->t.tr1, efftab, reg);
		atree = tree->t.tr2;
		goto again;

	case ITOL:
		tree = tree->t.tr1;
		break;

	case QUEST:
		l1 = isn;
		isn += 2;
		cbranch(tree->t.tr1, l1, 0, reg);
		cbranch(tree->t.tr2->t.tr1, lbl, cond, reg);
		branch(l1 + 1, 0, 0);
		label(l1);
		cbranch(tree->t.tr2->t.tr2, lbl, cond, reg);
		label(l1 + 1);
		return;

	}
	op = tree->t.op;
	if (opdope[op] & RELAT
		&& tree->t.tr1->t.op == ITOL && tree->t.tr2->t.op == ITOL
		&& uns(tree->t.tr1->t.tr1) == uns(tree->t.tr2->t.tr1)) {
		tree->t.tr1 = tree->t.tr1->t.tr1;
		tree->t.tr2 = tree->t.tr2->t.tr1;
		if (op >= LESSEQ && op <= GREAT && uns(tree->t.tr1))
			tree->t.op = op = op + LESSEQP - LESSEQ;
	}
	if (tree->t.type == LONG || tree->t.type == UNLONG
		|| opdope[op] & RELAT && (tree->t.tr1->t.type == LONG
								  || tree->t.tr1->t.type == UNLONG)) {
		longrel(tree, lbl, cond, reg);
		return;
	}
	rcexpr(tree, cctab, reg);
	op = tree->t.op;
	if ((opdope[op] & RELAT) == 0)
		op = NEQUAL;
	else {
		l1 = tree->t.tr2->t.op;
		if ((l1 == CON || l1 == SFCON) && tree->t.tr2->c.value == 0)
			op += 200;			/* special for ptr tests */
		else
			op = maprel[op - EQUAL];
	}
	branch(lbl, op, !cond);
}

branch(lbl, aop, c)
{
	register int op, skip;

	if (op = aop) {
		skip = prins(op, c, branchtab, lbl);
	} else {
		printf("\tjp");
		skip = 0;
	}
	if (skip)
		printf("\tL%d\nL%d:", lbl, skip);
	else
		printf("\tL%d\n", lbl);
}

longrel(atree, lbl, cond, reg)
union tree *atree;
int lbl;
char cond, reg;
{
	int xl1, xl2, xo, xz;
	char op, isrel;
	register union tree *tree;

	if (reg & 01)
		reg++;
	reorder(&atree, cctab, reg);
	tree = atree;
	isrel = 0;
	if (opdope[tree->t.op] & RELAT) {
		isrel++;
		op = tree->t.op;
	} else
		op = NEQUAL;
	if (!cond)
		op = notrel[op - EQUAL];
	xl1 = xlab1;
	xl2 = xlab2;
	xo = xop;
	xlab1 = lbl;
	xlab2 = 0;
	xop = op;
	xz = xzero;
	xzero = !isrel || (tree->t.tr2->t.op == ITOL
					   && tree->t.tr2->t.tr1->t.op == CON
					   && tree->t.tr2->t.tr1->c.value == 0);
	if (tree->t.op == ANDN) {
		tree->t.op = TAND;
		tree->t.tr2 = optim(tnode(COMPL, LONG, tree->t.tr2, TNULL));
	}
	if (cexpr(tree, cctab, reg) < 0) {
		reg = rcexpr(tree, regtab, reg);
		printf("\tcall\tltsthl\n");
		branch(xlab1, op, 0);
	}
	xlab1 = xl1;
	xlab2 = xl2;
	xop = xo;
	xzero = xz;
}

/*
 * Tables for finding out how best to do long comparisons.
 * First dimen is whether or not the comparison is with 0.
 * Second is which test: e.g. a>b->
 *  cmp a,b
 *  bgt YES     (first)
 *  blt NO      (second)
 *  cmp a+2,b+2
 *  bhi YES     (third)
 *  NO: ...
 * Note some tests may not be needed.
 *
 * EQUAL = 60
 * NEQUAL= 61
 * LESSEQ= 62
 * LESS  = 63
 * GREATEQ=64
 * GREAT  =65
 * LESSEQP=66
 * LESSP  =67
 * GREATQP=68
 * GREATP =69
 *
 * Third dimension (lrtab[][][x]) indexed by "x - EQUAL".
 */
char lrtab[2][3][10] = {
	{
		{ 0, NEQUAL, LESS, LESS, GREAT, GREAT,
			LESSP, LESSP, GREATP, GREATP },
		{ NEQUAL, 0, GREAT, GREAT, LESS, LESS,
			GREATP, GREATP, LESSP, LESSP },
		{ EQUAL, NEQUAL, LESSEQP, LESSP, GREATQP, GREATP,
			LESSEQP, LESSP, GREATQP, GREATP }
	},
	{
		{ 0, NEQUAL, LESS, LESS, GREATEQ, GREAT,
			LESSP, LESSP, GREATQP, GREATP },
		{ NEQUAL, 0, GREAT, 0, 0, LESS,
			GREATP, 0, 0, LESSP },
		{ EQUAL, NEQUAL, EQUAL, 0, 0, NEQUAL,
			EQUAL, 0, 0, NEQUAL }
	}
};

xlongrel(f)
char f;
{
	char op, bno;

	op = xop;
	if (f == 0) {
		if (bno = lrtab[xzero][0][op - EQUAL])
			branch(xlab1, bno, 0);
		if (bno = lrtab[xzero][1][op - EQUAL]) {
			xlab2 = isn++;
			branch(xlab2, bno, 0);
		}
		if (lrtab[xzero][2][op - EQUAL] == 0)
			return (1);
	} else {
		branch(xlab1, lrtab[xzero][2][op - EQUAL], 0);
		if (xlab2)
			label(xlab2);
	}
	return (0);
}

label(l)
{
	printf("L%d:", l);
}

popstk(a)
{
	switch (a) {

	case 0:
		return;

	case 2:
		printf("\tpop\tde\n");
		return;

	case 4:
		printf("\tpop\tde\n\tpop\tde\n");
		return;
	}
	printf("\tld\thl,%o\n\tadd\thl,sp\n\tld\tsp,hl\n", UNS(a));
}

werror(s)
char *s;
{

	fprintf(stderr, "%d: %s\n", line, s);
}

/*
 * VARARGS1 
 */
error(s, p1, p2, p3, p4, p5, p6)
char *s;
{

	nerror++;
	fprintf(stderr, "%d: ", line);
	fprintf(stderr, s, p1, p2, p3, p4, p5, p6);
	putc('\n', stderr);
}

psoct(an)
{
	register int n;
	register char *sign;

	sign = "";
	if ((n = an) < 0) {
		n = -n;
		sign = "-";
	}
	printf("%s%o", sign, n);
}

/*
 * Read in an intermediate file.
 */
#define	STKS	100
getree()
{
	union tree *expstack[STKS], **sp;
	register union tree *tp;
	register int t, op;
	char s[80];					/* big for asm() stuff & long variable
								 * names */
	struct swtab *swp;
	long outloc;
	int lbl, cond, lbl2, lbl3;
	double atof();

	curbase = funcbase;
	sp = expstack;
	for (;;) {
		if (sp >= &expstack[STKS])
			error("Stack overflow botch");
		op = geti();
		if ((op & 0177400) != 0177000) {
			error("Intermediate file error");
			exit(1);
		}
		lbl = 0;
		switch (op &= 0377) {

		case SINIT:
			printf(".dw %o\n", UNS(geti()));
			break;

		case EOFC:
			return;

		case BDATA:
			if (geti() == 1) {
				printf(".db ");
				for (;;) {
					printf("%o", UNS(geti()));
					if (geti() != 1)
						break;
					printf(",");
				}
				printf("\n");
			}
			break;

		case PROG:
			printf(".text\n");
			break;

		case DATA:
			printf(".data\n");
			break;

		case BSS:
			printf(".bss\n");
			break;

		case SYMDEF:
			outname(s);
			if (*s)
				printf(".globl\t%s\n", s);
			sfuncr.nloc = 0;
			break;

		case RETRN:
			/*
			 * Z80 function epilogue 
			 */
			printf("\tld\tsp,iy\n\tpop\tiy\n\tret\n");
			break;

		case CSPACE:
			outname(s);
			printf(".bss\n%s:\t.ds %d\n", s, UNS(geti()));
			break;

		case SSPACE:
			printf(".ds %d\n", UNS(t = geti()));
			totspace += (unsigned) t;
			break;

		case EVEN:
			/*
			 * Z80 doesn't need alignment 
			 */
			break;

		case SAVE:
			/*
			 * Z80 function prologue 
			 */
			printf("\tpush\tiy\n\tld\tiy,0\n\tadd\tiy,sp\n");
			break;

		case SETSTK:
			t = geti();
			if (t == 2)
				printf("\tdec\tsp\n\tdec\tsp\n");
			else if (t != 0)
				printf("\tld\thl,-%o\n\tadd\thl,sp\n\tld\tsp,hl\n",
					   UNS(t));
			break;

		case PROFIL:
			t = geti();
			outname(s);
			printf("\tld\thl,L%d\n\tcall\tmcount\n", t);
			printf("\t.data\nL%d:\t.dw\t%s+1\n\t.text\n", t, s);
			break;

		case ASSEM:
			outname(s);
			printf("%s\n", s);
			break;

		case SNAME:
			outname(s);
#ifdef DEBUG
			printf("; %s=L%d\n", s + 1, geti());
#else
			geti();
#endif
			break;

		case ANAME:
			outname(s);
#ifdef DEBUG
			printf("; %s=%o\n", s + 1, UNS(geti()));
#else
			geti();
#endif
			break;

		case RNAME:
			outname(s);
#ifdef DEBUG
			printf("; %s=%s\n", s + 1, regname[geti()]);
#else
			geti();
#endif
			break;

		case SWIT:
			t = geti();
			line = geti();
			curbase = funcbase;
			while (swp =
				   (struct swtab *) getblk(sizeof(*swp)), swp->swlab =
				   geti())
				swp->swval = geti();
			pswitch((struct swtab *) funcbase, swp, t);
			break;

		case C3BRANCH:			/* for fortran [sic] */
			lbl = geti();
			lbl2 = geti();
			lbl3 = geti();
			goto xpr;

		case CBRANCH:
			lbl = geti();
			cond = geti();

		case EXPR:
xpr:
			line = geti();
			if (sp != &expstack[1]) {
				error("Expression input botch");
				exit(1);
			}
			--sp;
			regpanic = 0;
			if (setjmp(jmpbuf)) {
				regpanic = 10;
				fseek(stdout, outloc, 0);
			}
			nstack = 0;
			panicposs = 0;
			*sp = tp = optim(*sp);
			if (regpanic == 0 && panicposs)
				outloc = ftell(stdout);
			if (op == CBRANCH)
				cbranch(tp, lbl, cond, 0);
			else if (op == EXPR)
				rcexpr(tp, efftab, 0);
			else {
				if (tp->t.type == LONG || tp->t.type == UNLONG) {
					rcexpr(tnode(RFORCE, tp->t.type, tp, TNULL), efftab,
						   0);
					/*
					 * Z80: test long result in HLHL' 
					 */
					printf("\tcall\tltsthl\n");
				} else {
					rcexpr(tp, cctab, 0);
					if (isfloat(tp))
						printf("\tcall\tfcmp0\n");
				}
				printf("\tjp\tp,L%d\n", lbl3);
				printf("\tjp\tm,L%d\n\tjp\tL%d\n", lbl, lbl2);
			}
			curbase = funcbase;
			break;

		case NAME:
			t = geti();
			if (t == EXTERN) {
				tp = getblk(sizeof(struct xtname));
				tp->t.type = geti();
				outname(s);
				tp->x.name = (char *) getblk(strlen(s) + 1);
				strcpy(tp->x.name, s);
			} else {
				tp = getblk(sizeof(struct tname));
				tp->t.type = geti();
				tp->n.nloc = geti();
			}
			tp->t.op = NAME;
			tp->n.class = t;
			tp->n.regno = 0;
			tp->n.offset = 0;
			*sp++ = tp;
			break;

		case CON:
			t = geti();
			*sp++ = tconst(geti(), t);
			break;

		case LCON:
			geti();				/* ignore type, assume long */
			t = geti();
			op = geti();
			if (t == 0 && op >= 0 || t == -1 && op < 0) {
				*sp++ = tnode(ITOL, LONG, tconst(op, INT), TNULL);
				break;
			}
			tp = getblk(sizeof(struct lconst));
			tp->t.op = LCON;
			tp->t.type = LONG;
			tp->l.lvalue = ((long) t << 16) + UNS(op); /* nonportable */
			*sp++ = tp;
			break;

		case FCON:
			t = geti();
			outname(s);
			tp = getblk(sizeof(struct ftconst));
			tp->t.op = FCON;
			tp->t.type = t;
			tp->f.value = isn++;
			tp->f.fvalue = atof(s);
			*sp++ = tp;
			break;

		case FSEL:
			tp = tnode(FSEL, geti(), *--sp, TNULL);
			t = geti();
			tp->t.tr2 =
				tnode(COMMA, INT, tconst(geti(), INT), tconst(t, INT));
			if (tp->t.tr2->t.tr1->c.value == 16)
				tp = paint(tp->t.tr1, tp->t.type);
			*sp++ = tp;
			break;

		case STRASG:
			tp = getblk(sizeof(struct fasgn));
			tp->t.op = STRASG;
			tp->t.type = geti();
			tp->F.mask = geti();
			tp->t.tr1 = *--sp;
			tp->t.tr2 = NULL;
			*sp++ = tp;
			break;

		case NULLOP:
			*sp++ = tnode(0, 0, TNULL, TNULL);
			break;

		case LABEL:
			label(geti());
			break;

		case NLABEL:
			outname(s);
			printf("%s:\n", s);
			break;

		case RLABEL:
			outname(s);
			printf("%s:\n", s);
			break;

		case BRANCH:
			branch(geti(), 0, 0);
			break;

		case SETREG:
			nreg = geti() - 1;
			break;

		default:
			if (opdope[op] & BINARY) {
				if (sp < &expstack[1]) {
					error("Binary expression botch");
					exit(1);
				}
				tp = *--sp;
				*sp++ = tnode(op, geti(), *--sp, tp);
			} else
				sp[-1] = tnode(op, geti(), sp[-1], TNULL);
			break;
		}
	}
}

geti()
{
	register short i;

	i = getchar() & 0xff;
	i |= (getchar() & 0xff) << 8;
	return (i);
}

outname(s)
register char *s;
{
	register int c;

	while (c = getchar())
		*s++ = c;
	*s++ = '\0';
}

strasg(atp)
union tree *atp;
{
	register union tree *tp;
	register nwords, i;

	nwords = atp->F.mask / sizeof(short);
	tp = atp->t.tr1;
	while (tp->t.op == SEQNC) {
		rcexpr(tp->t.tr1, efftab, 0);
		tp = tp->t.tr2;
	}
	if (tp->t.op != ASSIGN) {
		if (tp->t.op == RFORCE) { /* function return */
			if (sfuncr.nloc == 0) {
				sfuncr.nloc = isn++;
				printf("\t.bss\nL%d:\t.ds\t%o\n\t.text\n", sfuncr.nloc,
					   UNS(nwords * sizeof(short)));
			}
			atp->t.tr1 =
				tnode(ASSIGN, STRUCT, (union tree *) &sfuncr, tp->t.tr1);
			strasg(atp);
			printf("\tld\thl,L%d\n", sfuncr.nloc);
			return;
		}
		if (tp->t.op == CALL) {
			rcexpr(tp, efftab, 0);
			return;
		}
		error("Illegal structure operation");
		return;
	}
	tp->t.tr2 = strfunc(tp->t.tr2);
	if (nwords == 1)
		paint(tp, INT);
	else if (nwords == sizeof(short))
		paint(tp, LONG);
	else {
		if (tp->t.tr1->t.op != NAME && tp->t.tr1->t.op != STAR
			|| tp->t.tr2->t.op != NAME && tp->t.tr2->t.op != STAR) {
			error("unimplemented structure assignment");
			return;
		}
		tp->t.tr1 = tnode(AMPER, STRUCT + PTR, tp->t.tr1, TNULL);
		tp->t.tr2 = tnode(AMPER, STRUCT + PTR, tp->t.tr2, TNULL);
		tp->t.op = STRSET;
		tp->t.type = STRUCT + PTR;
		tp = optim(tp);
		rcexpr(tp, efftab, 0);
		/*
		 * Z80: DE=dest, HL=src after STRSET 
		 */
		if (nwords < 7) {
			for (i = 0; i < nwords; i++)
				printf
					("\tld\ta,(hl)\n\tld\t(de),a\n\tinc\thl\n\tinc\tde\n\tld\ta,(hl)\n\tld\t(de),a\n\tinc\thl\n\tinc\tde\n");
			return;
		}
		if (nreg <= 1)
			printf("\tpush\tbc\n");
		printf("\tld\tbc,%o\n", UNS(nwords * 2));
		printf("\tldir\n");
		if (nreg <= 1)
			printf("\tpop\tbc\n");
		return;
	}
	rcexpr(tp, efftab, 0);
}

/*
 * Reduce the degree-of-reference by one.
 * e.g. turn "ptr-to-int" into "int".
 */
decref(t)
register t;
{
	if ((t & ~TYPE) == 0) {
		error("Illegal indirection");
		return (t);
	}
	return (((unsigned) t >> TYLEN) & ~TYPE | t & TYPE);
}

/*
 * Increase the degree of reference by
 * one; e.g. turn "int" to "ptr-to-int".
 */
incref(t)
{
	return (((t & ~TYPE) << TYLEN) | (t & TYPE) | PTR);
}

/*
 * vim: set tabstop=4 shiftwidth=4 noexpandtab: 
 */
