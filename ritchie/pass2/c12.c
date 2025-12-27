/*
 *      C compiler part 2 -- expression optimizer
 *      Z80 target version
 */

#include "c1.h"
#include "z80.h"
/*
 * #include <sys/param.h> 
								 *//*
								 * for MAX - commented out, define locally 
								 */

/*
 * Minimal tree optimizer for Z80.
 * Most constant folding is done in pass1.
 * This just handles essential transformations.
 */
union tree *
optim(tree)
register union tree *tree;
{
	register op;
	int dope;

	if (tree == NULL)
		return (NULL);
	if ((op = tree->t.op) == 0)
		return (tree);

	/* Essential: convert AUTO to IY-relative addressing */
	if (op == NAME && tree->n.class == AUTO) {
		tree->n.class = OFFS;
		tree->n.regno = R_IY;
		tree->n.offset = tree->n.nloc;
	}

	dope = opdope[op];
	if (dope & LEAF)
		return (tree);

	/* Recursively process subtrees */
	tree->t.tr1 = optim(tree->t.tr1);
	if (dope & BINARY) {
		tree->t.tr2 = optim(tree->t.tr2);

		/* ITOP/LTOP: pointer scaling for array indexing */
		if (op == LTOP) {
			tree->t.op = ITOP;
			tree->t.tr1 = optim(tnode(LTOI, INT, tree->t.tr1, TNULL));
			op = ITOP;
		}
		if (op == ITOP)
			tree->t.op = TIMES;

		/* Signed/unsigned divide/mod distinction */
		if ((op == MOD || op == DIVIDE || op == ASMOD || op == ASDIV)
			&& (uns(tree->t.tr1) || uns(tree->t.tr2))) {
			if (op >= ASDIV)
				tree->t.op += ASUDIV - ASDIV;
			else
				tree->t.op += UDIV - DIVIDE;
		}

		/* Bitwise ops: treat as unsigned */
		if (op == AND || op == OR || op == EXOR ||
			op == ASAND || op == ASOR || op == ASXOR) {
			if (tree->t.type == INT)
				tree->t.type = UNSIGN;
			else if (tree->t.type == LONG)
				tree->t.type = UNLONG;
		}

		/* Long multiply/divide need library calls */
		if (tree->t.type == LONG || tree->t.type == UNLONG)
			tree = hardlongs(tree);
	}

	/* Compute degree for register allocation */
	tree->t.degree = degree(tree);

	return (tree);
}

union tree *
tnode(op, type, tr1, tr2)
union tree *tr1, *tr2;
{
	register union tree *p;

	p = getblk(sizeof(struct tnode));
	p->t.op = op;
	p->t.type = type;
	p->t.degree = 0;
	p->t.tr1 = tr1;
	p->t.tr2 = tr2;
	return (p);
}

union tree *
tconst(val, type)
{
	register union tree *p;

	p = getblk(sizeof(struct tconst));
	p->t.op = CON;
	p->t.type = type;
	p->c.value = val;
	return (p);
}

union tree *
getblk(size)
{
	register union tree *p;

	if (size & 01)
		size++;
	p = (union tree *) curbase;
	if ((curbase += size) >= coremax) {
		if (sbrk(1024) == (char *) -1) {
			error("Out of space-- c1");
			exit(1);
		}
		coremax += 1024;
	}
	return (p);
}

islong(t)
{
	if (t == LONG || t == UNLONG)
		return (2);
	return (1);
}

union tree *
isconstant(t)
register union tree *t;
{
	if (t->t.op == CON || t->t.op == SFCON)
		return (t);
	if (t->t.op == ITOL && t->t.tr1->t.op == CON)
		return (t->t.tr1);
	return (NULL);
}

union tree *
hardlongs(t)
register union tree *t;
{
	switch (t->t.op) {

	case TIMES:
	case DIVIDE:
	case MOD:
		if (t->t.type == UNLONG)
			t->t.op += ULTIMES - TIMES;
		else
			t->t.op += LTIMES - TIMES;
		break;

	case ASTIMES:
	case ASDIV:
	case ASMOD:
		if (t->t.type == UNLONG)
			t->t.op += ULASTIMES - ASTIMES;
		else
			t->t.op += LASTIMES - ASTIMES;
		t->t.tr1 = tnode(AMPER, LONG + PTR, t->t.tr1, TNULL);
		break;

	default:
		return (t);
	}
	return (optim(t));
}

/*
 * Is tree of unsigned type?
 */
uns(tp)
union tree *tp;
{
	register t;

	t = tp->t.type;
	return (t >= UNSIGN && t <= UNLONG) || t >= PTR;
}

/*
 * vim: set tabstop=4 shiftwidth=4 noexpandtab: 
 */
