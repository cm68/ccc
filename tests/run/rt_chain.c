/*
 * A chained assignment whose destination address has to be worked out.
 *
 * "a->left = a->right = NULL" stores through an address in HL, and
 * what HL holds afterwards is the ADDRESS, not the value stored.  The
 * outer assignment took that for the value of the inner one, so the
 * right was nulled and the left was filled with the address of the
 * right, one byte past it.
 *
 * The rule table has a value-producing form for an assignment whose
 * result is wanted - it leaves the value in a register - and had one
 * for every indirect store except a constant through HL.
 *
 * Only reachable when the address is computed.  A frame slot or a
 * register home has a store form of its own, so the pointer has to be
 * in BC, with IX already spoken for, before this path is taken: the
 * spare register pointer below is what forces that, and without it
 * the same lines compile correctly.
 *
 * This is what stopped c1 self-hosting.  docall detaches an argument
 * node before freeing it; with the left still set, freeexpr walked
 * into the node the loop was about to push.
 */
#include "rt.h"

/* the shape of pass2's Expr: the two pointers at +6 and +8 */
struct node {
	unsigned char op, width, dest, regs, tgt, nored;
	struct node *left, *right;
	long v;
};

struct node pool[8];
short npool;

struct node *
mk(op)
short op;
{
	struct node *n;

	n = &pool[npool++];
	n->op = (unsigned char)op;
	n->left = 0;
	n->right = 0;
	n->v = 0;
	return n;
}

/*
 * ip earns IX by using it for field access, which leaves the walking
 * pointer in BC and its member addresses to be worked out.
 */
short
detach(e, ip)
struct node *e;
register struct node *ip;
{
	struct node *a, *next;
	short bad;

	bad = 0;
	ip->dest = 1;
	ip->regs = 2;
	ip->tgt = 3;
	for (a = e->right; a && a->op == 218; a = next) {
		next = a->right;
		a->left = a->right = 0;
		if (a->left)
			bad |= 1;
		if (a->right)
			bad |= 2;
	}
	return bad;
}

/* the same through a byte member, which has a store form of its own */
short
bytechain(ip)
register struct node *ip;
{
	struct node *a;

	ip->tgt = 9;
	a = &pool[0];
	a->dest = a->regs = 0;
	return (a->dest == 0 && a->regs == 0) ? 0 : 1;
}

/* a long member, stored the same way */
short
longchain()
{
	struct node *a, *b;

	a = &pool[1];
	b = &pool[2];
	a->v = b->v = 0;
	return (a->v == 0L && b->v == 0L) ? 0 : 1;
}

main()
{
	struct node *call, *arg, *val, *ip;

	val = mk(21);
	arg = mk(218);
	arg->left = val;
	call = mk(205);
	call->right = arg;
	ip = mk(1);

	CHECK(1, detach(call, ip), 0);
	CHECK(2, arg->left, (struct node *)0);
	CHECK(3, arg->right, (struct node *)0);
	CHECK(4, bytechain(ip), 0);
	CHECK(5, longchain(), 0);
	return 0;
}
