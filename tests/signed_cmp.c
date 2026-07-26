/*
 * Signed relational comparison between two values.
 *
 * sbc hl,de sets carry on an unsigned borrow, so using carry alone
 * answers the unsigned question.  For signed operands the answer is
 * sign-exclusive-or-overflow: with a = -1 and b = 1 the subtraction
 * does not borrow, so carry says "not less" while the true answer is
 * that -1 < 1.
 *
 * The unsigned forms must keep using carry, which is correct for them
 * and three bytes shorter.
 */

int a, b, r;
unsigned int ua, ub;

int slt()  { return a <  b; }
int sgt()  { return a >  b; }
int sle()  { return a <= b; }
int sge()  { return a >= b; }

int ult()  { return ua <  ub; }
int uge()  { return ua >= ub; }

/* the case that exposes it: operands either side of zero */
int straddle()
{
	a = -1;
	b = 1;
	r = a < b;		/* must be 1 */
	return r;
}
