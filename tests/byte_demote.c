/*
 * Destination-width propagation.  Where the result is stored into a
 * narrow object, the computation may be done narrow - but only for
 * operators whose low n bits depend solely on the low n bits of their
 * operands.
 *
 * The second function is the important half: these must NOT be
 * demoted, because the discarded high bits change the answer.
 */

char c, c1, c2;
int i, i2;
long l;

/* transparent: may be computed in 8 bits */
int ok()
{
	c = c1 + c2;
	c = c1 + i;		/* the int operand narrows too */
	c = c1 * i;
	c = c1 & i;
	c = c1 | i;
	c = c1 ^ i;
	c = c1 - i;
	c = i;			/* plain narrowing assignment */
	c = l;
	i = l;
	return 0;
}

/* NOT transparent: must stay at full width */
int notok()
{
	c = i / i2;		/* quotient depends on the high bits */
	c = i % i2;
	c = i >> 8;		/* pulls bits down from above */
	c = i < i2;		/* comparison result is int-valued */
	return 0;
}
