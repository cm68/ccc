/*
 * Byte arithmetic with both operands live.  On the Z80 these want the
 * left operand in A and the right in E ("add a,e"), which is much
 * cheaper than promoting to 16 bits and using HL/DE.
 *
 * C says char operands promote to int, but where the result is stored
 * straight back into a char only the low 8 bits are observable, so the
 * whole thing may be done in 8 bits.  That holds for the operators
 * whose low n bits depend only on the low n bits of the operands:
 * + - * & | ^ << and the unary forms.  It does NOT hold for / % >>
 * or for comparisons, which are not exercised here.
 */

char c1, c2, c3;
unsigned char u1, u2, u3;

int barith()
{
	c3 = c1 + c2;
	c3 = c1 - c2;
	c3 = c1 & c2;
	c3 = c1 | c2;
	c3 = c1 ^ c2;
	return 0;
}

int uarith()
{
	u3 = u1 + u2;
	u3 = u1 & u2;
	return 0;
}

/*
 * Nested: only A can hold a byte accumulator, so an inner result and
 * an outer left operand contend for it.  Either this sequences
 * correctly or it must refuse and leave a marker - what it must not do
 * is quietly compute the wrong thing.
 */
int nested()
{
	c3 = c1 + (c2 & c1);
	c3 = (c1 + c2) & c1;
	return 0;
}
