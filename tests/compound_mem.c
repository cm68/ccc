/*
 * Compound assignment where the lvalue is a memory location rather
 * than a register variable: "x OP= y" lowers to "x = x OP y", which
 * needs the location named twice - once to store, once to load.
 *
 * Register lvalues are not covered here.  pass1 strips the DEREF from
 * a compound-assign lvalue, so "i += 5" with i in BC is
 * indistinguishable from "*p += 10" with p in BC, and pass2 refuses
 * both rather than guess.  See lowercompound() in pass2/rewrite.c.
 */

int g;
char c;

int garr[4];

int gmem()
{
	g += 5;
	g -= 3;
	g |= 0xF0;
	g &= 0x0F;
	g ^= 0xAA;
	return g;
}

int cmem()
{
	c += 1;
	c |= 0x80;
	return c;
}

int amem()
{
	garr[2] += 7;
	return garr[2];
}
