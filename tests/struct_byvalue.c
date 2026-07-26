/*
 * Expected to fail.  A struct has no value: it cannot be assigned,
 * passed or returned.  Each of these must be diagnosed rather than
 * silently mis-copied - see struct_value.c for the legal half.
 */

struct pt { int x; int y; };
struct big { int a; int b; int c; };

struct pt g1, g2;
struct big b1, b2;

int taking();

int assign4()
{
	g1 = g2;		/* four bytes: used to come out as a long */
	return g1.x;
}

int assign6()
{
	b1 = b2;		/* six bytes: used to copy only two */
	return b1.a;
}

int passing()
{
	return taking(g1);	/* by value */
}

struct pt returning()
{
	return g1;		/* by value */
}
