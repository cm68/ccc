/*
 * A struct has no value in this compiler: it cannot be assigned,
 * passed or returned.  Its address is the only handle on it, which is
 * what the member operators work through.
 *
 * This file is the legal half - it must compile.  The rejected forms
 * live in struct_byvalue.c, which is expected to fail.
 *
 * The reason these have to be diagnosed rather than left alone is that
 * nothing downstream can tell an aggregate from a scalar of the same
 * size: the width is chosen by byte count, so a four byte struct came
 * out as a long, and a struct of any other size fell to the default
 * and copied two bytes, losing the rest without a word.
 */

struct pt { int x; int y; };
struct big { int a; int b; int c; };

struct pt g1, g2;
struct big b1;

/* through a pointer, which is what is left */
int viaptr(p)
struct pt *p;
{
	p->x = 1;
	p->y = p->x;
	return p->x + p->y;
}

int addrof()
{
	return viaptr(&g1) + viaptr(&g2);
}

/* members are ordinary scalars and behave normally */
int members()
{
	g1.x = 5;
	g1.y = g1.x;
	b1.a = g1.y;
	b1.c = b1.a + 1;
	return b1.c;
}
