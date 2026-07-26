/*
 * Unions are entirely a c0 concern.  Every member sits at offset zero,
 * so member access is address arithmetic with a zero offset and c1
 * sees nothing union-shaped at all - just a load or store of the
 * member's own width at the union's address.
 *
 * The by-value forms are rejected with the struct ones; see
 * struct_byvalue.c.
 */

union u {
	char c;
	int i;
	long l;
	char buf[8];
};

union u gu;

/* a union inside a struct: its size has to push what follows along */
struct s { int a; union u m; int b; };
struct s gs;

int usize()
{
	return sizeof(union u);		/* the largest member, 8 */
}

/* every member is the same address, read and written at its own width */
int umembers()
{
	gu.i = 5;
	gu.c = 1;
	gu.l = 7;
	return gu.i;
}

int uptr(p)
union u *p;
{
	p->i = 3;
	return p->c;
}

/* gs.a at +0, the union at +2, so gs.b lands at +10 */
int nested()
{
	gs.a = 1;
	gs.m.i = 2;
	gs.b = 3;
	return gs.b;
}
