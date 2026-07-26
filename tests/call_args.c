/*
 * Test call argument passing: arg count, order, nesting, and the
 * caller-side stack cleanup.
 */

int one();
int two();
int add2();
int add4();

int nargs()
{
	one(1);
	two(1, 2);
	add4(1, 2, 3, 4);
	return 0;
}

int nested()
{
	return add2(one(1), two(2, 3));
}

int mixed(char *s, int n)
{
	return add2(n, one(s));
}
