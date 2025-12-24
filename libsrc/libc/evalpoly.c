/*
 * floating point evaluate a polynomial
 *
 * vim: tabstop=4 shiftwidth=4 noexpandtab:
 */
double
eval_poly(x, d, n)
double	x, d[];
int	n;
{
	int	i;
	double	res;

	res = d[i = n];
	while(i)
		res = x * res + d[--i];
	return res;
}
