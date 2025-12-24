/*
 * floating point hyperbolic sin
 *
 * vim: tabstop=4 shiftwidth=4 noexpandtab:
 */
#include	<math.h>

double
sinh(x)
double	x;
{
	x = exp(x);
	return 0.5*(x-1.0/x);
}
