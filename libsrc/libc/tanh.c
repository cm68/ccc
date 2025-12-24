/*
 * floating point hyperbolic tangent
 *
 * vim: tabstop=4 shiftwidth=4 noexpandtab:
 */

#include	<math.h>

double
tanh(x)
double	x;
{
	x = exp(x);
	return (x-1.0/x)/(x+1.0/x);
}
