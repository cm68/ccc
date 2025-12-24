/*
 * floating point arc sin
 *
 * vim: tabstop=4 shiftwidth=4 noexpandtab:
 */
#include	<math.h>

double
asin(x)
double	x;
{
	double	y;

	y = sqrt(1 - x*x);
	return atan(x/y);
}

