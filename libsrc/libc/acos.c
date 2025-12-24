/*
 * floating point arc cosine
 *
 * vim: tabstop=4 shiftwidth=4 noexpandtab:
 */
#include	<math.h>

double
acos(x)
double	x;
{
	double	y;

	y = sqrt(1 - x*x);
	return atan(y/x);
}

