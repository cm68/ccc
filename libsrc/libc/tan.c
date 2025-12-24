/*
 * floating point tangent
 *
 * vim: tabstop=4 shiftwidth=4 noexpandtab:
 */
#include	<math.h>

double
tan(x)
double	x;
{
	return sin(x)/cos(x);
}
