/*
 * floating point cosine
 *
 * vim: tabstop=4 shiftwidth=4 noexpandtab:
 */
#include	<math.h>

double
cos(f)
double	f;
{
	/* cos is pi/2 out of phase with sin, so ... */

	return sin(f + 1.570796326795);
}
