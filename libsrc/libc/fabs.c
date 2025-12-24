/*
 * floating point absolute value
 *
 * vim: tabstop=4 shiftwidth=4 noexpandtab:
 */
double
fabs(d)
double	d;
{
	if(d < 0.0)
		return -d;
	return d;
}
