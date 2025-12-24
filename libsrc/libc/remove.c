/*
 * remove is just unlink
 *
 * vim: tabstop=4 shiftwidth=4 noexpandtab:
 */
extern int	unlink();

remove(s)
char *	s;
{
	return unlink(s);
}
