/*
 * exit with stdio close
 *
 * vim: tabstop=4 shiftwidth=4 noexpandtab:
 */
exit(v)
{
	_cleanup();
	_exit(v);
}
