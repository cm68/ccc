/*
 * exit with stdio close
 *
 */
exit(v)
{
	_cleanup();
	_exit(v);
}

/* vim: set tabstop=4 shiftwidth=4 noexpandtab: */
