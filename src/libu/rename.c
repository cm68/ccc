/*
 * rename - give a file a new name
 *
 * There is no rename system call on this system.  It arrived in 4.2BSD
 * and Micronix is a v6, so the operation has to be built out of the
 * two calls that do exist: make a second name for the file, then take
 * the first one away.
 *
 * link fails if the new name already exists, so the old one goes
 * first.  That is the one place this differs from the real thing: a
 * genuine rename replaces the destination atomically, and this has a
 * window between the unlink and the link where neither name is there.
 * Nothing on a single-user machine is racing us for it, and the
 * alternative - link to a temporary, unlink, link, unlink - trades the
 * window for a temporary name that a crash would leave behind.
 *
 * libcpm has its own, because CP/M has a BDOS call for it.  The
 * driver carries a private copy of this as moveover() in ccc.c: it has
 * to compile on a host as well, where rename is the C library's and
 * means something subtly different.
 */
rename(from, to)
char *from, *to;
{
	unlink(to);
	if (link(from, to) != 0)
		return -1;
	return unlink(from);
}
