/*
 * this is a wrapper for seek
 */
#include <unistd.h>

long
lseek(unsigned char fd, long offset, int whence)
{
	short ret;
	int page;

	page = offset / 512;
	ret = seek(fd, page, whence);
	return ret;
}

/*
 * vim: tabstop=4 shiftwidth=4 noexpandtab:
 */
