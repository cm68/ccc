/*
 * sdccstub.c - syscall stubs for the sdcc CP/M size build (com-sdcc).
 *
 * The sdcc runtime has no CP/M file io; these no-op stubs let the link
 * complete so the image measures the compiler proper.  A runnable port
 * needs BDOS-backed implementations (see libsrc/libcpm for the ccc
 * versions, roughly 2.5KB text).  Do not link this into anything that
 * must actually run.
 */

int open(char *path, int mode)
{
	return -1;
}

int creat(char *path, int mode)
{
	return -1;
}

int close(int fd)
{
	return -1;
}

int read(int fd, char *buf, int len)
{
	return -1;
}

int write(int fd, char *buf, int len)
{
	return len;
}

int fork(void)
{
	return -1;
}

int wait(int *status)
{
	return -1;
}

int execl(char *path, char *arg0, ...)
{
	return -1;
}

int perror(char *msg)
{
	return 0;
}

/* vim: set tabstop=4 shiftwidth=4 noexpandtab: */
