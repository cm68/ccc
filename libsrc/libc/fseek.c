/*
 * seek a stdio stream
 *
 * vim: tabstop=4 shiftwidth=4 noexpandtab:
 */
#include <stdio.h>
#include <unistd.h>

int
fseek(FILE *fp, long off, int whence)
{
	if (fp->_flag & _IOERR)
		return EOF;

	if ((fp->_flag & _IOWRT) && _flsbuf(fp))
		return -1;
	else if (whence == SEEK_CUR && (fp->_flag & _IOREAD))
		off -= fp->_cnt; /* adjust for buffered but unread data */

	if (lseek(fp->_file, off, whence) < 0) {
		fp->_flag |= _IOERR;
		return EOF;
	}

	fp->_cnt = 0;
	fp->_ptr = fp->_base;

	if (fp->_flag & _IORW)
		fp->_flag &= ~(_IOREAD | _IOWRT);
	fp->_flag &= ~_IOEOF;

	return 0;
}
