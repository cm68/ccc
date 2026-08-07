/*
 * libutil.h - Shared utility functions for ccc
 */
#ifndef LIBUTIL_H
#define LIBUTIL_H

/*
 * Minimal formatter - handles %s, %d, %ld, %x, %c, %% (no width/padding)
 * Returns pointer to end of written string
 */
char *fmtstr(char *buf, char *fmt, ...);

#endif /* LIBUTIL_H */

/* vim: set tabstop=4 shiftwidth=4 noexpandtab: */
