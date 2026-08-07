#include "cgen.h"


/*
 * File - getchar.c
 * simple file used for function versions of getchar & putchar
 */
#
#ifdef CPM
/*
 * fgetchar - Read character from stdin
 *
 * CPM version: direct wrapper around fgetc(stdin)
 */
int fgetchar() {
    return fgetc(stdin);
}

/*
 * fputchar - Write character to stdout
 *
 * CPM version: direct wrapper around fputc(c, stdout)
 */
int fputchar(int c) {
    fputc(c, stdout);
}
#else
#if !defined(_WIN32)
/* assume unix convention and ingore \r on input and add \r on output */

/*
 * fgetchar - Read character from stdin with CR filtering
 *
 * Unix version: strips carriage returns from input stream.
 * Loops until non-CR character found.
 */
int fgetchar() {
    int c;
    while ((c = fgetc(stdin)) == '\r')
        ;
    return c;
}

/*
 * fputchar - Write character to stdout with CR insertion
 *
 * Unix version: converts LF to CRLF by adding CR before newline.
 * Returns result of fputc operation.
 */
int fputchar(int c) {
    if (c == '\n')
        fputc('\r', stdout);
    return fputc(c, stdout);
}
#endif
#endif
/* end of file getchar.c */

/* vim: set tabstop=4 shiftwidth=4 noexpandtab: */
