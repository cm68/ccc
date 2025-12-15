/*
 * emit.c - Output functions
 */
#include <stdio.h>
#include <stdarg.h>
#include <unistd.h>
#include "cc2.h"

/* Output state */
char outbuf[256];
int indent;
int labelCnt;   /* label counter within function */
int fnIndex;    /* function index for unique labels */
int blockCnt;   /* global block counter for debugging */

/*
 * Output
 */
void
emit(char *fmt, ...)
{
    va_list ap;
    char *p = outbuf;
    char *s;
    int n;

    va_start(ap, fmt);
    while (*fmt) {
        if (*fmt == '%') {
            fmt++;
            switch (*fmt++) {
            case 's':
                s = va_arg(ap, char *);
                while (*s) *p++ = *s++;
                break;
            case 'r':  /* register - may have %d for offset */
                s = va_arg(ap, char *);
                n = va_arg(ap, int);
                if (!s) { *p++ = '?'; break; }
                if (n > 127) n -= 256;  /* sign extend byte offset */
                p += sprintf(p, s, n);
                break;
            case 'o':  /* signed offset */
                n = va_arg(ap, int);
                p += sprintf(p, "%+d", n);
                break;
            case 'd':
                n = va_arg(ap, int);
                p += sprintf(p, "%d", n);
                break;
            case 'c':
                n = va_arg(ap, int);
                *p++ = n;
                break;
            case '%':
                *p++ = '%';
                break;
            }
        } else {
            *p++ = *fmt++;
        }
    }
    va_end(ap);
    *p++ = '\n';
    write(outfd, outbuf, p - outbuf);
}

void
emitLabel(char *name)
{
    unsigned char n;
    n = sprintf(outbuf, "%s:", name);
    outbuf[n++] = '\n';
    write(outfd, outbuf, n);
}

/* Emit comment with current indent */
void
comment(char *fmt, ...)
{
    va_list ap;
    unsigned char n;
    unsigned char i;

    n = sprintf(outbuf, "; ");
    for (i = 0; i < indent; i++)
        outbuf[n++] = ' ';

    va_start(ap, fmt);
    n += vsprintf(outbuf + n, fmt, ap);
    va_end(ap);
    outbuf[n++] = '\n';
    write(outfd, outbuf, n);
}
/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
