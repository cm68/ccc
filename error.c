#include "ccc.h"

#undef sprintf

#define MAXERR  0
char *errmsg[] = {
    "number format error",
    "bad character",
    "bad literal",
    "unknown operator"
    "endif without if",
    "too many elses",
    "macro name missing",
    "bad include format",
    "bad macro param",
    "cpp const required", 
    "unknown error",
};
int error;

void
err(char errcode)
{
    int i;

    i = errcode;
    if (i > MAXERR) i = 0;
    printf("file: %s line: %d error code %d %s\n",
        filename, lineno, errcode, errmsg[i]);
    error = errcode;
}

void
fatal(char errcode)
{
    err(errcode);
    printf("too severe to recover\n");
    exit(-errcode);
}

void
recover(char errcode, char skipto)
{
    err(errcode);
    while (curtok != skipto && curtok != EOF) {
        gettoken();
    }
}

void 
need(char check, char skipto, char errcode)
{
    if (curtok == check) {
        gettoken();
        return;
    }
    recover(errcode, skipto);
}

void
sprintf(char *d, char *fmt) {
    char **ap;
    char c;
    char zpad;
    char width;
    char base;
    char i;
    char rpad;
    int v;
    char b[8];
    char *s;

    ap = &fmt;
    ap++;

    while ((c = *fmt++)) {
        if (c != '%') {
            *d++ = c;
            continue;
        }
        c = *fmt++;
        zpad = 0;
        width = 0;
        base = 0;
        rpad = 0;
        if (c == '-') {
            rpad++; 
            c = *fmt++;
        }
        if (c == '0') {
            zpad++;
            c = *fmt++;
        }
        while (c >= '0' && c <= '9') {
            width = width * 10 + c - '0';
            c = *fmt++;
        }
        for (i = 0; i < sizeof(b); i++) {
            b[i] = zpad ? '0' : ' ';
        }
        if (c == 'd') base = 10;
        else if (c == 'x') base = 16;
        else if (c == 'b') base = 2;

        if ((c == 'd') || (c == 'x') || (c == 'b')) {
            v = *((int *)ap);
            ap++;
            i--;
            while (v) {
                c = v % base;
                v /= base;
                if ((base == 16) && (c > 9)) {
                    c += 'a' - 10;
                } else {
                    c += '0';
                }
                b[i--] = c;
            }
            if (rpad) {
                i = sizeof(b) - (i + 1);
            } else {
                i = sizeof(b) - width;
            }
            while (width--) {
                if (i > sizeof(b)) {
                    *d++ = ' ';
                } else {
                    *d++ = b[i++];
                }
            }
            continue;
        }
        if (c == 's') {
            s = *ap;
            ap++;
            i = strlen(s);
            if (!rpad) {
                i = width - i;
                while (i--) {
                    *d++ = ' ';
                    width--;
                }
            }
            while (width--) {
                if (*s) {
                    *d++ = *s++;
                } else {
                    *d++ = ' ';
                }
            }
            continue;
        }
    }
}
/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */

