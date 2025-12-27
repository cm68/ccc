/*
 * errors, messages, and recovery
 */
#include "cc1.h"

#define DEF_ERRMSG
#include "error.h"

int error;
int exitCode = 0;  /* Global exit code: 0=success, 1=errors occurred */

void
gripe(error_t errcode)
{
    int i = errcode;
    if (i > ER_WTF) i = ER_WTF;
    fdprintf(2, "%s:%d: %s\n", filename, lineno, errmsg[i]);
    error = errcode;
    exitCode = 1;
}

/*
 * some errors are too nasty to fix
 */
void
fatal(error_t errcode)
{
    gripe(errcode);
    fdprintf(2, "fatal\n");
    exit(-errcode);
}

/*
 * throw an error message and discard tokens until we see the token we specify
 */
void
recover(error_t errcode, token_t skipto)
{
    gripe(errcode);
    while ((cur.type != skipto) && (cur.type != E_O_F)) {
        gettoken();
    }
}

/*
 * the next token must be 'check'.  if it isn't, gripe about it and skip
 * until we find 'skipto'
 */
void
need(token_t check, token_t skipto, error_t errcode)
{
    if (cur.type == check) {
        gettoken();
        return;
    }
    recover(errcode, skipto);
}

/*
 * expect: simplified token checking - gripe if wrong, advance regardless
 * Used to reduce code duplication where error recovery isn't needed
 */
void
expect(token_t check, error_t errcode)
{
    if (cur.type != check) {
        gripe(errcode);
    }
    gettoken();
}

#ifdef notdef
/*
 * a variant sprintf with support for formats: 
 * %[<precision>]<format>
 * where precision is [-][0][0-9]*
 *    if -, right pad
 *    if leading 0, zero pad
 * formats:
 *    b - binary
 *    d - decimal
 *    x - hex
 *    s - string     
 */
void
sprintf(char *d, char *fmt)
{
    char **ap;
    char c;
    unsigned char zpad;
    unsigned char width;
    unsigned char base;
    unsigned char i;
    unsigned char rpad;
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
#endif

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */

