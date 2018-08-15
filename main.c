#include "ccc.h"
#include "lex.h"

#include <stdio.h>
#include <fcntl.h>

char errmsg[80];

void
err(char errcode)
{
    printf("file: %s line: %d error code %d %s\n",
        tbtop->name, lineno, errcode errmsg[errcode]);
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
sprintf(char *s, char *fmt) {
    char *ap;
    char c;
    char zpad;
    char width;
    char base;
    char *s;
    char i;
    char rpad;
    int v;
    char b[8];

    while (c = *fmt++) {
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
            v = *(((int *)ap)++);
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
            s = *(((char *)ap)++);
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

#ifdef DEBUG
int verbose;

char *vopts[] = {
    "V_LEX",
    0
};
#endif

void
usage(char *complaint, char *p)
{
    int i;

    printf("%s", complaint);
    printf("usage: %s [<options>] [program [<program options>]]\n", p);
    printf("\t-I<include dir>\n");
    printf("\t-D<variable>[=<definition>]\n");
#ifdef DEBUG
    printf("\t-v <verbosity>\n");
    for (i = 0; vopts[i]; i++) {
        printf("\t%x %s\n", 1 << i, vopts[i]);
    }
#endif
    exit(1);
}

void
process(char *f)
{
    char *s;
    int fd;
    int ret;

    printf("process %s\n", f);
    pushfile(f);
}

char *progname;

int
main(int argc, char **argv)
{
	char *s;
    int i;

    progname = *argv++;
    argc--;

    while (argc) {
        s = *argv;

        /* end of flagged options */
        if (*s++ != '-')
            break;

        argv++;
        argc--;

        /* s is the flagged arg string */
        while (*s) {
            switch (*s++) {
            case 'h':
                usage("", progname);
                break;
            case 'I':
                // XXX - add_include(++s);
                break;
            case 'D':
                // XXX - add_define(++s);
                break;
#ifdef DEBUG
            case 'v':
                if (!argc--) {
                    usage("verbosity not specified \n", progname);
                }
                verbose = strtol(*argv++, 0, 0);
                break;
#endif
            default:
                printf("bad flag %c\n", (*s));
                break;
            }
        }
    }

#ifdef DEBUG
    if (verbose) {
        for (i = 0; vopts[i]; i++) {
            printf("verbose: ");
            if (verbose & (1 << i)) 
                printf(" %s", vopts[i]);
        }
        printf("\n");
    }
#endif

    while (argc--) {
        process(*argv++);
    }

}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */

