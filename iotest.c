/*
 * this file is a test harness for the io path.
 */
#include "ccc.h"
#include <fcntl.h>

int verbose;

#ifdef DEBUG
int verbose;

char *vopts[] = {
    "V_IO",
    0
};
#endif

char *progname;

void
process(char *f)
{
    char *s;
    int i;

    printf("process %s\n", f);
    insertfile(f, 0);
    ioinit();
    while (curchar) {
        /*
         * the test file contains an xMy
         * this needs to expand to xtestingy
         */
        if (curchar == 'M') {
            cdump();
            insertmacro("M", "test");
            cdump();
        }
        printf("curchar %d %c nextchar %d %c column %d line %d\n", curchar, curchar, nextchar, nextchar, column, lineno);
        advance();
    }
}

void
usage(char *complaint, char *p)
{
    int i;

    printf("%s", complaint);
    printf("usage: %s [<options>] [program [<program options>]]\n", p);
#ifdef DEBUG
    printf("\t-v <verbosity>\n");
    printf("\t-E\n");
    for (i = 0; vopts[i]; i++) {
        printf("\t%x %s\n", 1 << i, vopts[i]);
    }
#endif
    exit(1);
}

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
