#include "ccc.h"

#include <fcntl.h>

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
    insertfile(f, 0);
    nexttok = curtok = NONE;
    while (curtok) {
        gettoken();
    }
    parse();
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
    setvbuf(stdout, 0, _IONBF, 0);
#endif

    while (argc--) {
        process(*argv++);
    }

}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */

