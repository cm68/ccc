/*
 * this file is a test harness for the lexer.  it subsumes much of
 * the cpp function, only having the restriction that the only kind of
 * expression that is handled by the cpp is literal integers or names
 * that directly expand into literal integers.  the full compiler of course
 * does not have this limitation.
 */
#include "ccc.h"
#include <fcntl.h>

#include "debugtags.c"

char *progname;

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
    printf("\t-E\n");
    for (i = 0; debugtag[i]; i++) {
        printf("\t%x %s\n", 1 << i, debugtag[i]);
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
            case 'I':
                add_include(++s);
                break;
            case 'D':
                add_define(++s);
                break;
            case 'E':
                write_cpp_file++;
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
        int j = verbose;
        printf("verbose: %x (", verbose);
        for (i = 0; debugtag[i]; i++) {
            if (verbose & (1 << i)) 
                printf("%s", debugtag[i]);
            j ^= (1 << i);
            if (j) {
                printf(" ", debugtag[i]);
            }
        }
        printf(")\n");
    }
    setvbuf(stdout, 0, _IONBF, 0);
#endif

    while (argc--) {
        process(*argv++);
    }

}

struct expr *
expr(char priority, struct stmt *parent)
{
    struct expr *e;

    return e;
}

void
freeexpr(struct expr *e)
{
    free(e);
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
