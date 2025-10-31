/*
 * this file is the compiler first pass.
 * it takes source code consisting of unpreprocessed c, include files,
 * and command line defines and outputs parse trees and static data
 * it does basically all of the semantic processing, like operator precedence
 * and type resolution
 */
#include "ccc.h"
// #include <fcntl.h>

#include "debugtags.c"

char *progname;
int verbose;

/*
 * each file on the command line gets this treatment
 */
void
process(char *f)
{
    int i;
    char *s;

    if (VERBOSE(V_TRACE)) {
        printf("process %s\n", f);
    }
    if (write_cpp_file) {
        if (cpp_file) {
            close(cpp_file);
            cpp_file = 0;
            free(cpp_file_name);
        }
        i = strlen(f);
        if (f[i-2] == '.' && f[i-1] == 'c') {
            i -= 2;
        }
        cpp_file_name = malloc(i+3);  // +2 for ".i", +1 for null terminator
        strncpy(cpp_file_name, f, i);
        cpp_file_name[i] = '\0';  // Ensure null termination
        strcat(cpp_file_name, ".i");
        cpp_file = creat(cpp_file_name, 0777);
        if (cpp_file == -1) {
            perror(cpp_file_name);
        }
        s = "/* preprocessed file */\n";
        cpp_out(s, strlen(s));
    }

    insertfile(f, 0);
    ioinit();
    lexinit();

    parse();

    if (write_cpp_file) {
        cpp_flush();
    }
}

void
usage(char *complaint, char *p)
{
    int i;

    printf("%s", complaint);
    printf("usage: %s [<options>] [program [<program options>]]\n", p);
    printf("\t-I<include dir>\n");
    printf("\t-D<variable>[=<definition>]\n");
    printf("\t-v <verbosity>\n");
    printf("\t-E\n");
    for (i = 0; vopts[i]; i++) {
        printf("\t%x %s\n", 1 << i, vopts[i]);
    }
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
                add_include(s);
                s="";
                break;
            case 'D':
                add_define(s);
                s="";
                break;
            case 'E':
                write_cpp_file++;
                break;
            case 'v':
                if (!argc--) {
                    usage("verbosity not specified \n", progname);
                }
                verbose = strtol(*argv++, 0, 0);
                break;
            default:
                printf("bad flag %c\n", (*s));
                break;
            }
        }
    }

    if (verbose) {
        int j = 0;

        for (i = 0; i < 32; i++) {
        	if (!vopts[i])
        		break;
        	if (verbose & (1 << i))
        		j |= (1 <<i);
        }

        printf("verbose: %x (", j);
        for (i = 0; vopts[i]; i++) {
            if (j & (1 << i)) {
                printf("%s", vopts[i]);
				j ^= (1 << i);
				if (j) {
					printf(" ");
				}
            }
        }
        printf(")\n");
    }
#ifdef __UNIX__
    setvbuf(stdout, 0, _IONBF, 0);
#endif

    /*
     * handle each source file on the command line
     */
    while (argc--) {
        process(*argv++);
    }
    return 0;
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
