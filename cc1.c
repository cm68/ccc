/*
 * this file is the compiler first pass.
 * it takes source code consisting of unpreprocessed c, include files,
 * and command line defines and outputs parse trees and static data
 * it does basically all of the semantic processing, like operator precedence
 * and type resolution
 */
#include "cc1.h"
#ifndef SDCC
#include <fcntl.h>
#include <signal.h>
#include <unistd.h>
#endif

#include "debugtags.c"

/*
 * timeout handler - catch infinite loops during parsing
 */
void
timeout_handler(int sig)
{
    fdprintf(2,"\n\n*** TIMEOUT after 5 seconds ***\n");
    fatal(ER_WTF);
}

char *progname;
int verbose;

/* Global context for static variable name mangling */
char *sourceFileRoot = NULL;
struct name *current_function = NULL;
unsigned char static_counter = 0;

/* AST output control */
int astFd;  // defaults to 1 (stdout), can be overridden with -o

/*
 * each file on the command line gets this treatment
 */
void
process(char *f)
{
    int i, len;
    char *s, *basename_start, *dot;

#ifdef DEBUG
    if (VERBOSE(V_TRACE)) {
        fdprintf(2,"process %s\n", f);
    }
#endif

    /* Extract source file root for static name mangling */
    if (sourceFileRoot) {
        free(sourceFileRoot);
    }
    /* Find last slash (if any) to get basename */
    basename_start = strrchr(f, '/');
    if (basename_start) {
        basename_start++;  /* skip the slash */
    } else {
        basename_start = f;
    }
    /* Find .c extension and copy everything before it */
    dot = strrchr(basename_start, '.');
    if (dot && strcmp(dot, ".c") == 0) {
        len = dot - basename_start;
    } else {
        len = strlen(basename_start);
    }
    sourceFileRoot = malloc(len + 1);
    strncpy(sourceFileRoot, basename_start, len);
    sourceFileRoot[len] = '\0';
    if (writeCppfile) {
        if (cppfile) {
            close(cppfile);
            cppfile = 0;
            free(cppfname);
        }
        /* Use basename only - put .i file in current directory */
        i = strlen(basename_start);
        if (i >= 2 && basename_start[i-2] == '.' &&
            basename_start[i-1] == 'c') {
            i -= 2;
        }
        cppfname = malloc(i+3);  // +2 for ".i", +1 for null terminator
        strncpy(cppfname, basename_start, i);
        cppfname[i] = '\0';  // Ensure null termination
        strcat(cppfname, ".i");
        cppfile = creat(cppfname, 0777);
        if (cppfile == -1) {
            perror(cppfname);
        }
        s = "/* preprocessed file */\n";
        cppOut(s, strlen(s));
    }

    insertfile(f, 0);
    ioinit();
    lexinit();

    parse();

    if (writeCppfile) {
        cppFlush();
    }
}

void
usage(char *complaint, char *p)
{
    int i;

    fdprintf(2,"%s", complaint);
    fdprintf(2,"usage: %s [<options>] [program [<program options>]]\n", p);
    fdprintf(2,"\t-I<include dir>\n");
    fdprintf(2,"\t-i<system include dir> (default /usr/include)\n");
    fdprintf(2,"\t-D<variable>[=<definition>]\n");
    fdprintf(2,"\t-v <verbosity>\n");
    fdprintf(2,"\t-o <output file> (AST output, default stdout)\n");
    fdprintf(2,"\t-E (write preprocessed .i file)\n");
    for (i = 0; vopts[i]; i++) {
        fdprintf(2,"\t%x %s\n", 1 << i, vopts[i]);
    }
    exit(1);
}

int
main(int argc, char **argv)
{
	char *s;
    int i;

#ifndef SDCC
    /* Set up timeout handler to catch infinite loops */
    signal(SIGALRM, timeout_handler);
    alarm(5);  /* 5 second timeout */
#endif

    astFd = 1;  // default AST output to stdout (fd 1)
    addInclude("");    // the null include prefix

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
                addInclude(s);
                s="";
                break;
            case 'i':
                sysIncludePath = s;
                s="";
                break;
            case 'D':
                addDefine(s);
                s="";
                break;
            case 'E':
                writeCppfile++;
                break;
            case 'v':
                if (!argc--) {
                    usage("verbosity not specified \n", progname);
                }
                verbose = strtol(*argv++, 0, 0);
                break;
            case 'o':
                if (!argc--) {
                    usage("output file not specified \n", progname);
                }
#ifdef SDCC
                astFd = creat(*argv++, 0644);
#else
                astFd = open(*argv++, O_WRONLY | O_CREAT | O_TRUNC, 0644);
#endif
                if (astFd < 0) {
                    perror("cannot open output file");
                    exit(1);
                }
                break;
            default:
                fdprintf(2,"bad flag %c\n", (*s));
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

        fdprintf(2,"verbose: %x (", j);
        for (i = 0; vopts[i]; i++) {
            if (j & (1 << i)) {
                fdprintf(2,"%s", vopts[i]);
				j ^= (1 << i);
				if (j) {
					fdprintf(2," ");
				}
            }
        }
        fdprintf(2,")\n");
    }
#ifdef __UNIX__
    setvbuf(stdout, 0, _IONBF, 0);
#endif

    /*
     * handle each source file on the command line
     */
    while (argc--) {
        process(*argv++);
        /* Clean up allocated memory after each file */
        cleanup_parser();
    }

    /* Close AST output file if not stdout */
    if (astFd > 1) {
        close(astFd);
    }

    return exit_code;  /* Return 0 if no errors, 1 if errors occurred */
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
