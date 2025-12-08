/*
 * this file is the compiler first pass.
 * it takes source code consisting of unpreprocessed c, include files,
 * and command line defines and outputs parse trees and static data
 * it does basically all of the semantic processing, like operator precedence
 * and type resolution
 */
#include "cc1.h"
#include <fcntl.h>
#include <signal.h>
#include <unistd.h>

#ifdef DEBUG
#include "debugtags.c"
#endif

#ifndef CCC
/*
 * Signal handler for compilation timeout
 *
 * Catches SIGALRM to detect infinite loops or hangs during compilation.
 * The main() function sets a 5-second alarm that triggers this handler
 * if compilation doesn't complete in time.
 *
 * This is a safety mechanism for compiler development to catch:
 *   - Infinite loops in parser
 *   - Circular macro expansion
 *   - Runaway recursion
 *   - Other pathological inputs
 *
 * Parameters:
 *   sig - Signal number (SIGALRM)
 *
 * Side effects:
 *   - Prints timeout message to stderr
 *   - Calls fatal() which exits the process
 */
void
timeoutHdlr(int sig)
{
    fdprintf(2,"\n\n*** TIMEOUT after 5 seconds ***\n");
    fatal(ER_WTF);
}
#endif

char *progname;
#ifdef DEBUG
int verbose;
#endif

/* Global context for static variable name mangling */
char *srcFileRoot = NULL;
struct name *curFunc = NULL;
unsigned char staticCtr = 0;
unsigned char shadowCtr = 0;  /* Counter for shadowed variable mangling */

/* AST output control */
unsigned char astFd;  // defaults to 1 (stdout), can be overridden with -o

/*
 * Process a single source file through compilation
 *
 * This is the main per-file compilation function that orchestrates the
 * complete compilation pipeline for one source file. Each file on the
 * command line is processed independently.
 *
 * Compilation steps:
 *   1. Extract source file basename for static variable name mangling
 *   2. Create .i preprocessor output file if -E flag specified
 *   3. Push source file onto input stack (insertfile)
 *   4. Initialize I/O system (ioinit) - prime character lookahead
 *   5. Initialize lexer state - clear token lookahead
 *   6. Parse entire file (parse) - generates AST output
 *   7. Flush preprocessor output if enabled
 *
 * Static variable name mangling:
 *   - Extracts basename without path or .c extension
 *   - Used as prefix for function-scoped static variable names
 *   - Example: foo/bar.c -> "bar" -> bar_func_var_0
 *
 * Preprocessor output (-E flag):
 *   - Creates <basename>.i file in current directory (not source directory)
 *   - Contains fully preprocessed source code
 *   - All macros expanded, includes resolved
 *   - Comments stripped (except header comment)
 *
 * Parameters:
 *   f - Source file path (can include directory components)
 *
 * Side effects:
 *   - Updates global srcFileRoot
 *   - Creates and writes .i file if -E specified
 *   - Outputs AST to astFd (stdout or -o file)
 *   - Consumes entire source file from disk
 */
void
process(char *f)
{
    int len;
    char *basenameStart, *dot;

    if (VERBOSE(V_TRACE)) {
        fdprintf(2,"process %s\n", f);
    }

    /* Extract source file root for static name mangling */
    if (srcFileRoot) {
        free(srcFileRoot);
    }
    /* Find last slash (if any) to get basename */
    basenameStart = strrchr(f, '/');
    if (basenameStart) {
        basenameStart++;  /* skip the slash */
    } else {
        basenameStart = f;
    }
    /* Find .c extension and copy everything before it */
    dot = strrchr(basenameStart, '.');
    if (dot && strcmp(dot, ".c") == 0) {
        len = dot - basenameStart;
    } else {
        len = strlen(basenameStart);
    }
    srcFileRoot = malloc(len + 1);
    strncpy(srcFileRoot, basenameStart, len);
    srcFileRoot[len] = '\0';

    insertfile(f, 0);
    ioinit();
    cur.type = next.type = NONE;

    parse();
}

/*
 * Print usage information and exit
 *
 * Displays command-line syntax, available options, and debug verbosity
 * flags. Called when invalid arguments are detected or -h flag specified.
 *
 * Options displayed:
 *   -I<dir>        Add user include search path
 *   -i<dir>        Set system include path (default: include)
 *   -D<name>=<val> Define preprocessor macro
 *   -v <mask>      Set debug verbosity (hex bitmask)
 *   -o <file>      Specify AST output file (default: stdout)
 *   -E             Generate preprocessed .i file
 *
 * Debug verbosity flags (from debugtags.c):
 *   - Listed with hex values and descriptions
 *   - Can be OR'd together for multiple categories
 *   - Examples: V_TRACE, V_LEX, V_PARSE, V_TYPE, etc.
 *
 * Parameters:
 *   complaint - Error message to display before usage (can be empty)
 *   p         - Program name (argv[0])
 *
 * Side effects:
 *   - Prints to stderr
 *   - Exits with status 1
 */
void
usage(char *complaint, char *p)
{
    fdprintf(2,"%s", complaint);
    fdprintf(2,"usage: %s [<options>] [program [<program options>]]\n", p);
    fdprintf(2,"\t-I<include dir>\n");
    fdprintf(2,"\t-i<system include dir>\n");
    fdprintf(2,"\t-D<name>[=<value>]\n");
    fdprintf(2,"\t-o <output file>\n");
#ifdef DEBUG
    fdprintf(2,"\t-v <verbosity>\n");
#ifndef CCC
    {
        int i;
        for (i = 0; vopts[i]; i++) {
            fdprintf(2,"\t%x %s\n", 1 << i, vopts[i]);
        }
    }
#endif
#endif
    exit(1);
}

/*
 * Main compiler driver - orchestrates command-line processing and compilation
 *
 * This is the entry point for the cc1 compiler (pass 1 of the CCC compiler).
 * It processes command-line arguments, initializes the compilation environment,
 * and orchestrates the compilation of each source file.
 *
 * Command-line processing:
 *   - Parses flags (-I, -i, -D, -E, -v, -o) until first non-flag argument
 *   - Flags can be combined or separate: -DFOO -E or -DEFOO
 *   - Multiple source files can be specified for batch compilation
 *   - Flag processing stops at first non-dash argument
 *
 * Initialization:
 *   - Sets up 5-second timeout alarm to catch infinite loops (Unix only)
 *   - Initializes AST output to stdout (can be overridden with -o)
 *   - Adds null include prefix (current directory)
 *   - Displays verbose flags if requested
 *
 * Per-file processing:
 *   - Calls process() for each source file on command line
 *   - Each file is parsed independently
 *   - Calls cleanupParse() after each file to free memory
 *   - Continues to next file even if previous had errors
 *
 * Exit behavior:
 *   - Returns exitCode (0 if no errors, 1 if errors occurred)
 *   - Closes AST output file if not stdout
 *   - Timeout kills process if compilation hangs (no cleanup)
 *
 * Verbosity output:
 *   - Displays enabled debug flags in human-readable form
 *   - Shows hex mask and corresponding flag names
 *   - Example: "verbose: 0x21 (TRACE TYPE)"
 *
 * Parameters:
 *   argc - Argument count
 *   argv - Argument vector (program name, flags, source files)
 *
 * Returns:
 *   exitCode - 0 if successful, 1 if errors occurred during compilation
 *
 * Side effects:
 *   - Sets global verbose, astFd variables
 *   - Modifies include path list
 *   - Defines macros from -D flags
 *   - Creates and writes to output files
 *   - Sets 5-second alarm (Unix only)
 */
int
main(int argc, char **argv)
{
	char *s;
    int i;

#ifndef CCC
    /* Set up timeout handler to catch infinite loops */
    signal(SIGALRM, timeoutHdlr);
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
                sysIncPath = s;
                s="";
                break;
            case 'D':
                addDefine(s);
                s="";
                break;
#ifdef DEBUG
            case 'v':
                if (!argc--) {
                    usage("verbosity not specified \n", progname);
                }
                verbose = strtol(*argv++, 0, 0);
                break;
#endif
            case 'o':
                if (!argc--) {
                    usage("output file not specified \n", progname);
                }
                astFd = open(*argv++, O_WRONLY | O_CREAT | O_TRUNC, 0644);
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

#ifdef DEBUG
#ifndef CCC
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
#endif
#endif
#ifdef __UNIX__
    setvbuf(stdout, 0, _IONBF, 0);
#endif

    /*
     * handle each source file on the command line
     */
    while (argc--) {
        process(*argv++);
        /* Clean up allocated memory after each file */
        cleanupParse();
    }

    /* Close AST output file if not stdout */
    if (astFd > 1) {
        close(astFd);
    }

    return exitCode;  /* Return 0 if no errors, 1 if errors occurred */
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
