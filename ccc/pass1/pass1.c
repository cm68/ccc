/*
 * this file is the compiler first pass.
 * it reads a binary lexeme stream from cpp and outputs AST files.
 * it does all semantic processing: type resolution, operator precedence,
 * expression parsing, and symbol table management.
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
short verbose;
#endif

/* Global context for static variable name mangling */
struct name *curFunc = NULL;
unsigned char staticCtr = 0;
unsigned char shadowCtr = 0;  /* Counter for shadowed variable mangling */

/* AST output control */
unsigned char astFd;  // set by process() to output file from argv[1]

/* Two-phase parsing control */
unsigned char phase;  // 1 = build symbol table, 2 = emit AST

/*
 * Process a single source file through compilation
 *
 * This is the main per-file compilation function that orchestrates the
 * complete compilation pipeline for one source file.
 *
 * Compilation steps:
 *   1. Open output file (o1) for AST output
 *   2. Open input lexeme stream (f)
 *   3. Phase 1: Parse to build symbol table and accumulate counts
 *   4. Phase 2: Parse again to emit AST with pre-computed counts
 *   5. Close files
 *
 * Parameters:
 *   f  - Input lexeme file (preprocessed .x file from cpp)
 *   o1 - Output AST file (base.1)
 *   o2 - Temp file (base.2) - currently unused
 *
 * Side effects:
 *   - Opens and writes to o1
 *   - Reads from f
 */
void
process(char *f, char *o1, char *o2)
{
#ifdef DEBUG
    if (VERBOSE(V_TRACE)) {
        fdprintf(2,"process %s\n", f);
    }
#endif

    /* Open output file for AST (o1 from ccc: base.1) */
    astFd = creat(o1, 0644);
    if (astFd < 0) {
        perror(o1);
        exit(1);
    }

    /* Read from preprocessed lexeme stream */
    lexOpen(f);

    /* Phase 1: Build symbol table, validate types, accumulate counts */
    phase = 1;
    parse();

    /* Rewind lexeme stream for phase 2 */
    lexRewind();
    lexlevel = 0;  /* Reset scope level for phase 2 */

    /* Phase 2: Emit AST (uses counts pushed by phase 1 in LIFO order) */
    phase = 2;
    parse();

    lexClose();
    close(astFd);
}

/*
 * Print usage information and exit
 *
 * Displays command-line syntax, available options, and debug verbosity
 * flags. Called when invalid arguments are detected or -h flag specified.
 *
 * Options displayed:
 *   -v <mask>      Set debug verbosity (hex bitmask)
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
usage(char *complaint)
{
    fdprintf(2,"%s", complaint);
    fdprintf(2,"usage: %s [<options>] lexfile out1 out2\n", progname);
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
#ifdef DEBUG
    int i;
#endif

#ifndef CCC
    /* Set up timeout handler to catch infinite loops */
    signal(SIGALRM, timeoutHdlr);
    alarm(5);  /* 5 second timeout */
#endif

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
                usage("");
                break;
#ifdef DEBUG
            case 'v':
                if (!argc--) {
                    usage("verbosity not specified \n");
                }
                verbose = strtol(*argv++, 0, 0);
                break;
#endif
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

    if (argc < 3) {
        usage("file count\n");
    }

    process(argv[0], argv[1], argv[2]);

    /* Report allocation counts */
#ifdef DEBUG
    fdprintf(2, "names: %d/%d exprs: %d/%d\n",
        nameAllocCnt, nameHighWater, exprAllocCnt, exprHighWater);
#endif

    return exitCode;  /* Return 0 if no errors, 1 if errors occurred */
}

/* vim: set tabstop=4 shiftwidth=4 noexpandtab: */
