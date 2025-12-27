#include "cgen.h"

/*
 * File - main.c
 */

/*
 * Print usage message and exit
 */
static void
usage(void)
{
    fprintf(stderr, "usage: c1 [options] <input.1> <input.2> <output.s>\n");
    fprintf(stderr, "  -w        Suppress warnings\n");
    fprintf(stderr, "  -R        Enable rflag\n");
    exit(1);
}

/*
 * main OK++ PMO
 *
 * Compiler pass 2 entry point. Parses command line options (-w suppress
 * warnings, -R enable rflag), opens input/output files, initializes
 * built-in types, then runs the main parsing loop to process intermediate
 * code and generate Z80 assembly output.
 */
int main(int argc, char **argv) {
#ifdef CPM
    baseHeap = sbrk(0); /* Current highest memory */
#endif
    --argc, ++argv;
    while (argc > 0 && **argv == '-') { /* Parsing options */
        switch (argv[0][1]) {
        case 'h':
            usage();
            break;
        case 'W':
        case 'w':
            wflag = true;
            break; /* Displaying warnings */
        case 'R':
            rflag = true;
            break;
        default:
            fatalErr("Illegal\tswitch %c", argv[0][1]);
            break;
        }
        argv++, argc--;
    }

    /*
     * ccc convention: c1 <input.1> <input.2> <output.s>
     * We ignore input.2 (the temp file) - pass1 merges it into input.1
     */
    if (argc != 3)
        usage();

    if (freopen(argv[0], "r", stdin) == NULL)
        fatalErr("Can't open %s", argv[0]);
    /* argv[1] is the .2 temp file - ignored */
    if (freopen(argv[2], "w", stdout) == NULL)
        fatalErr("Can't create %s", argv[2]);
    initTypes();                 /* Initialize built-in types */
    parseStmt();                 /* Compiling intermediate code */
    if (fclose(stdout) == EOF) { /* Close output file */
        prError("Error closing output file");
    }
    /* Exit with error code */
    exit(errcnt != 0); /* Generated code is not significantly different */
}

/*
 * To comply with C standard functions are replaced with similar
 * ones with a variable number of parameters
 */

/*
 * fatalErr - Print error message and exit
 *
 * Prints formatted error message to stderr with source location,
 * closes output file, and exits with status 2.
 */
_Noreturn void fatalErr(char *fmt, ...) {
    va_list args;
    va_start(args, fmt);

    prMsg(fmt, args);
    va_end(args);
    fclose(stdout);
    exit(2);
}

/*
 * prWarning - Print warning message
 *
 * Prints formatted warning message to stderr with source location,
 * unless warnings are suppressed via -w flag.
 */
void prWarning(char *fmt, ...) {
    va_list args;

    if (wflag == 0) {
        fprintf(stderr, "%s:%d:\t", progname, lineno);
        va_start(args, fmt);
        vfprintf(stderr, fmt, args);
        va_end(args);
        fprintf(stderr, " (warning)\n");
    }
}

/*
 * prError OK PMO
 *
 * Prints non-fatal error message. Increments error count and calls
 * fatalErr if too many errors have accumulated (MAXERR threshold).
 */
void prError(char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    prMsg(fmt, args);
    va_end(args);
    if (++errcnt >= MAXERR)
        fatalErr("Too many errors");
}

/*
 * prMsg OK PMO
 *
 * Core message printing function. Outputs source file name and line
 * number prefix, then the formatted message to stderr.
 */
void prMsg(char *fmt, va_list args) {
    fprintf(stderr, "%s:%d:\t", progname, lineno);
    vfprintf(stderr, fmt, args);
    fputc('\n', stderr);
}

/*
 * allocMem - Allocate memory, exits on failure
 *
 * Allocates zeroed memory. If malloc fails, tries to release node
 * free list before giving up. Fatal error if allocation fails.
 */
void *allocMem(size_t size) {
    register char *ptr;

    do {
        if ((ptr = malloc(size)))
            goto done;
    } while (relNodeFrList());
    fatalErr("No room");
done:
    blkclr(ptr, size);
    return ptr;
}

/* end of file main.c */

/* vim: set tabstop=4 shiftwidth=4 noexpandtab: */
