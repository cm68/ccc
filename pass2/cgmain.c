#include "cgen.h"

/*
 * File - main.c
 */
/*********************************************************
 * main OK++ PMO
 *
 *********************************************************/
int main(int argc, char **argv) {
#ifdef CPM
    baseHeap = sbrk(0); /* Current highest memory */
#endif
    --argc, ++argv;
    while (argc > 0 && **argv == '-') { /* Parsing options */
        switch (argv[0][1]) {
#if 0
        case 'P':
        case 'p':
            pflag = true;
            break; /* Not use */
#endif
        case 'W':
        case 'w':
            wflag = true;
            break; /* Displaying warnings */
        case 'R':
            rflag = true;
            break;
#if 0
        case 'B':
            bflag = true;
            break; /* Not use */
        case 'E':
            eflag = true;
            break; /* Not use */
        case 'H':
            hflag = true;
            break; /* Not use */
#endif
        default:
            fatalErr("Illegal\tswitch %c", argv[0][1]);
            break;
        }
        argv++, argc--;
    }
    if (argc-- > 0) {
        if (freopen(*argv, "r", stdin) == NULL) /* Open input file */
            fatalErr("Can't open %s", *argv);
        else if (argc > 0 && freopen(argv[1], "w", stdout) == NULL) /* Open output file */
            fatalErr("Can't create %s", argv[1]);
    }
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

/*********************************************************
 * fatalErr - Print error message and exit
 *********************************************************/
_Noreturn void fatalErr(char *fmt, ...) {
    va_list args;
    va_start(args, fmt);

    prMsg(fmt, args);
    va_end(args);
    fclose(stdout);
    exit(2);
}

/*********************************************************
 * prWarning - Print warning message
 *********************************************************/
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

/*********************************************************
 * prError OK PMO
 * Nonfatal error
 * Difference due to use of stdarg
 *********************************************************/
void prError(char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    prMsg(fmt, args);
    va_end(args);
    if (++errcnt >= MAXERR)
        fatalErr("Too many errors");
}

/*********************************************************
 * prMsg OK PMO	      	  Used in: ferror, prError
 * Difference due to use of stdarg
 *********************************************************/
void prMsg(char *fmt, va_list args) {
    fprintf(stderr, "%s:%d:\t", progname, lineno);
    vfprintf(stderr, fmt, args);
    fputc('\n', stderr);
}

/*********************************************************
 * allocMem - Allocate memory, exits on failure
 *********************************************************/
void *allocMem(size_t size) {
    register char *ptr;

    do {
        if (ptr = malloc(size))
            goto done;
    } while (relNodeFrList());
    fatalErr("No room");
done:
    blkclr(ptr, size);
    return ptr;
}

/* end of file main.c */
