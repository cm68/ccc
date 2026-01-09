/*
 * cpp - C Preprocessor
 *
 * Main driver for the preprocessor.
 * Produces two output files:
 *   <basename>.x - lexeme stream (compact token format)
 *   <basename>.i - preprocessed source (human readable)
 *
 * Uses lex.c for tokenization, io.c for file handling,
 * and macro.c for macro processing.
 */
#include "cpp.h"
#include <fcntl.h>
#include <unistd.h>

#ifdef DEBUG
#include "debugtags.c"
#endif

/* Global state */
char *curFile;
int lineNo;
char exitCode = 0;
char noLineMarkers = 0;  /* -N flag: suppress LINENO/NEWLINE in .x */
#ifdef DEBUG
short verbose;
#endif

/* Include path list */
#define MAX_INCLUDES 32
char *includePaths[MAX_INCLUDES];
unsigned char numIncludes = 0;

/*
 * Error reporting
 */
static void
errout(char *buf)
{
    write(2, buf, strlen(buf));
}

void
error(char *msg)
{
    char buf[256];
    fmtstr(buf, "%s:%d: error: %s\n", filename ? filename : curFile, lineno, msg);
    errout(buf);
    exitCode = 1;
}

void
fatal(char *msg)
{
    char buf[256];
    fmtstr(buf, "%s:%d: fatal: %s\n", filename ? filename : curFile, lineno, msg);
    errout(buf);
    exit(1);
}

void
usage(void)
{
    errout("usage: cpp [options] <source.c>\n");
    errout("  -o <base>      Output base name (.x and .i)\n");
    errout("  -I<dir>        Add include directory\n");
    errout("  -i<dir>        System include directory\n");
    errout("  -D<name>[=val] Define macro\n");
    errout("  -E             Preprocess only (output to stdout)\n");
    errout("  -h             Show this help\n");
#ifdef DEBUG
    errout("  -v <mask>      Set verbosity (hex bitmask)\n");
#ifndef CCC
    {
        int i;
        for (i = 0; vopts[i]; i++) {
            fdprintf(2, "\t%x %s\n", 1 << i, vopts[i]);
        }
    }
#endif
#endif
    exit(1);
}

/*
 * Process source file - lex all tokens and emit to .x stream
 */
void
process(char *sourcefile)
{
    curFile = sourcefile;

    /* Push source file then initialize I/O (advance() needs tbtop) */
    pushfile(sourcefile);
    ioinit();

    /* Emit initial line directive for source file */
    emitFileStart(sourcefile);

    /* Prime the lexer - two calls needed to fill cur and next */
    gettoken();
    gettoken();

    /* Lex and emit all tokens */
    while (cur.type != E_O_F) {
        emitCurToken();
        gettoken();
    }

    /* Emit EOF token */
    emitToken(E_O_F);
}

char lexFile[128];
char ppFile[128];

int
main(int argc, char **argv)
{
    char *source = NULL;
    char *outbase = NULL;
    int i;
    int ppOnly = 0;

    /* Parse arguments */
    for (i = 1; i < argc; i++) {
        if (strcmp(argv[i], "-o") == 0) {
            if (++i >= argc) usage();
            outbase = argv[i];
        } else if (argv[i][0] == '-' && argv[i][1] == 'I') {
            /* Add to include path */
            if (numIncludes < MAX_INCLUDES)
                includePaths[numIncludes++] = argv[i] + 2;
        } else if (argv[i][0] == '-' && argv[i][1] == 'i') {
            /* System include path */
            sysIncPath = argv[i] + 2;
        } else if (argv[i][0] == '-' && argv[i][1] == 'D') {
            /* Define macro */
            addDefine(argv[i] + 2);
        } else if (strcmp(argv[i], "-E") == 0) {
            ppOnly = 1;
        } else if (strcmp(argv[i], "-N") == 0) {
            noLineMarkers = 1;
        } else if (strcmp(argv[i], "-h") == 0) {
            usage();
#ifdef DEBUG
        } else if (strcmp(argv[i], "-v") == 0) {
            if (++i >= argc) usage();
            verbose = strtol(argv[i], 0, 0);
#endif
        } else if (argv[i][0] == '-') {
            char buf[64];
            fmtstr(buf, "Unknown option: %s\n", argv[i]);
            errout(buf);
            usage();
        } else {
            source = argv[i];
        }
    }

    if (!source) {
        errout("No source file specified\n");
        usage();
    }

    /* Derive output base from source if not specified */
    if (!outbase) {
        char *dot;
        outbase = strdup(source);
        dot = strrchr(outbase, '.');
        if (dot) *dot = '\0';
    }

    /* Create output file names */
    fmtstr(lexFile, "%s.x", outbase);
    fmtstr(ppFile, "%s.i", outbase);

#ifdef DEBUG
#ifndef CCC
    if (verbose) {
        int j = 0;

        for (i = 0; i < 32; i++) {
            if (!vopts[i])
                break;
            if (verbose & (1 << i))
                j |= (1 << i);
        }

        fdprintf(2, "verbose: %x (", j);
        for (i = 0; vopts[i]; i++) {
            if (j & (1 << i)) {
                fdprintf(2, "%s", vopts[i]);
                j ^= (1 << i);
                if (j) {
                    fdprintf(2, " ");
                }
            }
        }
        fdprintf(2, ")\n");
    }
#endif
#endif

    /* Open output files */
    lexFd = creat(lexFile, 0644);
    if (lexFd < 0) {
        char buf[140];
        fmtstr(buf, "cannot create: %s\n", lexFile);
        errout(buf);
        exit(1);
    }

    ppFd = creat(ppFile, 0644);
    if (ppFd < 0) {
        char buf[140];
        fmtstr(buf, "cannot create: %s\n", ppFile);
        errout(buf);
        exit(1);
    }

    /* Add include paths - current directory first, then -I paths */
    addInclude("");  /* Current directory */
    for (i = 0; i < numIncludes; i++) {
        addInclude(includePaths[i]);
    }

    /* Initialize token filter */
    filterInit();

    /* Process the source file */
    (void)ppOnly;  /* TODO: implement -E mode */
    process(source);

    close(lexFd);
    close(ppFd);

    return exitCode;
}

/* vim: set tabstop=4 shiftwidth=4 noexpandtab: */
