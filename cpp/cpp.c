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

/* Global state */
char *curFile;
int lineNo;
int exitCode = 0;

/* Include path list */
#define MAX_INCLUDES 32
char *includePaths[MAX_INCLUDES];
int numIncludes = 0;

/*
 * Error reporting
 */
void
error(char *msg)
{
    fprintf(stderr, "%s:%d: error: %s\n", filename ? filename : curFile, lineno, msg);
    exitCode = 1;
}

void
fatal(char *msg)
{
    fprintf(stderr, "%s:%d: fatal: %s\n", filename ? filename : curFile, lineno, msg);
    exit(1);
}

void
usage(void)
{
    fprintf(stderr, "usage: cpp [options] <source.c>\n");
    fprintf(stderr, "  -o <base>      Output base name (produces <base>.x and <base>.i)\n");
    fprintf(stderr, "  -I<dir>        Add include directory\n");
    fprintf(stderr, "  -i<dir>        System include directory\n");
    fprintf(stderr, "  -D<name>[=val] Define macro\n");
    fprintf(stderr, "  -E             Preprocess only (output to stdout)\n");
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

int
main(int argc, char **argv)
{
    char *source = NULL;
    char *outbase = NULL;
    char lexFile[256];
    char ppFile[256];
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
        } else if (argv[i][0] == '-') {
            fprintf(stderr, "Unknown option: %s\n", argv[i]);
            usage();
        } else {
            source = argv[i];
        }
    }

    if (!source) {
        fprintf(stderr, "No source file specified\n");
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
    sprintf(lexFile, "%s.x", outbase);
    sprintf(ppFile, "%s.i", outbase);

    /* Open output files */
    lexFd = open(lexFile, O_WRONLY | O_CREAT | O_TRUNC, 0644);
    if (lexFd < 0) {
        perror(lexFile);
        exit(1);
    }

    ppFd = open(ppFile, O_WRONLY | O_CREAT | O_TRUNC, 0644);
    if (ppFd < 0) {
        perror(ppFile);
        exit(1);
    }

    /* Add include paths - current directory first, then -I paths */
    addInclude("");  /* Current directory */
    for (i = 0; i < numIncludes; i++) {
        addInclude(includePaths[i]);
    }

    /* Process the source file */
    (void)ppOnly;  /* TODO: implement -E mode */
    process(source);

    close(lexFd);
    close(ppFd);

    return exitCode;
}
