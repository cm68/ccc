/*
 * cpp - C Preprocessor
 *
 * Main driver for the preprocessor.
 * Produces <basename>.x - lexeme stream (compact token format)
 *
 * Uses lex.c for tokenization, io.c for file handling,
 * and macro.c for macro processing.
 */
#include "cpp.h"
#include <fcntl.h>
#include <unistd.h>
#ifndef CCC
#include <sys/wait.h>
#endif

#ifdef DEBUG
#include "debugtags.c"
#endif

/* Filter chain functions */
extern void filtknr_init(void (*up)(struct token *));
extern void filtknr(struct token *out);
extern void filtdecl_init(void (*up)(struct token *));
extern void filtdecl(struct token *out);
extern void filtbrace_init(void (*up)(struct token *));
extern void filtbrace(struct token *out);
extern void filtbraceChk(void);
extern void filtctrl_init(void (*up)(struct token *));
extern void filtctrl(struct token *out);
extern void filtctrl_check(void);
extern void typedefReset(void);

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

#ifdef CCC
/*
 * cpp does all io on raw file descriptors; without this stub, exit()
 * drags in the stdio flush machinery (fclose/fflush/buf + __sibuf),
 * about 1KB of text and bss we never use.
 */
void
_cleanup(void)
{
}
#endif

/*
 * Error reporting
 */
static void
errout(char *buf)
{
    write(2, buf, strlen(buf));
}

static int
opcreat(char *file)
{
    int fd = creat(file, 0644);
    if (fd < 0) {
        char buf[140];
        fmtstr(buf, "cannot create: %s\n", file);
        errout(buf);
        exit(1);
    }
    return fd;
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
usage(void)
{
    errout("usage: cpp [options] <source.c>\n");
    errout("  -o <base>      Output base name (.x file)\n");
    errout("  -I<dir>        Add include directory\n");
    errout("  -i<dir>        System include directory\n");
    errout("  -D<name>[=val] Define macro\n");
    errout("  -E             Preprocess and dump to stdout (runs xdump)\n");
    errout("  -p             Also generate .i file (runs xdump)\n");
    errout("  -N             Suppress line markers\n");
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
 * Lexer wrapper for pull-based filter chain
 * Copies current token to output and advances lexer
 */
static void
lex_get(struct token *out)
{
    tokcpy(out, &cur);
#ifdef DEBUG
    if (VERBOSE(V_FILTER))
        fdprintf(2, "lex_get: type=%d\n", out->type);
#endif
    gettoken();
}

/*
 * Initialize the filter pipeline
 */
void
filterInit(void)
{
    typedefReset();
    filtknr_init(lex_get);
    filtdecl_init(filtknr);
    filtbrace_init(filtdecl);
    filtctrl_init(filtbrace);
}

/*
 * Process source file - lex all tokens and emit to .x stream
 */
void
process(char *sourcefile)
{
    struct token t;

    curFile = sourcefile;

    /* Push source file then initialize I/O (advance() needs tbtop) */
    pushfile(sourcefile);
    ioinit();

    /* Emit initial line directive for source file */
    emitFileStart(sourcefile);

    /* Prime the lexer - two calls needed to fill cur and next */
    gettoken();
    gettoken();

    /* Pull tokens through filter chain and emit */
    filtctrl(&t);
#ifdef DEBUG
    if (VERBOSE(V_FILTER))
        fdprintf(2, "process: first=%d\n", t.type);
#endif
    while (t.type != E_O_F) {
#ifdef DEBUG
        if (VERBOSE(V_FILTER))
            fdprintf(2, "process: emit=%d\n", t.type);
#endif
        emitStructTok(&t);
        filtctrl(&t);
    }

    /* Check brace balance and emit EOF token */
    filtbraceChk();
    filtctrl_check();
    emitChkBraces();
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
    int ppOutput = 0;

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
        } else if (strcmp(argv[i], "-p") == 0) {
            ppOutput = 1;
        } else if (strcmp(argv[i], "-N") == 0) {
            noLineMarkers = 1;
        } else if (strcmp(argv[i], "-h") == 0) {
            usage();
        } else if (strcmp(argv[i], "-v") == 0) {
            if (++i >= argc) usage();
#ifdef DEBUG
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
        outbase = permdup(source);
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

    /* Open output file */
    lexFd = opcreat(lexFile);

    /* Add include paths - current directory first, then -I paths */
    addInclude("");  /* Current directory */
    for (i = 0; i < numIncludes; i++) {
        addInclude(includePaths[i]);
    }

    /* Initialize token filter */
    filterInit();

    /* Process the source file */
    process(source);

    close(lexFd);

    /* -p mode: fork xdump to generate .i file */
    if (ppOutput) {
        int pid = fork();
        if (pid == 0) {
            /* Child: exec xdump */
#ifdef CCC
            if (noLineMarkers)
                execl("/bin/xdump", "xdump", "-N", "-o", ppFile, lexFile, (char *)0);
            else
                execl("/bin/xdump", "xdump", "-o", ppFile, lexFile, (char *)0);
            perror("xdump");
            exit(1);
#else
            if (noLineMarkers)
                execlp("xdump", "xdump", "-N", "-o", ppFile, lexFile, (char *)0);
            else
                execlp("xdump", "xdump", "-o", ppFile, lexFile, (char *)0);
            perror("xdump");
            _exit(1);
#endif
        } else if (pid > 0) {
            /* Parent: wait for xdump */
            int status;
#ifdef CCC
            wait(&status);
            if (status != 0)
                exitCode = 1;
#else
            waitpid(pid, &status, 0);
            if (!WIFEXITED(status) || WEXITSTATUS(status) != 0)
                exitCode = 1;
#endif
        } else {
            perror("fork");
            exitCode = 1;
        }
    }

    /* -E mode: exec xdump to dump preprocessed output to stdout */
    if (ppOnly) {
#ifdef CCC
        if (noLineMarkers)
            execl("/bin/xdump", "xdump", "-N", lexFile, (char *)0);
        else
            execl("/bin/xdump", "xdump", lexFile, (char *)0);
#else
        if (noLineMarkers)
            execlp("xdump", "xdump", "-N", lexFile, (char *)0);
        else
            execlp("xdump", "xdump", lexFile, (char *)0);
#endif
        perror("xdump");
        return 1;
    }

    return exitCode;
}

/* vim: set tabstop=4 shiftwidth=4 noexpandtab: */
