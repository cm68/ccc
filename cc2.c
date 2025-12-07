/*
 * cc2 - Second pass of ccc compiler
 * Code generation for Z80 target
 *
 * Z80 Register Allocation Strategy:
 *
 * Frame Pointer:
 *   IY - frame pointer for local variables and parameters
 *
 * Register Variables (available for allocation):
 *   Byte registers:  B, C
 *   Word registers:  BC, IX
 *   Note: IX preferred for structure pointers (indexed addressing: (IX+offset))
 *
 * Primary Accumulator (expression evaluation):
 *   Byte:  A
 *   Word:  HL
 *   Long:  HL:HL' (alternate set for upper word)
 *
 * Secondary Accumulator (expression evaluation):
 *   Byte:  E
 *   Word:  DE
 *
 * Register variables: B/C (byte), BC/IX (word)
 *
 * Register Allocation Constraints:
 *   - Variables whose address is taken (&var) cannot be allocated to
 *     registers
 *   - These variables must reside in memory (stack frame or global data
 *     section)
 *   - Includes variables passed by reference to functions
 *
 * Register Allocation Process:
 *   1. During parsing, track for each local variable:
 *      - Address-taken flag (disqualifies from register allocation)
 *      - Static reference count (prioritize frequently-used variables)
 *      - Approximate lifetime (scope where variable is live)
 *   2. After function parse completes, select register candidates:
 *      - Exclude address-taken variables
 *      - Prioritize by reference count
 *      - Allocate to available register set based on type (byte/word)
 *      - Prefer IX for structure pointers (efficient indexed addressing)
 */
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>
#include <signal.h>

#define	MAXTIME	30  /* timeout for debugging */

/* Forward declaration from util.c */
int fdprintf(int fd, const char *fmt, ...);

/* Empty string - used in xfree to avoid freeing static strings */
char noasm[] = "";

void xfree(void *p) { if (p && p != noasm) free(p); }

/* Forward declaration from parseast.c */
int parseAstFile(int inFd, int outFd);

/* Trace flag for debugging (like verbose in cc1) */
#ifdef DEBUG
int trace = 0;
#endif

char *progname;

/*
 * timeout handler - catch infinite loops during code generation
 */
void
timeoutHdlr(int sig)
{
    fdprintf(2, "cc2: timeout\n");
    exit(1);
}

void
usage(char *complaint)
{
    if (complaint) fdprintf(2, "%s\n", complaint);
    fdprintf(2, "usage: %s [-o out] [ast]\n", progname);
    exit(1);
}

/*
 * Generate output filename from input filename
 * Strips .ast extension and appends .s (preserves full path)
 * Example: /tmp/foo.c.ast -> /tmp/foo.c.s
 */
char *
makeOutName(char *input_file)
{
    char *output;
    int len;

    len = strlen(input_file);

    /* Check if file ends with ".ast" */
    if (len >= 4 && strcmp(input_file + len - 4, ".ast") == 0) {
        /* Strip .ast and append .s */
        /* -4 for .ast, +2 for .s, +1 for null */
        output = malloc(len - 4 + 2 + 1);
        strncpy(output, input_file, len - 4);
        output[len - 4] = '\0';
        strcat(output, ".s");
    } else {
        /* Just append .s */
        output = malloc(len + 2 + 1);  /* +2 for .s, +1 for null */
        strcpy(output, input_file);
        strcat(output, ".s");
    }

    return output;
}

int
main(int argc, char **argv)
{
    char *ast_file = NULL;
    char *output_file = NULL;
    int inFd;
    int outFd;
    int explicit_out = 0;

    progname = argv[0];
    argc--;
    argv++;

    /* Set up timeout handler to catch infinite loops */
    signal(SIGALRM, timeoutHdlr);
    alarm(MAXTIME);

    /* Parse arguments */
    while (argc > 0) {
        if (strcmp(argv[0], "-o") == 0) {
            argc--;
            argv++;
            if (argc == 0) {
                usage("output file not specified");
            }
            output_file = argv[0];
            explicit_out = 1;
            argc--;
            argv++;
        }
#ifdef DEBUG
        else if (strcmp(argv[0], "-v") == 0) {
            argc--;
            argv++;
            if (argc == 0) {
                usage("trace level not specified");
            }
            trace = strtol(argv[0], 0, 0);
            argc--;
            argv++;
        }
#endif
        else if (strcmp(argv[0], "-h") == 0 ||
                   strcmp(argv[0], "--help") == 0) {
            usage(NULL);
        } else if (argv[0][0] == '-') {
            fdprintf(2, "Unknown option: %s\n", argv[0]);
            usage(NULL);
        } else {
            /* AST input file */
            ast_file = argv[0];
            argc--;
            argv++;
        }
    }

    /* Determine output file name */
    if (!explicit_out) {
        if (ast_file) {
            /* Input from file: default to <basename>.s */
            output_file = makeOutName(ast_file);
        } else {
            /* Input from stdin: output to stdout (filter mode) */
            output_file = NULL;
        }
    }

    /* Open input: file or stdin */
    if (ast_file) {
        inFd = open(ast_file, O_RDONLY);
        if (inFd < 0) {
            fdprintf(2, "cc2: cannot open %s\n", ast_file);
            exit(1);
        }
    } else {
        inFd = 0;  /* stdin */
    }

    /* Open output: file or stdout */
    if (output_file) {
        outFd = open(output_file, O_WRONLY | O_CREAT | O_TRUNC, 0644);
        if (outFd < 0) {
            fdprintf(2, "cc2: cannot create %s\n", output_file);
            if (inFd != 0) close(inFd);
            exit(1);
        }
    } else {
        outFd = 1;  /* stdout */
    }

    /* Parse AST file and generate code */
    if (parseAstFile(inFd, outFd) != 0) {
        fdprintf(2, "cc2: parse failed\n");
        if (inFd != 0) close(inFd);
        if (outFd != 1) close(outFd);
        exit(1);
    }

    /* Close files */
    if (inFd != 0) close(inFd);
    if (outFd != 1) close(outFd);

    return 0;
}
