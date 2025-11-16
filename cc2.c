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
 *   Byte registers:  B, C, B', C' (4 byte registers)
 *   Word registers:  BC, IX (2 word registers)
 *   Note: IX preferred for structure pointers (indexed addressing: (IX+offset))
 *   Note: BC' excluded from allocation due to exx instruction complexity
 *
 * Primary Accumulator (expression evaluation):
 *   Byte:  A
 *   Word:  HL
 *   Long:  HL' (extended to alternate register set)
 *
 * Secondary Accumulator (expression evaluation):
 *   Byte:  E
 *   Word:  DE
 *   Long:  DE' (extended to alternate register set)
 *
 * Total available:
 *   6 byte registers (4 register variables + A primary + E secondary)
 *   5 word registers (3 register variables + HL primary + DE secondary)
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

#define	MAXTIME	30  /* 30 second timeout for debugging */

/* Forward declaration from util.c */
int fdprintf(int fd, const char *fmt, ...);

/* Forward declaration from parseast.c */
int parse_ast_file(int in_fd, int out_fd);

char *progname;

/*
 * timeout handler - catch infinite loops during code generation
 */
void
timeout_handler(int sig)
{
    fdprintf(2, "\n\n*** TIMEOUT after %d seconds ***\n", MAXTIME);
    fdprintf(2,
        "cc2: code generation took too long (possible infinite loop)\n");
    exit(1);
}

void
usage(char *complaint)
{
    if (complaint) {
        fdprintf(2, "%s\n", complaint);
    }
    fdprintf(2, "usage: %s [<options>] [<ast_file>]\n", progname);
    fdprintf(2,
        "  -o <output>    Output file (default: <input>.s or stdout "
        "for stdin)\n");
    fdprintf(2,
        "  <ast_file>     Input AST file (default: stdin, for filter "
        "mode)\n");
    exit(1);
}

/*
 * Generate output filename from input filename
 * Strips .ast extension and appends .s (preserves full path)
 * Example: /tmp/foo.c.ast -> /tmp/foo.c.s
 */
char *
make_output_name(char *input_file)
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
    int in_fd;
    int out_fd;
    int explicit_output = 0;

    progname = argv[0];
    argc--;
    argv++;

    /* Set up timeout handler to catch infinite loops */
    signal(SIGALRM, timeout_handler);
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
            explicit_output = 1;
            argc--;
            argv++;
        } else if (strcmp(argv[0], "-h") == 0 ||
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
    if (!explicit_output) {
        if (ast_file) {
            /* Input from file: default to <basename>.s */
            output_file = make_output_name(ast_file);
        } else {
            /* Input from stdin: output to stdout (filter mode) */
            output_file = NULL;
        }
    }

    /* Open input: file or stdin */
    if (ast_file) {
        fdprintf(2, "cc2: Reading AST from %s\n", ast_file);
        in_fd = open(ast_file, O_RDONLY);
        if (in_fd < 0) {
            fdprintf(2, "cc2: cannot open %s\n", ast_file);
            exit(1);
        }
    } else {
        fdprintf(2, "cc2: Reading AST from stdin\n");
        in_fd = 0;  /* stdin */
    }

    /* Open output: file or stdout */
    if (output_file) {
        fdprintf(2, "cc2: Writing assembly to %s\n", output_file);
        out_fd = open(output_file, O_WRONLY | O_CREAT | O_TRUNC, 0644);
        if (out_fd < 0) {
            fdprintf(2, "cc2: cannot create %s\n", output_file);
            if (in_fd != 0) close(in_fd);
            exit(1);
        }
    } else {
        fdprintf(2, "cc2: Writing assembly to stdout\n");
        out_fd = 1;  /* stdout */
    }

    /* Parse AST file and generate code */
    fdprintf(2, "cc2: Parsing AST and generating code...\n");
    if (parse_ast_file(in_fd, out_fd) != 0) {
        fdprintf(2, "cc2: failed to parse AST\n");
        if (in_fd != 0) close(in_fd);
        if (out_fd != 1) close(out_fd);
        exit(1);
    }

    /* Close files */
    if (in_fd != 0) close(in_fd);
    if (out_fd != 1) close(out_fd);

    fdprintf(2, "\ncc2: Generation complete\n");

    return 0;
}
