/*
 * cc2 - Second pass of ccc compiler
 * Code generation stub
 */
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>

/* Forward declaration from util.c */
int fdprintf(int fd, const char *fmt, ...);

/* Forward declaration from parseast.c */
int parse_ast_file(int fd);

char *progname;

void
usage(char *complaint)
{
    if (complaint) {
        fdprintf(2, "%s\n", complaint);
    }
    fdprintf(2, "usage: %s [<options>] [<ast_file>]\n", progname);
    fdprintf(2, "  -o <output>    Output file (default: <input>.s or stdout for stdin)\n");
    fdprintf(2, "  <ast_file>     Input AST file (default: stdin, for filter mode)\n");
    exit(1);
}

/*
 * Generate output filename from input filename
 * Changes .ast extension to .s, or appends .s if no extension
 */
char *
make_output_name(char *input_file)
{
    char *basename_start;
    char *dot;
    char *output;
    int len;

    /* Find basename (skip directory path) */
    basename_start = strrchr(input_file, '/');
    if (basename_start) {
        basename_start++;  /* skip the slash */
    } else {
        basename_start = input_file;
    }

    /* Find extension */
    dot = strrchr(basename_start, '.');
    if (dot) {
        len = dot - basename_start;
    } else {
        len = strlen(basename_start);
    }

    /* Allocate space for basename + ".s" + null */
    output = malloc(len + 3);
    strncpy(output, basename_start, len);
    output[len] = '\0';
    strcat(output, ".s");

    return output;
}

int
main(int argc, char **argv)
{
    char *ast_file = NULL;
    char *output_file = NULL;
    int in_fd;
    int explicit_output = 0;

    progname = argv[0];
    argc--;
    argv++;

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
        } else if (strcmp(argv[0], "-h") == 0 || strcmp(argv[0], "--help") == 0) {
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

    /* Parse AST file */
    fdprintf(2, "cc2: Parsing AST...\n");
    if (parse_ast_file(in_fd) != 0) {
        fdprintf(2, "cc2: failed to parse AST\n");
        if (in_fd != 0) close(in_fd);  /* Don't close stdin */
        exit(1);
    }
    if (in_fd != 0) close(in_fd);  /* Don't close stdin */

    fdprintf(2, "\ncc2: Parse complete\n");
    if (output_file) {
        fdprintf(2, "cc2: Code generation stub - would write to: %s\n", output_file);
    } else {
        fdprintf(2, "cc2: Code generation stub - would write to stdout\n");
    }

    /* TODO: Code generation will be implemented here */

    return 0;
}
