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
    fdprintf(2, "usage: %s [<options>] <ast_file>\n", progname);
    fdprintf(2, "  -o <output>    Output file (default: a.out)\n");
    exit(1);
}

int
main(int argc, char **argv)
{
    char *ast_file = NULL;
    char *output_file = "a.out";
    int in_fd;

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

    if (!ast_file) {
        usage("no AST input file specified");
    }

    fdprintf(1, "cc2: Reading AST from %s\n", ast_file);

    /* Open and read AST file */
    in_fd = open(ast_file, O_RDONLY);
    if (in_fd < 0) {
        fdprintf(2, "cc2: cannot open %s\n", ast_file);
        exit(1);
    }

    /* Parse AST file */
    fdprintf(1, "cc2: Parsing AST...\n");
    if (parse_ast_file(in_fd) != 0) {
        fdprintf(2, "cc2: failed to parse AST\n");
        close(in_fd);
        exit(1);
    }
    close(in_fd);

    fdprintf(1, "\ncc2: Parse complete\n");
    fdprintf(1, "cc2: Code generation stub - would write to: %s\n", output_file);

    /* TODO: Code generation will be implemented here */

    return 0;
}
