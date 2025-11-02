/*
 * ccc - Two-pass C compiler driver
 *
 * Orchestrates cc1 (parser/AST generator) and cc2 (code generator)
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/wait.h>
#include <libgen.h>

#define MAX_ARGS 256

char *progname;
char *scriptdir;

void
usage(void)
{
    printf("usage: %s [<options>] <source.c>\n", progname);
    printf("  -o <output>    Output file (default: a.out)\n");
    printf("  -keep-ast      Keep intermediate AST file\n");
    printf("  -v <level>     Verbosity level (passed to cc1)\n");
    printf("  -I<dir>        Include directory (passed to cc1)\n");
    printf("  -D<var>[=val]  Define macro (passed to cc1)\n");
    printf("  -E             Preprocess only (passed to cc1)\n");
    exit(1);
}

/*
 * Get the directory where this executable lives
 */
char *
get_script_dir(char *argv0)
{
    char *path;
    char *dir;
    char resolved[1024];

    /* Try to resolve the full path */
    if (realpath(argv0, resolved)) {
        path = strdup(resolved);
    } else {
        path = strdup(argv0);
    }

    dir = dirname(path);
    return strdup(dir);
}

/*
 * Create a temporary AST filename
 * Returns allocated string that must be freed
 */
char *
make_temp_ast(char *basename)
{
    char template[256];
    int fd;
    char *result;

    snprintf(template, sizeof(template), "%s.ast.XXXXXX", basename);
    fd = mkstemp(template);
    if (fd < 0) {
        perror("mkstemp");
        exit(1);
    }
    close(fd);  /* We just need the name */
    result = strdup(template);
    return result;
}

/*
 * Get basename without .c extension
 * Returns a newly allocated string
 */
char *
get_basename_no_ext(char *filename)
{
    char *temp = strdup(filename);
    char *base = basename(temp);
    char *result;
    char *dot;

    /* Make a copy since basename() result points into temp */
    result = strdup(base);
    free(temp);

    /* Remove .c extension if present */
    dot = strrchr(result, '.');
    if (dot && strcmp(dot, ".c") == 0) {
        *dot = '\0';
    }

    return result;
}

/*
 * Execute a command with arguments
 * Returns exit status of child process
 */
int
exec_command(char *cmd, char **args)
{
    pid_t pid;
    int status;

    pid = fork();
    if (pid < 0) {
        perror("fork");
        exit(1);
    }

    if (pid == 0) {
        /* Child process */
        execv(cmd, args);
        /* If execv returns, it failed */
        perror(cmd);
        exit(1);
    }

    /* Parent process - wait for child */
    if (waitpid(pid, &status, 0) < 0) {
        perror("waitpid");
        exit(1);
    }

    if (WIFEXITED(status)) {
        return WEXITSTATUS(status);
    } else {
        return 1;  /* Abnormal termination */
    }
}

int
main(int argc, char **argv)
{
    char *source_file = NULL;
    char *output_file = NULL;
    char *ast_file = NULL;
    int keep_ast = 0;

    char *cc1_args[MAX_ARGS];
    char *cc2_args[MAX_ARGS];
    int cc1_argc = 0;
    int cc2_argc = 0;

    char cc1_path[1024];
    char cc2_path[1024];

    char *basename_no_ext;
    int status;

    progname = argv[0];
    scriptdir = get_script_dir(argv[0]);

    /* Build paths to cc1 and cc2 */
    snprintf(cc1_path, sizeof(cc1_path), "%s/cc1", scriptdir);
    snprintf(cc2_path, sizeof(cc2_path), "%s/cc2", scriptdir);

    /* Initialize argument arrays with program names */
    cc1_args[cc1_argc++] = cc1_path;
    cc2_args[cc2_argc++] = cc2_path;

    /* Parse arguments */
    argc--;
    argv++;

    while (argc > 0) {
        if (strcmp(argv[0], "-h") == 0 || strcmp(argv[0], "--help") == 0) {
            usage();
        } else if (strcmp(argv[0], "-o") == 0) {
            argc--;
            argv++;
            if (argc == 0) {
                fprintf(stderr, "Error: -o requires an argument\n");
                usage();
            }
            output_file = argv[0];
            argc--;
            argv++;
        } else if (strcmp(argv[0], "-keep-ast") == 0) {
            keep_ast = 1;
            argc--;
            argv++;
        } else if (strcmp(argv[0], "-v") == 0) {
            /* Pass -v and its argument to cc1 */
            if (cc1_argc + 2 >= MAX_ARGS) {
                fprintf(stderr, "Error: too many arguments\n");
                exit(1);
            }
            cc1_args[cc1_argc++] = "-v";
            argc--;
            argv++;
            if (argc == 0) {
                fprintf(stderr, "Error: -v requires an argument\n");
                usage();
            }
            cc1_args[cc1_argc++] = argv[0];
            argc--;
            argv++;
        } else if (argv[0][0] == '-' && (argv[0][1] == 'I' || argv[0][1] == 'D')) {
            /* Pass -I or -D options to cc1 */
            if (cc1_argc >= MAX_ARGS) {
                fprintf(stderr, "Error: too many arguments\n");
                exit(1);
            }
            cc1_args[cc1_argc++] = argv[0];
            argc--;
            argv++;
        } else if (strcmp(argv[0], "-E") == 0) {
            /* Pass -E to cc1 */
            if (cc1_argc >= MAX_ARGS) {
                fprintf(stderr, "Error: too many arguments\n");
                exit(1);
            }
            cc1_args[cc1_argc++] = argv[0];
            argc--;
            argv++;
        } else if (argv[0][0] == '-') {
            fprintf(stderr, "Error: unknown option: %s\n", argv[0]);
            usage();
        } else {
            /* Source file */
            if (source_file) {
                fprintf(stderr, "Error: multiple source files not supported yet\n");
                exit(1);
            }
            source_file = argv[0];
            argc--;
            argv++;
        }
    }

    /* Check for source file */
    if (!source_file) {
        fprintf(stderr, "Error: no source file specified\n");
        usage();
    }

    /* Check if source file exists */
    if (access(source_file, R_OK) != 0) {
        fprintf(stderr, "Error: source file '%s' not found or not readable\n", source_file);
        exit(1);
    }

    /* Set default output file */
    if (!output_file) {
        output_file = "a.out";
    }

    /* Generate AST filename */
    basename_no_ext = get_basename_no_ext(source_file);
    if (keep_ast) {
        ast_file = malloc(strlen(basename_no_ext) + 10);
        sprintf(ast_file, "%s.ast", basename_no_ext);
    } else {
        ast_file = make_temp_ast(basename_no_ext);
    }

    printf("=== Pass 1: Parsing %s ===\n", source_file);

    /* Build cc1 argument list (cc1_path already at index 0) */
    cc1_args[cc1_argc++] = "-o";
    cc1_args[cc1_argc++] = ast_file;
    cc1_args[cc1_argc++] = source_file;
    cc1_args[cc1_argc] = NULL;

    /* Execute cc1 */
    status = exec_command(cc1_path, cc1_args);
    if (status != 0) {
        fprintf(stderr, "Error: cc1 failed with status %d\n", status);
        if (!keep_ast) {
            unlink(ast_file);
        }
        exit(status);
    }

    printf("\n=== Pass 2: Generating code from %s ===\n", ast_file);

    /* Build cc2 argument list (cc2_path already at index 0) */
    cc2_args[cc2_argc++] = "-o";
    cc2_args[cc2_argc++] = output_file;
    cc2_args[cc2_argc++] = ast_file;
    cc2_args[cc2_argc] = NULL;

    /* Execute cc2 */
    status = exec_command(cc2_path, cc2_args);
    if (status != 0) {
        fprintf(stderr, "Error: cc2 failed with status %d\n", status);
        if (!keep_ast) {
            unlink(ast_file);
        }
        exit(status);
    }

    /* Clean up temporary AST file if not keeping */
    if (!keep_ast) {
        unlink(ast_file);
    } else {
        printf("AST saved to: %s\n", ast_file);
    }

    printf("\n=== Compilation successful: %s ===\n", output_file);

    free(ast_file);
    free(basename_no_ext);
    free(scriptdir);

    return 0;
}
