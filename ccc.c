/*
 * ccc - Two-pass C compiler driver
 *
 * Orchestrates cpp, c0 (parser), and c1 (code generator)
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/wait.h>
#include <libgen.h>

#define MAX_ARGS 2560  // command-line arguments (original: 256, tested: 2560)

char *progname;

#define stringify2(s) #s
#define stringify(s) stringify2(s)

char *rootdir = stringify(ROOTDIR);

void
usage(void)
{
    printf("usage: %s [<options>] <files...>\n", progname);
    printf("  files: .c (compile) .s (assemble) .o .a (link)\n");
    printf("  -o <output>    Output file (default: a.out)\n");
    printf("  -c             Compile and assemble only, keep .o\n");
    printf("  -s             Compile only, keep .s (no assembly)\n");
    printf("  -k             Keep all intermediates (.x, .1, .2, .s, .o)\n");
    printf("  -S             Strip symbols from output\n");
    printf("  -9             Use 9-char symbols in output\n");
    printf("  -v <level>     Verbosity level (passed to cc1)\n");
    printf("  -V <level>     Verbosity level (passed to cc2)\n");
    printf("  -I<dir>        Include directory (passed to cc1)\n");
    printf("  -i<dir>        System include directory (passed to cc1, "
        "default /usr/include)\n");
    printf("  -D<var>[=val]  Define macro (passed to cc1)\n");
    printf("  -E             Preprocess only (not yet implemented)\n");
    printf("  -P             Run pretty printer after parse (produces .pp)\n");
    printf("  -x             Print commands as they execute\n");
    printf("  -n             Print commands without executing (dry run)\n");
    exit(1);
}

/*
 * Get the directory above where this executable lives
 */
char *
getroot(char *argv0)
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

    dir = dirname(dirname(path));
    return strdup(dir);
}

/*
 * Get basename without extension (.c, .s, .o, .a)
 * Returns a newly allocated string
 */
char *
getBaseNoExt(char *filename)
{
    char *temp = strdup(filename);
    char *base = basename(temp);
    char *result;
    char *dot;

    /* Make a copy since basename() result points into temp */
    result = strdup(base);
    free(temp);

    /* Remove known extensions */
    dot = strrchr(result, '.');
    if (dot && (strcmp(dot, ".c") == 0 || strcmp(dot, ".s") == 0 ||
                strcmp(dot, ".o") == 0 || strcmp(dot, ".a") == 0)) {
        *dot = '\0';
    }

    return result;
}

/*
 * Print a command line
 */
void
printCommand(char **args)
{
    int i;
    for (i = 0; args[i]; i++) {
        if (i > 0) printf(" ");
        printf("%s", args[i]);
    }
    printf("\n");
}

/*
 * Execute a command with arguments
 * Returns exit status of child process
 */
int
execCommand(char *cmd, char **args)
{
    int pid;
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
    char *output_file = NULL;
    int keep_all = 0;        /* -k: keep all intermediates */
    int compile_only = 0;    /* -c: compile+assemble to .o */
    int asm_only = 0;        /* -s: compile to .s only */
    int print_cmds = 0;      /* -x: print commands as they execute */
    int no_exec = 0;         /* -n: don't execute (dry run) */
    int strip_syms = 0;      /* -S: strip symbols from output */
    int nine_char = 0;       /* -9: use 9-char symbols */
    int pretty_print = 0;    /* -P: run ppic after parse */

    /* Input files by type */
    char *c_files[MAX_ARGS];
    char *s_files[MAX_ARGS];
    char *o_files[MAX_ARGS];
    char *a_files[MAX_ARGS];
    int c_count = 0, s_count = 0, o_count = 0, a_count = 0;
    int o_input_count = 0;   /* .o files from cmdline (vs generated) */

    char *cpp_base[MAX_ARGS];  /* Base cpp args (options only) */
    char *cc1_base[MAX_ARGS];  /* Base cc1 args (options only) */
    char *cc2_base[MAX_ARGS];  /* Base cc2 args (options only) */
    int cpp_base_argc = 0;
    int cc1_base_argc = 0;
    int cc2_base_argc = 0;

    char cpp_path[1024];
    char cc1_path[1024];
    char cc2_path[1024];
    char asm_path[1024];
    char ld_path[1024];
    char astpp_path[1024];
    char ppic_path[1024];

    char chdr_path[1024];
    char libc_path[1024];
    char libu_path[1024];
    char sysinc_path[1024];

    int status;
    int i;

    progname = argv[0];

    /* Build paths to cpp, pass1 (c0), pass2 (c1), assembler, linker */
    snprintf(cpp_path, sizeof(cpp_path), "%s/bin/cpp", rootdir);
    snprintf(cc1_path, sizeof(cc1_path), "%s/bin/c0", rootdir);
    snprintf(cc2_path, sizeof(cc2_path), "%s/bin/c1", rootdir);
    snprintf(asm_path, sizeof(asm_path), "%s/bin/asz", rootdir);
    snprintf(ld_path, sizeof(ld_path), "%s/bin/wsld", rootdir);
    snprintf(astpp_path, sizeof(astpp_path), "%s/bin/astpp", rootdir);
    snprintf(ppic_path, sizeof(ppic_path), "%s/bin/ppic", rootdir);

    snprintf(chdr_path, sizeof(chdr_path), "%s/lib/crt0.o", rootdir);
    snprintf(libc_path, sizeof(libc_path), "%s/lib/libc.a", rootdir);
    snprintf(libu_path, sizeof(libu_path), "%s/lib/libu.a", rootdir);
    snprintf(sysinc_path, sizeof(sysinc_path), "-i%s/usr/include", rootdir);

    /* Initialize base argument arrays with program names */
    cpp_base[cpp_base_argc++] = cpp_path;
    cc1_base[cc1_base_argc++] = cc1_path;
    cc2_base[cc2_base_argc++] = cc2_path;

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
        } else if (strcmp(argv[0], "-k") == 0) {
            keep_all = 1;
            argc--;
            argv++;
        } else if (strcmp(argv[0], "-c") == 0) {
            compile_only = 1;
            argc--;
            argv++;
        } else if (strcmp(argv[0], "-s") == 0) {
            asm_only = 1;
            argc--;
            argv++;
        } else if (strcmp(argv[0], "-S") == 0) {
            strip_syms = 1;
            argc--;
            argv++;
        } else if (strcmp(argv[0], "-9") == 0) {
            nine_char = 1;
            argc--;
            argv++;
        } else if (strcmp(argv[0], "-x") == 0) {
            print_cmds = 1;
            argc--;
            argv++;
        } else if (strcmp(argv[0], "-n") == 0) {
            no_exec = 1;
            argc--;
            argv++;
        } else if (strcmp(argv[0], "-v") == 0) {
            /* Pass -v and its argument to cc1 */
            if (cc1_base_argc + 2 >= MAX_ARGS) {
                fprintf(stderr, "Error: too many arguments\n");
                exit(1);
            }
            cc1_base[cc1_base_argc++] = "-v";
            argc--;
            argv++;
            if (argc == 0) {
                fprintf(stderr, "Error: -v requires an argument\n");
                usage();
            }
            cc1_base[cc1_base_argc++] = argv[0];
            argc--;
            argv++;
        } else if (strcmp(argv[0], "-V") == 0) {
            /* Pass -v and its argument to cc2 */
            if (cc2_base_argc + 2 >= MAX_ARGS) {
                fprintf(stderr, "Error: too many arguments\n");
                exit(1);
            }
            cc2_base[cc2_base_argc++] = "-v";
            argc--;
            argv++;
            if (argc == 0) {
                fprintf(stderr, "Error: -V requires an argument\n");
                usage();
            }
            cc2_base[cc2_base_argc++] = argv[0];
            argc--;
            argv++;
        } else if (argv[0][0] == '-' &&
                   (argv[0][1] == 'I' || argv[0][1] == 'i' ||
                    argv[0][1] == 'D')) {
            /* Pass -I, -i, or -D options to cpp */
            if (cpp_base_argc >= MAX_ARGS) {
                fprintf(stderr, "Error: too many arguments\n");
                exit(1);
            }
            cpp_base[cpp_base_argc++] = argv[0];
            argc--;
            argv++;
        } else if (strcmp(argv[0], "-E") == 0) {
            /* Pass -E to cpp (preprocess only) */
            if (cpp_base_argc >= MAX_ARGS) {
                fprintf(stderr, "Error: too many arguments\n");
                exit(1);
            }
            cpp_base[cpp_base_argc++] = argv[0];
            argc--;
            argv++;
        } else if (strcmp(argv[0], "-P") == 0) {
            pretty_print = 1;
            argc--;
            argv++;
        } else if (argv[0][0] == '-') {
            fprintf(stderr, "Error: unknown option: %s\n", argv[0]);
            usage();
        } else {
            /* Input file - classify by extension */
            char *ext = strrchr(argv[0], '.');
            if (access(argv[0], R_OK) != 0) {
                fprintf(stderr,
                    "Error: file '%s' not found or not readable\n",
                    argv[0]);
                exit(1);
            }
            if (ext && strcmp(ext, ".c") == 0) {
                c_files[c_count++] = argv[0];
            } else if (ext && strcmp(ext, ".s") == 0) {
                s_files[s_count++] = argv[0];
            } else if (ext && strcmp(ext, ".o") == 0) {
                o_files[o_count++] = argv[0];
            } else if (ext && strcmp(ext, ".a") == 0) {
                a_files[a_count++] = argv[0];
            } else {
                fprintf(stderr, "Error: unknown file type: %s\n", argv[0]);
                exit(1);
            }
            argc--;
            argv++;
        }
    }

    /* Check for input files */
    if (c_count + s_count + o_count + a_count == 0) {
        fprintf(stderr, "Error: no input files specified\n");
        usage();
    }

    /* Set default output file */
    if (!output_file) {
        output_file = "a.out";
    }

    /* Track how many .o files existed before we generate more from .c files */
    o_input_count = o_count;

    /* Process each .c file: cpp -> c0 -> c1 -> asm */
    for (i = 0; i < c_count; i++) {
        char *src = c_files[i];
        char *base = getBaseNoExt(src);
        char *lex_file;
        char *prep_file;
        char *temp1_file;
        char *temp2_file;
        char *asm_file;
        char *obj_file;
        char *cpp_args[MAX_ARGS];
        char *cc1_args[MAX_ARGS];
        char *cc2_args[MAX_ARGS];
        char *as_args[8];
        int cpp_argc, cc1_argc, cc2_argc, j;

        char *pp_file;

        /* Generate intermediate filenames */
        lex_file = malloc(strlen(base) + 10);
        sprintf(lex_file, "%s.x", base);
        prep_file = malloc(strlen(base) + 10);
        sprintf(prep_file, "%s.i", base);
        temp1_file = malloc(strlen(base) + 10);
        sprintf(temp1_file, "%s.1", base);
        temp2_file = malloc(strlen(base) + 10);
        sprintf(temp2_file, "%s.2", base);
        asm_file = malloc(strlen(base) + 10);
        sprintf(asm_file, "%s.s", base);
        obj_file = malloc(strlen(base) + 10);
        sprintf(obj_file, "%s.o", base);
        pp_file = malloc(strlen(base) + 10);
        sprintf(pp_file, "%s.pp", base);

        if (!no_exec) printf("=== Compiling %s ===\n", src);

        /* Build cpp args: base options + -DCCC + sysinc + -o base + source */
        cpp_argc = 0;
        for (j = 0; j < cpp_base_argc; j++)
            cpp_args[cpp_argc++] = cpp_base[j];
        cpp_args[cpp_argc++] = "-DCCC";
        cpp_args[cpp_argc++] = sysinc_path;
        cpp_args[cpp_argc++] = "-o";
        cpp_args[cpp_argc++] = base;
        cpp_args[cpp_argc++] = src;
        cpp_args[cpp_argc] = NULL;

        if (print_cmds || no_exec)
            printCommand(cpp_args);
        if (!no_exec) {
            status = execCommand(cpp_path, cpp_args);
            if (status != 0) {
                fprintf(stderr, "Error: cpp failed on %s\n", src);
                exit(status);
            }
        }

        /* Build pass1 args: c0 lex_file temp1 temp2 */
        cc1_argc = 0;
        for (j = 0; j < cc1_base_argc; j++)
            cc1_args[cc1_argc++] = cc1_base[j];
        cc1_args[cc1_argc++] = lex_file;
        cc1_args[cc1_argc++] = temp1_file;
        cc1_args[cc1_argc++] = temp2_file;
        cc1_args[cc1_argc] = NULL;

        if (print_cmds || no_exec)
            printCommand(cc1_args);
        if (!no_exec) {
            status = execCommand(cc1_path, cc1_args);
            if (status != 0) {
                fprintf(stderr, "Error: c0 failed on %s\n", src);
                exit(status);
            }
        }

        /* Clean up .x and .i if they exist */
        if (!keep_all && !no_exec) {
            unlink(lex_file);
            unlink(prep_file);
        }
        free(lex_file);
        free(prep_file);

        /* Run ppic if -P was specified */
        if (pretty_print) {
            char *ppic_args[4];
            ppic_args[0] = ppic_path;
            ppic_args[1] = base;
            ppic_args[2] = NULL;

            if (print_cmds || no_exec)
                printCommand(ppic_args);
            if (!no_exec) {
                status = execCommand(ppic_path, ppic_args);
                if (status != 0) {
                    fprintf(stderr, "Error: ppic failed on %s\n", src);
                    exit(status);
                }
            }
        }

        /* Build pass2 args: c1 temp1 temp2 asm_file */
        cc2_argc = 0;
        for (j = 0; j < cc2_base_argc; j++)
            cc2_args[cc2_argc++] = cc2_base[j];
        cc2_args[cc2_argc++] = temp1_file;
        cc2_args[cc2_argc++] = temp2_file;
        cc2_args[cc2_argc++] = asm_file;
        cc2_args[cc2_argc] = NULL;

        if (print_cmds || no_exec)
            printCommand(cc2_args);
        if (!no_exec) {
            status = execCommand(cc2_path, cc2_args);
            if (status != 0) {
                fprintf(stderr, "Error: c1 failed on %s\n", src);
                exit(status);
            }
        }

        /* Clean up temp files unless -k or -n */
        if (!keep_all && !no_exec) {
            unlink(temp1_file);
            unlink(temp2_file);
        }
        free(temp1_file);
        free(temp2_file);
        free(pp_file);

        /* If -s, we're done with this file */
        if (asm_only) {
            if (!no_exec) printf("  -> %s\n", asm_file);
            free(asm_file);
            free(obj_file);
            free(base);
            continue;
        }

        /* Assemble to .o */
        as_args[0] = asm_path;
        as_args[1] = "-o";
        as_args[2] = obj_file;
        as_args[3] = asm_file;
        as_args[4] = NULL;

        if (print_cmds || no_exec)
            printCommand(as_args);
        if (!no_exec) {
            status = execCommand(asm_path, as_args);
            if (status != 0) {
                fprintf(stderr, "Error: assembler failed on %s\n", asm_file);
                exit(status);
            }

            /* Clean up .s file unless -k */
            if (!keep_all)
                unlink(asm_file);
        }
        free(asm_file);

        /* Add to object list for linking */
        o_files[o_count++] = obj_file;
        if (!no_exec) printf("  -> %s\n", obj_file);
        free(base);
    }

    /* If -S, we're done */
    if (asm_only) {
        return 0;
    }

    /* Process each .s file: assemble to .o */
    for (i = 0; i < s_count; i++) {
        char *src = s_files[i];
        char *base = getBaseNoExt(src);
        char *obj_file;
        char *as_args[8];

        obj_file = malloc(strlen(base) + 10);
        sprintf(obj_file, "%s.o", base);

        if (!no_exec) printf("=== Assembling %s ===\n", src);

        as_args[0] = asm_path;
        as_args[1] = "-o";
        as_args[2] = obj_file;
        as_args[3] = src;
        as_args[4] = NULL;

        if (print_cmds || no_exec)
            printCommand(as_args);
        if (!no_exec) {
            status = execCommand(asm_path, as_args);
            if (status != 0) {
                fprintf(stderr, "Error: assembler failed on %s\n", src);
                exit(status);
            }
        }

        o_files[o_count++] = obj_file;
        if (!no_exec) printf("  -> %s\n", obj_file);
        free(base);
    }

    /* If -c, we're done */
    if (compile_only) {
        return 0;
    }

    /* Link all object files and libraries */
    {
        char *ld_args[MAX_ARGS];
        int ld_argc = 0;

        if (!no_exec) printf("\n=== Linking -> %s ===\n", output_file);

        ld_args[ld_argc++] = ld_path;
        if (strip_syms)
            ld_args[ld_argc++] = "-s";
        if (nine_char)
            ld_args[ld_argc++] = "-9";
        ld_args[ld_argc++] = "-o";
        ld_args[ld_argc++] = output_file;

	ld_args[ld_argc++] = "-Ttext=0x100";

	/* c object header */
	ld_args[ld_argc++] = chdr_path;

        /* Add object files */
        for (i = 0; i < o_count; i++)
            ld_args[ld_argc++] = o_files[i];

        ld_args[ld_argc++] = libc_path;

        /* Add library files */
        for (i = 0; i < a_count; i++)
            ld_args[ld_argc++] = a_files[i];

        ld_args[ld_argc++] = libu_path;
        ld_args[ld_argc++] = libc_path;
        ld_args[ld_argc++] = libc_path;

        ld_args[ld_argc] = NULL;

        if (print_cmds || no_exec)
            printCommand(ld_args);
        if (!no_exec) {
            status = execCommand(ld_path, ld_args);
            if (status != 0) {
                fprintf(stderr, "Error: linker failed\n");
                exit(status);
            }

            /* Clean up generated .o files unless -k */
            if (!keep_all) {
                for (i = o_input_count; i < o_count; i++) {
                    if (unlink(o_files[i]) != 0)
                        perror(o_files[i]);
                }
            }
        }
    }

    if (!no_exec)
        printf("\n=== Build successful: %s ===\n", output_file);

    return 0;
}
