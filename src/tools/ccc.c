/*
 * ccc - C compiler driver
 *
 * Orchestrates cpp, c0 (parser), and c1 (code generator)
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/wait.h>
#include <fcntl.h>
#include <libgen.h>

#define MAX_ARGS 2560

char *progname;

#define stringify2(s) #s
#define stringify(s) stringify2(s)

char *rootdir = stringify(ROOTDIR);

/*
 * Duplicate a string (strdup is POSIX, not C99)
 */
char *
strdup_(char *s)
{
    char *p = malloc(strlen(s) + 1);
    if (p)
        strcpy(p, s);
    return p;
}

/*
 * Resolve a path to an absolute path, handling . and .. components.
 * Returns resolved path in 'resolved' buffer, or NULL on failure.
 */
char *
realpath_(char *path, char *resolved)
{
    char *parts[64];
    char temp[1024];
    int nparts = 0;
    char *p, *tok;
    int i;

    if (!path || !resolved)
        return NULL;

    /* Start with cwd for relative paths */
    if (path[0] != '/') {
        if (!getcwd(temp, sizeof(temp)))
            return NULL;
        strcat(temp, "/");
        strcat(temp, path);
    } else {
        strcpy(temp, path);
    }

    /* Parse path components */
    p = temp;
    while ((tok = strtok(p, "/")) != NULL) {
        p = NULL;
        if (strcmp(tok, ".") == 0) {
            continue;
        } else if (strcmp(tok, "..") == 0) {
            if (nparts > 0)
                nparts--;
        } else {
            parts[nparts++] = tok;
        }
    }

    /* Rebuild path */
    resolved[0] = '\0';
    for (i = 0; i < nparts; i++) {
        strcat(resolved, "/");
        strcat(resolved, parts[i]);
    }
    if (resolved[0] == '\0')
        strcpy(resolved, "/");

    return resolved;
}

#define strdup strdup_
#define realpath realpath_

void
usage(void)
{
    printf("usage: %s [<options>] <files...>\n", progname);
    printf("  files: .c (compile) .s (assemble) .o .a (link)\n");
    printf("  -o <output>    Output file (default: a.out)\n");
    printf("  -c             Compile and assemble only, keep .o\n");
    printf("  -s             Compile only, keep .s (no assembly)\n");
    printf("  -k             Keep all intermediates (.x, .1, .2, .s, .o)\n");
    printf("  -O             Run the peephole optimizer over the assembly\n");
    printf("  -S             Strip symbols from output\n");
    printf("  -9             Use 9-char symbols in output\n");
    printf("  -I<dir>        Include directory\n");
    printf("  -i<dir>        System include directory (default /usr/include)\n");
    printf("  -m <system>    Target system: micronix (default) or cpm\n");
    printf("  -D<var>[=val]  Define macro\n");
    printf("  -E             Preprocess only\n");
    printf("  -H             Use .i (human-readable) input for pass1 instead of .x\n");
    printf("  -l<lib>        Link with library lib<lib>.a\n");
    printf("  -L<dir>        Add <dir> to library search path\n");
    printf("  -x             Print commands as they execute\n");
    printf("  -n             Print commands without executing (dry run)\n");
    printf("  -C <flags>     Pass -v <flags> to cpp\n");
    printf("  -1 <flags>     Pass -v <flags> to pass1 (c0)\n");
    printf("  -2 <flags>     Pass -v <flags> to pass2 (c1)\n");
    exit(1);
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
 * Drop the 16-byte object header from a linked image, in place, so
 * what is left is what CP/M loads at 0x100.  Reads the whole file:
 * a .com is 64KB at the very most and usually a great deal less.
 */
#define WSHDRLEN 16

int
stripHeader(char *path)
{
    FILE *f;
    char *buf;
    long len;
    size_t got;

    f = fopen(path, "rb");
    if (!f) {
        perror(path);
        return -1;
    }
    fseek(f, 0L, SEEK_END);
    len = ftell(f);
    if (len <= WSHDRLEN) {
        fprintf(stderr, "%s: too short to be an image\n", path);
        fclose(f);
        return -1;
    }
    buf = malloc(len - WSHDRLEN);
    if (!buf) {
        fprintf(stderr, "%s: out of memory\n", path);
        fclose(f);
        return -1;
    }
    fseek(f, (long)WSHDRLEN, SEEK_SET);
    got = fread(buf, 1, len - WSHDRLEN, f);
    fclose(f);
    if (got != (size_t)(len - WSHDRLEN)) {
        fprintf(stderr, "%s: short read\n", path);
        free(buf);
        return -1;
    }

    f = fopen(path, "wb");
    if (!f) {
        perror(path);
        free(buf);
        return -1;
    }
    fwrite(buf, 1, got, f);
    fclose(f);
    free(buf);
    return 0;
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

/*
 * The passes know identifiers only as @id; their spellings
 * sit in the .n sidecar.  The passes stay ignorant - the driver owns
 * diagnostics, so it runs c0 and c1 with stderr through a pipe and
 * rewrites @id to the name on the way past.  Lookup is two seeks,
 * same as c1's: a 2-byte count, 2-byte offsets, NUL-terminated names
 * in id order, ids 1-based.
 */
static int nfd = -1;

static void
nname(unsigned int id, char *buf, int size)
{
    unsigned char two[2];
    int n, i;

    lseek(nfd, (long)(2 + 2 * (id - 1)), 0);
    read(nfd, (char *)two, 2);
    lseek(nfd, (long)(two[0] | (two[1] << 8)), 0);
    n = read(nfd, buf, size - 1);
    for (i = 0; i < n; i++)
        if (!buf[i])
            return;
    buf[i] = 0;
}

int
execFiltered(char *cmd, char **args, char *nfile)
{
    int pfd[2];
    int pid;
    int status;
    char buf[128];
    char nam[20];
    int n, i;
    char c;
    int at = 0;             /* digits seen after '@', +1 */
    unsigned int id = 0;

    nfd = nfile ? open(nfile, O_RDONLY) : -1;
    if (nfd < 0)
        return execCommand(cmd, args);

    if (pipe(pfd) < 0) {
        perror("pipe");
        exit(1);
    }

    pid = fork();
    if (pid < 0) {
        perror("fork");
        exit(1);
    }

    if (pid == 0) {
        close(pfd[0]);
        dup2(pfd[1], 2);
        close(pfd[1]);
        execv(cmd, args);
        perror(cmd);
        exit(1);
    }

    close(pfd[1]);
    while ((n = read(pfd[0], buf, sizeof(buf))) > 0) {
        for (i = 0; i < n; i++) {
            c = buf[i];
            if (at) {
                if (c >= '0' && c <= '9') {
                    id = id * 10 + (c - '0');
                    at = 2;
                    continue;
                }
                if (at > 1) {
                    nname(id, nam, sizeof(nam));
                    write(2, nam, strlen(nam));
                } else {
                    write(2, "@", 1);
                }
                at = 0;
            }
            if (c == '@') {
                at = 1;
                id = 0;
                continue;
            }
            write(2, &c, 1);
        }
    }
    if (at > 1) {
        nname(id, nam, sizeof(nam));
        write(2, nam, strlen(nam));
    } else if (at) {
        write(2, "@", 1);
    }
    close(pfd[0]);
    close(nfd);
    nfd = -1;

    if (waitpid(pid, &status, 0) < 0) {
        perror("waitpid");
        exit(1);
    }

    if (WIFEXITED(status)) {
        return WEXITSTATUS(status);
    } else {
        return 1;
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
    int use_prep = 0;        /* -H: use .i file for pass1 instead of .x */
    int optimize = 0;        /* -O: run the peephole over c1's assembly */

    /*
     * -m: which system the output runs on.  It selects the runtime
     * tree - micronix/ or cpm/ - that the headers, the libraries and
     * the startup object come from.  Micronix is the default because
     * that is what the simulator runs and what every test here links.
     */
    char *target = "micronix";
    int cpm_target = 0;      /* target is CP/M: image layout differs */

    /* Input files by type */
    char *c_files[MAX_ARGS];
    char *s_files[MAX_ARGS];
    char *o_files[MAX_ARGS];
    char *a_files[MAX_ARGS];
    int c_count = 0, s_count = 0, o_count = 0, a_count = 0;
    int o_input_count = 0;   /* .o files from cmdline (vs generated) */

    /* Linker options */
    char *ld_libs[MAX_ARGS];  /* -l<lib> options */
    char *ld_paths[MAX_ARGS]; /* -L<dir> options */
    int ld_lib_count = 0;
    int ld_path_count = 0;

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
    char peep_path[1024];

    char chdr_path[1024];
    char libc_path[1024];
    char libu_path[1024];
    char sysinc_path[1024];

    int status;
    int i;

    progname = argv[0];

    /* Check for ROOTDIR env var override, normalize path */
    {
        char *env_rootdir;
        char resolved[1024];

        env_rootdir = getenv("ROOTDIR");
        if (env_rootdir)
            rootdir = env_rootdir;
        if (realpath(rootdir, resolved))
            rootdir = strdup(resolved);
    }

    /*
     * The tools that do the work all run on this machine and come
     * out of unix/bin, whichever system the output is for.
     */
    sprintf(cpp_path, "%s/unix/bin/cpp", rootdir);
    sprintf(cc1_path, "%s/unix/bin/c0", rootdir);
    sprintf(cc2_path, "%s/unix/bin/c1", rootdir);
    sprintf(asm_path, "%s/unix/bin/asz", rootdir);
    sprintf(ld_path, "%s/unix/bin/wsld", rootdir);
    sprintf(astpp_path, "%s/unix/bin/astpp", rootdir);
    sprintf(peep_path, "%s/unix/bin/peep", rootdir);

    /*
     * The runtime is per target and cannot be resolved until -m has
     * been seen, so it is built after the argument loop below.
     */

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
        } else if (strcmp(argv[0], "-O") == 0) {
            optimize = 1;
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
        } else if (strcmp(argv[0], "-H") == 0) {
            use_prep = 1;
            argc--;
            argv++;
        } else if (strcmp(argv[0], "-x") == 0) {
            print_cmds = 1;
            argc--;
            argv++;
        } else if (strcmp(argv[0], "-m") == 0) {
            /* Which system to build for: picks the runtime tree */
            argc--;
            argv++;
            if (argc == 0) {
                fprintf(stderr, "Error: -m requires an argument\n");
                usage();
            }
            target = argv[0];
            if (strcmp(target, "micronix") != 0 &&
                strcmp(target, "cpm") != 0) {
                fprintf(stderr, "Error: unknown target %s "
                        "(micronix or cpm)\n", target);
                exit(1);
            }
            argc--;
            argv++;
        } else if (strcmp(argv[0], "-n") == 0) {
            no_exec = 1;
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
        } else if (strcmp(argv[0], "-N") == 0) {
            /* Pass -N to cpp (suppress line markers) */
            if (cpp_base_argc >= MAX_ARGS) {
                fprintf(stderr, "Error: too many arguments\n");
                exit(1);
            }
            cpp_base[cpp_base_argc++] = argv[0];
            argc--;
            argv++;
        } else if (strcmp(argv[0], "-C") == 0) {
            /* Pass -v <flags> to cpp */
            argc--;
            argv++;
            if (argc == 0) {
                fprintf(stderr, "Error: -C requires an argument\n");
                usage();
            }
            if (cpp_base_argc >= MAX_ARGS - 1) {
                fprintf(stderr, "Error: too many arguments\n");
                exit(1);
            }
            cpp_base[cpp_base_argc++] = "-v";
            cpp_base[cpp_base_argc++] = argv[0];
            argc--;
            argv++;
        } else if (strcmp(argv[0], "-1") == 0) {
            /* Pass -v <flags> to pass1 */
            argc--;
            argv++;
            if (argc == 0) {
                fprintf(stderr, "Error: -1 requires an argument\n");
                usage();
            }
            if (cc1_base_argc >= MAX_ARGS - 1) {
                fprintf(stderr, "Error: too many arguments\n");
                exit(1);
            }
            cc1_base[cc1_base_argc++] = "-v";
            cc1_base[cc1_base_argc++] = argv[0];
            argc--;
            argv++;
        } else if (strcmp(argv[0], "-2") == 0) {
            /* Pass -v <flags> to pass2 */
            argc--;
            argv++;
            if (argc == 0) {
                fprintf(stderr, "Error: -2 requires an argument\n");
                usage();
            }
            if (cc2_base_argc >= MAX_ARGS - 1) {
                fprintf(stderr, "Error: too many arguments\n");
                exit(1);
            }
            cc2_base[cc2_base_argc++] = "-v";
            cc2_base[cc2_base_argc++] = argv[0];
            argc--;
            argv++;
        } else if (argv[0][0] == '-' && argv[0][1] == 'l') {
            /* -l<lib>: pass to linker */
            if (argv[0][2] == '\0') {
                fprintf(stderr, "Error: -l requires a library name\n");
                usage();
            }
            ld_libs[ld_lib_count++] = argv[0];
            argc--;
            argv++;
        } else if (argv[0][0] == '-' && argv[0][1] == 'L') {
            /* -L<dir>: pass to linker */
            if (argv[0][2] == '\0') {
                fprintf(stderr, "Error: -L requires a directory\n");
                usage();
            }
            ld_paths[ld_path_count++] = argv[0];
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

    /*
     * Now that -m is known, resolve the runtime.  libc and libccc are
     * system-independent and are in both trees; the system-call layer
     * and the startup object are not, so they are named per target.
     */
    cpm_target = (strcmp(target, "cpm") == 0);
    sprintf(libc_path, "%s/%s/lib/libc.a", rootdir, target);
    sprintf(sysinc_path, "-i%s/%s/include", rootdir, target);
    if (strcmp(target, "cpm") == 0) {
        sprintf(libu_path, "%s/%s/lib/libcpm.a", rootdir, target);
        sprintf(chdr_path, "%s/%s/lib/crtcpm.o", rootdir, target);
    } else {
        sprintf(libu_path, "%s/%s/lib/libu.a", rootdir, target);
        sprintf(chdr_path, "%s/%s/lib/crt0.o", rootdir, target);
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
        char *name_file;
        char *temp1_file;
        char *temp2_file;
        char *asm_file;
        char *obj_file;
        char *cpp_args[MAX_ARGS];
        char *cc1_args[MAX_ARGS];
        char *cc2_args[MAX_ARGS];
        char *as_args[8];
        int cpp_argc, cc1_argc, cc2_argc, j;

        /* Generate intermediate filenames */
        lex_file = malloc(strlen(base) + 10);
        sprintf(lex_file, "%s.x", base);
        prep_file = malloc(strlen(base) + 10);
        sprintf(prep_file, "%s.i", base);
        name_file = malloc(strlen(base) + 10);
        sprintf(name_file, "%s.n", base);
        temp1_file = malloc(strlen(base) + 10);
        sprintf(temp1_file, "%s.1", base);
        temp2_file = malloc(strlen(base) + 10);
        sprintf(temp2_file, "%s.2", base);
        asm_file = malloc(strlen(base) + 10);
        sprintf(asm_file, "%s.s", base);
        obj_file = malloc(strlen(base) + 10);
        sprintf(obj_file, "%s.o", base);

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

        /* Build pass1 args: c0 source.x temp1 temp2 (or .i with -H) */
        cc1_argc = 0;
        for (j = 0; j < cc1_base_argc; j++)
            cc1_args[cc1_argc++] = cc1_base[j];
        cc1_args[cc1_argc++] = use_prep ? prep_file : lex_file;
        cc1_args[cc1_argc++] = temp1_file;
        cc1_args[cc1_argc++] = temp2_file;
        cc1_args[cc1_argc] = NULL;

        if (print_cmds || no_exec)
            printCommand(cc1_args);
        if (!no_exec) {
            status = execFiltered(cc1_path, cc1_args, name_file);
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
            status = execFiltered(cc2_path, cc2_args, name_file);
            if (status != 0) {
                fprintf(stderr, "Error: c1 failed on %s\n", src);
                exit(status);
            }
        }

        /* Clean up temp files unless -k or -n */
        if (!keep_all && !no_exec) {
            unlink(temp1_file);
            unlink(temp2_file);
            unlink(name_file);
        }
        free(temp1_file);
        free(temp2_file);
        free(name_file);

        /*
         * Peephole, if asked for.  It rewrites the assembly in place -
         * through a temporary, so a failure leaves the original where
         * it was rather than a half written file - which keeps .s
         * meaning "the assembly that gets assembled" whether or not
         * -O was given.  To see what it changed, compile twice.
         */
        if (optimize) {
            char *peep_file;
            char *peep_args[8];

            peep_file = malloc(strlen(base) + 10);
            sprintf(peep_file, "%s.ps", base);

            peep_args[0] = peep_path;
            peep_args[1] = asm_file;
            peep_args[2] = peep_file;
            peep_args[3] = NULL;

            if (print_cmds || no_exec)
                printCommand(peep_args);
            if (!no_exec) {
                status = execCommand(peep_path, peep_args);
                if (status != 0) {
                    fprintf(stderr, "Error: peep failed on %s\n", asm_file);
                    exit(status);
                }
                if (rename(peep_file, asm_file) != 0) {
                    fprintf(stderr, "Error: cannot replace %s\n", asm_file);
                    exit(1);
                }
            }
            free(peep_file);
        }

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

	/*
	 * Both systems load at 0x100.  Micronix reads the segment
	 * table out of the header and places data and bss itself; CP/M
	 * has no loader worth the name - it reads the file to 0x100
	 * and jumps - so text, data and bss have to come out as one
	 * contiguous image, which is what naming the same origin for
	 * all three gets.
	 */
	ld_args[ld_argc++] = "-Ttext=0x100";
	if (cpm_target) {
	    ld_args[ld_argc++] = "-Tdata=0x100";
	    ld_args[ld_argc++] = "-Tbss=0x100";
	}

	/* Add library search paths (-L options) */
	for (i = 0; i < ld_path_count; i++)
	    ld_args[ld_argc++] = ld_paths[i];

	/* c object header */
	ld_args[ld_argc++] = chdr_path;

        /* Add object files */
        for (i = 0; i < o_count; i++)
            ld_args[ld_argc++] = o_files[i];

        ld_args[ld_argc++] = libc_path;

        /* Add library files */
        for (i = 0; i < a_count; i++)
            ld_args[ld_argc++] = a_files[i];

        /* Add user-specified libraries (-l options) */
        for (i = 0; i < ld_lib_count; i++)
            ld_args[ld_argc++] = ld_libs[i];

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

            /*
             * A .com file is the bytes CP/M loads at 0x100 and
             * nothing else.  wsld writes a 16-byte Whitesmith's
             * header in front of them, so cut it off; the Makefiles
             * that built these images by hand all ended in a
             * "tail -c +17" for the same reason.
             */
            if (cpm_target && stripHeader(output_file) != 0)
                exit(1);

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

/* vim: set tabstop=4 shiftwidth=4 noexpandtab: */
