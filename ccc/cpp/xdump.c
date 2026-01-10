/*
 * xdump.c - Convert binary .x lexeme stream to readable text
 *
 * Usage: xdump [-N] [-o outfile] [-h] file.x
 *
 * Reads binary lexeme stream and outputs compact readable C source
 * with periodic # line sync points for error reporting.
 *
 * Options:
 *   -o file  Write output to file instead of stdout
 *   -N       Suppress line sync points (debug mode)
 *   -h       Show usage
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "lexeme.h"

#define SYNC_INTERVAL 10  /* emit sync point every N output lines */

int nflag;         /* suppress sync points */
int outlines;      /* output line counter */
int lastsync;      /* output line of last sync point */
long srcline;      /* current source line from LINENO/NEWLINE */
char srcfile[256]; /* current source file from LINENO */
char lastfile[256];/* file at last sync point */

/*
 * Emit a sync point if needed (file changed or interval reached)
 */
static void
maybesync(int force)
{
    if (nflag)
        return;
    /* Emit sync on file change or every SYNC_INTERVAL lines */
    if (force || strcmp(srcfile, lastfile) != 0 ||
        (outlines - lastsync >= SYNC_INTERVAL)) {
        printf("# %ld \"%s\"\n", srcline, srcfile);
        outlines++;
        lastsync = outlines;
        strcpy(lastfile, srcfile);
    }
}

/*
 * Output a newline and maybe emit sync point
 */
static void
outnl(void)
{
    putchar('\n');
    outlines++;
    maybesync(0);
}

int
main(int argc, char **argv)
{
    FILE *f;
    int c, len, i;
    long val;
    union { float f; unsigned long l; } u;
    char buf[256];
    char *fname;
    char *outfile = NULL;

    /* parse options */
    while (argc > 1 && argv[1][0] == '-') {
        if (argv[1][1] == 'N' && argv[1][2] == 0) {
            nflag = 1;
        } else if (argv[1][1] == 'o' && argv[1][2] == 0) {
            if (argc < 3) {
                fprintf(stderr, "-o requires argument\n");
                return 1;
            }
            outfile = argv[2];
            argc--;
            argv++;
        } else if (argv[1][1] == 'h' && argv[1][2] == 0) {
            goto usage;
        } else {
            fprintf(stderr, "Unknown option: %s\n", argv[1]);
            return 1;
        }
        argc--;
        argv++;
    }

    if (argc != 2) {
usage:
        fprintf(stderr, "Usage: %s [-N] [-o outfile] [-h] file.x\n", argv[0]);
        fprintf(stderr, "  -o file  Write to file instead of stdout\n");
        fprintf(stderr, "  -N       Suppress line sync points\n");
        fprintf(stderr, "  -h       Show this help\n");
        return 1;
    }
    fname = argv[1];

    f = fopen(fname, "rb");
    if (!f) {
        perror(fname);
        return 1;
    }

    /* Redirect stdout to outfile if specified */
    if (outfile && !freopen(outfile, "w", stdout)) {
        perror(outfile);
        return 1;
    }

    while ((c = fgetc(f)) != EOF) {
        switch (c) {
        case E_O_F:
            goto done;

        case SEMI:
            printf(";");
            outnl();
            continue;
        case BEGIN:
            printf("{");
            outnl();
            continue;
        case END:
            printf("}");
            outnl();
            continue;
        case LBRACK:
            printf("[ ");
            break;
        case RBRACK:
            printf("] ");
            break;
        case LPAR:
            printf("( ");
            break;
        case RPAR:
            printf(") ");
            break;
        case COLON:
            printf(": ");
            break;
        case COMMA:
            printf(", ");
            break;

        /* Keyword tokens (128-160) handled in default */

        case SYM:
            len = fgetc(f);
            for (i = 0; i < len; i++)
                buf[i] = fgetc(f);
            buf[len] = 0;
            printf("%s ", buf);
            break;

        case NUMBER:
        case LNUMBER:
            val = fgetc(f) & 0xff;
            val |= (fgetc(f) & 0xff) << 8;
            val |= (long)(fgetc(f) & 0xff) << 16;
            val |= (long)(fgetc(f) & 0xff) << 24;
            printf("%ld ", val);
            break;

        case FNUMBER:
            u.l = fgetc(f) & 0xff;
            u.l |= (fgetc(f) & 0xff) << 8;
            u.l |= (long)(fgetc(f) & 0xff) << 16;
            u.l |= (long)(fgetc(f) & 0xff) << 24;
            printf("%g ", u.f);
            break;

        case STRING:
            /* 2-byte little-endian length + text */
            len = fgetc(f) & 0xff;
            len |= (fgetc(f) & 0xff) << 8;
            printf("\"");
            for (i = 0; i < len; i++) {
                int ch = fgetc(f);
                if (ch == '"') printf("\\\"");
                else if (ch == '\\') printf("\\\\");
                else if (ch == '\n') printf("\\n");
                else if (ch == '\t') printf("\\t");
                else if (ch >= 32 && ch < 127) putchar(ch);
                else printf("\\x%02x", ch);
            }
            printf("\" ");
            break;

        case LABEL:
            len = fgetc(f);
            for (i = 0; i < len; i++)
                buf[i] = fgetc(f);
            buf[len] = 0;
            printf("%s: ", buf);
            break;

        case ASMSTR:
            /* 2-byte little-endian length + text */
            len = fgetc(f) & 0xff;
            len |= (fgetc(f) & 0xff) << 8;
            printf("{ ");
            for (i = 0; i < len; i++) {
                int ch = fgetc(f);
                if (ch == '\n') printf("\\n");
                else if (ch == '\t') printf("\\t");
                else if (ch >= 32 && ch < 127) putchar(ch);
                else printf("\\x%02x", ch);
            }
            printf(" } ");
            break;

        case NEWLINE:
            /* Line increment by 1 - just track, don't output */
            srcline++;
            continue;

        case LINENO:
            /* Full line+file: LINENO + 2-byte line + len + filename */
            srcline = fgetc(f) & 0xff;
            srcline |= (fgetc(f) & 0xff) << 8;
            len = fgetc(f);
            for (i = 0; i < len; i++)
                srcfile[i] = fgetc(f);
            srcfile[len] = 0;
            /* Force sync point on file change */
            if (strcmp(srcfile, lastfile) != 0)
                maybesync(1);
            continue;

        case INCR:   printf("++ "); break;
        case DECR:   printf("-- "); break;
        case BANG:   printf("! "); break;
        case AMPER:  printf("& "); break;
        case STAR:   printf("* "); break;
        case TWIDDLE: printf("~ "); break;
        case DOT:    printf(". "); break;
        case PLUS:   printf("+ "); break;
        case MINUS:  printf("- "); break;
        case TIMES:  printf("* "); break;
        case DIV:    printf("/ "); break;
        case MOD:    printf("%% "); break;
        case RSHIFT: printf(">> "); break;
        case LSHIFT: printf("<< "); break;
        case AND:    printf("& "); break;
        case OR:     printf("| "); break;
        case XOR:    printf("^ "); break;
        case ARROW:  printf("-> "); break;
        case LAND:   printf("&& "); break;
        case LOR:    printf("|| "); break;

        case EQ:     printf("== "); break;
        case NEQ:    printf("!= "); break;
        case LE:     printf("<= "); break;
        case LT:     printf("< "); break;
        case GE:     printf(">= "); break;
        case GT:     printf("> "); break;

        case PLUSEQ:  printf("+= "); break;
        case SUBEQ:   printf("-= "); break;
        case MULTEQ:  printf("*= "); break;
        case DIVEQ:   printf("/= "); break;
        case MODEQ:   printf("%%="); break;
        case RSHIFTEQ: printf(">>= "); break;
        case LSHIFTEQ: printf("<<= "); break;
        case ANDEQ:   printf("&= "); break;
        case OREQ:    printf("|= "); break;
        case XOREQ:   printf("^= "); break;
        case ASSIGN:  printf("= "); break;
        case QUES:    printf("? "); break;
        case SIZEOF:  printf("sizeof "); break;
        case ELLIPSIS: printf("... "); break;

        default:
            /* Handle keyword tokens (128-160) */
            if (c >= KW_FIRST && c <= KW_LAST) {
                switch (c) {
                case INT: printf("int "); break;
                case CHAR: printf("char "); break;
                case FLOAT: printf("float "); break;
                case DOUBLE: printf("double "); break;
                case STRUCT: printf("struct "); break;
                case SIGNED: printf("signed "); break;
                case LONG: printf("long "); break;
                case UNSIGNED: printf("unsigned "); break;
                case UNION: printf("union "); break;
                case TYPEDEF: printf("typedef "); break;
                case VOID: printf("void "); break;
                case SHORT: printf("short "); break;
                case AUTO: printf("auto "); break;
                case EXTERN: printf("extern "); break;
                case STATIC: printf("static "); break;
                case REGISTER: printf("register "); break;
                case GOTO: printf("goto "); break;
                case RETURN: printf("return "); break;
                case IF: printf("if "); break;
                case WHILE: printf("while "); break;
                case ELSE: printf("else "); break;
                case SWITCH: printf("switch "); break;
                case CASE: printf("case "); break;
                case BREAK: printf("break "); break;
                case CONTINUE: printf("continue "); break;
                case DO: printf("do "); break;
                case DEFAULT: printf("default "); break;
                case FOR: printf("for "); break;
                case ENUM: printf("enum "); break;
                case ASM: printf("asm "); break;
                case CONST: printf("const "); break;
                case VOLATILE: printf("volatile "); break;
                case SIZEOF_KW: printf("sizeof "); break;
                default: printf("KW%d ", c); break;
                }
            } else {
                printf("?%d? ", c);
            }
            break;
        }
    }

done:
    fclose(f);
    return 0;
}

/* vim: set tabstop=4 shiftwidth=4 noexpandtab: */
