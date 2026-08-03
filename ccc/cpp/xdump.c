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
int depth;         /* brace depth for indentation */
int atbol = 1;     /* at beginning of line */
long srcline;      /* current source line from LINENO/NEWLINE */
char srcfile[256]; /* current source file from LINENO */
char lastfile[256];/* file at last sync point */

/*
 * Emit indentation spaces
 */
void
doindent(void)
{
    int i;
    for (i = 0; i < depth * 4; i++)
        putchar(' ');
    atbol = 0;
}

/*
 * Emit a sync point if needed (file changed or interval reached)
 */
void
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
        atbol = 1;
    }
}

/*
 * Output a newline and maybe emit sync point
 */
void
outnl(void)
{
    putchar('\n');
    outlines++;
    atbol = 1;
    maybesync(0);
}

/*
 * Emit indent if at beginning of line
 */
void
needindent(void)
{
    if (atbol)
        doindent();
}

int
main(int argc, char **argv)
{
    FILE *f;
    int c, len, i;
    long val;
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
            needindent();
            printf(";");
            outnl();
            continue;
        case BEGIN:
            needindent();
            printf("{");
            depth++;
            outnl();
            continue;
        case END:
            depth--;
            needindent();
            printf("}");
            outnl();
            continue;
        case LBRACK:
            needindent();
            printf("[ ");
            break;
        case RBRACK:
            needindent();
            printf("] ");
            break;
        case LPAR:
            needindent();
            printf("( ");
            break;
        case RPAR:
            needindent();
            printf(") ");
            break;
        case COLON:
            needindent();
            printf(": ");
            break;
        case COMMA:
            needindent();
            printf(", ");
            break;

        /* Keyword tokens (128-160) handled in default */

        case SYM:
            len = fgetc(f);
            for (i = 0; i < len; i++)
                buf[i] = fgetc(f);
            buf[len] = 0;
            needindent();
            printf("%s ", buf);
            break;

        case NUMBER:
        case LNUMBER:
            val = fgetc(f) & 0xff;
            val |= (fgetc(f) & 0xff) << 8;
            val |= (long)(fgetc(f) & 0xff) << 16;
            val |= (long)(fgetc(f) & 0xff) << 24;
            needindent();
            printf("%ld ", val);
            break;

        case STRING:
            /* 2-byte little-endian length + text */
            len = fgetc(f) & 0xff;
            len |= (fgetc(f) & 0xff) << 8;
            needindent();
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
            /* Labels outdented by one level */
            len = fgetc(f);
            for (i = 0; i < len; i++)
                buf[i] = fgetc(f);
            buf[len] = 0;
            if (atbol) {
                int j;
                for (j = 0; j < (depth - 1) * 4; j++)
                    putchar(' ');
                atbol = 0;
            }
            printf("%s: ", buf);
            break;

        case ASMSTR:
            /* 2-byte little-endian length + text */
            len = fgetc(f) & 0xff;
            len |= (fgetc(f) & 0xff) << 8;
            needindent();
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

        case INCR:   needindent(); printf("++ "); break;
        case DECR:   needindent(); printf("-- "); break;
        case BANG:   needindent(); printf("! "); break;
        case AMPER:  needindent(); printf("& "); break;
        case STAR:   needindent(); printf("* "); break;
        case TWIDDLE: needindent(); printf("~ "); break;
        case DOT:    needindent(); printf(". "); break;
        case PLUS:   needindent(); printf("+ "); break;
        case MINUS:  needindent(); printf("- "); break;
        case TIMES:  needindent(); printf("* "); break;
        case DIV:    needindent(); printf("/ "); break;
        case MOD:    needindent(); printf("%% "); break;
        case RSHIFT: needindent(); printf(">> "); break;
        case LSHIFT: needindent(); printf("<< "); break;
        case AND:    needindent(); printf("& "); break;
        case OR:     needindent(); printf("| "); break;
        case XOR:    needindent(); printf("^ "); break;
        case ARROW:  needindent(); printf("-> "); break;
        case LAND:   needindent(); printf("&& "); break;
        case LOR:    needindent(); printf("|| "); break;

        case EQ:     needindent(); printf("== "); break;
        case NEQ:    needindent(); printf("!= "); break;
        case LE:     needindent(); printf("<= "); break;
        case LT:     needindent(); printf("< "); break;
        case GE:     needindent(); printf(">= "); break;
        case GT:     needindent(); printf("> "); break;

        case PLUSEQ:  needindent(); printf("+= "); break;
        case SUBEQ:   needindent(); printf("-= "); break;
        case MULTEQ:  needindent(); printf("*= "); break;
        case DIVEQ:   needindent(); printf("/= "); break;
        case MODEQ:   needindent(); printf("%%="); break;
        case RSHIFTEQ: needindent(); printf(">>= "); break;
        case LSHIFTEQ: needindent(); printf("<<= "); break;
        case ANDEQ:   needindent(); printf("&= "); break;
        case OREQ:    needindent(); printf("|= "); break;
        case XOREQ:   needindent(); printf("^= "); break;
        case ASSIGN:  needindent(); printf("= "); break;
        case QUES:    needindent(); printf("? "); break;
        case SIZEOF:  needindent(); printf("sizeof "); break;
        case ELLIPSIS: needindent(); printf("... "); break;

        default:
            /* Handle keyword tokens (128-160) */
            needindent();
            if (c >= KW_FIRST && c <= KW_LAST) {
                switch (c) {
                case INT: printf("int "); break;
                case CHAR: printf("char "); break;
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
