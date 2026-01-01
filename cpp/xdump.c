/*
 * xdump.c - Convert binary .x lexeme stream to readable text
 *
 * Usage: xdump file.x
 *
 * Reads binary lexeme stream and outputs text that semantically
 * matches the preprocessed .i file.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "lexeme.h"

int
main(int argc, char **argv)
{
    FILE *f;
    int c, len, i;
    long val;
    union { float f; unsigned long l; } u;
    char buf[256];

    if (argc != 2) {
        fprintf(stderr, "Usage: %s file.x\n", argv[0]);
        return 1;
    }

    f = fopen(argv[1], "rb");
    if (!f) {
        perror(argv[1]);
        return 1;
    }

    while ((c = fgetc(f)) != EOF) {
        switch (c) {
        case E_O_F:
            goto done;

        case SEMI:
            printf(";\n");
            break;
        case BEGIN:
            printf("{\n");
            break;
        case END:
            printf("}\n");
            break;
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
            /* Line increment by 1 */
            printf("# +1\n");
            break;

        case LINENO:
            /* Full line+file: LINENO + 2-byte line + len + filename */
            val = fgetc(f) & 0xff;
            val |= (fgetc(f) & 0xff) << 8;
            len = fgetc(f);
            for (i = 0; i < len; i++)
                buf[i] = fgetc(f);
            buf[len] = 0;
            printf("# %ld \"%s\"\n", val, buf);
            break;

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
