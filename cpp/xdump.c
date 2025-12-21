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

/* Token types from c0.h */
#define E_O_F   0
#define SEMI    1
#define BEGIN   2
#define END     3
#define LBRACK  4
#define RBRACK  5
#define LPAR    6
#define RPAR    7
#define COLON   8
#define COMMA   9

#define KEYW    19
#define SYM     20
#define NUMBER  21
#define STRING  22
#define FNUMBER 23
#define LNUMBER 25

#define INCR    30
#define DECR    31
#define BANG    34
#define AMPER   35
#define STAR    36
#define TWIDDLE 38
#define DOT     39
#define PLUS    40
#define MINUS   41
#define TIMES   42
#define DIV     43
#define MOD     44
#define RSHIFT  45
#define LSHIFT  46
#define AND     47
#define OR      48
#define XOR     49
#define ARROW   50
#define LAND    53
#define LOR     54

#define EQ      60
#define NEQ     61
#define LE      62
#define LT      63
#define GE      64
#define GT      65

#define PLUSEQ  70
#define SUBEQ   71
#define MULTEQ  72
#define DIVEQ   73
#define MODEQ   74
#define RSHIFTEQ 75
#define LSHIFTEQ 76
#define ANDEQ   77
#define OREQ    78
#define XOREQ   79
#define ASSIGN  80
#define QUES    90
#define SIZEOF  91

#define LABEL   112

/* Keyword values */
static char *kwnames[] = {
    "int",      /* 0 */
    "char",     /* 1 */
    "float",    /* 2 */
    "double",   /* 3 */
    "struct",   /* 4 */
    NULL,       /* 5 */
    "long",     /* 6 */
    "unsigned", /* 7 */
    "union",    /* 8 */
    "typedef",  /* 9 */
    "void",     /* 10 */
    "auto",     /* 11 */
    "extern",   /* 12 */
    "static",   /* 13 */
    "register", /* 14 */
    NULL, NULL, NULL, NULL, NULL,  /* 15-19 */
    "goto",     /* 20 */
    "return",   /* 21 */
    "if",       /* 22 */
    "while",    /* 23 */
    "else",     /* 24 */
    "switch",   /* 25 */
    "case",     /* 26 */
    "break",    /* 27 */
    "continue", /* 28 */
    "do",       /* 29 */
    "default",  /* 30 */
    "for",      /* 31 */
    "enum",     /* 32 */
    "asm",      /* 33 */
};
#define NKEYWORDS (sizeof(kwnames)/sizeof(kwnames[0]))

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

        case KEYW:
            c = fgetc(f);
            if (c < NKEYWORDS && kwnames[c])
                printf("%s ", kwnames[c]);
            else
                printf("KW%d ", c);
            break;

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
            len = fgetc(f);
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

        default:
            printf("?%d? ", c);
            break;
        }
    }

done:
    fclose(f);
    return 0;
}
