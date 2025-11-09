/*
 * Test program for keyword lookup table validation
 * Compile with: gcc -o test_kw test_kw_main.c kw.o
 */
#include <stdio.h>
#include <string.h>
#include "token.h"

extern char kwlook(unsigned char *str, unsigned char *table);
extern unsigned char ckw[];
extern unsigned char cppkw[];

struct test_case {
    char *keyword;
    char expected_token;
    char *description;
    unsigned char *table;
};

struct test_case tests[] = {
    /* C keywords */
    {"asm", ASM, "ASM", ckw},
    {"auto", AUTO, "AUTO", ckw},
    {"break", BREAK, "BREAK", ckw},
    {"case", CASE, "CASE", ckw},
    {"char", CHAR, "CHAR", ckw},
    {"const", CONST, "CONST", ckw},
    {"continue", CONTINUE, "CONTINUE", ckw},
    {"default", DEFAULT, "DEFAULT", ckw},
    {"do", DO, "DO", ckw},
    {"double", DOUBLE, "DOUBLE", ckw},
    {"else", ELSE, "ELSE", ckw},
    {"enum", ENUM, "ENUM", ckw},
    {"extern", EXTERN, "EXTERN", ckw},
    {"float", FLOAT, "FLOAT", ckw},
    {"for", FOR, "FOR", ckw},
    {"goto", GOTO, "GOTO", ckw},
    {"if", IF, "IF", ckw},
    {"int", INT, "INT", ckw},
    {"long", LONG, "LONG", ckw},
    {"return", RETURN, "RETURN", ckw},
    {"register", REGISTER, "REGISTER", ckw},
    {"sizeof", SIZEOF, "SIZEOF", ckw},
    {"short", SHORT, "SHORT", ckw},
    {"static", STATIC, "STATIC", ckw},
    {"struct", STRUCT, "STRUCT", ckw},
    {"switch", SWITCH, "SWITCH", ckw},
    {"typedef", TYPEDEF, "TYPEDEF", ckw},
    {"union", UNION, "UNION", ckw},
    {"unsigned", UNSIGNED, "UNSIGNED", ckw},
    {"void", VOID, "VOID", ckw},
    {"volatile", VOLATILE, "VOLATILE", ckw},
    {"while", WHILE, "WHILE", ckw},

    /* CPP keywords */
    {"define", DEFINE, "CPP DEFINE", cppkw},
    {"undef", UNDEF, "CPP UNDEF", cppkw},
    {"include", INCLUDE, "CPP INCLUDE", cppkw},
    {"if", IF, "CPP IF", cppkw},
    {"ifdef", IFDEF, "CPP IFDEF", cppkw},
    {"ifndef", IFNDEF, "CPP IFNDEF", cppkw},
    {"else", ELSE, "CPP ELSE", cppkw},
    {"elif", ELIF, "CPP ELIF", cppkw},
    {"endif", ENDIF, "CPP ENDIF", cppkw},

    {NULL, 0, NULL, NULL}
};

int main() {
    int passed = 0;
    int failed = 0;
    struct test_case *t;
    char result;

    printf("Keyword Table Validation Test\n");
    printf("==============================\n\n");

    for (t = tests; t->keyword != NULL; t++) {
        result = kwlook((unsigned char *)t->keyword, t->table);

        if (result == t->expected_token) {
            printf("✓ %-12s -> '%c' (0x%02x) %s\n",
                   t->keyword, result ? result : '?', result, t->description);
            passed++;
        } else {
            printf("✗ %-12s -> '%c' (0x%02x) EXPECTED '%c' (0x%02x) %s\n",
                   t->keyword,
                   result ? result : '?', result ? result : 0,
                   t->expected_token, t->expected_token,
                   t->description);
            failed++;
        }
    }

    printf("\n==============================\n");
    printf("Results: %d passed, %d failed\n", passed, failed);

    if (failed > 0) {
        printf("\nFailed keywords need keyword table fixes in kw.c\n");
        return 1;
    }

    printf("\nAll keywords validated successfully!\n");
    return 0;
}
