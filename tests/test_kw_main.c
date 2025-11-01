/*
 * Test program for keyword lookup table validation
 * Compile with: gcc -o test_kw test_kw_main.c kw.o
 */
#include <stdio.h>
#include <string.h>
#include "token.h"

extern char kwlook(unsigned char *str, unsigned char *table);
extern unsigned char ckw[];

struct test_case {
    char *keyword;
    char expected_token;
    char *description;
};

struct test_case tests[] = {
    {"asm", ASM, "ASM"},
    {"auto", AUTO, "AUTO"},
    {"boolean", BOOLEAN, "BOOLEAN"},
    {"break", BREAK, "BREAK"},
    {"case", CASE, "CASE"},
    {"char", CHAR, "CHAR"},
    {"const", CONST, "CONST"},
    {"continue", CONTINUE, "CONTINUE"},
    {"default", DEFAULT, "DEFAULT"},
    {"do", DO, "DO"},
    {"double", DOUBLE, "DOUBLE"},
    {"else", ELSE, "ELSE"},
    {"enum", ENUM, "ENUM"},
    {"extern", EXTERN, "EXTERN"},
    {"float", FLOAT, "FLOAT"},
    {"for", FOR, "FOR"},
    {"goto", GOTO, "GOTO"},
    {"if", IF, "IF"},
    {"int", INT, "INT"},
    {"long", LONG, "LONG"},
    {"return", RETURN, "RETURN"},
    {"register", REGISTER, "REGISTER"},
    {"sizeof", SIZEOF, "SIZEOF"},
    {"short", SHORT, "SHORT"},
    {"static", STATIC, "STATIC"},
    {"struct", STRUCT, "STRUCT"},
    {"switch", SWITCH, "SWITCH (NOT IN TABLE YET)"},
    {"typedef", TYPEDEF, "TYPEDEF"},
    {"union", UNION, "UNION"},
    {"unsigned", UNSIGNED, "UNSIGNED"},
    {"void", VOID, "VOID"},
    {"volatile", VOLATILE, "VOLATILE"},
    {"while", WHILE, "WHILE"},
    {NULL, 0, NULL}
};

int main() {
    int passed = 0;
    int failed = 0;
    struct test_case *t;
    char result;

    printf("Keyword Table Validation Test\n");
    printf("==============================\n\n");

    for (t = tests; t->keyword != NULL; t++) {
        result = kwlook((unsigned char *)t->keyword, ckw);

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
