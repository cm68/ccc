/* Test function-scope and block-scope variable declarations */

int test_function_scope(int param) {
    /* Function-scope variables */
    int a;
    int b;
    char c;

    a = 10;
    b = 20;
    c = 'x';

    return a + b + c + param;
}

int test_block_scope(int x) {
    int outer;

    outer = 5;

    {
        /* Block-scope variable */
        int inner;
        inner = 10;
        outer = outer + inner;
    }

    return outer;
}

int test_nested_blocks(void) {
    int level1;
    level1 = 1;

    {
        int level2;
        level2 = 2;

        {
            int level3;
            level3 = 3;
            level1 = level1 + level2 + level3;
        }
    }

    return level1;
}

int test_shadowing(void) {
    int x;
    x = 10;

    {
        int x;  /* Shadows outer x */
        x = 20;
    }

    return x;  /* Should return 10 */
}

int test_multiple_declarations(void) {
    int a, b, c;
    char d, e;
    long f;

    a = 1;
    b = 2;
    c = 3;
    d = 'a';
    e = 'b';
    f = 100;

    return a + b + c;
}

int test_initialized_locals(void) {
    int a = 10;
    int b = 20;
    char c = 'z';

    return a + b + c;
}

int test_mixed_statements_and_decls(void) {
    int a;
    a = 5;

    int b;  /* Declaration after statement - modern C99 style */
    b = 10;

    return a + b;
}
