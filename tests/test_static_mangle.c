/* Test static variable name mangling */

/* File-scoped statics */
static int file_static1;
static int file_static2;

/* Test multiple function-scoped statics */
int
test_multiple_statics(x)
int x;
{
    static int static_a;
    static int static_b;
    static int static_c;

    static_a = static_a + 1;
    static_b = static_b + x;
    static_c = static_a + static_b;

    return static_c + file_static1 + file_static2;
}

/* Test statics in different functions */
int
another_function(y)
int y;
{
    static int counter;

    counter = counter + y;
    return counter;
}

/* Same-named statics in different functions should get different mangled names */
int
third_function()
{
    static int counter;  /* Same name as in another_function, but different mangled name */

    counter = counter + 1;
    return counter;
}
