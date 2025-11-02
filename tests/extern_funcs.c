/*
 * Test extern function declarations
 */

/* Extern function declaration (forward reference) */
extern int external_func();

/* Extern function with parameters */
extern int external_func2(int a, int b);

/* Regular function declaration (no extern) */
int regular_func();

/* Extern function with K&R style */
extern int kr_external(x, y)
int x;
int y;
{
    return x + y;
}

/* Regular function definition */
int
regular_func()
{
    return 42;
}

/* Function with local variables (no calls yet) */
int
caller()
{
    int x;
    x = 1;
    x = 2;
    x = 3;
    return x;
}

/* Multiple extern declarations of same function (should be allowed) */
extern int foo();
extern int foo();

/* Extern followed by definition */
extern int bar();
int
bar()
{
    return 99;
}
