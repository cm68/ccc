/*
 * Test that function pointer types with different parameter names
 * are treated as the same type
 */

/* These should all be compatible types */
int (*fp1)(int x);
int (*fp2)(int y);
int (*fp3)(int z);

/* With multiple parameters */
int (*gp1)(int a, char b);
int (*gp2)(int x, char y);

/* Function declarations with different parameter names */
int func1(int x, char y);
int func2(int a, char b);

/* Definitions */
int func1(int x, char y) {
    return x + y;
}

int func2(int a, char b) {
    return a + b;
}
