/*
 * Simple ANSI function definition test
 */

/* Just a definition, no forward declaration */
int simple(int x) {
    return x + 1;
}

/* With forward declaration */
int with_forward(int a, int b);

int with_forward(int a, int b) {
    return a + b;
}
