/* Test modern/ANSI C function syntax (inline parameter types) */

/* Single parameter */
int add_one(int x) {
    return x + 1;
}

/* Multiple parameters */
int add(int a, int b) {
    return a + b;
}

/* No parameters */
int get_zero(void) {
    return 0;
}

/* Mixed types */
int calc(int x, char c, long l) {
    return x + c + l;
}
