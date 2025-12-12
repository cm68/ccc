/*
 * Simple varargs test - returns first vararg
 * Expected return: 42
 */
#include <stdarg.h>

int first(int n, ...) {
    va_list ap;
    int val;
    va_start(ap, n);
    val = va_arg(ap, int);
    va_end(ap);
    return val;
}

int main() {
    return first(1, 42);
}
