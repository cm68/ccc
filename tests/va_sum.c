#include <stdarg.h>

int sum(int n, ...)
{
    va_list ap;
    int total;
    int i;

    va_start(ap, n);
    total = 0;
    for (i = 0; i < n; i++) {
        total = total + va_arg(ap, int);
    }
    va_end(ap);
    return total;
}

int main()
{
    return sum(3, 10, 20, 12);  /* Should return 42 */
}
