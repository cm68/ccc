/* Test AST output format */

int
add(x, y)
int x;
int y;
{
    return x + y;
}

int
factorial(n)
int n;
{
    if (n < 2) {
        return 1;
    }
    return n * factorial(n - 1);
}

int
loop_test(n)
int n;
{
    int i;
    int sum;

    sum = 0;
    i = 0;
    while (i < n) {
        sum = sum + i;
        i = i + 1;
    }
    return sum;
}
