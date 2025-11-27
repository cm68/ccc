/* Test AST with various control flow */

int
tSwitch(n)
int n;
{
    switch (n) {
    case 0:
        return 10;
    case 1:
        return 20;
    default:
        return 30;
    }
}

int
tFor(n)
int n;
{
    int i;
    int sum;

    sum = 0;
    for (i = 0; i < n; i = i + 1) {
        sum = sum + i;
    }
    return sum;
}

int
test_do(n)
int n;
{
    int i;

    i = 0;
    do {
        i = i + 1;
    } while (i < n);
    return i;
}
