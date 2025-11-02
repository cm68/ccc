/* Test function calls in expressions */

int add(int x, int y);
int multiply(int a, int b);

int
test_simple_call()
{
    return add(1, 2);
}

int
test_nested_call()
{
    return add(multiply(2, 3), multiply(4, 5));
}

int
test_call_with_vars(x, y)
int x;
int y;
{
    int z;
    z = add(x, y);
    return multiply(z, 2);
}

int
test_no_args()
{
    int value;
    value = add(0, 0);
    return value;
}

int
test_expr_in_call(n)
int n;
{
    return add(n + 1, n * 2);
}
