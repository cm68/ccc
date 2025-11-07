/*
 * Increment and decrement in expressions
 * Tests that postfix returns old value, prefix returns new value
 */

int test_postfix_in_assign()
{
    int i = 5;
    int a;
    a = i++;
    /* a should be 5 (old value), i should be 6 */
    return a;
}

int test_prefix_in_assign()
{
    int i = 5;
    int a;
    a = ++i;
    /* a should be 6 (new value), i should be 6 */
    return a;
}

int test_in_arithmetic()
{
    int i = 5;
    int a;
    a = i++ + 10;
    /* a should be 15 (5 + 10), i should be 6 */
    return a;
}

int test_multiple_in_expr()
{
    int i = 0;
    int j = 10;
    int a;
    a = ++i + j--;
    /* i should be 1, j should be 9, a should be 11 (1 + 10) */
    return a;
}

int test_in_array_subscript()
{
    int arr[10];
    int i = 0;
    arr[i++] = 5;
    arr[i++] = 10;
    /* i should be 2, arr[0]=5, arr[1]=10 */
    return i;
}

int test_prefix_postfix_same()
{
    int i = 5;
    int a = i++;
    int b = ++i;
    /* a=5, i after first is 6, then b=7, i=7 */
    return b;
}

int test_parens()
{
    int i = 5;
    int a;
    a = (i++);
    /* a should be 5, i should be 6 */
    return a;
}
