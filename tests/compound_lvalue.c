/*
 * Test that compound assignment evaluates lvalue only once
 * This is critical for correctness when lvalue has side effects
 */

int counter = 0;

int* get_ptr()
{
    counter++;
    return &counter;
}

int test_simple_compound()
{
    int i = 10;
    i += 5;      /* i = 15 */
    i -= 3;      /* i = 12 */
    i *= 2;      /* i = 24 */
    i /= 4;      /* i = 6 */
    i %= 4;      /* i = 2 */
    return i;
}

int test_pointer_compound()
{
    int arr[10];
    int *p = arr;
    arr[0] = 5;

    *p += 10;    /* arr[0] = 15 */
    return arr[0];
}

int test_array_subscript()
{
    int arr[10];
    int i = 0;

    arr[0] = 100;
    arr[i] += 50;    /* arr[0] = 150 */
    return arr[0];
}

int test_complex_lvalue()
{
    int arr[10];
    int *p = arr;

    arr[0] = 20;
    *p++ += 5;       /* arr[0] = 25, p now points to arr[1] */
    return arr[0];   /* should be 25 */
}

int test_function_call()
{
    counter = 10;
    *get_ptr() += 5;   /* counter should be incremented once (to 11), then += 5 makes it 16 */
    return counter;    /* should be 16, NOT 17 */
}
