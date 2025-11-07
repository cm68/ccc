/*
 * Increment and decrement with pointers
 * Tests pointer arithmetic and dereferencing combinations
 */

int test_ptr_postfix()
{
    int arr[5];
    int *p = arr;
    p++;
    p++;
    /* p should point to arr[2] */
    return 0;
}

int test_ptr_prefix()
{
    int arr[5];
    int *p = arr;
    ++p;
    ++p;
    /* p should point to arr[2] */
    return 0;
}

int test_deref_postfix()
{
    int i = 5;
    int *p = &i;
    (*p)++;
    /* i should be 6 */
    return i;
}

int test_deref_prefix()
{
    int i = 5;
    int *p = &i;
    ++(*p);
    /* i should be 6 */
    return i;
}

int test_ptr_then_deref()
{
    int arr[5];
    int *p = arr;
    arr[0] = 10;
    arr[1] = 20;
    arr[2] = 30;
    *p++;
    /* p should now point to arr[1] */
    return *p;  /* should be 20 */
}

int test_deref_ptr_incr()
{
    int arr[5];
    int *p = arr;
    arr[0] = 10;
    arr[1] = 20;
    *++p;
    /* p should now point to arr[1] */
    return *p;  /* should be 20 */
}

int test_incr_deref()
{
    int arr[5];
    int *p = arr;
    arr[0] = 10;
    ++*p;
    /* arr[0] should be 11 */
    return arr[0];
}

int test_complex_ptr()
{
    int arr[5];
    int *p = arr;
    arr[0] = 5;
    arr[1] = 10;
    arr[2] = 15;
    int a = *p++;
    /* a = 5, p points to arr[1] */
    int b = *++p;
    /* p points to arr[2], b = 15 */
    return b;
}

int test_char_ptr()
{
    char str[10];
    char *p = str;
    p++;
    ++p;
    /* p should be at str[2] */
    return 0;
}

int test_deref_then_incr()
{
    int arr[5];
    int *p = arr;
    arr[0] = 10;
    arr[1] = 20;
    int a = (*p)++;
    /* a = 10, arr[0] = 11 */
    return a;
}
