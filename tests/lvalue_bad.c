/*
 * Invalid lvalue test
 */

int test()
{
    int x;
    5 = x;   /* This should error - 5 is not an lvalue */
    return 0;
}
