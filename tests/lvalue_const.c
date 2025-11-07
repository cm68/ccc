/*
 * Invalid lvalue test - assign to constant
 */

int test()
{
    5 = 10;   /* This should error - 5 is not an lvalue */
    return 0;
}
