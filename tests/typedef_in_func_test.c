/*
 * Test typedef inside function body
 */

void test_fn()
{
    typedef int myint;
    myint x;
    x = 5;
}

void another_fn()
{
    typedef char mybyte;
    mybyte b;
    b = 10;
}
