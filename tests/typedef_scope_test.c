/*
 * Test typedef scoping inside functions
 */

/* Global typedef */
typedef int global_t;

void test_scope()
{
    /* Local typedef shadows global */
    typedef char global_t;

    /* Use local typedef */
    global_t x;  /* should be char */
    x = 'A';

    {
        /* Nested scope - another typedef */
        typedef long global_t;
        global_t y;  /* should be long */
        y = 100;
    }

    /* Back to outer scope - should be char again */
    global_t z;  /* should be char */
    z = 'B';
}

void another_test()
{
    /* Different function - should see global typedef */
    global_t w;  /* should be int */
    w = 42;
}
