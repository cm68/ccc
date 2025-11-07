/*
 * Basic increment and decrement operators
 * Tests both prefix and postfix forms
 */

int test_postfix_incr()
{
    int i = 5;
    i++;
    return i;  /* should be 6 */
}

int test_prefix_incr()
{
    int i = 5;
    ++i;
    return i;  /* should be 6 */
}

int test_postfix_decr()
{
    int i = 5;
    i--;
    return i;  /* should be 4 */
}

int test_prefix_decr()
{
    int i = 5;
    --i;
    return i;  /* should be 4 */
}

int test_multiple()
{
    int i = 0;
    i++;
    i++;
    ++i;
    i--;
    return i;  /* should be 2 */
}
