/*
 * Basic increment and decrement operators
 * Tests both prefix and postfix forms
 */

int tPostIncr()
{
    int i = 5;
    i++;
    return i;  /* should be 6 */
}

int tPreIncr()
{
    int i = 5;
    ++i;
    return i;  /* should be 6 */
}

int tPostDecr()
{
    int i = 5;
    i--;
    return i;  /* should be 4 */
}

int tPreDecr()
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
