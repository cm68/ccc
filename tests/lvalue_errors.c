/*
 * Test lvalue validation - these should all produce errors
 */

int test_invalid_assignment()
{
    int x = 5;

    /* ERROR: Assigning to a constant (not an lvalue) */
    10 = x;

    /* ERROR: Assigning to expression result (not an lvalue) */
    (x + 5) = 10;

    return 0;
}

int test_invalid_increment()
{
    int x = 5;

    /* ERROR: Incrementing a constant (not an lvalue) */
    ++10;
    10++;

    /* ERROR: Incrementing expression result (not an lvalue) */
    ++(x + 5);
    (x + 5)++;

    return 0;
}

int test_invalid_decrement()
{
    int x = 5;

    /* ERROR: Decrementing a constant (not an lvalue) */
    --10;
    10--;

    /* ERROR: Decrementing expression result (not an lvalue) */
    --(x + 5);
    (x + 5)--;

    return 0;
}

int test_invalid_compound_assign()
{
    int x = 5;

    /* ERROR: Compound assignment to constant (not an lvalue) */
    10 += x;

    /* ERROR: Compound assignment to expression result (not an lvalue) */
    (x + 5) += 10;

    return 0;
}
