/*
 * Test automatic type conversions in assignments
 */

char c;
int i;
long l;
unsigned char uc;
unsigned int ui;

int t_narrow()
{
    /* Narrowing conversions - should emit NARROW */
    c = i;      // int (2) -> char (1): NARROW
    c = l;      // long (4) -> char (1): NARROW
    i = l;      // long (4) -> int (2): NARROW

    return 0;
}

int t_widen_sign()
{
    /* Widening signed - should emit SEXT (sign extend) */
    i = c;      // char (1) -> int (2): SEXT
    l = c;      // char (1) -> long (4): SEXT
    l = i;      // int (2) -> long (4): SEXT

    return 0;
}

int t_widen_unsig()
{
    /* Widening unsigned - should emit WIDEN (zero extend) */
    ui = uc;    // unsigned char (1) -> unsigned int (2): WIDEN

    return 0;
}

int t_same_size()
{
    /* Same size - no conversion needed */
    i = 5;
    c = 'A';

    return 0;
}

int t_compound()
{
    /* Compound assignments should also get conversions */
    c = 10;
    c += i;     // int operand, but result is char

    return 0;
}

int t_local_conv()
{
    char lc;
    int li;
    long ll;

    /* Local variable conversions */
    lc = li;    // int -> char: NARROW
    li = lc;    // char -> int: SEXT
    ll = li;    // int -> long: SEXT
    li = ll;    // long -> int: NARROW

    return 0;
}
