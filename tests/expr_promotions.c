/*
 * Test automatic type promotion in binary expressions
 */

char c;
int i;
long l;
unsigned char uc;
unsigned int ui;

int test_arithmetic_promotions()
{
    int result;

    /* Arithmetic operations - smaller operand should be widened */
    result = c + i;     /* char + int: char widened to int */
    result = i + l;     /* int + long: int widened to long */
    result = c + l;     /* char + long: char widened to long */

    result = c - i;     /* char - int: char widened to int */
    result = c * i;     /* char * int: char widened to int */
    result = c / i;     /* char / int: char widened to int */
    result = c % i;     /* char % int: char widened to int */

    return 0;
}

int test_bitwise_promotions()
{
    int result;

    /* Bitwise operations - smaller operand should be widened */
    result = c & i;     /* char & int: char widened to int */
    result = c | i;     /* char | int: char widened to int */
    result = c ^ i;     /* char ^ int: char widened to int */
    result = c << i;    /* char << int: char widened to int */
    result = i >> c;    /* int >> char: char widened to int */

    return 0;
}

int test_comparison_promotions()
{
    int result;

    /* Comparison operations - smaller operand should be widened */
    result = c < i;     /* char < int: char widened to int */
    result = c > i;     /* char > int: char widened to int */
    result = c <= i;    /* char <= int: char widened to int */
    result = c >= i;    /* char >= int: char widened to int */
    result = c == i;    /* char == int: char widened to int */
    result = c != i;    /* char != int: char widened to int */

    return 0;
}

int test_unsigned_promotions()
{
    unsigned int result;

    /* Unsigned operands should use WIDEN (zero extend) */
    result = uc + ui;   /* unsigned char + unsigned int: uc widened with WIDEN */
    result = uc * ui;   /* unsigned char * unsigned int: uc widened with WIDEN */

    return 0;
}

int test_mixed_size_chain()
{
    long result;

    /* Chain of operations with different sizes */
    result = c + i + l; /* (char + int) + long */

    return 0;
}
