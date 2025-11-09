/*
 * Test arithmetic operations with different type widths
 * Tests char (byte), short (word), and long (dword) arithmetic
 */

int main()
{
    char c1;
    char c2;
    char c3;
    short s1;
    short s2;
    short s3;
    long l1;
    long l2;
    long l3;
    int result;

    /* Byte arithmetic */
    c1 = 10;
    c2 = 20;
    c3 = c1 + c2;           /* 30 */

    /* Short arithmetic */
    s1 = 100;
    s2 = 200;
    s3 = s1 + s2;           /* 300 */

    /* Long arithmetic */
    l1 = 1000;
    l2 = 2000;
    l3 = l1 + l2;           /* 3000 */

    /* Mixed width operations - promotions */
    result = c3 + s3 + l3;  /* 30 + 300 + 3000 = 3330 */

    /* More byte operations */
    c1 = 5;
    c2 = 3;
    c3 = c1 * c2;           /* 15 */

    /* More short operations */
    s1 = 10;
    s2 = 7;
    s3 = s1 - s2;           /* 3 */

    /* Combine results */
    result = result + c3 + s3;  /* 3330 + 15 + 3 = 3348 */

    /* Test division and modulo */
    l1 = 100;
    l2 = 7;
    l3 = l1 / l2;           /* 14 */

    s1 = 100;
    s2 = 7;
    s3 = s1 % s2;           /* 2 */

    result = result + l3 + s3;  /* 3348 + 14 + 2 = 3364 */

    /* Test bitwise operations */
    c1 = 15;                /* 0b00001111 */
    c2 = 3;                 /* 0b00000011 */
    c3 = c1 & c2;           /* 0b00000011 = 3 */

    s1 = 255;
    s2 = 15;
    s3 = s1 | s2;           /* 255 */

    result = result + c3 + s3;  /* 3364 + 3 + 255 = 3622 */

    return result;
}
