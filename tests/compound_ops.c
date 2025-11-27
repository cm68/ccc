/* Test all compound assignment operators */

int x, y, z;

int tCmpndOps()
{
    int a;
    
    /* Arithmetic compound assignments */
    a = 10;
    a += 5;     /* a = 15 */
    a -= 3;     /* a = 12 */
    a *= 2;     /* a = 24 */
    a /= 4;     /* a = 6 */
    a %= 4;     /* a = 2 */
    
    /* Bitwise compound assignments */
    a = 0xFF;
    a &= 0x0F;  /* a = 0x0F */
    a |= 0xF0;  /* a = 0xFF */
    a ^= 0xAA;  /* a = 0x55 */
    
    /* Shift compound assignments */
    a = 8;
    a <<= 2;    /* a = 32 */
    a >>= 1;    /* a = 16 */
    
    return a;
}
