/* Test compound assignment precedence and associativity */
int x, y, z;

int test()
{
    /* Right associativity: a += b += c should be a += (b += c) */
    x = y = z = 5;
    
    /* Compound assignments have same precedence as = */
    x += y += 1;  /* Should be: x += (y += 1) */
    
    return x;
}
