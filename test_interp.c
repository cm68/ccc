/*
 * Simple test program for the Lisp interpreter
 */

int factorial(int n)
{
    if (n <= 1) {
        return 1;
    }
    return n * factorial(n - 1);
}

int main()
{
    int result;
    result = factorial(5);
    return result;  /* Should return 120 */
}
