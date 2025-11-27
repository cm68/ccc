/*
 * Tests K&R style functions with progressive complexity (empty, return, locals, if, if-else)
 */

// Statement tests using K&R function style (which works)

// Empty function
int empty()
{
}

// Function with return statement
int return_value()
{
    return 42;
}

// Function with local variables
int locals()
{
    int x;
    int y;
    x = 10;
    y = 20;
    return x + y;
}

// Function with if statement  
int test_if(n)
int n;
{
    if (n > 0)
        return 1;
    return 0;
}

// Function with if-else
int tIfElse(n)
int n;
{
    if (n > 0)
        return 1;
    else
        return -1;
}
