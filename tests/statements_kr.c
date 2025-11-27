/*
 * Statement tests using K&R function style (which works)
 */

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

// Function with while loop
int tWhile(n)
int n;
{
    int i;
    i = 0;
    while (i < n)
        i = i + 1;
    return i;
}

// Function with for loop
int tFor(n)
int n;
{
    int i;
    int sum;
    sum = 0;
    for (i = 0; i < n; i = i + 1)
        sum = sum + i;
    return sum;
}

// Function with break
int test_break(n)
int n;
{
    int i;
    i = 0;
    while (1) {
        if (i >= n)
            break;
        i = i + 1;
    }
    return i;
}

// Function with continue
int test_continue(n)
int n;
{
    int i;
    int count;
    i = 0;
    count = 0;
    while (i < n) {
        i = i + 1;
        if (i == 5)
            continue;
        count = count + 1;
    }
    return count;
}

// Function with switch
int tSwitch(n)
int n;
{
    switch (n) {
        case 0:
            return 100;
        case 1:
            return 200;
        default:
            return -1;
    }
}

// Function with goto and label
int tGoto(n)
int n;
{
    int i;
    i = 0;
start:
    if (i >= n)
        goto end;
    i = i + 1;
    goto start;
end:
    return i;
}

// Function with nested blocks
int test_blocks()
{
    int x;
    x = 0;
    {
        int y;
        y = 10;
        x = y + 5;
    }
    return x;
}

// Function with do-while
int tDoWhile(n)
int n;
{
    int i;
    i = 0;
    do {
        i = i + 1;
    } while (i < n);
    return i;
}
