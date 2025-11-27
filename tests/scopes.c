/* Test variable scoping - function and block scope */

int tFuncVars(int p) {
    int a;
    int b;
    a = 10;
    b = 20;
    return a + b + p;
}

int tBlkVars(void) {
    int outer;
    outer = 5;
    {
        int inner;
        inner = 10;
        outer = outer + inner;
    }
    return outer;
}

int test_nested(void) {
    int x;
    x = 1;
    {
        int y;
        y = 2;
        {
            int z;
            z = 3;
            x = x + y + z;
        }
    }
    return x;
}
