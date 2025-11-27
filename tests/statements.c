/*
 * Tests comprehensive statement parsing with all control flow constructs
 */

// Test statement parsing - if/else, loops, switch, break, continue, return, goto

// Simple if statement
int test_if(int x) {
    if (x > 0) {
        return 1;
    }
}

// if-else statement
int tIfElse(int x) {
    if (x > 0) {
        return 1;
    } else {
        return 0;
    }
}

// if-else-if chain
int tIfElse(int x) {
    if (x > 0) {
        return 1;
    } else if (x < 0) {
        return -1;
    } else {
        return 0;
    }
}

// Nested if statements
int tNestedIf(int x, int y) {
    if (x > 0) {
        if (y > 0) {
            return 1;
        }
    }
    return 0;
}

// while loop
int tWhile(int n) {
    int i = 0;
    while (i < n) {
        i = i + 1;
    }
    return i;
}

// do-while loop
int tDoWhile(int n) {
    int i = 0;
    do {
        i = i + 1;
    } while (i < n);
    return i;
}

// for loop
int tFor(int n) {
    int i;
    int sum = 0;
    for (i = 0; i < n; i = i + 1) {
        sum = sum + i;
    }
    return sum;
}

// for loop with empty expressions
int tFor_empty() {
    int i = 0;
    for (;;) {
        if (i >= 10) break;
        i = i + 1;
    }
    return i;
}

// break statement
int test_break(int n) {
    int i = 0;
    while (1) {
        if (i >= n) break;
        i = i + 1;
    }
    return i;
}

// continue statement
int test_continue(int n) {
    int i = 0;
    int count = 0;
    while (i < n) {
        i = i + 1;
        if (i % 2 == 0) continue;
        count = count + 1;
    }
    return count;
}

// switch statement
int tSwitch(int x) {
    switch (x) {
        case 0:
            return 100;
        case 1:
            return 200;
        case 2:
            return 300;
        default:
            return -1;
    }
}

// switch with fall-through
int tSwFall(int x) {
    int result = 0;
    switch (x) {
        case 0:
        case 1:
            result = 10;
            break;
        case 2:
            result = 20;
            break;
        default:
            result = -1;
    }
    return result;
}

// goto and labels
int tGoto(int n) {
    int i = 0;
    loop_start:
    if (i >= n) goto loop_end;
    i = i + 1;
    goto loop_start;
    loop_end:
    return i;
}

// Nested loops
int tNestLoop(int m, int n) {
    int i, j;
    int count = 0;
    for (i = 0; i < m; i = i + 1) {
        for (j = 0; j < n; j = j + 1) {
            count = count + 1;
        }
    }
    return count;
}

// Block statements
int test_blocks(int x) {
    int result = 0;
    {
        int temp = x * 2;
        result = temp + 5;
    }
    return result;
}

// Multiple statements in if/else
int tCplxIf(int x) {
    if (x > 10) {
        int a = x * 2;
        int b = a + 5;
        return b;
    } else {
        int c = x + 1;
        return c;
    }
}

// return with expressions
int tRetExpr(int x, int y) {
    return x * y + 10;
}

// return without expression
void tRetVoid() {
    return;
}
