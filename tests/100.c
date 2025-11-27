/*
 * Tests comprehensive statement parsing including control flow and jump statements
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
