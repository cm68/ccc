/* Test short-circuit evaluation for && and || operators */

int main() {
    unsigned char a = 1;
    unsigned char b = 0;
    unsigned char c = 1;
    unsigned char d = 0;

    /* Test 1: && should short-circuit when first operand is false */
    if (b && a) {
        return 1;  /* Should not reach - b is false */
    }

    /* Test 2: || should short-circuit when first operand is true */
    if (a || b) {
        /* Should reach here - a is true */
    } else {
        return 2;
    }

    /* Test 3: Nested (a && b) || (c && d) */
    /* a && b = 1 && 0 = 0, c && d = 1 && 0 = 0, result = 0 || 0 = 0 */
    if ((a && b) || (c && d)) {
        return 3;  /* Should not reach */
    }

    /* Test 4: Nested a || (b && c) || d */
    /* a = 1, so entire expression short-circuits to true */
    if (a || (b && c) || d) {
        /* Should reach here */
    } else {
        return 4;
    }

    /* Test 5: Nested a && (b || c) && d */
    /* a = 1, (b || c) = 0 || 1 = 1, d = 0, result = 1 && 1 && 0 = 0 */
    if (a && (b || c) && d) {
        return 5;  /* Should not reach - d is false */
    }

    /* Test 6: Triple nesting with short-circuit */
    /* (a && b) = 0, short-circuits entire expression to false */
    if ((a && b) && (c || d)) {
        return 6;  /* Should not reach */
    }

    /* Test 7: 6-level deep nesting - should be TRUE */
    /* ((((((a || b) && c) || d) && a) || b) && c) */
    /* (1||0)=1, (1&&1)=1, (1||0)=1, (1&&1)=1, (1||0)=1, (1&&1)=1 */
    if (((((((a || b) && c) || d) && a) || b) && c)) {
        /* Should reach here */
    } else {
        return 7;
    }

    /* Test 8: 6-level deep nesting - should be FALSE */
    /* ((((((b && a) || d) && c) || b) && a) || d) */
    /* (0&&1)=0, (0||0)=0, (0&&1)=0, (0||0)=0, (0&&1)=0, (0||0)=0 */
    if (((((((b && a) || d) && c) || b) && a) || d)) {
        return 8;  /* Should not reach */
    }

    /* If we reach here, all tests passed */
    return 42;
}
