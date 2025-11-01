/*
 * Tests do-while loop statement
 */

int test_do_while(int n) {
    int i = 0;
    do {
        i = i + 1;
    } while (i < n);
    return i;
}
