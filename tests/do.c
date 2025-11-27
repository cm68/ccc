/*
 * Tests do-while loop statement
 */

int tDoWhile(int n) {
    int i = 0;
    do {
        i = i + 1;
    } while (i < n);
    return i;
}
