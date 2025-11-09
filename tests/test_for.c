/* Test for loops */

int main()
{
    int sum;
    int i;
    int j;

    sum = 0;

    /* Simple for loop: sum 1 to 10 */
    for (i = 1; i <= 10; i = i + 1) {
        sum = sum + i;
    }

    /* Nested for loop */
    for (i = 1; i <= 3; i = i + 1) {
        for (j = 1; j <= 2; j = j + 1) {
            sum = sum + 1;
        }
    }

    return sum;  /* Should return 55 + 6 = 61 */
}
