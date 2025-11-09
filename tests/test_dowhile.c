/* Test do-while loops */

int main()
{
    int sum;
    int i;

    sum = 0;
    i = 1;

    /* Sum 1 to 10 */
    do {
        sum = sum + i;
        i = i + 1;
    } while (i <= 10);

    /* Test that do-while executes at least once */
    i = 100;
    do {
        sum = sum + 5;
    } while (i < 50);  /* Condition false, but body runs once */

    return sum;  /* Should return 55 + 5 = 60 */
}
