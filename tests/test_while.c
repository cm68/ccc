/* Test while loops */

int main()
{
    int sum;
    int i;

    sum = 0;
    i = 1;

    /* Sum 1 to 10 */
    while (i <= 10) {
        sum = sum + i;
        i = i + 1;
    }

    return sum;  /* Should return 55 */
}
