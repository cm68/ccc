/* Test break statement */

int main()
{
    int sum;
    int i;

    sum = 0;

    /* Break out of loop early */
    for (i = 1; i <= 100; i = i + 1) {
        if (i > 10) {
            break;
        }
        sum = sum + i;
    }

    return sum;  /* Should return 55 (sum of 1 to 10) */
}
