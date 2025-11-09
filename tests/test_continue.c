/* Test continue statement */

int main()
{
    int sum;
    int i;

    sum = 0;

    /* Skip even numbers, sum only odd */
    for (i = 1; i <= 10; i = i + 1) {
        if ((i % 2) == 0) {
            continue;
        }
        sum = sum + i;
    }

    return sum;  /* Should return 25 (1+3+5+7+9) */
}
