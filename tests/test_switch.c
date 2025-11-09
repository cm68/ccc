/* Test switch statement */

int main()
{
    int value;
    int result;

    result = 0;

    /* Test case 2 */
    value = 2;
    switch (value) {
        case 1:
            result = 10;
            break;
        case 2:
            result = 20;
            break;
        case 3:
            result = 30;
            break;
        default:
            result = 99;
            break;
    }

    /* Add test for fallthrough */
    value = 5;
    switch (value) {
        case 5:
            result = result + 100;  /* Falls through */
        case 6:
            result = result + 200;  /* Falls through */
        case 7:
            result = result + 300;
            break;
        default:
            result = result + 1000;
    }

    return result;  /* Should return 20 + 100 + 200 + 300 = 620 */
}
