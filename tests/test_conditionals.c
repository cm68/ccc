/* Test if/else conditionals */

int main()
{
    int result;
    int a;
    int b;

    /* Simple if */
    a = 10;
    if (a > 5) {
        result = 100;
    }

    /* if/else */
    b = 3;
    if (b > 5) {
        result = 200;
    } else {
        result = 300;
    }

    /* Nested if */
    if (a > 5) {
        if (b < 5) {
            result = 400;
        }
    }

    /* if/else if/else chain */
    if (a < 0) {
        result = 1;
    } else if (a < 5) {
        result = 2;
    } else if (a < 15) {
        result = 500;  /* This should match */
    } else {
        result = 4;
    }

    return result;  /* Should return 500 */
}
