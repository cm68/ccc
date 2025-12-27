/* Test goto labels with blank line after */
int foo(int c) {
    switch (c) {
    case 1:
        goto again;

    default:
        return(c);
    }
again:
    c = c + 1;
    return c;
}
