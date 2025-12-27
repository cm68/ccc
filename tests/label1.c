int foo(c) int c; {
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
