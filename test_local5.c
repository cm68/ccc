// Test statement parsing

int test_if(int x) {
    if (x > 0) {
        return 1;
    }
}

int test_while(int n) {
    int i = 0;
    while (i < n) {
        i = i + 1;
    }
    return i;
}
