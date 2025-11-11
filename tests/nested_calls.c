/* Test nested function calls */
int xyzzy() { return 10; }
int twee(int x) { return x + 1; }
int bar() { return 20; }
int foo(int a, int b, int c) { return a + b + c; }

int main() {
    return foo(bar(), 4, twee(xyzzy()));
}
