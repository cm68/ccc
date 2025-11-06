/* Test array initialization with string literals */

/* Array with inferred size from string */
char foo[] = "string";
char bar[] = "test";
char empty[] = "";
char escaped[] = "hello\nworld";

int main() {
    /* Local array with inferred size */
    char local[] = "local";
    char long_string[] = "this is a longer string";

    return 0;
}
