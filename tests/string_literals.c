/* Test string literals with various escape sequences and contexts */

/* Global string literals */
char *global1 = "hello world";
char *global2 = "test";

/* String with escape sequences */
char *escaped = "line1\nline2\ttab\"quote\\backslash";

/* Empty string */
char *empty = "";

int main() {
    /* Local string literals */
    char *local1 = "local string";
    char *local2 = "another\nstring";

    /* String in expression */
    char *result = local1;

    return 0;
}
