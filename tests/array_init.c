/* Test array initialization with string literals */

/* Array with inferred size from string */
char foo[] = "string";
char bar[] = "test";
char empty[] = "";
char escaped[] = "hello\nworld";

/*
 * No local arrays here: auto aggregate initializers are a documented
 * compiler restriction (see ccc/RESTRICTIONS.md), not a gap to test.
 */
int main() {
    return 0;
}
