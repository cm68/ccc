/*
 * Test string literal concatenation
 * Tests various forms of adjacent string literal concatenation
 */

/* Test 1: Simple concatenation */
char *s1 = "Hello" "World";

/* Test 2: Multi-line with whitespace */
char *s2 = "Line1\n"
           "Line2\n"
           "Line3\n";

/* Test 3: Block comment between strings */
char *s3 = "Before" /* comment */ "After";

/* Test 4: Line comment between strings */
char *s4 = "First"  // comment
           "Second";

/* Test 5: Multiple comments and whitespace */
char *s5 = "A"
           /* comment 1 */
           "B"
           // comment 2
           "C";

/* Test 6: Multi-line strings in function */
int main() {
    char buf[256];
    char *p = "\tld l, a\n"
              "\trlca\n"
              "\tsbc a, a\n"
              "\tld h, a";
    return 0;
}
