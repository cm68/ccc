/*
 * Tests K&R style (old-style) function declarations with various parameter configurations
 */

// Test K&R style (old-style) function declarations

// K&R style with single parameter (WORKS)
int simple(x)
int x;
{
}

// K&R style with no parameters (WORKS)
int noparams()
{
}

// Modern ANSI style for comparison (these work)
int modern1(int x);
int modern2(int a, char b, float c);
char *modern3(char *s, int n);

// TODO: Multi-parameter K&R functions not yet fully supported
// The parser has issues with multiple K&R parameters - needs more work
// int multi(a, b, c)
// int a;
// char b;
// float c;
// {
// }
