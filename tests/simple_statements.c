/*
 * Tests K&R style functions with forward declarations and basic statements
 */

// Simpler statement test - forward declarations and K&R style

// Forward declarations (these work)
int test_if();
int test_while();
int test_for();
int test_switch();
void test_return();

// K&R style function with simple statements
int simple()
{
}

int with_return()
{
    return 42;
}

// Variables with initializers
int x = 10;
int y = 20;
int z = x + y;

// Arrays and pointers
int arr[10];
int *ptr;
char *str = "hello";
