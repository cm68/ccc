/*
 * Test asm block support with macro expansion
 */

#define REG_A 0x10
#define REG_B 0x20

int test_asm(void)
{
    int result;

    asm {
        ld a, REG_A
        ld b, REG_B
        add a, b
        ld (result), a
    }

    return result;
}

int test_nested_braces(void)
{
    asm {
        if (condition) {
            do_something();
        }
    }

    return 0;
}

int main(void)
{
    int x = test_asm();
    int y = test_nested_braces();
    return x + y;
}
