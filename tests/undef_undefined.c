/*
 * Test that using an undefined macro after #undef produces an error
 * This test is expected to fail - it tests error detection
 */

#define TESTMACRO 100
int before_undef = TESTMACRO;

#undef TESTMACRO

/* After #undef, TESTMACRO is undefined and should cause an error */
int after_undef = TESTMACRO;  /* Should be undefined identifier error */
