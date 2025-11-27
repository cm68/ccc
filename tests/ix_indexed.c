/* Test case for IX-indexed addressing optimization
 *
 * Tests code generation for struct member access patterns where the
 * base pointer has been allocated to the IX register. The compiler
 * should generate efficient IX-indexed addressing instructions:
 *   ld (ix+offset), value
 * instead of computing the address via arithmetic and then storing.
 *
 * Example AST pattern: (=:s (+ (M:p $var) offset) value)
 * Desired output:  ld (ix + offset), l
 *                  ld (ix + offset+1), h
 */

struct test_struct {
    int field0;      /* offset 0 */
    int field1;      /* offset 2 */
    int field2;      /* offset 4 */
    int field3;      /* offset 6 */
    int field4;      /* offset 8 */
    int field5;      /* offset 10 */
    char field6;     /* offset 12 */
};

/* Test function that writes to offset 10 (field5)
 * This should generate: (=:s (+ (M:p $Ain) 10) value)
 */
void xreplace_test(struct test_struct *in) {
    /* Write to field at offset 10 - should use ld (ix+10), ... */
    in->field5 = 42;

    /* Read from field at offset 10 - should use ld l, (ix+10); ld h, (ix+11) */
    int val = in->field5;

    /* Use val to prevent optimization */
    if (val) {
        in->field0 = val;
    }
}

/* Additional test with various offsets */
void tMultiOff(struct test_struct *ptr) {
    ptr->field0 = 1;   /* offset 0 */
    ptr->field1 = 2;   /* offset 2 */
    ptr->field2 = 3;   /* offset 4 */
    ptr->field3 = 4;   /* offset 6 */
    ptr->field4 = 5;   /* offset 8 */
    ptr->field5 = 6;   /* offset 10 - the specific case mentioned */
    ptr->field6 = 7;   /* offset 12 - byte access */
}

/* Test byte access at various offsets */
void tByteAcc(struct test_struct *ptr) {
    ptr->field6 = 'X';  /* offset 12 - should use ld (ix+12), a */

    char c = ptr->field6;  /* should use ld a, (ix+12) */

    if (c) {
        ptr->field6 = c + 1;
    }
}
