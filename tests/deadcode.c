/* Test dead code elimination for constant conditionals */

int result;

void testIfZero() {
    if (0) {
        result = 999;  /* Should be eliminated */
    }
}

void testIfZElse() {
    if (0) {
        result = 888;  /* Should be eliminated */
    } else {
        result = 42;   /* Should become unconditional */
    }
}

void testIfOne() {
    if (1) {
        result = 100;  /* Should become unconditional */
    }
}

void testIf1Else() {
    if (1) {
        result = 200;  /* Should become unconditional */
    } else {
        result = 777;  /* Should be eliminated */
    }
}

void testNested() {
    if (0) {
        if (result) {
            result = 111;
        }
    } else {
        result = 300;
    }
}

void testConst() {
    if (2 + 2 == 5) {
        result = 666;  /* Should be eliminated (2+2==5 is 0) */
    } else {
        result = 400;  /* Should become unconditional */
    }
}
