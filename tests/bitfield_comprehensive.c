/* Comprehensive bitfield test */

/* Test 1: Basic bitfield packing */
struct flags {
    int enable : 1;
    int mode : 3;
    int count : 4;
};

/* Test 2: Multiple bitfields spanning word boundary */
struct packed {
    int a : 5;
    int b : 8;
    int c : 6;  /* This should spill to next word */
    int d : 2;
};

/* Test 3: Mixed bitfields and regular members */
struct mixed {
    int flag : 1;
    int value;      /* Regular int forces alignment */
    int bits : 3;
};

/* Test 4: Reading and writing bitfields */
int tRdWr() {
    struct flags f;

    /* Write to bitfields */
    f.enable = 1;
    f.mode = 7;
    f.count = 15;

    /* Read from bitfields */
    int e = f.enable;
    int m = f.mode;
    int c = f.count;

    return e + m + c;  /* Should return 1 + 7 + 15 = 23 */
}

/* Test 5: Bitfield operations in expressions */
int tExpr() {
    struct flags f;

    f.enable = 0;
    f.mode = 3;
    f.count = 5;

    /* Use bitfields in arithmetic */
    int result = (f.mode * 2) + f.count;  /* 3*2 + 5 = 11 */

    /* Modify through expression */
    f.count = f.count + 3;  /* 5 + 3 = 8 */

    return result + f.count;  /* 11 + 8 = 19 */
}

/* Test 6: Bitfield assignment from variables */
int tAssign() {
    struct flags f;
    int val = 5;

    f.mode = val;
    f.count = val + 2;

    return f.mode + f.count;  /* 5 + 7 = 12 */
}

/* Test 7: Wide bitfields */
struct wide {
    int bits : 12;
};

int tWideBit() {
    struct wide w;
    w.bits = 4095;  /* Max value for 12 bits */
    return w.bits;  /* Should return 4095 */
}

/* Test 8: Multiple bit extractions */
int tMultiRd() {
    struct packed p;

    p.a = 31;   /* 5 bits max */
    p.b = 255;  /* 8 bits max */
    p.c = 63;   /* 6 bits max */
    p.d = 3;    /* 2 bits max */

    /* Multiple reads in one expression */
    return p.a + p.b + p.c + p.d;  /* 31 + 255 + 63 + 3 = 352 */
}

/* Test 9: Conditional with bitfields */
int tCond() {
    struct flags f;
    f.enable = 1;
    f.mode = 2;

    if (f.enable) {
        return f.mode * 10;  /* 20 */
    } else {
        return 0;
    }
}

/* Test 10: Bitfield comparison */
int tCompare() {
    struct flags f1, f2;

    f1.mode = 5;
    f2.mode = 3;

    if (f1.mode > f2.mode) {
        return 1;
    } else {
        return 0;
    }
}

int main() {
    int result = 0;

    result += tRdWr();      /* 23 */
    result += tExpr();     /* 19 */
    result += tAssign();      /* 12 */
    result += tWideBit();   /* 4095 */
    result += tMultiRd();  /* 352 */
    result += tCond();     /* 20 */
    result += tCompare();      /* 1 */

    /* Total: 23 + 19 + 12 + 4095 + 352 + 20 + 1 = 4522 */
    return result;
}
