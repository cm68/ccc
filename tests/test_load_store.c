/* Comprehensive test for loads and stores of 8, 16, and 32-bit values
 * across global, argument, and local storage
 */

/* ===== GLOBAL VARIABLES ===== */
char global_char;
int global_int;
long global_long;

/* ===== TEST 1: GLOBAL STORES ===== */

void t_gsto_char(char value) {
    global_char = value;  /* Should: ld (global_char), a */
}

void t_gsto_int(int value) {
    global_int = value;   /* Should: ld (global_int), hl */
}

void t_gsto_long(long value) {
    global_long = value;  /* Should: ld (global_long), hl; exx; ld (global_long+2), hl; exx */
}

/* ===== TEST 2: GLOBAL LOADS ===== */

char t_gld_char() {
    return global_char;   /* Should: ld a, (global_char) */
}

int t_gld_int() {
    return global_int;    /* Should: ld hl, (global_int) */
}

long t_gld_long() {
    return global_long;   /* Should: ld hl, (global_long); exx; ld hl, (global_long+2); exx */
}

/* ===== TEST 3: ARGUMENT LOADS (reading parameters) ===== */

char t_ald_char(char arg) {
    return arg;           /* If on stack: ld a, (iy+N); if in register: ld a, b/c */
}

int t_ald_int(int arg) {
    return arg;           /* If on stack: ld hl, (iy+N); if in register: ld h,b; ld l,c */
}

long t_ald_long(long arg) {
    return arg;           /* If on stack: ld a, N; call getlong */
}

/* ===== TEST 4: ARGUMENT STORES (writing to parameters - unusual but valid) ===== */

void t_asto_char(char arg) {
    arg = 42;             /* If on stack: ld a, 42; ld (iy+N), a */
}

void t_asto_int(int arg) {
    arg = 100;            /* If on stack: ld hl, 100; ld (iy+N), hl */
}

void t_asto_long(long arg) {
    arg = 12345L;         /* If on stack: ld hl, 12345; exx; ld hl, 0; exx; ld a, N; call putlong */
}

/* ===== TEST 5: LOCAL STORES ===== */

void t_lsto_char() {
    char local;
    local = 99;           /* Should: ld a, 99; ld (iy-N), a */
}

void t_lsto_int() {
    int local;
    local = 500;          /* Should: ld hl, 500; ld (iy-N), hl */
}

void t_lsto_long() {
    long local;
    local = 98765L;       /* Should: ld hl'hl, value; ld a, -N; call putlong */
}

/* ===== TEST 6: LOCAL LOADS ===== */

char t_lld_char() {
    char local;
    local = 55;
    return local;         /* Should: ld a, (iy-N) */
}

int t_lld_int() {
    int local;
    local = 777;
    return local;         /* Should: ld hl, (iy-N) */
}

long t_lld_long() {
    long local;
    local = 11111L;
    return local;         /* Should: ld a, -N; call getlong */
}

/* ===== TEST 7: MIXED OPERATIONS ===== */

/* Global to local */
void t_g2l_char() {
    char local;
    local = global_char;  /* ld a, (global); ld (iy-N), a */
}

void t_g2l_int() {
    int local;
    local = global_int;   /* ld hl, (global); ld (iy-N), hl */
}

void t_g2l_long() {
    long local;
    local = global_long;  /* ld hl, (global); exx; ld hl, (global+2); exx; ld a, -N; call putlong */
}

/* Local to global */
void t_l2g_char() {
    char local;
    local = 33;
    global_char = local;  /* ld a, (iy-N); ld (global), a */
}

void t_l2g_int() {
    int local;
    local = 444;
    global_int = local;   /* ld hl, (iy-N); ld (global), hl */
}

void t_l2g_long() {
    long local;
    local = 55555L;
    global_long = local;  /* ld a, -N; call getlong; ld (global), hl; exx; ld (global+2), hl; exx */
}

/* Argument to global */
void t_a2g_char(char arg) {
    global_char = arg;    /* Move arg to A, then: ld (global), a */
}

void t_a2g_int(int arg) {
    global_int = arg;     /* Move arg to HL, then: ld (global), hl */
}

void t_a2g_long(long arg) {
    global_long = arg;    /* Load arg to HL'HL, then: ld (global), hl; exx; ld (global+2), hl; exx */
}

/* Global to argument (writing back to parameter) */
void t_g2a_char(char arg) {
    arg = global_char;    /* ld a, (global); store to arg location */
}

void t_g2a_int(int arg) {
    arg = global_int;     /* ld hl, (global); store to arg location */
}

void t_g2a_long(long arg) {
    arg = global_long;    /* ld hl, (global); exx; ld hl, (global+2); exx; store to arg */
}

/* Argument to local */
void t_a2l_char(char arg) {
    char local;
    local = arg;          /* Load arg, store to local */
}

void t_a2l_int(int arg) {
    int local;
    local = arg;          /* Load arg, store to local */
}

void t_a2l_long(long arg) {
    long local;
    local = arg;          /* Load arg, store to local */
}

/* Local to argument */
void t_l2a_char(char arg) {
    char local;
    local = 77;
    arg = local;          /* ld a, (iy-N_local); store to arg location */
}

void t_l2a_int(int arg) {
    int local;
    local = 888;
    arg = local;          /* ld hl, (iy-N_local); store to arg location */
}

void t_l2a_long(long arg) {
    long local;
    local = 99999L;
    arg = local;          /* Load local long, store to arg */
}

/* ===== TEST 8: MULTIPLE VARIABLES ===== */

void t_multi_loc() {
    char c1, c2;
    int i1, i2;
    long l1, l2;

    c1 = 10;
    c2 = 20;
    i1 = 1000;
    i2 = 2000;
    l1 = 100000L;
    l2 = 200000L;

    /* Mix of operations */
    global_char = c1;
    global_int = i1;
    global_long = l1;

    c2 = global_char;
    i2 = global_int;
    l2 = global_long;
}

/* ===== TEST 9: REGISTER ALLOCATION TEST ===== */

void t_reg_alloc(int a, int b) {
    int result;
    /* If a and b are register-allocated, should use register operations */
    result = a + b;
    global_int = result;
}

void t_multi_args(char c1, char c2, int i1, int i2, long l1, long l2) {
    /* Test calling with many arguments - some will be on stack */
    global_char = c1;
    global_int = i1;
    global_long = l1;
}

/* ===== TEST 10: CHAIN OPERATIONS ===== */

void t_chain_ops() {
    char c;
    int i;
    long l;

    /* Chain: global -> local -> global */
    c = global_char;
    global_char = c;

    i = global_int;
    global_int = i;

    l = global_long;
    global_long = l;
}
