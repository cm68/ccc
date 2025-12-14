/*
 * Test special pattern recognition in cc2
 * Tests: SP_INCR, SP_DECR, SP_SYMOFS, SP_IXOD, SP_SYMOFD,
 *        SP_MSYM, SP_MUL2, SP_SIGN, SP_CMPIX, SP_CMPIY,
 *        SP_CMPHL, SP_STCONST, SP_INCGLOB, SP_SIGNREG
 */

int gword;
char gbyte;
int *gptr;
int garr[10];

/* SP_MUL2: multiply by power of 2 */
int mulPow2(int x) { return x * 2; }
int mulPow2x4(int x) { return x * 4; }
int mulPow2x8(int x) { return x * 8; }

/* SP_SIGN: x >= 0 sign test */
int signWord(void) { return gword >= 0; }
int signByte(void) { return gbyte >= 0; }

/* SP_MSYM: deref global directly */
int derefWord(void) { return gword; }
char derefByte(void) { return gbyte; }

/* SP_SYMOFD: deref global+offset */
int derefArr(void) { return garr[3]; }

/* SP_SYMOFS: address of global+offset */
int *addrArr(void) { return &garr[5]; }

/* SP_INCR/SP_DECR: inc/dec regvar */
void incrReg(void) {
    register char *p;
    p++;
    p--;
    ++p;
    --p;
}

struct rec {
    int x;
    char flag;
    int y;
};

/* SP_IXOD: (ix+d) addressing */
void ixodRead(struct rec *pp) {
    register struct rec *p = pp;
    char c;
    c = p->flag;
}

/* SP_CMPIX: byte compare with (ix+d) */
int cmpEq(struct rec *pp, char c) {
    register struct rec *p = pp;
    return p->flag == c;
}
int cmpNe(struct rec *pp, char c) {
    register struct rec *p = pp;
    return p->flag != c;
}
int cmpLt(struct rec *pp, char c) {
    register struct rec *p = pp;
    return p->flag < c;
}
int cmpGt(struct rec *pp, char c) {
    register struct rec *p = pp;
    return p->flag > c;
}
int cmpLe(struct rec *pp, char c) {
    register struct rec *p = pp;
    return p->flag <= c;
}
int cmpGe(struct rec *pp, char c) {
    register struct rec *p = pp;
    return p->flag >= c;
}

/* SP_CMPIX flipped: compare with (ix+d) on left */
int cmpEqFlip(struct rec *pp, char c) {
    register struct rec *p = pp;
    return c == p->flag;
}
int cmpLtFlip(struct rec *pp, char c) {
    register struct rec *p = pp;
    return c < p->flag;
}

/* SP_CMPHL: compare with (hl) from complex address */
char *names[10];
int cmpHL1(int i, char c) {
    /* names[i] + offset compared with param byte */
    return names[i][4] < c;
}
int cmpHL2(int i) {
    /* compare with global byte directly */
    return names[i][4] < gbyte;
}
int cmpHL3(int i) {
    /* compare with local auto byte */
    char x = 42;
    return names[i][4] < x;
}

/* SP_STCONST: store constant through HL */
void stcWord(void) { gword = 0; }
void stcByte(void) { gbyte = 0; }
void stcPtr(void) { *gptr = 0; }
void stcMember(void) {
    struct rec r;
    r.flag = 42;
}

/* SP_INCGLOB: inc/dec global word */
void incGlob(void) {
    ++gword;
    --gword;
    gword++;
    gword--;
}

/* SP_SIGNREG: sign test of register variable */
int signReg(int n) {
    register int i = n;
    if (i >= 0)
        return 1;
    return 0;
}
