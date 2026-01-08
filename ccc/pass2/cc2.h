/*
 * cc2.h - Code generator
 */
#ifndef CC2_H
#define CC2_H

/* Type suffixes from AST */
#define T_BYTE    'b'
#define T_UBYTE   'B'
#define T_SHORT   's'
#define T_USHORT  'S'
#define T_LONG    'l'
#define T_ULONG   'L'
#define T_FLOAT   'f'
#define T_VOID    'v'

/* Type size helpers - pointers are 's' (16-bit like short) */
#define ISWORD(t)  ((t) == T_SHORT || (t) == T_USHORT)
#define ISBYTE(t)  ((t) == T_BYTE || (t) == T_UBYTE)
#define ISLONG(t)  ((t) == T_LONG || (t) == T_ULONG || (t) == T_FLOAT)
#define TSIZE(t) (ISBYTE(t) ? 1 : ISWORD(t) ? 2 : ISLONG(t) ? 4 : 0)
#define ISSIGNED(t) ((t) == T_BYTE || (t) == T_SHORT || (t) == T_LONG)

/* Register/addressing mode indices for dest field */
#define R_B     1
#define R_C     2
#define R_BC    3
#define R_IX    4       /* ix register (matches regvar aux) */
#define R_DE    5
#define R_HL    6
#define R_A     7
#define R_IY    8       /* iy register */
#define R_IXI   9       /* (ix) indirect */
#define R_IYI   10      /* (iy) indirect */
#define R_IXO   11      /* (ix+ofs) indirect with offset */
#define R_IYO   12      /* (iy+ofs) indirect with offset */
#define R_TOS   13      /* top of stack (push to get there) */

extern char *regnames[];

/*
 * Expression tree node
 * op: uses token constants from lexeme.h (AST_CONST, SYM, REGVAR, LOCALVAR, DEREF, etc.)
 */
struct expr {
    unsigned char op;       /* operator: lexeme.h tokens (SYM, DEREF, ASSIGN, CALL, etc.) */
    char size;              /* size in bytes */
    char type;              /* type suffix: 'b', 's', 'l', 'p', etc. */
    struct expr *left;      /* left/first child */
    struct expr *right;     /* right/second child */
    union {
        long l;
        short s;
        char c;
    } v;                    	/* constant value */
    char *sym;			/* symbol name (malloc'd) */
    unsigned char aux;          /* auxiliary: nargs for call, width for bitfield */
    short aux2;                 /* auxiliary: offset for bitfield, incr, or label */
    unsigned char demand;       /* temporary demand */
    unsigned char dest;     	/* destination register index (R_HL, R_DE, R_A) */
    unsigned char spill;        /* need to spill DE before right child */
    unsigned char unused;       /* result is unused (expr stmt, void call) */
    unsigned char cond;         /* used as condition (emit flags, not value) */
    unsigned char special;      /* special case type (0=none) */
    char offset;           	/* IY/IX-relative offset for specials */
    short incr;             	/* increment amount for specials */
};

/* Special case types */
#define SP_NONE     0
#define SP_INCR     1       /* inc regvar (incr <= 4) */
#define SP_DECR     2       /* dec regvar (incr <= 4) */
#define SP_SYMOFS   3       /* PLUS SYM AST_CONST -> ld hl,sym+ofs */
#define SP_SYMOFD   4       /* DEREF[PLUS SYM AST_CONST] -> ld hl,(sym+ofs) */
#define SP_MUL2     5       /* STAR AST_CONST (pow2) -> add hl,hl */
#define SP_SIGN     6       /* DEREF[SYM] >= 0 -> bit 7, result in NZ */
#define SP_MSYM     7       /* DEREF[SYM] -> ld hl,(sym) */
/* SP_IXOD, SP_CMPIX, SP_CMPIY, SP_STIX removed - IX/IY patterns collapsed to V node */
#define SP_CMPHL    11      /* cmp byte where one operand needs (hl) */
#define SP_STCONST  12      /* ASSIGN DEREF AST_CONST -> ld (hl),n */
#define SP_INCGLOB  13      /* PREINC/PREDEC SYM -> load/inc/store global */
#define SP_SIGNREG  14      /* DEREF[REGVAR(BC)] >= 0 -> bit 7,b */
#define SP_BITTEST  15      /* AND DEREF[(ix+ofs)] AST_CONST(pow2) -> bit */
#define SP_ADDBC    16      /* PLUS DEREF[REGVAR(BC)] AST_CONST -> add hl,bc */
#define SP_CMPEQ    17      /* EQ/NEQ expr AST_CONST(0/1/-1) -> test HL */
#define SP_CMPV     18      /* LT/EQ/NEQ LOCALVAR AST_CONST -> cp (iy+off) */
#define SP_CMPR     19      /* LT/EQ/NEQ REGVAR AST_CONST -> cp reg */

/* Expression allocation */
struct expr *newExpr(unsigned char op, char type);
void freeExpr(struct expr *e);

/* Expression parsing */
struct expr *parseExpr(void);

/* Global state */
extern int infd;
extern int outfd;
#define ASTEOF 255
extern unsigned char curchar;
extern int lineno;
extern int patternOnly;  /* -p flag: emit patterns as comments only */

/* AST I/O (binary format) */
void advance(void);
unsigned char read1(void);
unsigned int read2(void);
long read4(void);
void readName(char *buf);

/* Output */
void emit(char *fmt, ...);
void emitLabel(char *name);
void comment(char *fmt, ...);

/* Output state */
extern char outbuf[];
extern int indent;
extern int labelCnt;
extern int fnIndex;
extern int hasFrame;
extern char fnRetType;
extern int blockCnt;

/* Scheduling */
void annotate(struct expr *e);
unsigned char treeDepth(struct expr *e);
unsigned char isSimpleByte(struct expr *e);

/* Code emission */
void emitExpr(struct expr *e);
int pemitExpr(struct expr *e);  /* pattern-driven emission, returns 1 if handled */
void emitCompare(struct expr *e);
void emitCondJmp(unsigned char op, int aux2);
void emitCmpArith(struct expr *e);
void emitCmpShift(struct expr *e);
void emitCmpMulDiv(struct expr *e);
void emitPreIncDec(struct expr *e);
void emitPostInc(struct expr *e);
void emitPrimary(struct expr *e);
void dumpStmt(void);
void emitInit(void);

/* Pattern-based emission (emitpat.c) */
void emitBOp(char op);
void emitBOpImm(char op, int val);
void emitWBit(char op);
void emitWBitBC(char op);
void emitWBitImm(char op, int val);
void emitWCpl(void);
void emitTestZero(unsigned char size, unsigned char reg);
void emitTestExpr(struct expr *e);
void emitWAddSub(char op);
void emitWSubBC(char op, int val);
void emitLLoad(unsigned char dest);
void emitLStore(void);
void emitLStoreR(void);
void emitLImm(long val);
void emitLImmR(long val);

/* Switch statement support */
#define MAXCASES 32
#define MAXSWDEPTH 4
struct swctx {
    unsigned char ncases;       /* number of cases seen */
    unsigned char hasdef;       /* has default case */
    int tblLabel;               /* label for jump table */
    int endLabel;               /* label for switch end */
    int defLabel;               /* label for default case */
    unsigned char vals[MAXCASES];  /* case values */
    int labels[MAXCASES];       /* case labels */
};
extern struct swctx swstack[];
extern unsigned char swdepth;

/* Top-level parsing */
void parseGlobal(void);
void parseString(void);
void parseGlobAsm(void);
void parseFunc(void);
void parseAst(void);

/*
 * Pattern-based expression representation (pattern.c)
 * Format: operators are 2 chars (op + size)
 * Markers add 2 hex digits for operand index
 * Example: "=sSs02+sSs00Ts01" for x = y + 5
 */

/* Pattern markers - values 81-89 don't conflict with lexeme.h tokens */
#define PAT_SYM    83   /* global symbol */
#define PAT_CONST  84   /* constant value */
#define PAT_LOCAL  86   /* local variable */
#define PAT_REG    82   /* register variable */

/*
 * Sized to fit in 127 bytes for Z80 IY-indexed addressing:
 * 64 + 1 + 30*2 + 1 = 126 bytes
 */
#define MAX_OPERANDS 30
#define MAX_PATTERN  64

struct pattern {
	char str[MAX_PATTERN];
	unsigned char len;
	void *ops[MAX_OPERANDS];
	unsigned char nops;
};

void patInit(struct pattern *p);
void patSym(struct pattern *p, char size, char *name);
void patConst(struct pattern *p, char size, long val);
void patLocal(struct pattern *p, char size, int offset);
void patReg(struct pattern *p, char size, int reg);
void patUnary(struct pattern *p, char op, char size, struct pattern *child);
void patBinary(struct pattern *p, char op, char size,
	       struct pattern *left, struct pattern *right);
void patCall(struct pattern *p, char retsize, int nargs,
	     struct pattern *func, struct pattern *args[], int narg);
void patPrint(struct pattern *p);
void *patGetOp(struct pattern *p, char *pos);
char *patGetSym(struct pattern *p, char *pos);
long patGetConst(struct pattern *p, char *pos);
int patGetLocal(struct pattern *p, char *pos);
int patGetReg(struct pattern *p, char *pos);
void patFromExpr(struct pattern *p, struct expr *e);
void patEmitComment(struct pattern *p);

#endif

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
