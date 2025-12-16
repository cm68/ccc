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
 * Symbol table entry for locals/register vars
 */
struct sym {
    char name[14];
    char type;          /* type suffix */
    char reg;           /* register (0=stack) */
    char off;		/* stack offset */
};

#define MAXLOCALS 64
extern struct sym locals[];
extern unsigned char nlocals;

/* Symbol table operations */
void clearLocals(void);
void addLocal(char *name, char type, char reg, char off);
struct sym *findLocal(char *name);

/*
 * Expression tree node
 * op: '#'=const, '$'=global, 'R'=regvar, 'V'=local, 'M'=deref, etc.
 */
struct expr {
    char op;                /* operator: '#', '$', 'R', 'V', 'M', '=', '@', '+', etc. */
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
    unsigned char aux2;         /* auxiliary: offset for bitfield, incr for inc/dec */
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
#define SP_SYMOFS   3       /* +p $sym #const -> ld hl,sym+ofs */
#define SP_SYMOFD   4       /* M[+p $sym #const] -> ld hl,(sym+ofs) */
#define SP_MUL2     5       /* * #pow2 -> add hl,hl (incr = shift count) */
#define SP_SIGN     6       /* M$sym >= 0 -> bit 7, result in NZ */
#define SP_MSYM     7       /* Ms $sym -> ld hl,(sym) */
/* SP_IXOD, SP_CMPIX, SP_CMPIY, SP_STIX removed - IX/IY patterns collapsed to V node */
#define SP_CMPHL    11      /* cmpB where one operand needs (hl) */
#define SP_STCONST  12      /* =type [M addr] [#const] -> ld (hl),n */
#define SP_INCGLOB  13      /* (s $sym -> ld hl,(_sym); inc hl; ld (_sym),hl */
#define SP_SIGNREG  14      /* Ms[Rs bc] >= 0 -> bit 7,b; sign test regvar */
#define SP_BITTEST  15      /* &B (ix+ofs) #pow2 -> bit n,(ix+ofs) */
#define SP_ADDBC    16      /* +p Mp[Rp bc] #const -> ld hl,const; add hl,bc */
#define SP_CMPEQ    17      /* Qs/ns expr #0/1/-1 -> inc/dec/nop then test HL */

/* Expression allocation */
struct expr *newExpr(char op, char type);
void freeExpr(struct expr *e);

/* Expression parsing */
struct expr *parseExpr(void);

/* Global state */
extern int infd;
extern int outfd;
#define ASTEOF 255
extern unsigned char curchar;
extern int lineno;

/* AST I/O */
void advance(void);
unsigned char hex2(void);
unsigned int hex4(void);
long hex8(void);
void readName(char *buf);
void skipWs(void);

/* Output */
void emit(char *fmt, ...);
void emitLabel(char *name);
void comment(char *fmt, ...);

/* Output state */
extern char outbuf[];
extern int indent;
extern int labelCnt;
extern int fnIndex;
extern int blockCnt;

/* Scheduling */
unsigned char calcDemand(struct expr *e);
void assignDest(struct expr *e, char dest);
unsigned char treeDepth(struct expr *e);
unsigned char isSimpleByte(struct expr *e);

/* Code emission */
void emitExpr(struct expr *e);
void dumpStmt(void);
void emitInit(void);

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

#endif

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
