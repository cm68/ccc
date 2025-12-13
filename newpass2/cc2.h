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
#define T_PTR     'p'
#define T_FLOAT   'f'
#define T_VOID    'v'

/* Type size helpers */
#define ISWORD(t)  ((t) == T_SHORT || (t) == T_USHORT || (t) == T_PTR)
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

extern char *regnames[];

/*
 * Symbol table entry for locals/register vars
 */
struct sym {
    char name[14];
    char type;              /* type suffix */
    char reg;               /* register (0=stack) */
    short off;              /* stack offset */
};

#define MAXLOCALS 64
extern struct sym locals[];
extern int nlocals;

/* Symbol table operations */
void clearLocals(void);
void addLocal(char *name, char type, int reg, int off);
struct sym *findLocal(char *name);

/*
 * Expression tree node
 * op: '#'=const, '$'=global, 'R'=regvar, 'V'=local, 'M'=deref, etc.
 */
struct expr {
    char op;                /* operator: '#', '$', 'R', 'V', 'M', '=', '@', '+', etc. */
    char type;              /* type suffix: 'b', 's', 'l', 'p', etc. */
    struct expr *left;      /* left/first child */
    struct expr *right;     /* right/second child */
    union {
        long l;
        short s;
        char c;
    } v;                    /* constant value */
    char *sym;              /* symbol name (malloc'd) */
    short aux;              /* auxiliary: nargs for call, width for bitfield */
    short aux2;             /* auxiliary: offset for bitfield, incr for inc/dec */
    short demand;           /* temporary demand */
    unsigned char dest;     /* destination register index (R_HL, R_DE, R_A) */
    char unused;            /* result is unused (expr stmt, void call) */
    char cond;              /* used as condition (emit flags, not value) */
    char special;           /* special case type (0=none) */
    short offset;           /* IY/IX-relative offset for specials */
    short incr;             /* increment amount for specials */
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
#define SP_IXOD     8       /* M[+p Mp[Rp(ix)] #ofs] -> (ix+ofs) */
#define SP_CMPIX    9       /* cmpB where one operand is (ix+d) */
#define SP_CMPIY    10      /* cmpB where one operand is (iy+d) */
#define SP_CMPHL    11      /* cmpB where one operand needs (hl) */

/* Expression allocation */
struct expr *newExpr(char op, char type);
void freeExpr(struct expr *e);

/* Expression parsing */
struct expr *parseExpr(void);

/* Global state */
extern int infd;
extern int outfd;
extern int curchar;
extern int lineno;

/* AST I/O */
void advance(void);
int hex2(void);
int hex4(void);
long hex8(void);
void readName(char *buf);
void skipWs(void);

/* Output */
void emit(char *fmt, ...);
void emitLabel(char *name);

#endif
