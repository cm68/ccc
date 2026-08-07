#ifndef _CGEN_H
#define _CGEN_H 1
/* File cgen.h Created 17.05.2019 Last Modified 17.06.2020 */

/* Not a commercial goal of this laborious work is to popularize among
 * potential fans of 8-bit computers the old HI-TECH C compiler V3.09
 * (HI-TECH Software) and extend its life, outside of the CP/M environment
 * (Digital Research, Inc), for full operation in a  Unix-like operating
 * system UZI-180 without using the CP/M emulator.
 *
 * The HI-TECH C compiler V3.09 is provided free of charge for any use,
 * private or commercial, strictly as-is. No warranty or product support
 * is offered or implied including merchantability, fitness for a particular
 * purpose, or non-infringement. In no event will HI-TECH Software or its
 * corporate affiliates be liable for any direct or indirect damages.
 *
 * You may use this software for whatever you like, providing you acknowledge
 * that the copyright to this software remains with HI-TECH Software and its
 * corporate affiliates.
 *
 * All copyrights to the algorithms used, binary code, trademarks, etc.
 * belong to the legal owner - Microchip Technology Inc. and its subsidiaries.
 * Commercial use and distribution of recreated source codes without permission
 * from the copyright holderis strictly prohibited.
 *
 * The solution to this problem is to recreate the object code being moved,
 * replace the CP/M system functions (I/O, memory allocation, etc.) with
 * similar UZI-180 calls, and compile an executable file for this operating
 * system.
 */

/*
#define DEBUG
*/
#include <assert.h>
#include <ctype.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#if defined(__STDC__) || defined(__STDC_VERSION__)
#include <stdbool.h>
#include <stdint.h>
#if __STDC_VERSION__ < 201112L
#define _Noreturn
#endif
#if __STDC_VERSION__ >= 201710L
#define register
#endif
/* in certain functions the VC compiler complains reasonably that
 *  certain variables may be used before being initialised
 * this macro forces a 0 initialisation which will cause
 * exceptions if the variable is used, rather than rely on
 * random data
 */
#ifdef _MSC_VER
#define FORCEINIT = NULL
#else
#define FORCEINIT
#endif
#else
typedef unsigned short uint16_t;
typedef short int16_t;
typedef unsigned char uint8_t;
typedef char int8_t;
typedef unsigned long uint32_t;
#ifndef bool
#define bool char
#define true 1
#define false 0
#endif
#define FORCEINIT
#define _Noreturn
#endif

#undef max /* replaced by function call */
#ifndef CPM
#define bmove(src, dst, cnt) memcpy(dst, src, cnt)
#endif
/*
 *	Constant declarations
 */
#define MININT  -32768 /* min for int (0x8000)	*/
#define MAXINT  32767  /* max for int (0x7fff)	*/
#define MAXFUN  0x20   /* Maximum limit nested function */
#define MAXBUF  60     /* Maximum buffer size 		*/
#define MAXNAME 40     /* Maximum file name length	*/
#define MAXERR  30     /* Maximum number nonfatal errors*/

#define NULSTR   0    /*  0  ""	*/
#define NOT      1    /*  1  "!"	*/
#define NEQL     2    /*  2  "!="	*/
#define HASHSIGN 3    /*  3  "#"	*/
#define DOLLAR   4    /*  4  "$"	*/
#define DOLLAR_U 5    /*  5  "$U"	*/
#define MOD      6    /*  6  "%"	*/
#define BAND     7    /*  7  "&"	*/
#define LAND     8    /*  8  "&&"	*/
#define GADDR    9    /*  9  "&U"	*/
#define LPAREN   0xA  /* 10  "("	*/
#define RPAREN   0xB  /* 11  ")"	*/
#define MUL      0xC  /* 12  "*"	*/
#define MUL_U    0xD  /* 13  "*U"	*/
#define ADD      0xE  /* 14  "+"	*/
#define INCR     0xF  /* 15  "++"	*/
#define PLUS_U   0x10 /* 16  "+U"	*/
#define COMMA    0x11 /* 17  ","	*/
#define SUB      0x12 /* 18  "-"	*/
#define DECR     0x13 /* 19  "--"	*/
#define CONV     0x14 /* 20  "->"	*/
#define MINUS_U  0x15 /* 21  "-U"	*/
#define DOT      0x16 /* 22  "."	*/
#define MEMBER   0x16 /* Alias for use in class*/
#define DOT_DOT  0x17 /* 23  ".."	*/
#define DIV      0x18 /* 24  "/"	*/
#define COLON    0x19 /* 25  ":"	*/
#define COLON_U  0x1A /* 26  ":U"	*/
#define COLON_S  0x1B /* 27  ":s"	*/
#define SCOLON   0x1C /* 28  ";"	*/
#define T_SCOLON 0x1D /* 29  ";;"	*/
#define LT       0x1E /* 30  "<"	*/
#define LSHIFT   0x1F /* 31  "<<"	*/
#define LEQ      0x20 /* 32  "<="	*/
#define ASSIGN   0x21 /* 33  "="	*/
#define ASMOD    0x22 /* 34  "=%"	*/
#define ASAND    0x23 /* 35  "=&"	*/
#define ASMUL    0x24 /* 36  "=*"	*/
#define ASADD    0x25 /* 37  "=+"	*/
#define ASSUB    0x26 /* 38  "=-"	*/
#define ASDIV    0x27 /* 39  "=/"	*/
#define ASLSHIFT 0x28 /* 40  "=<<"	*/
#define EQL      0x29 /* 41  "=="	*/
#define ASRSHIFT 0x2A /* 42  "=>>"	*/
#define ASEXOR   0x2B /* 43  "=^"	*/
#define ASEOR    0x2C /* 44  "=|"	*/
#define GT       0x2D /* 45  ">"	*/
#define GEQ      0x2E /* 46  ">="	*/
#define RSHIFT   0x2F /* 47  ">>"	*/
#define QUEST    0x30 /* 48  "?"	*/
#define ATGIGN   0x31 /* 49  "@"	*/
#define CASE     0x32 /* 50  "[\\"	*/
#define UNKNOWN  0x33 /* 51  "[a"	*/
#define ENUM     0x34 /* 52  "[c"	*/
#define EXPR     0x35 /* 53  "[e"	*/
#define INIT     0x36 /* 54  "[i"	*/
#define STRUCT   0x37 /* 55  "[s"	*/
#define UNION    0x38 /* 56  "[u"	*/
#define VAR      0x39 /* 57  "[v"	*/
#define BXOR     0x3A /* 58  "^"	*/
#define LBRACE   0x3B /* 59  "{"	*/
#define BOR      0x3C /* 60  "|"	*/
#define LOR      0x3D /* 61  "||"	*/
#define RBRACE   0x3E /* 62  "}"	*/
#define BNOT     0x3F /* 63  "~"	*/
#define RECIP    0x40 /* 64  "RECIP"   */
#define TYPE     0x41 /* 65  "TYPE"    */
#define IDOP     0x42 /* 66  "ID"	*/
#define CONST    0x43 /* 67  "CONST"   */
#define FCONST   0x44 /* 68  "FCONST"  */
#define USEREG   0x45 /* 69  "REG"     */
#define INAREG   0x46 /* 70  "INAREG"  */
#define BFIELD   0x47 /* 71  "BITFIELD"*/

/*
 * dopetab[] bit masks - operator property flags
 *
 * Each entry in dopetab[] encodes properties of an operator:
 *
 * Bits 0-1  (0x0003): Result category for code generation
 *                     2=identifier, 3=float, 4=unary, 7=complement, 8=binary
 * Bits 2-3  (0x000C): Operand count (DOPE_OPCOUNT)
 *                     0=leaf, 4=unary (DOPE_UNARY), 8=binary (DOPE_BINARY), C=statement
 * Bit 4     (0x0010): Constant-like, evaluable at compile time (DOPE_LEAF)
 * Bit 5     (0x0020): Logical/comparison operator (DOPE_LOGICAL)
 * Bit 6     (0x0040): Commutative operation (DOPE_COMMUTE)
 * Bit 7     (0x0080): Shift operation (DOPE_SHIFT)
 * Bit 8     (0x0100): Short-circuit / non-commutative binary (DOPE_SIDEEFF)
 * Bit 9     (0x0200): Signed/conversion related (DOPE_SIGNED)
 * Bits 10-11(0x0C00): Operator class (DOPE_OPCLASS)
 *                     0x000=multiplicative, 0x800=bitwise, 0xC00=additive
 * Bit 12    (0x1000): Indirection/dereference (DOPE_INDIR)
 * Bit 13    (0x2000): Associative operation (DOPE_ASSOC)
 * Bit 14    (0x4000): Control flow / statement marker (DOPE_CTRL)
 */
#define DOPE_OPCOUNT  0x000C  /* Mask for operand count (bits 2-3) */
#define DOPE_UNARY    0x0004  /* Unary operator (one child) */
#define DOPE_BINARY   0x0008  /* Binary operator (two children) */
#define DOPE_LEAF     0x0010  /* Constant-like / leaf node */
#define DOPE_LOGICAL  0x0020  /* Logical/comparison operator */
#define DOPE_COMMUTE  0x0040  /* Commutative operation */
#define DOPE_SHIFT    0x0080  /* Shift operation */
#define DOPE_SIDEEFF  0x0100  /* Side effects / short-circuit */
#define DOPE_SIGNED   0x0200  /* Signed/conversion */
#define DOPE_OPCLASS  0x0C00  /* Mask for operator class (bits 10-11) */
#define DOPE_BITWISE  0x0800  /* Bitwise operator class */
#define DOPE_ADDITIVE 0x0C00  /* Additive operator class (+, -) */
#define DOPE_DIVMOD   0x0400  /* Division/modulo class bit (bit 10) */
#define DOPE_INDIR    0x1000  /* Indirection/dereference */
#define DOPE_ASSOC    0x2000  /* Associative operation */
#define DOPE_CTRL     0x4000  /* Control flow / statement */

/*
 * b_sloc field bit flags - symbol location/status bits
 */
#define B_SLOC_EMITTED  0x01  /* Label has been emitted to output */
#define B_SLOC_GLOBAL   0x02  /* Global directive has been emitted */
#define B_SLOC_SAVE     0x04  /* Register needs save in prologue */
#define B_SLOC_REGVAR   0x08  /* Variable is a register variable */
#define B_SLOC_BITFIELD 0x10  /* Member is a bitfield */

/*
 * b_refl field bit flags - type reference flags
 */
#define B_REFL_PTR      0x01  /* Type is a pointer */
#define B_REFL_FUNC     0x02  /* Type is a function */
#define B_REFL_STRUCT   0x04  /* Type reference to struct */

/*
 * tFlags field bit flags - node type flags
 */
#define T_PTR           0x01  /* Node type is pointer */
#define T_FUNC          0x02  /* Node type is function */

/*
 * node_t.flags bit flags - tree node flags
 */
#define NF_EVALORDER    0x01  /* Evaluation order hint */
#define NF_STACKSPILL   0x02  /* Value spilled to stack */

/*
 * Register constraint masks
 */
#define REG_INVMASK     0x8000  /* Inverse bitmask mode (use ~mask) */
#define REG_MASKMASK    0x7fff  /* Mask to extract register bits */
#define REGS_ALL        0x17F   /* All available general registers (a,c,b,e,d,l,h,ix) */

/*
 * Code fragment flags (c_2 field)
 */
#define CF_REGCONSTRAINT 0x40  /* Register constraint present */

/*
 * Byte extraction mask
 */
#define LOBYTE_MASK     0xFF  /* Mask for low byte extraction */

/*
 * Dopetab result category (low 2 bits)
 */
#define DOPE_RESCAT     0x03  /* Result category mask (bits 0-1) */

/*
 * Memory allocator flags (cgmalloc.c)
 */
#define MALLOC_LARGEFLAG 0x80  /* Large block flag in header */

/* register indexes*/
enum { /* REG_A=1, REG_C, REG_B, REG_E, REG_D, REG_L*/
       REG_H = 7,
       /* REG_IX, REG_IY, REG_SP */
       REG_AF = 11,
       /* REG_BC, REG_DE, REG_HL */
       REG_DEHL = 15
};

/* enums */
enum psect { P_BSS = 1, P_TEXT, P_DATA };

/*
 *	Structural declarations
 */

typedef struct node {
    uint8_t op;         /* operator/node type code */
    uint8_t nPat;       /* number of matched code patterns */
    uint8_t flags;      /* bit 0: eval order, bit 1: stack spill */
    uint8_t pat[6];     /* code pattern indices into codeFrag[] */
    uint8_t wantReg[6]; /* expected/constraint register per pattern */
    uint8_t resReg[6];  /* actual result register per pattern */
    uint16_t tFlags;    /* type flags: bit 0=ptr, bit 1=func */
    struct member *pm;  /* pointer to type/member */
    union {
        long l;               /* long */
        unsigned long ul;     /* unsigned long, to avoid cast*/
        struct node *np[2];   /* node pair */
        struct member *mp[2]; /* member pair */
        struct {              /* string + int value */
            char *s;
            int v;
        } sv;
    } info;
} node_t;

typedef struct member {    /* Offset Member Description			*/
    char *name;            /* +0   0 Symbol/member name identifier	*/
                           /* +1   1					*/
    char sclass;           /* +2   2 Symbol class: MEMBER, ENUM, STRUCT, UNION, VAR, TYPE */
    char sflags;           /* +3   3 Storage/status flags (B_SLOC_* bits)	*/
    uint8_t depth;         /* +4   4 Scope nesting depth			*/
    uint8_t bWidth;        /* +5   5 Bitfield width			*/
    uint8_t bOffset;       /* +6   6 Bitfield offset			*/
    struct member *next;   /* +7   7 Next member in chain		*/
                           /* +8   8					*/
    uint16_t size;         /* +9   9 Type size in bytes			*/
                           /* +A  10					*/
    uint16_t nelem;        /* +B  11 Array element count			*/
                           /* +C  12					*/
    uint16_t refl;         /* +D  13 Reference level (ptr/func flags)	*/
                           /* +E  14					*/
    struct member *type;   /* +F  15 Base type pointer			*/
                           /* +10 16					*/
    int offset;            /* +11 17 Stack frame offset / member offset	*/
                           /* +12 18					*/
    union {                /* +13 18  - +14 19 */
        int16_t i;         /* Register number (for regvars) */
        struct _memb {
            int16_t cnt;
            struct member *vals[1];
        } * mlist;         /* Member list (for struct/union) */
        struct _memi {
            int16_t cnt;
            int16_t vals[1];
        } * ilist;         /* Integer list (for enum) */
    } u;
    char tflag;            /* +15 21 Type flag: 1=signed, 2=unsigned, 3=float, etc. */
} member_t;

struct codeFrag_t {
    char nodeTest;   /* Pattern test index for testPattern() */
    char leftReg;    /* Left operand register constraint */
    char resultReg;  /* Result register class/constraint */
    char subMatch;   /* Sub-pattern match type ('H'=recurse, etc) */
    uint8_t leftPat; /* Left operand pattern index */
    uint8_t rightPat;/* Right operand pattern index */
    char *emitCode;  /* Primary code emission string */
    char *auxCode;   /* Auxiliary code string for reg calc */
};

/*
 *	Descriptions of variables and arrays
 *
 * Declarations are located in sequence of being in
 * original binary image of CGEN.COM
 *					Purpose				Where it is used
 * ===== Start bss section ======= */
extern int lineno;             /* getToken, prWarning, prMsg*/
extern char progname[MAXNAME]; /* getToken  */
extern member_t *typeLong;     /* initTypes, mkConstNode, mkNode */
extern int funcLocalSize[MAXFUN];  /* local frame size per function */
extern member_t *typeUChar;    /* "uc" - uint8_t	   */
extern uint16_t nstdpth;       /* Current nesting depth   */
extern int funcScopeDepth[MAXFUN];  /* saved scope depth at function entry */
extern int funcParmOff[MAXFUN]; /* parameter stack offset tracker */
extern member_t *typeDouble;         /* "d" - double 	   */
extern member_t *typeChar;           /* "c" - char 		   */
extern member_t *funcSymbol[MAXFUN]; /* function symbol per nesting level */
extern member_t *typeB;              /* "b"			   */
extern member_t *typeX;              /* "x"			   */
#define HASHTABSIZE 101
extern member_t *hashtab[HASHTABSIZE]; /* hash table (array_AF1F) */
extern member_t *typeVar;              /* "v" - variable  	   */
extern int lvlidx;                     /* Level nested function - leaveBlock, initTypes */

extern int emitOffset;           /* emitCodePat, emitNodeCode */
extern char lastResultReg;       /* calcUsedRegs, getResultReg */
extern int exprNestDepth;        /* emitCodePat, emitExprTree */
extern node_t *deferList[0x14];  /* deferPostInc, emitExpr */
extern void *nodeFreeList;       /* freeNode, allocNode, relNodeFrList */
extern bool treeChanged;         /* optimization loop flag */
extern uint8_t deferCnt;         /* deferPostInc, emitExpr */
extern char *warningMsg;         /* msgptr  Pointer str printf       */
extern int availRegs;            /* leaveBlock, parseStmt, calcUsedRegs, genExprCode, tryAllocReg */
extern bool wflag;               /* Warning messages */
#if 0
extern bool pflag;               /* Not used */
extern bool hflag;               /* Not used */
#endif
extern int errcnt;               /* Number of errors */
#if 0
extern bool bflag;               /* Not used */
#endif
extern char *baseHeap;           /* Current highest memory */
#if 0
extern bool eflag;               /* Not used */
#endif
extern bool rflag;

/* ===== End of bss section ======= */

/* ===== Start data section ======= */

extern char *otherFrag[];            /*+emitCodePat */
extern struct codeFrag_t codeFrag[]; /* matchEmitPat, calcUsedRegs, emitCodePat, emitNodeCode */

extern uint16_t dopetab[72]; /*+matchEmitPat, calcUsedRegs, emitCodePat, emitExprTree, copyTree, ... */
extern int regBitMask[];     /* register index to bitmask conversion */
extern uint8_t regPairHiLo[];  /* register pair decomposition */
extern uint8_t regClassRegs[]; /* register class member lists */

extern char *regNames[];

/* ===== End of data section ======= */

/*
 * Libbrary Function prototypes
 *
 */
#ifdef CPM
long atol(char *str);
void blkclr(char *, uint16_t size);
void *sbrk(int);
#else
#define blkclr(buf, len) memset(buf, 0, len)
#endif

/****************************************************************
 * Prototype functions are defined in sequence of being in
 * original binary image of CGEN.COM

 ****************************************************************/
#ifndef _WIN32
int fgetchar(void);
int fputchar(int);
#else
#define fgetchar getchar
#define fputchar putchar
#endif

/* for some functions the list of declarations below causes the compilation
 * to fail. When this happens the individual function defines MINI and declares
 * its used functions only
 */
#ifndef MINI
/* lex.c -----------------------------------------------------*/
int lookupEmitCode(int, int);
char lookupToken(char *);
member_t **gethashptr(char *);
member_t *lookupSymbol(char *);
char *getToken();
void leaveBlock();
member_t *parseTypeSpec(char *, uint16_t *);
void badIntCode();
void parseStmt();
void expect(char);
void parseData();
/* code.c ----------------------------------------------------*/
int emitInitData(member_t *, node_t *);
void parseInit();
void prFrameHead(int);
void prFrameTail(int, int);
void prStrcRetCpy(int, int);
void prGlobalDef(member_t *);
void emitBssDef(member_t *);
void emitVarLbl(member_t *);
void prDefb0s(int);
void prPsect(int);
void sortCaseLabels(int *pCase, int *pLabel, int nCase);
void parseSwitch();
void prCaseCmp(int);
void prPush(uint8_t);
void prPop(uint8_t);
void prIXnPush(member_t *);
uint8_t getTypeClass(node_t *);
void prTypeChar(node_t *);
void prDefb(char *, int);
void prJmpLabel(int);
void prJump(int);
void setEnumSize(member_t *, int, int);
int max(int, int);
void initTypes();
int newLocal();
member_t *declareSymbol(char *, uint8_t);
/* sym.c -----------------------------------------------------*/
void parseVariable();
void parseMembers(int);
void parseEnum();
int alignOffset(int, int);
int varSize(member_t *);
int memberAlign(member_t *);
void layoutStruct(member_t *);
void saveRegVars();
int matchEmitPat(node_t *, int, int, int, int *);
int calcUsedRegs(node_t *, int);
int getUsedRegs(node_t *);
uint8_t getResultReg(node_t *);
/* cgen.c ----------------------------------------------------*/
void genExprCode(node_t *);
long signExtend(node_t *, long);
void prSignedVal(node_t *, long);
void emitCodePat(node_t *, char *, char);
void emitNodeCode(node_t *);
void emitExprTree(node_t *);
void freeNode(node_t *);
node_t *allocNode(void);
bool relNodeFrList(void);
node_t *copyTree(node_t *);
void peelType(node_t *);
void addPtrType(node_t *);
uint16_t derefSize(node_t *);
bool hasTypeFlag(node_t *, int);
bool isPointer(node_t *);
bool isFuncType(node_t *);
bool isStructVal(node_t *);
uint16_t nodesize(node_t *);
node_t *castConst(node_t *);
void setNodeType(node_t *, node_t *);
void unsignedOp(unsigned long *, long, int);
void signedOp(long *, long, int);
node_t *deferPostInc(node_t *);
/* tree.c ----------------------------------------------------*/
void emitExpr(node_t *);
node_t *constFold(node_t *);
node_t *mkConstNode(long);
node_t *parseExpr(void);
node_t *mkNode(uint8_t, node_t *, node_t *);
uint8_t isPow2Bit(long);
void freeExprTree(node_t *);
uint8_t testPattern(node_t *, int);
bool isAddrable(node_t *);
bool isZeroConst(node_t *);
int addrLevel(node_t *);
bool hasRegChild(node_t *);
node_t *simplifyNot(node_t *);
node_t *expandId(node_t *);
node_t *mkArrayOp(node_t *);
node_t *dropRightOp(node_t *);
node_t *pow2ToShift(node_t *);
node_t *optConv(node_t *);
node_t *canAddrDeref(node_t *);
/* local.c ---------------------------------------------------*/
node_t *localOptimize(node_t *);
void allocVar(member_t *, int);
node_t *subToAdd(node_t *);
node_t *negAddToSub(node_t *);
node_t *optimizeExpr(node_t *);
int invertTest(int);
bool constFits(node_t *, node_t *);
bool tryAllocReg(member_t *, int);
bool sameType(node_t *, node_t *);
bool bothSigned(node_t *, node_t *);
bool constFitsType(node_t *, node_t *);
uint8_t selCompatReg(int, int, int);
uint8_t findAvailReg(int, int);
uint16_t findRegPair(uint16_t, uint8_t);
int selResultReg(int, int, int, char *);
/* main.c  ---------------------------------------------------*/
/* int main(int, char **);	*/

/*
 * original C functions replaced to use stdarg
 */
#ifdef CPM
#define vfprintf _doprnt
#endif
_Noreturn void fatalErr(char *fmt, ...); /* ok fatalErr.c	*/
void prWarning(char *fmt, ...);          /* ok prWarning */
void prError(char *fmt, ...);            /* ok prError.c	*/
void prMsg(char *fmt, va_list args);
void *allocMem(size_t);                  /* ok allocMem.c	*/
/* end of function declarations */
#endif

/* End file cgen.h */
#endif

/* vim: set tabstop=4 shiftwidth=4 noexpandtab: */
