/*
 * cc2.h - Data structures for code generation (pass 2)
 *
 * Extends the basic expr/stmt structures from cc1.h with
 * code generation fields (scheduling, jump instructions, label numbers)
 */
#ifndef CC2_H
#define CC2_H

#include <stdlib.h>
#include <string.h>

/* Free that tolerates NULL */
void xfree(void *p);

/*
 * AST operator codes - ASCII values used in .ast files
 * These reuse keyword character values that can never appear
 * as expression operators (context distinguishes them).
 */
#define AST_SEXT      'x'   /* sign extend */
#define AST_PREINC    '('   /* pre-increment */
#define AST_POSTINC   ')'   /* post-increment */
#define AST_PREDEC    '{'   /* pre-decrement */
#define AST_POSTDEC   '}'   /* post-decrement */
#define AST_SUBEQ     'o'   /* subtract-equals */
#define AST_ANDEQ     'a'   /* and-equals */
#define AST_MODEQ     'm'   /* mod-equals */
#define AST_BFEXTRACT 'e'   /* bitfield extract */
#define AST_BFASSIGN  'f'   /* bitfield assign */

/*
 * Assembler directive syntax
 * Different assemblers use different syntax for these directives
 */
#define ASM_EXTERN ".extern"
#define ASM_GLOBAL ".global"

/*
 * Register allocation enumeration
 * Tracks which Z80 register (if any) a variable is allocated to
 */
enum register_id {
    REG_NO = 0,     /* Not allocated to a register */

    /* Byte registers */
    REG_B,          /* B register */
    REG_C,          /* C register */

    /* Word registers */
    REG_BC,         /* BC register pair */
    REG_IX          /* IX index register (preferred for struct pointers) */
};

/*
 * Expression flags (e->flags)
 */
#define E_UNSIGNED  0x01        // Value is unsigned
#define E_UNUSED    0x02        // Value is not used (result discarded)
#define E_GENERATED 0x04        // Node has been code-generated (scheduled/emitted)
#define E_IXASSIGN  0x08        // ASSIGN to IX-indexed struct member
#define E_IXDEREF   0x10        // DEREF of IX-indexed struct member
#define E_JUMP      0x20        // Node has associated jump (ternary, return)

/*
 * Operand pattern flags (e->opflags) - set during analysis phase
 * These identify common patterns to avoid repeated checking in codegen
 */
#define OP_CONST    0x01        // Right operand is a constant (C node)
#define OP_SIMPLEVAR 0x02       // Left is simple var deref: (M $var)
#define OP_REGVAR   0x04        // Left var is register-allocated
#define OP_IXMEM    0x08        // Left is IX-indexed: (M (+ (M:p $ixvar) ofs))
#define OP_IYMEM    0x10        // Left is IY-indexed (stack var)
#define OP_GLOBAL   0x20        // Left is global variable
#define OP_INDIR    0x40        // Left is indirect through pointer: (M (M $ptr))
#define OP_BCINDIR  0x80        // Byte DEREF of BC-allocated pointer: (M:b (M:p $bc))

/*
 * Location types for scheduled expressions (e->loc)
 * Set during scheduling phase, used by emit
 */
#define LOC_NONE    0           // Not yet scheduled
#define LOC_CONST   1           // Constant value in e->value
#define LOC_REG     2           // In register specified by e->reg
#define LOC_MEM     3           // Global memory, symbol in e->symbol
#define LOC_STACK   4           // Stack-relative (IY + e->offset)
#define LOC_IX      5           // IX-indexed (IX + e->offset)
#define LOC_INDIR   6           // Indirect through register e->reg
#define LOC_FLAGS   7           // Result in flags, e->cond specifies which

/*
 * Condition codes for LOC_FLAGS (e->cond field)
 * Encodes which flag and what sense for conditional jumps
 */
#define CC_NONE     0           // No condition
#define CC_Z        1           // Zero flag set (jp z)
#define CC_NZ       2           // Zero flag clear (jp nz)
#define CC_C        3           // Carry flag set (jp c)
#define CC_NC       4           // Carry flag clear (jp nc)
#define CC_M        5           // Sign flag set / negative (jp m)
#define CC_P        6           // Sign flag clear / positive (jp p)

/*
 * Register identifiers for e->reg and e->dest
 */
#define R_NONE      0
#define R_A         1           // 8-bit accumulator
#define R_HL        2           // 16-bit primary
#define R_DE        3           // 16-bit secondary
#define R_BC        4           // 16-bit tertiary / register var
#define R_IX        5           // Index register (struct pointer)
#define R_IY        6           // Frame pointer
#define R_SP        7           // Stack pointer
#define R_B         8           // 8-bit B register
#define R_C         9           // 8-bit C register

/*
 * Emit opcodes (EO_*) - scheduled instructions
 * Set by scheduler in e->ins[], blindly executed by emit
 */
enum {
    EO_NOP = 0,         // No operation (value already in place)

    /* Register-to-register moves */
    EO_HL_BC,           // ld h,b; ld l,c
    EO_HL_DE,           // ex de,hl (DE->HL)
    EO_DE_HL,           // ex de,hl (HL->DE)
    EO_DE_BC,           // ld d,b; ld e,c
    EO_BC_HL,           // ld b,h; ld c,l
    EO_A_L,             // ld a,l
    EO_A_H,             // ld a,h
    EO_A_B,             // ld a,b
    EO_A_C,             // ld a,c
    EO_L_A,             // ld l,a; ld h,0
    EO_HL_IX,           // push ix; pop hl
    EO_IX_HL,           // push hl; pop ix

    /* Memory loads - word to HL */
    EO_HL_IYW,          // ld l,(iy+ofs); ld h,(iy+ofs+1)
    EO_HL_IXW,          // ld l,(ix+ofs); ld h,(ix+ofs+1)
    EO_HL_MEM,          // ld hl,(symbol)
    EO_HL_CONST,        // ld hl,value

    /* Memory loads - long (4 bytes) to HLHL' */
    EO_HLHL_IYL,        // ld a,ofs; call getLiy
    EO_HLHL_IXL,        // ld a,ofs; call getLix

    /* Memory loads - word to DE */
    EO_DE_IYW,          // ld e,(iy+ofs); ld d,(iy+ofs+1)
    EO_DE_IXW,          // ld e,(ix+ofs); ld d,(ix+ofs+1)
    EO_DE_MEM,          // ld de,(symbol)
    EO_DE_CONST,        // ld de,value

    /* Memory loads - byte to A */
    EO_A_IY,            // ld a,(iy+ofs)
    EO_A_IX,            // ld a,(ix+ofs)
    EO_A_MEM,           // ld a,(symbol)
    EO_A_CONST,         // ld a,value
    EO_A_BC_IND,        // ld a,(bc)
    EO_A_HL_IND,        // ld a,(hl)

    /* Memory stores - word from HL */
    EO_IYW_HL,          // ld (iy+ofs),l; ld (iy+ofs+1),h
    EO_IXW_HL,          // ld (ix+ofs),l; ld (ix+ofs+1),h
    EO_MEM_HL,          // ld (symbol),hl
    EO_MEM_BC,          // ld (symbol),bc (ED 43) - 4 bytes
    EO_MEM_DE,          // ld (symbol),de (ED 53) - 4 bytes
    EO_BC_IND_HL,       // ld (bc),l; inc bc; ld (bc),h; dec bc

    /* Memory stores - byte from A */
    EO_IY_A,            // ld (iy+ofs),a
    EO_IX_A,            // ld (ix+ofs),a
    EO_MEM_A,           // ld (symbol),a
    EO_BC_IND_A,        // ld (bc),a
    EO_HL_IND_A,        // ld (hl),a

    /* Word arithmetic - HL */
    EO_ADD_HL_DE,       // add hl,de
    EO_ADD_HL_BC,       // add hl,bc
    EO_SBC_HL_DE,       // or a; sbc hl,de

    /* Word arithmetic - IX as accumulator */
    EO_ADD_IX_DE,       // add ix,de
    EO_ADD_IX_BC,       // add ix,bc
    EO_IX_CONST,        // ld ix,value

    /* IX register moves */
    EO_DE_IX,           // push ix; pop de

    /* Byte arithmetic */
    EO_ADD_A_N,         // add a,N (value in e->value)
    EO_SUB_A_N,         // sub N
    EO_AND_A_N,         // and N
    EO_OR_A_N,          // or N
    EO_XOR_A_N,         // xor N
    EO_CP_N,            // cp N (sets Z and C)

    /* Inc/Dec */
    EO_INC_HL,          // inc hl (no flags)
    EO_DEC_HL,          // dec hl (no flags)
    EO_INC_A,           // inc a (sets Z)
    EO_DEC_A,           // dec a (sets Z)
    EO_INC_IY,          // inc (iy+ofs) (sets Z)
    EO_DEC_IY,          // dec (iy+ofs) (sets Z)

    /* Flag tests */
    EO_OR_A,            // or a (sets Z from A)
    EO_AHORL,           // ld a,h; or l (sets Z from HL)

    /* Stack operations (spill/fill) */
    EO_PUSH_HL,         // push hl
    EO_PUSH_DE,         // push de
    EO_PUSH_AF,         // push af
    EO_POP_HL,          // pop hl
    EO_POP_DE,          // pop de
    EO_POP_AF,          // pop af

    /* Calls */
    EO_CALL,            // call symbol

    /* Type conversions */
    EO_WIDEN,           // ld l,a; ld h,0 (byte->word unsigned)
    EO_SEXT,            // sign extend A to HL

    EO_MAX              // Marker for enum size
};

/*
 * Expression tree node for code generation
 * This is the parse tree representation of expressions
 */
struct expr {
    unsigned char op;           // Operation ('+', '-', 'M', '=', '@', etc.)
    unsigned char size;         // Result size in bytes (1=byte, 2=short/ptr, 4=long/float, 8=double)
    unsigned char flags;        // E_UNSIGNED, E_UNUSED, etc.
    unsigned char opflags;      // OP_CONST, OP_REGVAR, etc. (operand patterns)
    unsigned char label;        // Label number (if needed for this expression)
    struct expr *left;          // Left operand
    struct expr *right;         // Right operand

    /* Type and value information from AST */
    char type_str;              // Type annotation char: 'b', 's', 'l', 'p', etc. (0 if none)
    long value;                 // Constant value (for numeric constants)
    char *symbol;               // Symbol name (for SYM nodes)

    /* Scheduling fields - set by codegen, used by emit */
    unsigned char loc;          // Location type: LOC_CONST, LOC_REG, LOC_MEM, etc.
    unsigned char reg;          // Register if LOC_REG or LOC_INDIR: R_A, R_HL, R_DE, etc.
    unsigned char dest;         // Destination register for result: R_HL, R_DE, R_A
    unsigned char cond;         // Condition code if LOC_FLAGS: CC_Z, CC_NZ, CC_C, etc.
    char offset;                // Stack/IX offset if LOC_STACK or LOC_IX (-128..127)

    /* Code generation fields */
    char *cleanup_block;        // Deferred cleanup code (for CALL stack cleanup)
    struct local_var *cached_var; // Cached variable lookup result (set by setLeftFlags)

    /* Scheduled instructions - set by scheduler, blindly emitted */
    unsigned char ins[3];       // Instructions to emit (EO_* opcodes)
    unsigned char nins;         // Number of instructions (0-3)
};

/*
 * Statement tree node for code generation
 * This is the parse tree representation of statements
 */
struct stmt {
    unsigned char type;         // Statement type ('I', 'W', 'R', 'B', 'E', etc.)
    unsigned char label;        // Primary label number for this statement
    unsigned char label2;       // Secondary label (for if/else end)
    struct expr *expr;          // Expression (condition for if/while, value for return, etc.)
    struct expr *expr2;         // Second expression (for for-loops, assignments, etc.)
    struct expr *expr3;         // Third expression (for for-loops)

    struct stmt *then_branch;   // Then/body branch
    struct stmt *else_branch;   // Else branch (or NULL)
    struct stmt *next;          // Next statement in sequence

    char *symbol;               // Symbol name (for labels, declarations)
    char type_str;              // Type annotation char: 'b', 's', 'l', 'p' (for declarations)
    char frm_off;               // Frame offset for declarations (signed: params +, locals -)

    /* Code generation fields */
    char *asm_block;            // Inline assembly text (type 'A' statements only)
};

/*
 * Local variable entry - tracks automatic variables and parameters in stack frame
 */
struct local_var {
    char *name;                 // Variable name
    char offset;                // Offset in stack frame (-128..127)
    unsigned char size;         // Size in bytes
    unsigned char is_param;     // 1 if parameter, 0 if local variable
    unsigned char is_array;     // 1 if array, 0 if scalar (arrays can't be in registers)
    unsigned char ref_count;    // Number of times variable is referenced
    unsigned char agg_refs;     // Number of struct/array member accesses (for IX allocation)
    unsigned char reg;          // Allocated register (REG_NO if on stack)
    struct local_var *next;     // Next in linked list
};

/*
 * Function context globals - state for current function being compiled
 */
extern char *fnName;            /* Function name */
extern char *fnRettype;         /* Return type */
extern struct stmt *fnBody;     /* Function body statement tree */
extern unsigned short fnLblCnt; /* For generating unique labels */
extern struct local_var *fnLocals;  /* List of local variables */
extern char fnFrmSize;          /* Total stack frame size in bytes */
extern char fnCallStk;          /* Call argument bytes to clean at exit */
extern unsigned char fnCurLbl;  /* Current label for lifetime tracking */
extern char fnDESaveCnt;        /* Counter for nested DE saves */
extern char fnDInUse;           /* Flag: D register holds spilled byte */
extern char fnPendClean;        /* Bytes to clean up after CALL */
extern char fnLoopDep;          /* Nesting depth of loops */
extern char fnDEValid;          /* 1 if DE holds valid value */
extern char fnTargetDE;         /* 1 if next expr should load to DE */
extern char fnZValid;           /* 1 if Z flag valid for HL test */
extern char fnCondOnly;         /* 1 if only condition flags needed, not value */
extern char fnDualCmp;          /* 'L' or '>' for two-test cmp with 0 */
extern char fnDualReg;          /* Register for fnDualCmp: R_BC, R_HL, R_DE */
extern char fnCmpFlag;          /* 0=none, 'Z'=z, 'N'=nz, 'C'=c, 'c'=nc */
extern char fnIXAOfs;           /* When >=0, A has byte from (ix+fnIXAOfs) */
extern char fnIXHLOfs;          /* When >=0, HL has word from (ix+fnIXHLOfs) */
extern char fnIXHL32;           /* When 1, HL/HL' has long from (ix+fnIXHLOfs) */
extern char fnIYHLOfs;          /* When valid, HL has word from (iy+fnIYHLOfs) */
extern char fnIYHLValid;        /* 1 if fnIYHLOfs is valid */
extern char fnABCValid;         /* 1 if A has byte from (bc) */
extern char fnAZero;            /* 1 if A is known to be 0 */
extern char fnARegvar;          /* REG_B/REG_C if A has that regvar value, 0 otherwise */

/* Label generation - 255 is reserved as "no label" sentinel */
#ifdef DEBUG
#define newLabel() (fnLblCnt >= 65534 ? (fdprintf(2, "fatal: label overflow in %s\n", fnName), exit(1), 0) : fnLblCnt++)
#else
#define newLabel() (fnLblCnt >= 65534 ? 65534 : fnLblCnt++)
#endif

/* Forward declarations from util.c */
int fdprintf(unsigned char fd, const char *fmt, ...);
int fdputs(unsigned char fd, const char *s);

/* Trace options (similar to VERBOSE in cc1) */
#ifdef DEBUG
#include "trace2.h"
#define TRACE(x) (trace & (x))
extern int trace;
#else
#define TRACE(x) 0
#endif

/* Global variables */
extern unsigned char outFd;  /* Assembly output file descriptor (from parseast.c) */
extern int fnIndex;          /* Function index for unique labels (from parseast.c) */
extern struct expr nullExpr; /* Sentinel for null-safe derefs (from emitops.c) */

/* Tree construction functions */
struct expr *newExpr(unsigned char op);
struct stmt *newStmt(unsigned char type);
void freeExpr(struct expr *e);
void frStmt(struct stmt *s);

/* Width and signedness extraction from type annotations */
unsigned char getSizeFTStr(unsigned char type_str);
unsigned char getSizeFromTN(const char *typename);
unsigned char getSignFTStr(unsigned char type_str);

/* Pattern recognizers */
int isStructMem(struct expr *e, char **out_var, long *out_offset);
int isMulByPow2(struct expr *e, struct expr **out_expr);

/* Code generation functions (codegen.c) */
void assignFrmOff(void);
void setOpFlags(void);
void dumpFnAst(char fd);
void dumpScheduled(char fd);
void specialize(void);
void scheduleCode(void);
void sched2Code(void);      /* New prescriptive scheduler */
void generateCode(void);
void optFrmLayout(void);
struct local_var *findVar(const char *symbol);

/* Symbol tracking (parseast.c) */
void addRefSym(const char *name);

/* Code emission functions (emit.c) */
void emitAssembly(char outFd);

#endif /* CC2_H */

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
