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
 * Jump instruction types
 */
enum jump_type {
    JUMP_NONE = 0,
    JMP_UNCOND,     /* jp label */
    JUMP_IF_ZERO,          /* jr z, label or jp z, label */
    JMP_IF_NOT_Z,      /* jr nz, label or jp nz, label */
    JUMP_IF_CARRY,         /* jr c, label or jp c, label */
    JMP_IF_NOT_C,     /* jr nc, label or jp nc, label */
    JUMP_CALL              /* call label */
};

/*
 * Jump instruction node - deferred until emission for optimization
 */
struct jump_instr {
    unsigned char type;         // Type of jump
    unsigned char target_label; // Target label number (0-255)
    char *condition;            // Condition string (for conditional jumps)
    struct jump_instr *next;    // Next in list
};

/*
 * Label mapping for jump optimization
 * Compact: 4 bytes per entry (max 256 labels per function)
 */
struct labelMap {
    unsigned char label;        // Label number (0-255)
    unsigned char target;       // Target label (255 = no target)
    unsigned char jump_type;    // Type of jump at this label
    unsigned char refcnt;       // Number of jumps targeting this label
};

/*
 * Expression flags (e->flags)
 */
#define E_UNSIGNED  0x01        // Value is unsigned
#define E_UNUSED    0x02        // Value is not used (result discarded)
#define E_GENERATED 0x04        // Node has been code-generated (scheduled/emitted)
#define E_IXASSIGN  0x08        // ASSIGN to IX-indexed struct member
#define E_IXDEREF   0x10        // DEREF of IX-indexed struct member

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
    struct jump_instr *jump;    // Jump instruction (deferred) or NULL
    struct local_var *cached_var; // Cached variable lookup result (set by setLeftFlags)
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

    /* Code generation fields */
    char *asm_block;            // Inline assembly text (type 'A' statements only)
    struct jump_instr *jump;    // Jump instruction (deferred) or NULL
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
    unsigned char first_label;  // First label where variable is used (255 if unused)
    unsigned char last_label;   // Last label where variable is used (high water mark)
    unsigned char ref_count;    // Number of times variable is referenced
    unsigned char agg_refs;     // Number of struct/array member accesses (for IX allocation)
    unsigned char reg;          // Allocated register (REG_NO if on stack)
    struct local_var *next;     // Next in linked list
};

/*
 * Function context globals - state for current function being compiled
 */
extern char *fnName;            /* Function name */
extern char *fnParams;          /* Parameter list string */
extern char *fnRettype;         /* Return type */
extern struct stmt *fnBody;     /* Function body statement tree */
extern unsigned char fnLblCnt;  /* For generating unique labels (0-254, 255=overflow) */
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
extern char fnIYHLOfs;          /* When valid, HL has word from (iy+fnIYHLOfs) */
extern char fnIYHLValid;        /* 1 if fnIYHLOfs is valid */
extern char fnABCValid;         /* 1 if A has byte from (bc) */
extern char fnAZero;            /* 1 if A is known to be 0 */

/* Label generation with overflow check */
#ifdef DEBUG
#define newLabel() (fnLblCnt == 255 ? (fdprintf(2, "fatal: label overflow in %s\n", fnName), exit(1), 0) : fnLblCnt++)
#else
#define newLabel() (fnLblCnt++)
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

/* Tree construction functions */
struct expr *newExpr(unsigned char op);
struct stmt *newStmt(unsigned char type);
void freeExpr(struct expr *e);
void frStmt(struct stmt *s);

/* Jump instruction management */
struct jump_instr *newJump(enum jump_type type, int target_label);
void freeJump(struct jump_instr *j);

/* Width and signedness extraction from type annotations */
unsigned char getSizeFTStr(unsigned char type_str);
unsigned char getSizeFromTN(const char *typename);
unsigned char getSignFTStr(unsigned char type_str);

/* Pattern recognizers */
int isStructMem(struct expr *e, char **out_var, long *out_offset);
int isMulByPow2(struct expr *e, struct expr **out_expr);

/* Code generation functions (codegen.c) */
void assignFrmOff(void);
void analyzeVars(void);
void allocRegs(void);
void setOpFlags(void);
void dumpFnAst(char fd);
void dumpScheduled(char fd);
void specialize(void);
void scheduleCode(void);
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
