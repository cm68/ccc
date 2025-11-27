/*
 * cc2.h - Data structures for code generation (pass 2)
 *
 * Extends the basic expr/stmt structures from cc1.h with
 * code generation fields (asm_block and label numbers)
 */
#ifndef CC2_H
#define CC2_H

#include <stdlib.h>
#include <string.h>

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

    /* Byte registers (4 available) */
    REG_B,          /* B register */
    REG_C,          /* C register */
    REG_Bp,         /* B' (alternate) register */
    REG_Cp,         /* C' (alternate) register */

    /* Word registers (3 available) */
    REG_BC,         /* BC register pair */
    REG_BCp,        /* BC' (alternate) register pair */
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
 * Expression flags
 */
#define E_UNSIGNED  0x01        // Value is unsigned
#define E_UNUSED    0x02        // Value is not used (result discarded)
#define E_GENERATED 0x04        // Node has been code-generated (asm_block emitted)

/*
 * Expression tree node for code generation
 * This is the parse tree representation of expressions
 */
struct expr {
    unsigned char op;           // Operation ('+', '-', 'M', '=', '@', etc.)
    unsigned char size;         // Result size in bytes (1=byte, 2=short/ptr, 4=long/float, 8=double)
    unsigned char flags;        // E_UNSIGNED, etc.
    unsigned char label;        // Label number (if needed for this expression)
    struct expr *left;          // Left operand
    struct expr *right;         // Right operand

    /* Type and value information from AST */
    char *type_str;             // Type annotation from AST (":s", ":b", ":l", ":p", etc.)
    long value;                 // Constant value (for numeric constants)
    char *symbol;               // Symbol name (for SYM nodes)

    /* Code generation fields */
    char *asm_block;            // Generated assembly code (or NULL)
    char *cleanup_block;        // Deferred cleanup code (for CALL stack cleanup)
    struct jump_instr *jump;    // Jump instruction (deferred) or NULL
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
    char *type_str;             // Type annotation (for declarations)

    /* Code generation fields */
    char *asm_block;            // Generated assembly code (or NULL)
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
extern int fnLblCnt;            /* For generating unique labels */
extern struct local_var *fnLocals;  /* List of local variables */
extern int fnFrmSize;           /* Total stack frame size in bytes */
extern int fnCurLbl;            /* Current label for lifetime tracking */
extern int fnDESaveCnt;         /* Counter for nested DE saves */
extern int fnDInUse;            /* Flag: D register holds spilled byte */
extern int fnPendClean;         /* Bytes to clean up after CALL */
extern int fnLoopDep;           /* Nesting depth of loops */
extern int fnDEValid;           /* 1 if DE holds valid value */
extern int fnZValid;            /* 1 if Z flag valid for HL test */
extern struct expr *fnHLCache;  /* Shallow copy of expr in HL */
extern struct expr *fnDECache;  /* Shallow copy of expr in DE */
extern struct expr *fnACache;   /* Shallow copy of byte expr in A */
extern char fnIXAOfs;           /* When >=0, A has byte from (ix+fnIXAOfs) */
extern char fnIXHLOfs;          /* When >=0, HL has word from (ix+fnIXHLOfs) */

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

/* Tree construction functions */
struct expr *newExpr(unsigned char op);
struct stmt *newStmt(unsigned char type);
void freeExpr(struct expr *e);
void frStmt(struct stmt *s);

/* Jump instruction management */
struct jump_instr *newJump(enum jump_type type, int target_label);
void freeJump(struct jump_instr *j);

/* Width and signedness extraction from type annotations */
unsigned char getSizeFTStr(const char *type_str);
unsigned char getSizeFromTN(const char *typename);
unsigned char getSignFTStr(const char *type_str);

/* Pattern recognizers */
int isStructMem(struct expr *e, char **out_var, long *out_offset);
int isMulByPow2(struct expr *e, struct expr **out_expr);

/* Code generation functions (codegen.c) */
void assignFrmOff(void);
void generateCode(void);
void optFrmLayout(void);
void allocRegs(void);
struct local_var *findVar(const char *symbol);

/* Symbol tracking (parseast.c) */
void addRefSym(const char *name);

/* Code emission functions (emit.c) */
void emitAssembly(int outFd);

#endif /* CC2_H */

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
