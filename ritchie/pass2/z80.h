/*
 * Z80-specific definitions for Ritchie C pass2
 */

/*
 * Z80 Register Model
 *
 * HL  - primary accumulator, function return value
 * DE  - secondary accumulator (binary op temporary)
 * BC  - register variable
 * IX  - register variable (struct pointers)
 * IY  - stack frame pointer
 * A   - byte accumulator
 * SP  - stack pointer
 *
 * Stack frame layout:
 *        +--------+
 *        | arg N  |  IY + positive offset
 *        +--------+
 *        | arg 1  |
 *        +--------+
 *        | ret PC |  IY + 2
 *        +--------+
 *        | old IY |  IY + 0 (saved by prologue)
 *        +--------+
 *        | local1 |  IY - 2 (negative offsets)
 *        +--------+
 *        | local2 |  IY - 4
 *        +--------+  <- SP
 */

/*
 * Register indices for code generation
 * These map to the register names used in output
 */
#define R_HL    0				/* primary accumulator */
#define R_DE    1				/* secondary accumulator */
#define R_BC    2				/* register variable */
#define R_IX    3				/* register variable (structs) */
#define R_IY    4				/* frame pointer */
#define R_A     5				/* byte accumulator */
#define R_SP    6				/* stack pointer */

/*
 * Number of registers available for register variables
 * BC and IX only (IY is frame pointer)
 */
#define NREG    2

/*
 * Calling convention: CALLERCLN=1 means caller cleans args
 * Define CALLER_CLEAN or CALLEE_CLEAN to select
 */
#ifdef CALLEE_CLEAN
#define CALLERCLN 0
#else
#define CALLERCLN 1
#endif

/*
 * Stack frame offsets
 * Arguments are at positive offsets from IY
 * Locals are at negative offsets from IY
 */
#define STARG   4				/* first arg offset: old_IY(2) + ret_PC(2) 
								 */
#define STAESSION -2			/* first auto offset (grows negative) */

/*
 * Long (32-bit) operation storage
 * lL and lR are 4-byte memory locations for long operands
 */
extern char *lL;				/* left operand storage */
extern char *lR;				/* right operand storage */

/*
 * Helper function names for operations Z80 can't do directly
 */
#define H_MUL16   "mul16"		/* HL = HL * DE */
#define H_DIV16   "div16"		/* HL = HL / DE, DE = remainder */
#define H_UDIV16  "udiv16"		/* unsigned divide */
#define H_MOD16   "mod16"		/* HL = HL % DE */
#define H_UMOD16  "umod16"		/* unsigned modulo */

#define H_LADD    "ladd"		/* lR = lL + lR */
#define H_LSUB    "lsub"		/* lR = lL - lR */
#define H_LMUL    "lmul"		/* lR = lL * lR */
#define H_LDIV    "ldiv"		/* lR = lL / lR */
#define H_LMOD    "lmod"		/* lR = lL % lR */
#define H_LCMP    "lcmp"		/* compare lL vs lR, set flags */
#define H_LNEG    "lneg"		/* lR = -lR */
#define H_LCOM    "lcom"		/* lR = ~lR */
#define H_LSHL    "lshl"		/* lR = lR << A */
#define H_LSHR    "lshr"		/* lR = lR >> A (signed) */
#define H_LUSHR   "lushr"		/* lR = lR >> A (unsigned) */

#define H_ULDIV   "uldiv"		/* unsigned long divide */
#define H_ULMOD   "ulmod"		/* unsigned long modulo */

/*
 * Type/size indicators
 */
#define TBYTE   1				/* 8-bit byte */
#define TWORD   2				/* 16-bit word */
#define TLONG   4				/* 32-bit long */

/*
 * vim: set tabstop=4 shiftwidth=4 noexpandtab: 
 */
