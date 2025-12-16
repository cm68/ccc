# cc2 Architecture - Code Generation Pass

## Overview

cc2 is the second pass of the ccc compiler. It reads AST from cc1 (compact
paren-free hex format) and generates Z80 assembly code.

## Two-Phase Architecture

cc2 uses a two-phase approach for code generation:

```
AST Input -> Parse -> Schedule/Emit -> Assembly Output
```

**Note:** Register allocation is performed by cc1 (outast.c) and communicated
to cc2 via the AST. cc2 receives variables already assigned to registers or
stack offsets.

### Phase 1: Parse (parseast.c)

Reads the AST and builds in-memory trees:
- `struct expr` - Expression tree nodes
- `struct stmt` - Statement tree nodes
- `struct local_var` - Variable information (name, type, offset, register)

All loops are pre-lowered to labeled if/goto by cc1, simplifying cc2.

Register assignments are parsed from the AST declaration format:
`d<type><hexname><reg><offset>` where reg is:
- `00` - No register (on stack)
- `01` - B register (byte)
- `02` - C register (byte)
- `03` - BC register pair (word)
- `04` - IX register (word, for struct pointers)

### Phase 2: Schedule/Emit (codegen.c, emitexpr.c, emitops.c)

The scheduler annotates expression trees with:
- **Location** (`e->loc`): Where the value is/goes
  - `LOC_REG` - In a register (BC, IX)
  - `LOC_STACK` - On stack (IY-indexed)
  - `LOC_MEM` - Global memory
  - `LOC_CONST` - Constant value
  - `LOC_IX` - IX-indexed pointer dereference
  - `LOC_INDIR` - Indirect through HL
  - `LOC_FLAGS` - Result in condition flags

- **Destination** (`e->dest`): Target register for result
  - `R_A` - Accumulator (byte operations)
  - `R_HL` - Primary word register
  - `R_DE` - Secondary word register
  - `R_BC` - BC register pair
  - `R_IX` - IX register
  - `R_NONE` - No register needed

The emitters walk scheduled trees and output Z80 assembly.

## Data Structures

### Expression Node
```c
struct expr {
    unsigned char op;       // Operation: '+', '-', 'M', '=', '@', etc.
    unsigned char size;     // Result size in bytes (1, 2, or 4)
    unsigned char flags;    // E_UNSIGNED, E_LVALUE, E_FLOAT, etc.
    unsigned char opflags;  // Scheduler flags: OP_REGVAR, OP_IYMEM, etc.
    struct expr *left;      // Left operand
    struct expr *right;     // Right operand
    char *symbol;           // Symbol name (for $ nodes)
    long value;             // Constant value (for # nodes)

    // Scheduler annotations
    unsigned char loc;      // Location type
    unsigned char dest;     // Destination register
    unsigned char reg;      // Source register (if LOC_REG)
    unsigned char cond;     // Condition code (if LOC_FLAGS)
    int offset;             // Stack/IX offset
    struct local_var *cached_var;  // Variable info for optimizations
};
```

### Statement Node
```c
struct stmt {
    unsigned char type;     // 'B'lock, 'I'f, 'E'xpr, 'R'eturn, etc.
    struct expr *expr;      // Condition or expression
    struct stmt *then_branch;  // Then/body
    struct stmt *else_branch;  // Else branch
    struct stmt *next;      // Next statement in sequence
    char *symbol;           // Label name (for L/G statements)
};
```

### Local Variable
```c
struct local_var {
    char *name;             // Variable name
    int size;               // Size in bytes
    int offset;             // IY offset (params positive, locals negative)
    int ref_count;          // Reference count (from cc1 analysis)
    int reg;                // Allocated register (0=none, 1=B, 2=C, 3=BC, 4=IX)
    char is_param;          // Is this a parameter?
    struct local_var *next;
};
```

## Register Allocation (Done in cc1)

### Available Registers
- **BC** - Word variable (most common)
- **IX** - Word variable (preferred for struct pointers)
- **B** or **C** - Byte variables

### Allocation Strategy (in cc1/outast.c)
1. Count references to each variable during expression walks
2. Track aggregate accesses (ptr->field patterns)
3. Allocate IX to pointer variables with struct member accesses
4. Allocate BC to highest-ref word variable
5. Remaining variables stay on stack (IY-indexed)

cc2 receives these allocations via the AST and generates appropriate code.

### IX-Indexed Optimization

When a pointer variable is allocated to IX, dereferences use direct indexed
addressing:

```c
struct foo *p;  // p allocated to IX
x = p->field;   // Generates: ld r,(ix+offset)
```

## Stack Frame Layout

```
Higher addresses
+------------------+
| Parameters       |  IY + 4, IY + 6, ...
+------------------+
| Return address   |  IY + 2
+------------------+
| Saved IY         |  IY + 0  <-- Frame pointer
+------------------+
| Local variables  |  IY - 2, IY - 4, ...
+------------------+
| Saved registers  |  BC, IX if used as regvars
+------------------+
Lower addresses
```

Frame management via runtime library:
- `framealloc` - Allocate frame, save IY, push regvars
- `framefree` - Pop regvars, restore IY, deallocate frame

## Code Generation Patterns

### Variable Access

**Register variable (BC):**
```asm
ld h, b         ; Load to HL
ld l, c
ld b, h         ; Store from HL
ld c, l
```

**Stack variable (IY-indexed):**
```asm
ld l, (iy + offset)    ; Load word
ld h, (iy + offset+1)
ld (iy + offset), l    ; Store word
ld (iy + offset+1), h
```

**IX-indexed pointer:**
```asm
ld c, (ix + 0)         ; Load word through IX
ld b, (ix + 1)
ld c, (ix + 2)         ; Struct member at offset 2
ld b, (ix + 3)
```

### Long (32-bit) Operations

Uses HL:HL' (primary/alternate register pair):
```asm
ld hl, low_word
exx
ld hl, high_word       ; HLHL' = 32-bit value
exx
```

Arithmetic via helper functions: `_ladd`, `_lsub`, `_lmul`, `_ldiv`

### Float Operations

Float and double are both 32-bit IEEE 754. All operations call helpers:
- `_fadd`, `_fsub`, `_fmul`, `_fdiv` - Arithmetic
- `_fcmp` - Comparison
- `_itof`, `_ftoi` - Conversions

Calling convention:
- Left operand: DEDE' (pushed, popped into DE/DE')
- Right operand: HLHL'
- Result: HLHL'

### Function Calls

```asm
; Push arguments right-to-left
push bc         ; Last arg
ld hl, value
push hl         ; First arg
call _function
pop af          ; Clean up N bytes
pop af
; Result in HL (word), A (byte), or HLHL' (long/float)
```

## File Organization

- **cc2.c** - Main entry, command-line processing
- **parseast.c** - AST parser, builds stmt/expr trees from hex format
- **astio.c** - Low-level AST I/O (character reading, hex parsing)
- **codegen.c** - Scheduler: pattern recognition, instruction selection
- **emitexpr.c** - Expression emission dispatcher
- **emitops.c** - Operation emitters (assign, binop, call, incdec, etc.)
- **emithelper.c** - Helper functions (loadVar, storeVar, emit strings)
- **emit.c** - Statement emission, emit table and string lookup
- **dumpast.c** - Debug AST dumper

## Debug Support

When compiled with `-DDEBUG`:
- `-v` flags enable tracing (T_EXPR, T_ASSIGN, T_VAR, etc.)
- Function headers include variable info comments:
  ```asm
  ; frame=4
  ;   local x: size=2 iy-2 refs=5 reg=BC
  ;   param argc: size=2 iy+4 refs=3 reg=-
  ```
- Scheduled tree dumped before emission

## Current Status

**Complete:**
- Full AST parsing (hex format with register assignments)
- All expression types (unary, binary, ternary, calls)
- All statement types (IF, loops lowered to labels, switch)
- Long (32-bit) arithmetic via helpers
- Float support (32-bit IEEE 754) via helpers
- Struct copy (Y operator)
- Bitfield extract/assign
- IX-indexed struct access optimization

**Note:** Register allocation moved to cc1 for whole-function analysis.
