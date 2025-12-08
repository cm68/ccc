# cc2 Architecture - Code Generation Pass

## Overview

cc2 is the second pass of the ccc compiler. It reads AST from cc1 (compact
paren-free hex format) and generates Z80 assembly code.

## Three-Phase Architecture

cc2 uses a three-phase approach for code generation:

```
AST Input -> Parse -> Schedule -> Emit -> Assembly Output
```

### Phase 1: Parse (parseast.c)

Reads the AST and builds in-memory trees:
- `struct expr` - Expression tree nodes
- `struct stmt` - Statement tree nodes
- `struct local_var` - Variable information (name, type, offset, register)

All loops are pre-lowered to labeled if/goto by cc1, simplifying cc2.

### Phase 2: Schedule (codegen.c)

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

- **Offset** (`e->offset`): For IX/IY-indexed addressing
- **Condition** (`e->cond`): For comparison results (CC_Z, CC_NZ, CC_C, etc.)

The scheduler also:
- Allocates variables to registers (BC, IX for words; B, C for bytes)
- Identifies optimization patterns (IX-indexed struct access, comparison with 0)
- Specializes compound operations (bit ops, shifts, inc/dec)

### Phase 3: Emit (emitexpr.c, emitops.c, emithelper.c, emit.c)

Walks the scheduled trees and outputs Z80 assembly:
- `emitExpr()` - Main expression emitter, dispatches by op and location
- `emitAssign()` - Assignment operations
- `emitBinop()` - Binary operations with accumulator management
- `emitIncDec()` - Increment/decrement operations
- `emitCall()` - Function calls
- Helper functions for common patterns (loadVar, storeVar, etc.)

## Data Structures

### Expression Node
```c
struct expr {
    unsigned char op;       // Operation: '+', '-', 'M', '=', '@', etc.
    unsigned char size;     // Result size in bytes
    unsigned char flags;    // E_UNSIGNED, E_LVALUE, etc.
    unsigned char opflags;  // Scheduler flags: OP_REGVAR, OP_IYMEM, etc.
    struct expr *left;      // Left operand
    struct expr *right;     // Right operand
    char *symbol;           // Symbol name (for $ nodes)
    long value;             // Constant value (for C nodes)

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
    struct expr *expr2;     // Second expression (for-loops)
    struct expr *expr3;     // Third expression (for-loops)
    struct stmt *then_branch;  // Then/body
    struct stmt *else_branch;  // Else branch
    struct stmt *next;      // Next statement in sequence
};
```

### Local Variable
```c
struct local_var {
    char *name;             // Variable name
    int size;               // Size in bytes
    int offset;             // IY offset (negative for locals, positive for params)
    int ref_count;          // Reference count for register allocation
    int reg;                // Allocated register (REG_NO, REG_B, REG_C, REG_BC, REG_IX)
    char is_param;          // Is this a parameter?
    struct local_var *next;
};
```

## Register Allocation

### Available Registers
- **BC** - Word variable (most common)
- **IX** - Word variable (usually pointers for indexed access)
- **B** or **C** - Byte variables

### Allocation Strategy
1. Count references to each variable during parse
2. Sort by reference count
3. Allocate BC to highest-ref word variable
4. Allocate IX to second-highest (if pointer type preferred)
5. Remaining variables stay on stack (IY-indexed)

### IX-Indexed Optimization

When a pointer variable is allocated to IX, dereferences use direct indexed
addressing:

```c
struct foo *p;  // p allocated to IX
x = p->field;   // Generates: ld r,(ix+offset)
```

Pattern recognition in scheduler:
- `(M (M $ptr))` - Simple pointer deref, offset 0
- `(M (+ (M $ptr) const))` - Struct member access

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
| Saved registers  |  BC, IX if used
+------------------+
Lower addresses
```

Frame management:
- `framealloc` - Allocate frame, save IY
- `framefree` - Restore IY, deallocate frame

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

### Comparisons

**Word comparison with 0:**
```asm
ld a, h
or l            ; Z flag set if HL == 0
```

**General word comparison:**
```asm
; HL = left, DE = right
or a
sbc hl, de      ; Sets flags for comparison
```

### Increment/Decrement

**Register variable:**
```asm
inc bc          ; or dec bc
```

**Stack variable:**
```asm
dec (iy + offset)      ; Low byte
jr nc, $+3             ; Skip if no borrow
dec (iy + offset+1)    ; High byte
```

### Function Calls

```asm
; Push arguments right-to-left
push bc         ; Last arg
ld hl, value
push hl         ; First arg
call _function
pop af          ; Clean up N bytes (pop pairs for efficiency)
pop af
; Result in HL (word) or A (byte)
```

## Optimization Passes

### Dead Code Elimination
- Performed at AST emission time in cc1
- Removes unreachable code after unconditional jumps/returns
- Handles IF statement elimination when condition is constant

### Specialization (codegen.c)
- Bit operations: `var |= (1<<n)` -> `set n, r`
- Shifts: `var <<= n` -> `sla r` (repeated)
- Compound byte ops: `var += expr` -> `add a, r; ld r, a`

### Value Caching (regcache.c)
- Tracks what values are in A, HL
- Eliminates redundant loads
- Cleared on function calls and jumps

## File Organization

- **cc2.c** - Main entry, command-line processing
- **parseast.c** - AST parser, builds stmt/expr trees
- **astio.c** - Low-level AST I/O
- **codegen.c** - Scheduler: register allocation, pattern recognition
- **emitexpr.c** - Expression emission dispatcher
- **emitops.c** - Operation emitters (assign, binop, call, etc.)
- **emithelper.c** - Helper functions (loadVar, storeVar, emit strings)
- **emit.c** - Low-level emit table and string lookup
- **regcache.c** - Value caching for A and HL registers
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
- Full AST parsing
- Register allocation (BC, IX, B, C)
- Stack frame management
- All expression types
- All statement types (IF, loops lowered to labels)
- Function calls with argument passing
- Struct member access optimization
- Dead code elimination

**In Progress:**
- Long (32-bit) arithmetic
- Floating point support
- Further size optimization
