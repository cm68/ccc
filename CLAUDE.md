# CLAUDE.md

## Project Overview

**ccc** - a native C compiler written in C. 2-pass compiler:
1. **Pass 1** (cc1): Recursive descent parser with embedded CPP - parses C code
2. **Pass 2** (cc2): Code generator for Z80 target

**Status**: Both passes complete. All 142 tests pass. Small programs run in simulation.

## CRITICAL: Memory Footprint Constraint

Must fit in **<64KB total (code + data)** when compiled natively.

- Minimize code duplication; find symmetry and reuse patterns
- Consolidate consecutive output calls
- Prefer single-pass algorithms
- Use compact data structures

## Build Commands

```bash
make              # Build compiler
make test         # Run all 142 tests
make stage1       # Verify complete pipeline (parse -> codegen -> assemble)
make unit-tests   # Run unit tests
make regen        # Regenerate auto-generated files
make clean        # Remove build artifacts
```

## CRITICAL: Testing Validation Protocol

**ALWAYS use make rules** to ensure correct flags are passed.

```bash
# CORRECT
make expr.ast     # Uses required flags: -DCCC -i./include -I.
make expr.s
```

**Before any commit:**
1. `make stage1` - verify complete pipeline
2. `make test` - verify all tests pass

**Before any push:**
- `make sizecheck` - updates prev.size with current emitted code sizes
- Commit prev.size with your changes to memorialize the current emitted size

## Running the Compiler

```bash
./ccc -k source.c              # Compile to .s, keep intermediate .ast file
make source.ast                # Generate AST only
make source.s                  # Generate assembly
```

## Running in Simulation

Programs can be run in the Z80 simulator after linking:

```bash
cd tests
../root/bin/ccc -o program program.c
../root/sim ./program arg1 arg2
```

## Architecture

### Code Organization

- **cc1.c**: Main entry, command-line processing
- **lex.c**: Lexer with embedded CPP
- **parse.c**: Statement/declaration parser
- **expr.c**: Expression parsing, constant folding, K&R implicit declarations
- **type.c**: Type system (primitives, pointers, arrays, functions, structs)
- **declare.c**: Declaration parsing (K&R and ANSI)
- **outast.c**: AST emission (compact paren-free hex format)
- **kw.c**: Keyword tables
- **io.c**: I/O, file stack, macro buffer
- **macro.c**: Macro definition/expansion
- **error.c**: Error reporting
- **util.c**: Utilities, fdprintf

**Pass 2:**
- **parseast.c**: AST parser, builds expression/statement trees
- **astio.c**: Low-level AST I/O
- **codegen.c**: Scheduling passes (demand calculation, dest assignment, instruction selection)
- **dumpast.c**: AST dump with scheduling annotations
- **emitexpr.c**: Emit scheduled instructions (slave to scheduler)
- **emitops.c**: Emit helpers for specific operations
- **emit.c**: Statement-level emission

### cc2 Scheduler Architecture

Three-phase code generation with strict separation of concerns:

**Phase 1: Demand Calculation (bottom-up)**
- Walk tree bottom-up calculating temp register demand
- Leaves (constants, simple derefs): demand = 1
- Binary ops: demand = max(left_demand, right_demand + 1)
- Function calls: demand = 3 (clobbers all temps)
- Set `e->spill = 1` on binops where right child demand > 2

**Phase 2: Dest Assignment (top-down)**
- Assign destination registers based on tree position
- Binary ops: left child → DE, right child → HL
- Result always in HL (primary accumulator)
- Dests propagate down - children respect parent's assignment

**Phase 3: Instruction Selection**
- Walk tree with dests already assigned
- Select instructions that produce results in the assigned dest
- Z80 has direct load-to-DE instructions: `ld de,const`, `ld de,(addr)`,
  `ld e,(iy+n); ld d,(iy+n+1)` - use these when dest=DE
- Only case requiring `ex de,hl`: function call result when dest=DE
  (calls always return in HL per calling convention)
- Populate `e->ins[]` array with selected instructions

**Emit Phase (slave)**
- Strictly mechanical - no decision making
- Check `e->spill` flag → emit `push de` before right child, `pop de` after
- Execute `e->ins[]` array in order
- All register allocation decisions made by scheduler

**Register Model:**
- HL: primary accumulator (word results)
- DE: secondary accumulator (left operand of binops)
- A: byte accumulator
- BC: available for register variables
- IX: struct pointer register variable
- IY: frame pointer (stack locals)

**Calling Convention:**
Controlled by `#define CALLER_FREE` or `#define CALLEE_FREE` in cc2.h:
- **CALLER_FREE** (default): Caller cleans up arguments after call returns.
  Arguments pushed, call made, then `inc sp` or `pop de` to clean stack.
- **CALLEE_FREE**: Callee cleans up arguments before returning.
  Requires modified framefree to skip args (not yet implemented).

### Auto-Generated Files (do NOT edit)

enumlist.h, tokenlist.c, error.h, debug.h, debugtags.c

### Key Data Structures

- **struct expr** (cc1.h): Expression tree nodes
- **struct stmt** (cc1.h): Statement nodes
- **struct type** (cc1.h): Type descriptors
- **struct name** (cc1.h): Symbol table entries

### AST Format

The AST uses a compact paren-free hex format with counted children and
dot-terminated hex numbers. Memory ops use size suffixes:
`:b` (byte), `:s` (short), `:l` (long), `:p` (pointer)

```c
char c; int i;
c = 10;    // =b$_c a.
i = c;     // =s$_i Mb$_c
```

### Type Conversions

- **N** (NARROW): Truncate to smaller type
- **W** (WIDEN): Zero-extend unsigned
- **X** (SEXT): Sign-extend signed

### Function/Array Decay

- Functions and arrays decay to pointers (no DEREF wrapper)
- Variables get DEREF wrapper: `x` -> `M$_x`

### Value Union Optimization

The `e->value` field in struct expr uses a union to reduce code size:

```c
union { long l; short s; char c; } value;
```

Access the appropriately-sized member to avoid 32-bit operations when unnecessary:
- `e->value.c` for byte values (shift counts, small constants, argc)
- `e->value.s` for 16-bit values (most constants, offsets)
- `e->value.l` for 32-bit values (long constants, float bit patterns)

This saves ~1200 bytes in cc2 by eliminating 32-bit load/mask operations where
8-bit or 16-bit access suffices. Example: `e->value.c & 0xff` becomes just
`e->value.c` since .c is already char.

### Float Support

Float and double are both 32-bit IEEE 754. Code size strategies used:

1. **Reuse long infrastructure**: Float is size 4, same as long. Uses HLHL' register
   pair and existing 32-bit load/store code paths.

2. **Helper functions for all operations**: No inline float math. All arithmetic
   (+, -, *, /) and comparisons call external helpers (_fadd, _fsub, _fmul, _fdiv,
   _fcmp). Keeps compiler small; float library can be optimized separately.

3. **Single flag for float detection**: E_FLOAT flag on expressions distinguishes
   float from long. Intercept before long handling, delegate to helpers.

4. **Reuse libc for parsing**: Float literals parsed with atof() - no custom
   float-to-binary converter in the compiler.

5. **Minimal type system changes**: Float uses 'f' type suffix in AST, same
   encoding as other types. Double aliased to float (both 4 bytes).

6. **Conversion helpers**: _itof (int→float) and _ftoi (float→int) avoid inline
   conversion code.

Calling convention for float helpers:
- Left operand: DEDE' (pushed to stack, popped into DE/DE')
- Right operand: HLHL'
- Result: HLHL'

## Known Issues

- 'signed' keyword not supported
- Anonymous struct/union don't work properly

## Code Style

- Minimize code size, even at cost to clarity
- **Identifier limit: 13 characters max** - use camelCase to preserve meaning
  - Good: `notConstStore`, `emitRegVarDrf`, `fnIXHLOfs`
  - Bad: `not_const_store` (too long), `noCstSt` (loses meaning)
