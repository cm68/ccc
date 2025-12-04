# CLAUDE.md

## Project Overview

**ccc** - a native C compiler written in C. 2-pass compiler:
1. **Pass 1** (cc1): Recursive descent parser with embedded CPP - parses C code
2. **Pass 2** (cc2): Code generator and assembler (in development)

**Status**: Pass 1 complete (~7,500 lines). All 142 tests pass. Self-hosting.

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

**ALWAYS use make rules.** Never run `./cc1 -E file.c` directly.

```bash
# CORRECT
make expr.ast     # Uses required flags: -DCCC -i./include -I.
make expr.s

# WRONG
./cc1 -E expr.c   # Missing required flags!
```

**Before any commit:**
1. `make stage1` - verify complete pipeline
2. `make test` - verify all tests pass

## Running the Compiler

```bash
./ccc -k source.c              # Compile with cc2, keep AST file
./cc1 -E source.c              # Preprocessor only
./cc1 -v 0x3f -E file.c        # Maximum verbosity
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

### Auto-Generated Files (do NOT edit)

enumlist.h, tokenlist.c, error.h, debug.h, debugtags.c, op_pri.h

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

## Known Issues

- 'signed' keyword not supported
- Anonymous struct/union don't work properly

## Code Style

- Minimize code size, even at cost to clarity
