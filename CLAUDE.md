# CLAUDE.md

## Project Overview

**ccc** - Ritchie C compiler ported to Z80. Classic 2-pass architecture:
1. **Pass 1** (c0): Parser with embedded preprocessor (c00.c - c05.c)
2. **Pass 2** (c1): Code generator for Z80 target (c10.c - c13.c)

Based on the 2.11BSD portable C compiler, enhanced with C89/C90 features.

## Directory Structure

```
ws/           - Ritchie C compiler (pass1 + pass2) and Whitesmith tools
  c00.c-c05.c - Pass 1: parser
  c0.h        - Pass 1 header
  c10.c-c13.c - Pass 2: code generator
  c1.h        - Pass 2 header
  cvopt.c     - Code table preprocessor
  asz.c,asm.c - Z80 assembler
  wsld.c      - Linker
  wslib.c     - Librarian
  wsnm.c      - Object file dump
cpp/          - Standalone C preprocessor
attic/        - Old ccc (failed memory constraints, kept for reference)
native/       - Native runtime library
root/         - Installation target
```

## Build Commands

```bash
cd ws
make              # Build compiler and tools for Linux host
make stage1       # Self-compile pass1/pass2 with ccc
make test         # Run tests
make clean        # Remove build artifacts
```

## CRITICAL: Memory Footprint Constraint

Must fit in **<64KB total (code + data)** when compiled natively for Z80.

- Minimize code duplication
- Use compact data structures
- Prefer table-driven code generation

## Z80 Register Model

| Register | Purpose |
|----------|---------|
| **HL** | Primary accumulator, function return value |
| **DE** | Secondary accumulator (binary op temporary) |
| **BC** | Register variable |
| **IX** | Register variable (struct pointers) |
| **IY** | Stack frame pointer |
| **A** | Byte accumulator |
| **SP** | Stack pointer |

## Stack Frame Layout

```
        +--------+
        | arg N  |  IY + positive offset
        +--------+
        | arg 1  |
        +--------+
        | ret PC |  IY + 2
        +--------+
        | old IY |  IY + 0 (saved by prologue)
        +--------+
        | local1 |  IY - 2 (negative offsets)
        +--------+
        | local2 |  IY - 4
        +--------+  <- SP
```

## Calling Convention

Controlled by `#ifdef CALLER_CLEAN` / `#ifdef CALLEE_CLEAN`:
- **CALLER_CLEAN**: Caller pops arguments after call
- **CALLEE_CLEAN**: Callee pops arguments before ret

Return values: HL (word), A (byte), lR memory (long)

## Pass 1 Architecture (c00-c05)

- **c00.c**: Main entry, file handling
- **c01.c**: Lexer, symbol table
- **c02.c**: External definitions, functions
- **c03.c**: Declarations, types
- **c04.c**: Expression parsing, tree output
- **c05.c**: Statements

Key data structures in c0.h:
- `struct tnode` - Expression tree node
- `struct nmlist` - Symbol table entry
- `paraml/parame` - Parameter list

## Pass 2 Architecture (c10-c13)

Table-driven code generation:
- **c10.c**: Main codegen, `rcexpr()`, `cexpr()`, `match()`
- **c11.c**: Input parser `getree()`, branches, output
- **c12.c**: Optimizer, type utilities, register degree
- **c13.c**: Instruction tables (Z80 mnemonics)
- **cvopt.c**: Code table preprocessor

Code tables in c13.c define instruction patterns:
```c
struct optab addtab[] = {
    { 0, TWORD, 0, TWORD, "add hl,de\n" },
    { 0, 0, 0, 0, 0 }
};
```

## Intermediate Code Format

Pass 1 outputs binary intermediate code to Pass 2:
- Operator codes with type information
- Expression trees in postfix form
- Symbol references with storage class and offset

Key opcodes: EXPR, CBRANCH, BRANCH, LABEL, NAME, CON, LCON

## Assembler Syntax (asz)

The target assembler is `ws/asz`. Pseudo-ops:

| Directive | Purpose |
|-----------|---------|
| `.text` | Switch to text segment |
| `.data` | Switch to data segment |
| `.bss` | Switch to BSS segment |
| `.globl sym` | Export symbol |
| `.db n` | Define byte(s) |
| `.dw n` | Define word(s) |
| `.dl n` | Define long(s) |
| `.ds n` | Reserve n bytes |
| `sym:` | Local label |
| `sym::` | Exported label |

## Type Sizes

```
SZCHAR  = 1    (char)
SZINT   = 2    (int, short)
SZPTR   = 2    (pointer)
SZLONG  = 4    (long)
SZFLOAT = 4    (float)
SZDOUB  = 8    (double)
```

## C89/C90 Enhancements

Beyond original Ritchie C:
- String literal concatenation ("a" "b" -> "ab")
- Duplicate typedef handling
- ANSI function prototypes
- Improved preprocessor

## Code Style

- **Identifier limit: 13 characters max** (Ritchie C constraint)
- Use camelCase: `emitByte`, `getToken`, `symUpdate`
- Minimize code size over clarity
- Table-driven patterns preferred

## Testing

```bash
# Compile a test file
./c0 test.c /tmp/test.i    # Pass 1
./c1 < /tmp/test.i > test.s # Pass 2
./asz test.s -o test.o      # Assemble
./wsld -o test test.o       # Link
```

## Known Limitations

- 'signed' keyword not supported
- No anonymous struct/union
- Limited constant expression evaluation
- 15-character external symbol names (object format limit)

## IMPORTANT: Session Hints

**DO NOT use `git checkout` on files.** You have lost uncommitted Z80 porting work
twice by accidentally reverting files. If you need to see original content, use
`git show HEAD:path/to/file` instead.
