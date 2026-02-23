ccc - full native C compiler

This is a 2-pass C compiler written in C, targeting Z80. Both passes are
complete; small programs run in simulation.

## Project Status

**Pass 1 (cc1) - Complete** Tagged as **cc1_complete** and **self-parse**
- Two-phase design: phase 1 builds symbol table and counts, phase 2 streams AST
- Constant folding, register allocation (IX for struct pointers, BC for words)
- 142 tests passing, 18/18 source files self-host
- Binary size under 40KB (target: <64KB for native Z80 build)
- All loops lowered to labeled if/goto by cpp filter pipeline
- See PHASE1.md, PHASE2.md for detailed architecture

## Memory Constraints (CP/M 2.2)

The standard CP/M 2.2 TPA (Transient Program Area) on a 64KB system is 56KB.
This must hold:
- **Text** (code)
- **Data** (initialized globals)
- **BSS** (uninitialized globals)
- **Heap** (malloc'd memory)
- **Stack**

With a static footprint (text + data + bss) of ~48KB, this leaves approximately
8-10KB for heap and stack combined. The compiler passes are designed to fit
within these constraints when compiled natively for Z80/CP/M.

**Pass 2 (cc2) - Complete** (source in pass2/)
- Stream code generator: builds one statement tree at a time, emits immediately
- Three-phase per-expression: demand calculation, dest assignment, emit
- Register allocation done in cc1 (outast.c), communicated via AST
- BC and IX register allocation, IX-indexed struct pointer optimization
- Long (32-bit) and float (IEEE 754) support via helper functions
- Generates working Z80 assembly; programs run in simulation
- See pass2/NEWPASS2.md for implementation details

**Whitesmith's Object Tools (ws/)** - Relocatable object support
- **asz**: Z80 assembler producing relocatable objects
- **wsld**: Linker for object files and libraries
- **wsnm**: Symbol table and disassembly utility
- **wslib**: Static library manager
- See ws/README.md for details

**Debugging Tools**
- **AST Pretty Printer** (astpp): Format AST for human inspection
- See ASTPP.md for details

## Architecture

This is a multi-stage compiler:

**Preprocessor (cpp)**: C preprocessor with pull-based filter pipeline
- Full macro expansion and conditional compilation
- Filter pipeline: `lex -> filtknr -> filtdecl -> filtbrace -> filtctrl -> emit`
  - **filtknr**: K&R to ANSI function definition conversion
  - **filtdecl**: Declaration initializer splitting (`int x = 5;` → `int x; x = 5;`)
  - **filtbrace**: Brace insertion around single-statement control bodies
  - **filtctrl**: Loop lowering (while/for/do → if/goto/label), break/continue resolution
- Typedef tracking across filters for type-aware parsing
- Outputs binary lexeme stream (.x file)
- See cpp/FILTERS.md for pipeline details

**Pass 1 (cc1)**: Two-phase recursive descent parser
- Phase 1: builds symbol table, counts statements/cases/blocks, tracks if/else
- Phase 2: streams AST directly as each construct is parsed (no statement trees)
- Bottom-up constant folding before AST emission
- Register allocation: IX for struct pointers with field access, BC for words
- Only handles `if` and `goto` for control flow (loops lowered by cpp)
- Outputs AST in compact paren-free hex format
- Uses Unix syscalls (write) instead of stdio for output

**Pass 2 (cc2)**: Code generator targeting Z80
- Reads AST from pass 1 (paren-free hex format)
- Expression tree rewriting with compact pattern language
- Pattern-based code generation via rules table
- Strength reduction (multiply by small constants)
- Sethi-Ullman register labeling for optimal evaluation order
- Uses Unix syscalls (read/write) instead of stdio
- Generates Z80 assembly code
- See pass2/REWRITE.md, pass2/HELPERS.md for details

## File Organization

**Preprocessor (cpp) files:** (in cpp/)
- cpp.c - Main entry point, command-line processing
- lex.c - Lexer/tokenizer with directive handling
- macro.c - Macro definition, lookup, and expansion
- io.c - Character I/O and include stack
- emit.c - Token output to .x and .i files
- filtknr.c - Filter: K&R to ANSI function conversion
- filtdecl.c - Filter: declaration initializer splitting
- filtbrace.c - Filter: brace insertion around control bodies
- filtctrl.c - Filter: loop lowering and break/continue resolution
- filtutil.c - Shared filter utilities (pending buffers, label emission)
- typetab.c - Typedef name tracking across filters
- kw.c - Compressed keyword lookup tables
- util.c - Error reporting, expression parsing
- xdump.c - Human-readable .i output tool

**Pass 1 (cc1) files:** (in pass1/)
- pass1.c - Main entry point, orchestration
- lexread.c - Lexeme stream reader
- parse.c - Statement parsing and streaming emission
- expr.c - Expression parsing with precedence
- type.c - Type system management
- decl.c - Top-level declaration parsing
- declare.c - Declarator parsing
- outast.c - AST emission in compact hex format
- regalloc.c - Register allocation analysis
- error.c - Error reporting
- util.c - Utilities

**Pass 2 (cc2) files:** (in pass2/)
- pass2.c - Main entry point, command-line processing
- parseast.c - AST parser, builds expression trees
- astio.c - Low-level AST I/O (character reading, hex parsing)
- expr.c - Expression tree construction and manipulation
- rewrite.c - Expression tree rewriting engine
- rules.c - Pattern matching rules table for code generation

**Auto-generated files:**
- tokenlist.c, enumlist.h - Token definitions
- error.h - Error code definitions
- debug.h, debugtags.c - Debug/verbose infrastructure

**Stub system headers (libsrc/include/):**
- stdio.h, stdlib.h, string.h, stdarg.h - C standard library stubs
- fcntl.h, unistd.h, signal.h - POSIX system call stubs
- libgen.h - Path manipulation stubs
- sys/stat.h, sys/wait.h - System header stubs
- Minimal declarations to avoid GNU libc advanced preprocessor features

## Directory Structure

```
ccc/
├── cpp/              # C preprocessor with filter pipeline
├── ccc/
│   ├── pass1/        # Pass 1 source (cc1 - two-phase parser)
│   ├── pass2/        # Pass 2 source (cc2 - code generator)
│   └── lib/          # Shared library (libutil, libccc)
├── tools/            # Whitesmith's object tools (asz, wsld, wsnm, wslib)
├── libsrc/           # Runtime library source
│   ├── include/      # System headers for target
│   ├── libc/         # C library (printf, malloc, etc.)
│   └── libu/         # Unix syscall wrappers
├── tests/            # Test suite
├── attic/            # Obsolete code
├── root/             # Installed toolchain (after make install)
│   ├── bin/          # Executables (cpp, c0, c1, ccc, asz, wsld, astpp)
│   ├── lib/          # Runtime libraries (crt0.o, libc.a, libu.a)
│   └── usr/include/  # Installed headers
└── stage1/           # Cross-compiled Z80 object files
```

## Command Line Reference

### ccc - Compiler Driver

```
ccc [options] files...
```

Files: `.c` (compile), `.s` (assemble), `.o` `.a` (link)

| Option | Description |
|--------|-------------|
| `-o <file>` | Output file (default: a.out) |
| `-c` | Compile and assemble only (produce .o) |
| `-s` | Compile only (produce .s, no assembly) |
| `-S` | Strip symbols from output |
| `-9` | Use 9-char symbols in output |
| `-k` | Keep intermediate files (.ast, .s, .o) |
| `-P` | Generate pretty-printed .pp file from AST |
| `-v <level>` | Verbosity level (passed to cc1) |
| `-V <level>` | Verbosity level (passed to cc2) |
| `-I<dir>` | Include directory |
| `-i<dir>` | System include directory (default: /usr/include) |
| `-D<name>[=val]` | Define macro |
| `-x` | Print commands as they execute |
| `-n` | Dry run (print commands without executing) |

### cc1 - Parser (Pass 1)

```
cc1 [options] source.c
```

| Option | Description |
|--------|-------------|
| `-o <file>` | Output AST file (default: source.ast) |
| `-I<dir>` | Include directory |
| `-i<dir>` | System include directory |
| `-D<name>[=val]` | Define macro |
| `-v <level>` | Verbosity/trace level (debug builds) |

### cc2 - Code Generator (Pass 2)

```
cc2 [options] source.ast
```

| Option | Description |
|--------|-------------|
| `-o <file>` | Output assembly file (default: source.s) |
| `-v <level>` | Trace level (debug builds) |

## Usage

**Using the ccc driver (recommended):**
```bash
./ccc source.c           # Compile to executable
./ccc -k source.c        # Keep intermediate files (.ast, .s, .o)
./ccc -S source.c        # Compile to assembly only
```

**Individual passes:**
```bash
./cc1 source.c           # Generate AST (writes source.ast)
./cc2 source.ast         # Generate assembly (writes source.s)
```

**Running in simulation:**
```bash
cd tests
../root/bin/ccc -o prog prog.c    # Compile with installed toolchain
../root/sim prog                   # Run in Z80 simulator
```

## Debugging the Parser

### AST Pretty Printer

For visual inspection of AST structure, use the standalone pretty printer:

```bash
# Generate AST
make test.ast

# Pretty print with human-readable formatting
./astpp test.ast

# Or use ccc -P to generate .pp automatically
./ccc -k -P -c test.c    # Creates test.ast, test.pp, test.s, test.o
```

**Output:**
```
FUNCTION main() -> _short_
{
  BLOCK {
    DECL a : _short_
    DECL b : _short_
    DECL c : _short_
    (ASSIGN:short $a (NARROW:short 10))
    (ASSIGN:short $b (NARROW:short 20))
    (ASSIGN:short $c (ADD (DEREF:short $a) (DEREF:short $b)))
    RETURN (DEREF:short $c)
  }
}
```

The pretty printer translates single-char operators to readable names
(M->DEREF, =->ASSIGN, +->ADD, etc.) and shows type width annotations, making
it easy to verify the AST structure at a glance.

**Use cases:**
- Debug parser output by visual inspection
- Understand AST structure for complex constructs
- Compare AST between different versions
- Learn the AST format

See [ASTPP.md](ASTPP.md) for complete documentation.
