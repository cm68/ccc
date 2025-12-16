ccc - full native C compiler

This is a 2-pass C compiler written in C, targeting Z80. Both passes are
complete; small programs run in simulation.

## Project Status

**Pass 1 (cc1) - Complete** Tagged as **cc1_complete** and **self-parse**
- Full C preprocessor (embedded, no separate cpp output), type system,
  expression/statement parsing, AST emission
- 142 tests passing, 18/18 source files self-host
- Binary size under 40KB (target: <64KB for native Z80 build)
- All loops lowered to labeled if/goto for simplified code generation
- See CLAUDE.md for detailed architecture and features

**Pass 2 (cc2) - Complete** (source in newpass2/)
- Stream code generator: builds one statement tree at a time, emits immediately
- Three-phase per-expression: demand calculation, dest assignment, emit
- Register allocation done in cc1 (outast.c), communicated via AST
- BC and IX register allocation, IX-indexed struct pointer optimization
- Long (32-bit) and float (IEEE 754) support via helper functions
- Generates working Z80 assembly; programs run in simulation
- See newpass2/NEWPASS2.md for implementation details

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

This is a 2-pass compiler:

**Pass 1 (cc1)**: Recursive descent parser with embedded C preprocessor
- Parses and validates C source code
- Outputs AST in compact paren-free hex format
- Uses Unix syscalls (write) instead of stdio for output
- ~7,500 lines of C code

**Pass 2 (cc2)**: Stream code generator targeting Z80
- Reads AST from pass 1 (paren-free hex format)
- Builds one statement tree at a time, emits code immediately
- Three-phase per-expression: demand calculation, dest assignment, emit
- Register assignments received from cc1 via AST declarations
- Uses Unix syscalls (read/write) instead of stdio
- Handles memory width annotations (:b :s :l :p :f :d)
- Generates Z80 assembly code

## File Organization

**Pass 1 (cc1) files:** (in pass1/)
- pass1.c - Main entry point, orchestration
- lex.c - Lexical analyzer (tokenizer)
- parse.c - Statement and declaration parsing
- expr.c - Expression parsing with precedence
- type.c - Type system management
- declare.c - Declaration processing
- outast.c - AST emission in compact hex format
- macro.c - CPP macro definition and expansion
- io.c - Character I/O and file stack management
- error.c - Error reporting
- util.c - Utilities (fdprintf, bitdef, etc.)
- kw.c - Keyword lookup tables

**Pass 2 (cc2) files:** (in newpass2/)
- newcc2.c - Main entry point, command-line processing
- parseast.c - AST parser, builds expression/statement trees
- astio.c - Low-level AST I/O (character reading, hex parsing)
- codegen.c - Scheduler: demand calculation, dest assignment, instruction selection
- emitexpr.c - Expression emission dispatcher
- emitops.c - Operation emitters (binop, shift, logical, etc.)
- emitcmp.c - Comparison and short-circuit evaluation
- emitincdec.c - Pre/post increment/decrement emission
- emit.c - Statement emission (if, while, for, switch, etc.)

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
├── pass1/            # Pass 1 source (cc1 - parser)
├── newpass2/         # Pass 2 source (cc2 - code generator)
├── ws/               # Whitesmith's object tools (asz, wsld, wsnm, wslib)
├── libsrc/           # Runtime library source
│   ├── include/      # System headers for target
│   ├── libc/         # C library (printf, malloc, etc.)
│   └── libu/         # Unix syscall wrappers
├── tests/            # Test suite
├── attic/            # Obsolete code (pass2/, astpp.py, unit_test/)
├── root/             # Installed toolchain (after make install)
│   ├── bin/          # Executables (cc1, cc2, ccc, asz, wsld, astpp, etc.)
│   ├── lib/          # Runtime libraries (crt0.o, libc.a, libu.a)
│   └── usr/include/  # Installed headers
├── stage1/           # Cross-compiled Z80 object files
├── astpp.c           # AST pretty printer
├── ccc.c             # Compiler driver
└── util.c            # Shared utilities
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
    EXPR:
      (ASSIGN:short $a (NARROW:short 10))
    EXPR:
      (ASSIGN:short $b (NARROW:short 20))
    EXPR:
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
