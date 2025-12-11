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

**Pass 2 (cc2) - Complete**
- Tree-based AST parser with complete function representation
- Two-phase code generation: parse -> schedule/emit
- Register allocation done in cc1 (outast.c), communicated via AST
- BC and IX register allocation, IX-indexed struct pointer optimization
- Long (32-bit) and float (IEEE 754) support via helper functions
- Generates working Z80 assembly; programs run in simulation
- See CC2_ARCHITECTURE.md for implementation details

**Whitesmith's Object Tools (ws/)** - Relocatable object support
- **asz**: Z80 assembler producing relocatable objects
- **wsld**: Linker for object files and libraries
- **wsnm**: Symbol table and disassembly utility
- **wslib**: Static library manager
- See ws/README.md for details

**Debugging Tools**
- **AST Pretty Printer** (astpp.py): Format AST for human inspection
- See ASTPP.md for details

## Architecture

This is a 2-pass compiler:

**Pass 1 (cc1)**: Recursive descent parser with embedded C preprocessor
- Parses and validates C source code
- Outputs AST in compact paren-free hex format
- Uses Unix syscalls (write) instead of stdio for output
- ~7,500 lines of C code

**Pass 2 (cc2)**: Tree-based code generator targeting Z80
- Reads AST from pass 1 (paren-free hex format)
- Two-phase architecture: parse -> schedule/emit
- Builds complete function trees in memory before code generation
- Register assignments received from cc1 via AST declarations
- Uses Unix syscalls (read/write) instead of stdio
- Handles memory width annotations (:b :s :l :p :f :d)
- Generates Z80 assembly code

## File Organization

**Pass 1 (cc1) files:**
- cc1.c - Main entry point, orchestration
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

**Pass 2 (cc2) files:**
- cc2.c - Main entry point, command-line processing
- parseast.c - AST parser, builds expression/statement trees
- astio.c - Low-level AST I/O (character reading, hex parsing)
- codegen.c - Scheduler: pattern recognition, instruction selection
- emitexpr.c - Expression emission dispatcher
- emitops.c - Operation emitters (assign, binop, call, incdec, etc.)
- emithelper.c - Helper functions (loadVar, storeVar, emit strings)
- emit.c - Statement emission
- util.c - Shared utilities (fdprintf)

**Auto-generated files:**
- tokenlist.c, enumlist.h - Token definitions
- error.h - Error code definitions
- debug.h, debugtags.c - Debug/verbose infrastructure

**Stub system headers (include/):**
- stdio.h, stdlib.h, string.h, stdarg.h - C standard library stubs
- fcntl.h, unistd.h, signal.h - POSIX system call stubs
- libgen.h - Path manipulation stubs
- sys/stat.h, sys/wait.h - System header stubs
- Minimal declarations to avoid GNU libc advanced preprocessor features

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
./astpp.py test.ast
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
