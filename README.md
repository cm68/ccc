ccc - full native C compiler

This is a 2-pass C compiler written in C, currently under reconstruction from
a paper printout. Pass 1 (cc1) is complete; pass 2 (cc2) is actively being
developed and generates Z80 assembly code.

## Project Status

**Pass 1 (cc1) - Complete** Tagged as **cc1_complete** and **self-parse**
- Full C preprocessor, type system, expression/statement parsing, AST emission
- 142 tests passing, 18/18 source files self-host
- ~7,500 lines of C code
- See CLAUDE.md for detailed architecture and features

**Debugging Tools**
- **AST Pretty Printer** (astpp.lisp): Format AST for human inspection
- See ASTPP.md for details

**Pass 2 (cc2) - Active Development** Generating Z80 Assembly
- Tree-based AST parser with complete function representation (~3,400 lines)
- Three-phase code generation: parse -> codegen -> emit
- Register allocation and stack frame management
- Generates working Z80 assembly for simple functions
- See CC2_ARCHITECTURE.md for implementation details

## Architecture

This is a 2-pass compiler:

**Pass 1 (cc1)**: Recursive descent parser with embedded C preprocessor
- Parses and validates C source code
- Outputs AST in compact paren-free hex format
- Uses Unix syscalls (write) instead of stdio for output
- ~7,500 lines of C code

**Pass 2 (cc2)**: Tree-based code generator targeting Z80
- Reads AST from pass 1 (paren-free hex format)
- Three-phase architecture: parse -> codegen -> emit
- Builds complete function trees in memory before code generation
- Register allocation and stack frame management
- Uses Unix syscalls (read/write) instead of stdio
- parseast.c: ~3,400 lines (parser, code generation, emission)
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
- parseast.c - Table-driven AST parser
- util.c - Shared utilities (fdprintf)

**Auto-generated files:**
- tokenlist.c, enumlist.h - Token definitions
- error.h - Error code definitions
- debug.h, debugtags.c - Debug/verbose infrastructure
- op_pri.h - Operator priority table

**Stub system headers (include/):**
- stdio.h, stdlib.h, string.h, stdarg.h - C standard library stubs
- fcntl.h, unistd.h, signal.h - POSIX system call stubs
- libgen.h - Path manipulation stubs
- sys/stat.h, sys/wait.h - System header stubs
- Minimal declarations to avoid GNU libc advanced preprocessor features

## Usage

**Using the ccc driver (recommended):**
```bash
# Full compilation (when cc2 is complete)
./ccc -o program source.c

# Keep intermediate AST file
./ccc -k -o program source.c
```

**Pass 1 - Parse and output AST:**
```bash
./cc1 -E source.c > output.ast
```

**Pass 2 - Generate Z80 assembly from AST:**
```bash
./cc2 output.ast              # Generates output.s assembly file
./cc2 output.ast -o custom.s  # Specify output file
```

**Full pipeline (when complete):**
```bash
./cc1 -E source.c | ./cc2 -o executable
```

## Debugging the Parser

### AST Pretty Printer

For visual inspection of AST structure, use the standalone pretty printer:

```bash
# Generate AST
make test.ast

# Pretty print with human-readable formatting
./astpp.lisp test.ast
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
