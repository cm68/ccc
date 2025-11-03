ccc - full native C compiler

This is a 2-pass C compiler written in C, currently under reconstruction from
a paper printout. Pass 1 (cc1) is substantially complete; pass 2 (cc2) is not
yet implemented.

## Project Status

**Pass 1 (cc1) - Complete**

The compiler successfully parses C code and outputs AST in S-expression format:
- Full C preprocessor (CPP) with macros, includes, conditional compilation
- Complete lexical analysis (tokenization)
- Type system: primitives, pointers, arrays, functions, structs, unions, enums
- Declaration parsing: variables, functions (K&R and ANSI-style), typedefs
- Expression parsing with constant folding and proper operator precedence
- Statement parsing: all control flow, function bodies, scoped blocks
- AST emission: S-expression output with global vars, functions, initializers
- Unix syscall I/O: fdprintf() instead of stdio for AST output
- Comprehensive test suite (95+ tests organized by category)

**Pass 2 (cc2) - Work In Progress**

AST parser foundation in place, code generation planned:
- Table-driven S-expression parser in parseast.c
- Unix syscall I/O (read/write) instead of stdio
- Handler functions for all AST node types
- Memory width annotation support (:b :s :l :p :f :d)
- Successfully parses simple AST constructs
- Complex nested structures need debugging
- Code generation not yet started

## Architecture

This is a 2-pass compiler:

**Pass 1 (cc1)**: Recursive descent parser with embedded C preprocessor
- Parses and validates C source code
- Outputs AST in S-expression format (single-char operators)
- Uses Unix syscalls (write) instead of stdio for output
- ~4,800 lines of C code

**Pass 2 (cc2)**: Code generator and assembler (WIP)
- Reads AST from pass 1 (S-expression format)
- Table-driven parser with handler functions for each operation
- Uses Unix syscalls (read/write) instead of stdio
- parseast.c: ~730 lines of parser infrastructure
- Handles memory width annotations (:b :s :l :p :f :d)
- Code generation not yet implemented

## File Organization

**Pass 1 (cc1) files:**
- cc1.c - Main entry point, orchestration
- lex.c - Lexical analyzer (tokenizer)
- parse.c - Statement and declaration parsing
- expr.c - Expression parsing with precedence
- type.c - Type system management
- declare.c - Declaration processing
- outast.c - AST emission in S-expression format
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

## Usage

**Pass 1 - Parse and output AST:**
```bash
./cc1 -E source.c > output.i
```

**Pass 2 - Parse AST (code generation not yet implemented):**
```bash
./cc2 output.i -o executable
```

**Full pipeline (when complete):**
```bash
./cc1 -E source.c | ./cc2 -o executable
```
