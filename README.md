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

**Pass 2 (cc2) - Not Yet Implemented**

Code generation planned. AST format ready for consumption.

## Architecture

This is a 2-pass compiler:

**Pass 1 (cc1)**: Recursive descent parser with embedded C preprocessor
- Parses and validates C source code
- Outputs AST in S-expression format (single-char operators)
- Uses Unix syscalls (write) instead of stdio for output
- ~4,800 lines of C code

**Pass 2 (cc2)**: Code generator and assembler
- Reads AST from pass 1 (S-expression format)
- Generates object file
- Not yet implemented
