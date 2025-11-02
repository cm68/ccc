ccc - full native C compiler

This is a 2-pass C compiler written in C, currently under reconstruction from
a paper printout. Pass 1 (cc1) is substantially complete; pass 2 (cc2) is not
yet implemented.

## Project Status

**Pass 1 (cc1) - Substantially Complete**

The compiler successfully parses C code including:
- Full C preprocessor (CPP) with macros, includes, conditional compilation
- Complete lexical analysis (tokenization)
- Type system: primitives, pointers, arrays, functions, structs, unions, enums
- Declaration parsing: variables, functions (K&R and ANSI-style prototypes), typedefs
- Expression parsing with constant folding and proper operator precedence
- Statement parsing: all control flow, function bodies, scoped blocks
- Forward declarations and incomplete types
- Typedef support including scoped typedefs inside functions
- Comprehensive test suite (95+ tests organized by category)

**Pass 2 (cc2) - Not Yet Implemented**

Code generation and assembler output are planned but not yet started.

## Architecture

This is a 2-pass compiler:

**Pass 1 (cc1)**: Recursive descent parser with embedded C preprocessor
- Parses and validates C source code
- Outputs preprocessed intermediate file (.i)
- ~4,600 lines of C code

**Pass 2 (cc2)**: Code generator and assembler
- Reads parse tree from pass 1
- Generates object file
- Not yet implemented
