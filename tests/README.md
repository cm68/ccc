# ccc Compiler Tests

This directory contains test files for the ccc C compiler (pass 1: preprocessor and parser).

## Test Categories

### Preprocessor Tests
- **macro.c** - Macro definition, expansion, stringify (#), token pasting (##)
- **include.c** - #include directive handling
- **test_stringify.c** - Stringify operator (#)
- **multistr.c** - Multiple string literals

### Type System Tests
- **decl.c** - Basic variable declarations (int, char, pointers, arrays)
- **complex_types.c** - Complex type declarations (multi-level pointers, multi-dimensional arrays)
- **test_up_to_struct.c** - Struct declarations
- **test_no_collision.c** - Namespace separation (struct tags vs variables)

### Expression Parsing Tests
- **simpleexpr.c** - Simple arithmetic expressions
- **cfold.c** - Constant folding for all operators (binary, unary, precedence)
- **test_with_add.c** - Expression parsing with addition

### Function Declaration Tests
- **kr_funcs.c** - K&R (old-style) function declarations
  - Single-parameter K&R functions (WORKS)
  - No-parameter K&R functions (WORKS)
  - Multi-parameter K&R functions (LIMITED - parser issues)

### Statement Tests
- **simple_statements.c** - Forward declarations and simple K&R functions
- **statements_kr.c** - K&R functions with statement constructs
  - Note: parsefunc() currently just skips function bodies by counting braces
  - Tests verify the compiler doesn't crash on statement-like code
  - Actual statement parsing is not yet fully connected

### String Handling Tests
- **string.c** - String literal handling
- **t2.c** - Various macro and string tests

## Current Limitations

### Function Definitions
- **ANSI-style function definitions don't work**: `int foo(int x) { }` 
  - Parser treats parameters as global declarations
  - Error: "more than one basetype"
- **ANSI-style forward declarations work**: `int foo(int x);`
- **K&R-style definitions work**: `int foo(x) int x; { }`

### Statement Parsing
- parsefunc() in parse.c currently just consumes function bodies without parsing
- Statement parsing code exists but isn't fully integrated
- Tests verify the compiler handles complex code without crashing

### What Works
- Preprocessor (macros, includes, stringify, token pasting)
- Type system (primitives, pointers, arrays, structs, unions, enums)
- Expression parsing with constant folding
- Function forward declarations (ANSI and K&R style)
- K&R function definitions with body skipping

## Running Tests

```bash
# Run all tests
make test

# Run single test
./runtest.sh tests/decl.c

# Run with verbosity
./runtest.sh -v 3 tests/macro.c
```

Tests pass if cc1 completes without crashing, even if there are parse errors.
