# ccc Compiler Tests

This directory contains 64 test files for the ccc C compiler (pass 1: preprocessor and parser).

All test files have descriptive header comments explaining what they test. Each test validates a specific aspect of the compiler's functionality.

## Test Organization

Tests are listed in `Testlist` and run via:
- `make test` - Run all tests
- `make valgrind` - Run all tests with memory leak detection
- `./runtest.sh tests/filename.c` - Run a single test
- `./runtest.sh -v 3 tests/filename.c` - Run with verbosity level 3

## Test Categories

### Preprocessor Tests (8 tests)
- **macro.c** - Macro definition and expansion with stringify operator
- **include.c** - #include directive handling
- **stringify.c** - Stringify operator (#)
- **multistr.c** - Multiple string literals
- **t2.c** - Preprocessor macros including stringify and token pasting
- **conditional.c**, **conditional_false.c**, **conditional_simple.c** - Conditional compilation
- **defined_test.c**, **ifdef_test.c**, **ifndef_test.c** - Preprocessor conditionals

### Type System Tests (15 tests)
- **decl.c** - Basic variable declarations (int, char, pointers, arrays)
- **complex_types.c** - Complex type declarations (multi-level pointers, arrays)
- **up_to_struct.c** - Declarations up to and including struct definitions with macros
- **no_collision.c** - Namespace separation (struct members don't collide with globals)
- **ptest.c** - Struct declarations with bitfields and arrays
- **struct.c** - Basic struct declaration
- **typedef.c**, **typedefs.c** - Typedef declarations for various types
- **sizeof_basic.c**, **sizeof_bitfield.c**, **sizeof_const.c**, **sizeof_one.c**, **sizeof_struct.c**, **sizeof_test.c** - sizeof operator tests

### Expression Parsing Tests (6 tests)
- **simpleexpr.c** - Simple arithmetic expression in variable initializer
- **cfold.c** - Constant folding for arithmetic, bitwise, and logical operators with proper precedence
- **with_add.c** - Macro expansion with add macro and struct declaration
- **compound_ops.c** - Compound operators (+=, -=, etc.)
- **compound_precedence.c** - Compound operator precedence

### Function Declaration Tests (10 tests)
- **kr_funcs.c** - K&R style function declarations with various parameter configurations
- **kr_locals.c** - K&R style function with local variable declarations
- **kr_minimal.c** - Minimal K&R style function with simple if statement
- **kr_multi.c** - Multiple K&R style functions with minimal implementations
- **kr_noparams.c** - K&R style functions with no parameters
- **kr_oneparam.c** - K&R style function with one parameter and local variables
- **kr_progressive.c** - K&R style functions with progressive complexity
- **empty_func.c** - Empty function
- **minimal.c** - Minimal function with void parameter and local variable
- **modern_func.c** - Modern ANSI-style function declarations
- **simple_func.c** - Simple function declarations

### Statement Parsing Tests (19 tests)
- **statements_kr.c** - K&R functions with all control flow constructs (comprehensive)
- **statements.c** - Comprehensive statement parsing with all control flow
- **simple_statements.c** - K&R style functions with forward declarations and basic statements
- **100.c** - Comprehensive statement parsing including control flow and jump statements
- **cont.c** - Continue statement in while loop
- **do.c** - Do-while loop statement
- **hang.c** - K&R style function with if-else statement
- **local.c**, **local2.c**, **local3.c**, **local4.c**, **local5.c** - Functions with local variables
- **more.c** - Extended statement parsing
- **no_switch.c** - Statement parsing without switch (all other control flow)
- **partial.c** - Partial statement parsing
- **stat.c** - Static storage class specifier
- **simple_switch.c** - Simple switch statement with single case
- **sw2.c** - Empty switch statement with no cases
- **switch.c** - Switch statement with multiple cases and default
- **two_locals.c** - Function with two local variable declarations with initializers

### String Handling Tests (2 tests)
- **string.c** - String literal declarations
- **multistr.c** - Multiple string literal declarations

### Scope Tests (2 tests)
- **scopes.c** - Lexical scope handling
- **local_vars.c** - Local variable scoping

### Development/Debug Tests (2 tests)
- **simple_int.c** - Simple integer declarations
- **hang.c** - Test for parser hanging issues

## Current Compiler Status

### What Works
✅ **Preprocessor** - Complete implementation
  - Macro definition and expansion
  - #include directive handling
  - Stringify (#) and token pasting (##) operators
  - Conditional compilation (#if, #ifdef, #ifndef, #else, #endif)

✅ **Type System** - Substantially complete
  - Primitive types (char, short, int, long, unsigned variants, void, float, double)
  - Pointer types (multi-level)
  - Array types (multi-dimensional)
  - Function types with parameter lists
  - Struct, union, enum declarations
  - Forward references and incomplete types
  - Tag namespace separation
  - Bitfield support

✅ **Expression Parsing** - Functional
  - All unary operators with constant folding
  - All binary operators with constant folding
  - Proper operator precedence and associativity
  - sizeof operator

✅ **Statement Parsing** - Active
  - All control flow statements (if/else, while, do-while, for, switch/case)
  - Jump statements (break, continue, return, goto/labels)
  - Block statements with proper scoping
  - Statement trees attached to function bodies

✅ **Function Declarations**
  - ANSI-style function prototypes
  - K&R style function definitions
  - Undeclared K&R parameters default to int

### Current Limitations

⚠️ **Function Definitions**
- **ANSI-style function definitions don't work**: `int foo(int x) { }`
  - Parser treats parameters as global declarations
  - Error: "more than one basetype"
- **Workaround**: Use K&R style: `int foo(x) int x; { }`
- **ANSI-style forward declarations work**: `int foo(int x);`

⚠️ **Not Yet Implemented**
- typedef (partially implemented)
- Local variable declarations inside function bodies (conflicts with statement parsing)
- Type checking and validation
- Code generation (pass 2)

### Memory Management
- Valgrind clean on basic tests (no definite leaks)
- Fixed major memory leaks in declaration parsing
- Proper cleanup in pop_scope()

## Running Tests

```bash
# Build compiler
make cc1

# Run all tests
make test

# Run memory leak tests
make valgrind

# Run single test
./runtest.sh tests/decl.c

# Run with verbosity (hex bitmask for debug flags)
./runtest.sh -v 3 tests/macro.c
./runtest.sh -v 0x3f tests/decl.c  # Maximum verbosity
```

### Test Result Interpretation

Tests pass if:
- cc1 completes without crashing
- Exit code is 0

Tests may produce parse errors but still pass - this is expected for incomplete features.

### Valgrind Testing

The `make valgrind` target runs all tests with memory leak detection:
- Reports: PASS (no leaks), LEAK (memory leaks detected), FAIL (crash/error)
- Uses `--leak-check=full` for detailed leak information
- Only reports definite leaks (not reachable blocks)

## Test File Naming

Test files follow these naming conventions:
- Descriptive names (e.g., `cfold.c`, `kr_funcs.c`, `statements.c`)
- All test files have header comments describing their purpose
- Tests are organized by functionality, not by implementation order

## Adding New Tests

To add a new test:
1. Create test file in `tests/` directory
2. Add descriptive header comment explaining what it tests
3. Add filename to `tests/Testlist`
4. Run `make test` to verify
5. Run `make valgrind` to check for memory leaks
