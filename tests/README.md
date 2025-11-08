# ccc Compiler Tests

This directory contains 122 test files for the ccc C compiler (pass 1: preprocessor and parser).

All test files have descriptive header comments explaining what they test. Each test validates a specific aspect of the compiler's functionality.

## Test Organization

Tests are organized in `tests/Makefile` by category and run via:
- `make test` - Run all tests (from project root)
- `make -C tests test` - Run all tests (from project root, explicit)
- `make valgrind` - Run all tests with memory leak detection
- `./runtest.sh filename.c` - Run a single test (from tests/ directory)
- `./runtest.sh -v 3 filename.c` - Run with verbosity level 3

### Category-Specific Test Targets

You can run specific test categories using these make targets:
- `make -C tests test-expr` - Expression and constant folding tests
- `make -C tests test-decl` - Declaration and type tests
- `make -C tests test-cpp` - Preprocessor tests
- `make -C tests test-kr` - K&R style function tests
- `make -C tests test-func` - Modern function tests
- `make -C tests test-stmt` - Statement tests
- `make -C tests test-sizeof` - sizeof operator tests
- `make -C tests test-typedef` - Typedef tests
- `make -C tests test-cast` - Type cast operator tests
- `make -C tests test-string` - String literal tests
- `make -C tests test-incr-decr` - Increment/decrement operator tests
- `make -C tests test-ptr-compat` - Pointer compatibility tests
- `make -C tests test-lvalue` - Lvalue validation tests

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
  - Typedef support (global and scoped inside functions)

✅ **Expression Parsing** - Complete
  - All unary operators with constant folding
  - All binary operators with constant folding
  - Proper operator precedence and associativity
  - sizeof operator (types and expressions)
  - Type cast operator with typedef disambiguation
  - Increment/decrement operators (prefix and postfix)
  - Compound assignment operators (+=, -=, etc.)
  - Ternary conditional operator (? :)
  - Function calls and function pointers
  - Array subscripting with scaled arithmetic
  - Struct member access (. and ->)

✅ **Type Checking** - Partially implemented
  - Automatic type conversions in assignments (NARROW/SEXT/WIDEN)
  - Automatic operand widening in binary expressions
  - Pointer type compatibility validation
  - Lvalue validation for assignments and increment/decrement

✅ **Statement Parsing** - Complete
  - All control flow statements (if/else, while, do-while, for, switch/case)
  - Jump statements (break, continue, return, goto/labels)
  - Block statements with proper scoping
  - Statement trees attached to function bodies
  - Local variable declarations inside function bodies
  - Typedef declarations inside function bodies

✅ **Function Declarations** - Complete
  - ANSI-style function prototypes
  - ANSI-style function definitions: `int foo(int x) { }` ✅
  - K&R style function definitions
  - Undeclared K&R parameters default to int
  - Forward declarations with different parameter names
  - Function type normalization (parameter names don't affect type)

### Current Limitations

⚠️ **Not Yet Implemented**
- Full type compatibility checking (sametype for all contexts)
- Function signature checking at call sites
- Code generation (pass 2)

### Memory Management
- Valgrind clean on basic tests (no definite leaks)
- Fixed major memory leaks in declaration parsing
- Proper cleanup in pop_scope()

## Running Tests

```bash
# Build compiler (from project root)
make cc1

# Run all tests (from project root)
make test

# Run memory leak tests (from project root)
make valgrind

# Run category-specific tests (from project root)
make -C tests test-expr      # Expression tests
make -C tests test-cpp       # Preprocessor tests
make -C tests test-sizeof    # sizeof operator tests
make -C tests test-typedef   # Typedef tests

# Run single test (from tests/ directory)
cd tests
./runtest.sh decl.c

# Run single test with path prefix (works from anywhere)
tests/runtest.sh tests/decl.c

# Run with verbosity (hex bitmask for debug flags)
./runtest.sh -v 3 macro.c
./runtest.sh -v 0x3f decl.c  # Maximum verbosity

# Run valgrind on specific tests (from tests/ directory)
./runvalgrind.sh sizeof_basic.c typedef.c
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
3. Add filename to appropriate category in `tests/Makefile`:
   - Add to `EXPR_TESTS` for expression/constant folding tests
   - Add to `DECL_TESTS` for declaration/type tests
   - Add to `CPP_TESTS` for preprocessor tests
   - Add to `KR_FUNC_TESTS` for K&R style function tests
   - Add to `FUNC_TESTS` for modern function tests
   - Add to `STMT_TESTS` for statement parsing tests
   - Add to `SIZEOF_TESTS` for sizeof operator tests
   - Add to `TYPEDEF_TESTS` for typedef tests
   - Add to `LOCAL_TESTS` for local variable tests
   - Add to `SCOPE_TESTS` for scope tests
   - Add to `STRUCT_TESTS` for struct tests
   - Add to `CAST_TESTS` for type cast operator tests
   - Add to `STRING_TESTS` for string literal tests
   - Add to `INCR_DECR_TESTS` for increment/decrement operator tests
   - Add to `PTR_COMPAT_TESTS` for pointer compatibility tests
   - Add to `LVALUE_TESTS` for lvalue validation tests
   - Add to `MINIMAL_TESTS` for minimal/smoke tests
   - Add to `MISC_TESTS` for miscellaneous/regression tests
4. Run `make test` to verify
5. Run `make valgrind` to check for memory leaks

## Test Infrastructure

The test infrastructure consists of:
- **tests/Makefile** - Organizes tests by category, defines test targets
- **tests/runtest.sh** - Executes individual tests, displays source and output
- **tests/runvalgrind.sh** - Runs tests with valgrind memory leak detection
- **Makefile** (root) - Top-level targets that invoke tests/Makefile recursively

All test scripts automatically handle path prefixes and work from any directory.
