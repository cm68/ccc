# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with
code in this repository.

## Project Overview

This is **ccc** - a native C compiler written in C, currently under
reconstruction. The source was retyped from a paper printout with missing
chunks. This is a 2-pass compiler:

1. **Pass 1** (cc1): Recursive descent parser with embedded C preprocessor (CPP)
  - parses and validates C code
2. **Pass 2** (cc2): Code generator and assembler - writes object file (not yet
  implemented)

**Current Status**: Pass 1 is complete (~7,500 lines of C code). Tagged as
**cc1_complete**. The compiler successfully parses C code including full
preprocessor support with conditional directives, declarations, types,
expressions, and statements. All 142 tests pass.

## CRITICAL: Memory Footprint Constraint

**IMPORTANT**: This compiler must fit in **<64KB total (code + data)** when
compiled natively for the target platform. Memory efficiency is a PRIMARY GOAL
throughout development.

**Optimization Guidelines**:
- **Minimize code duplication**: Find symmetry and reuse code patterns
- **Consolidate function calls**: Multiple consecutive output calls should be
  combined
  - Example: `fdprintf(fd, "x"); fdprintf(fd, "y");` → `fdprintf(fd, "xy");`
  - Example: `cpp_out("text", 4); cpp_out(" ", 1);` → combine into single call
- **Avoid redundant operations**: Each function call has overhead (call
  instruction, parameter setup, stack frame)
- **Prefer single-pass algorithms**: Multiple passes over data cost memory and
  cycles
- **Use compact data structures**: Every byte counts

**Known Optimization Opportunities**:
- outast.c: 54 single-character fdprintf() calls (28 spaces, 20 parens)
- lex.c output_token(): 4 pairs of consecutive cpp_asm_out() calls appending
  single space
- Symbol prefix logic (outast.c:70-92): Multiple branches with similar fdprintf
  patterns could share code

When implementing new features or refactoring, always consider: "How can this
use less memory?"

## Build Commands

```bash
# Build the compiler (pass 1 only)
make

# Build specific targets
make cc1          # Main compiler executable

# Run tests
make test         # Run all 142 tests in tests/
make tests        # Same as 'make test'

# Run test categories
make test-typedef # Run typedef tests only
make test-sizeof  # Run sizeof tests only
make test-cpp     # Run preprocessor tests only
make test-string  # Run string literal tests only
# (See tests/Makefile for all categories)

# Run single test
cd tests && ./runtest.sh decl.c
cd tests && ./runtest.sh -v 3 decl.c  # With verbosity level 3

# Run unit tests
make unit-tests   # Run component-level unit tests

# Regenerate auto-generated files
make regen

# Clean build artifacts
make clean        # Remove objects and generated files
make clobber      # Remove all build artifacts including binaries

# Generate ctags
make tags
```

## CRITICAL: Testing Validation Protocol

**IMPORTANT**: When validating changes to the compiler, ALWAYS use the provided
make rules. Never manually run `./cc1 -E file.c` or `./cc2 file.ast` directly.

**Required testing commands after any changes:**

```bash
# Verify self-hosting (compiler parses its own sources)
make selfcheck    # Uses correct flags: -DCCC -i./include -I.

# Run full test suite
make test         # All 135+ tests must pass

# Comprehensive validation (includes selfcheck + tests)
make fullcheck    # Run this before committing
```

**Testing individual files:**

```bash
# CORRECT: Use make rules with proper flags
make expr.ast     # Compile expr.c to AST (uses -DCCC -i./include -I.)
make expr.s       # Compile expr.ast to assembly

# WRONG: Don't run compilers manually
./cc1 -E expr.c   # Missing required flags, will fail!
./cc2 expr.ast    # Works but inconsistent with project conventions
```

**Why this matters:**
- Make rules include essential flags (-DCCC -i./include -I.) required for
  parsing the compiler's own sources
- Suffix rules (%.ast, %.s) have the correct flags baked in
- Manually running compilers without proper flags produces false failures
- The Makefiles have the correct, tested commands - always use them

**Before any commit:**
1. Run `make selfcheck` - verify self-hosting works
2. Run `make test` - verify all tests pass
3. Or simply run `make fullcheck` - does both

## Running the Compiler

```bash
# Using the ccc driver (recommended)
./ccc -x source.c              # Compile and execute with interpreter
./ccc -k source.c              # Compile with cc2, keep AST file
./ccc -o program source.c      # Full compilation (when cc2 is complete)

# Direct cc1 usage
./cc1 [options] <source.c>

# Common options
./cc1 -E source.c              # Preprocessor only (outputs .i file)
./cc1 -I<dir> source.c         # Add include directory
./cc1 -D<var>=<val> source.c   # Define preprocessor variable
./cc1 -v <level> source.c      # Set verbosity level (hex bitmask)

# Debug with verbosity
./cc1 -v 0x3f -E tests/decl.c  # Maximum verbosity
```

## Debugging the Parser with the Interpreter

The `-x` option is a critical debugging tool that validates parser correctness
without
needing a working code generator. This allows development of pass 1
independently of pass 2.

### Quick Validation Workflow

When making parser changes, validate them immediately:

```bash
# Make parser changes to expr.c, parse.c, etc.
make cc1

# Test with interpreter
./ccc -x tests/arith_widths.c
# Expected: Program exited with code: 3622

# If it works, the parser is correct!
```

### Debugging Workflow

**Step 1**: Create a minimal test case for the feature you're working on:
```c
// test_feature.c
int main() {
    int x = 10;
    int y = x + 5;  // Testing addition
    return y;       // Should return 15
}
```

**Step 2**: Compile and execute:
```bash
./ccc -x test_feature.c
```

**Step 3**: Check the result:
- ✓ Correct exit code (15) → Parser works!
- ✗ Wrong exit code → Parser bug, inspect AST

**Step 4**: If wrong, inspect the generated AST:
```bash
cat test_feature.ast
```

Look for:
- Missing operators
- Wrong operator precedence
- Type mismatches
- Missing declarations
- Incorrect control flow

**Step 5**: Add debug output to parser if needed:
```c
// In expr.c
VERBOSE(V_PARSE) {
    fdprintf(2, "Parsing expression, op=%d\n", op);
}
```

**Step 6**: Run with verbose mode:
```bash
./cc1 -v 0x20 -E test_feature.c > test_feature.ast
```

### Common Debugging Scenarios

**Scenario 1: Parser crashes**
```bash
# Isolate the crashing feature
./ccc -x minimal_test.c
# Use gdb if needed
gdb --args ./cc1 -E minimal_test.c
```

**Scenario 2: Wrong result**
```bash
# Compare AST with expected structure
./ccc -x test.c              # Returns wrong value
cat test.ast                 # Inspect AST
# Compare against working test
cat tests/simple_arith.ast   # Known-good AST
```

**Scenario 3: Type conversion issues**
```c
// Create test showing problem
int main() {
    char c = 10;
    int i = c;    // Should widen char to int
    return i;     // Should return 10
}
```

```bash
./ccc -x test_conv.c
# Check AST for WIDEN/SEXT operators
```

### Interpreter Limitations (Important!)

The interpreter has simplified semantics - it validates AST structure but not
all
runtime behaviors:

- **Memory model**: Simplified, no real addresses
- **Type conversions**: Pass-through (no actual narrowing)
- **Pointer arithmetic**: Symbolic, not validated
- **Undefined behavior**: May differ from real execution

**What the interpreter DOES validate:**
- ✓ Parser produces syntactically correct AST
- ✓ Control flow (loops, conditionals, function calls)
- ✓ Expression evaluation order
- ✓ Variable scoping
- ✓ Function parameter passing
- ✓ Return values

**What it DOESN'T validate:**
- ✗ Memory layout and alignment
- ✗ Pointer arithmetic correctness
- ✗ Actual type width conversions
- ✗ Platform-specific behavior

### Best Practices

1. **Test incrementally**: After each parser change, run interpreter tests
2. **Start simple**: Test basic cases before complex ones
3. **Build test suite**: Create .c files in tests/ for regression testing
4. **Check exit codes**: Use return values to validate computations
5. **Inspect AST**: When in doubt, look at the generated S-expressions
6. **Use -k flag**: Keep AST files for later inspection: `./ccc -k -x test.c`

### Example: Adding a New Operator

When adding a new operator to the parser:

1. Implement parsing in expr.c
2. Add AST emission in outast.c
3. Create test case:
```c
int main() {
    int a = 5;
    int b = 3;
    return a NEW_OP b;  // Expected result: <value>
}
```
4. Test with interpreter: `./ccc -x test_newop.c`
5. Verify exit code matches expected result
6. If wrong, check AST structure
7. Add to test suite once working

### Interpreter Implementation

The interpreter (interp.lisp) is a Common Lisp program that:
- Reads S-expression AST from cc1
- Executes it as a virtual machine
- Supports all major C operations
- Uses hash tables for variables and functions
- Implements stack frames for function calls

See INTERP.md for complete documentation.

## Architecture

### Code Organization

- **cc1.c**: Main entry point, command-line processing, file processing
  orchestration
- **lex.c**: Lexical analyzer (tokenizer) with embedded CPP support
- **parse.c**: Recursive descent parser for statements and declarations
- **expr.c**: Expression parsing with operator precedence, constant folding, and
  K&R implicit function declarations
- **type.c**: Type system - manages primitive types, pointers, arrays,
  functions, structs/unions/enums
- **declare.c**: Declaration parsing (variables, functions, types) including K&R
  style support
- **outast.c**: AST emission in S-expression format for pass 2 with memory width
  annotations
- **kw.c**: Keyword lookup tables (C keywords, CPP keywords, asm keywords)
- **io.c**: Character-level I/O, file stack management, macro expansion buffer
- **macro.c**: CPP macro definition and expansion
- **error.c**: Error reporting (lose() for recoverable errors, fatal() for
  critical errors)
- **util.c**: Utility functions (string ops, bit manipulation, fdprintf for Unix
  syscalls)

**Pass 2 (cc2) Code Organization:**
- **parseast.c**: Semantic AST parser - parses S-expression AST and builds
  expression/statement trees
- **astio.c**: Low-level AST I/O - buffered input, whitespace/comment handling,
  token reading
- **astio.h**: AST I/O interface declarations

### Auto-Generated Files

The build system auto-generates several files from source code and data files.
These should NOT be edited manually:

- **enumlist.h**: Generated from token.h, lists all token enum values for
  validation
- **tokenlist.c**: Generated by maketokens, contains token name arrays
- **error.h**: Generated from errorcodes via makeerror.awk, defines error codes
  and messages
- **debug.h**: Generated by makedebug.sh, extracts VERBOSE() tags from sources
- **debugtags.c**: Generated by makedebug.sh, verbose option descriptions
- **op_pri.h**: Generated by genop_pri, operator priority table for expression
  parsing

### Key Data Structures

**struct expr** (ccc.h:39-60): Expression tree nodes
- Contains operator, left/right/up/prev/next pointers for tree navigation
- Type information, variable reference, constant value
- Flags: E_CONST (constant expression), E_RESOLVED, E_FUNARG

**struct stmt** (ccc.h:79-95): Statement nodes
- Parent/next linkage, left/right expressions
- chain: child/body statement
- otherwise: else branch for if statements
- middle: middle expression for for-loops
- function: owning function reference
- Flags: S_PARENT, S_LABEL, S_FUNC
- Used for if/else/while/for/switch/case/return/goto/labels/blocks

**struct type** (ccc.h:153-161): Type descriptors
- name, size, count (for arrays)
- elem list (for struct/union members, function parameter types)
- sub (for pointers, arrays, function return types)
- Flags: TF_AGGREGATE, TF_INCOMPLETE, TF_UNSIGNED, TF_FUNC, TF_POINTER,
  TF_ARRAY, TF_FLOAT, TF_OLD

**struct name** (ccc.h:159-178): Symbol table entries
- Represents variables, typedefs, function names, struct/union/enum tags
- Contains type pointer, visibility, storage class, offset (for struct members)
- body: function body statement tree
- kind enum: prim, etag, stag, utag, var, elem, tdef
- Scope management via lexical level

### Expression Tree Construction

Expression trees are built using two helper functions in expr.c:

**makeexpr(op, left)**: Basic expression node allocator
- Allocates and zero-initializes an expr structure
- Sets operator and left child pointer
- Returns pointer to new expression node

**makeexpr_init(op, left, type, v, flags)**: Convenience wrapper
- Calls makeexpr(op, left) to create the node
- Also sets type, value (v), and flags fields
- Reduces code duplication for common patterns
- Pass NULL for type to skip setting it

**Usage patterns:**
```c
// Basic node creation
e = makeexpr(PLUS, left_expr);

// Node with type and value (common for constants)
e = makeexpr_init(CONST, 0, inttype, 42, E_CONST);

// Cast operator with inner expression as left child
e = makeexpr(NARROW, inner);
e->type = target_type;
```

This design keeps expression tree construction simple while avoiding repetitive
field assignments.

### Type System Design

The type system is designed to be "squeaky-clean" with zero redundancy:
- Two variables of the same type share the identical type pointer
- Types are unified in a global type list, searched before creating new types
- Basic types are pre-loaded at initialization (char, short, long, unsigned
  variants, void, boolean, float, double)
- Primitive type indices: 0-2 are signed char/short/long, 3-5 are unsigned, 6+
  are void/bool/float/double
- Type construction: pointers, arrays, functions are composed from subtypes
- Function types store parameter list in type->elem as a linked list of names

**Type Features**:
- All primitive, pointer, array, function, struct/union/enum types
- Automatic type conversions (NARROW/SEXT/WIDEN) in assignments and expressions
- Pointer compatibility checking with ER_E_PT error reporting
- Lvalue validation for assignments and increment/decrement operators
- Static local variables in global data section with proper scoping

### Function Parameter Architecture

Function parameters are handled with proper separation between type signatures
and actual parameter symbols:

**Function Type Signature** (type->elem):
- Contains parameter types (stored as name entries with types)
- Parameter names are kept in type->elem for K&R type matching convenience
- Function type comparison uses `compatible_function_types()` which ignores
  names
- Two functions with same parameter types but different names have same type
  signature

**Function Parameter Symbols** (namespace at level 2):
- When parsefunc() processes a function body, it:
  1. Pushes a new scope (lexlevel becomes 2)
  2. Reads parameter info from type->elem
  3. Creates NEW name entries at level 2 with V_FUNARG flag
  4. Function body references parameters from level 2 namespace
  5. Dumps function output while params are still visible
  6. Pops scope (removes parameters from names[])

**Benefits of this design**:
- Function types properly normalized (names don't affect type compatibility)
- Parameters are regular symbols at level 2 in namespace (no special cases)
- Forward declarations with different parameter names work correctly
- dump_function() shows both type signature (types only) and actual parameters
  (with names)

**Example**:
```c
int foo(int x);           // Forward declaration
int foo(int y) { }        // Definition with different name - works!
```
Both have same function type (int -> int), but function body sees parameter as
'y' at level 2.

### Scope Management

Lexical scope is managed via a simple stack-based system:
- names[] array holds all name entries
- Each name has a 'level' field (0=basic types, 1=global, 2+=inner blocks)
- push_scope() increments lexlevel
- pop_scope() decrements lexlevel and removes all names at higher levels
- Lookup searches from most recent (highest index) to oldest

### Expression Parsing

Expression parsing uses precedence climbing:
- parse_expr(priority, stmt) recursively parses expressions
- Lower priority numbers bind tighter (higher precedence)
- op_pri[] table maps operators to priorities (auto-generated in op_pri.h)
- Constant folding (cfold) is performed during parsing for compile-time
  evaluation
- Precedence comparison: `p >= pri` stops parsing when encountering same/lower
  precedence
- Recursive call uses `parse_expr(p, st)` for right operand, ensuring
  left-associativity

**Complete expression support**: All C operators (unary, binary, ternary,
assignment, compound assignment, increment/decrement, sizeof, casts), constant
folding, proper precedence/associativity, function/array decay, automatic type
conversions. See detailed sections below for specific operator implementations.

### K&R Implicit Function Declarations

The compiler supports K&R-style implicit function declarations as an extension,
allowing classic K&R C code to compile without explicit forward declarations:

**Feature**: When an undefined symbol is followed by `(`, automatically declare
it as `int name()`:

```c
int main() {
    printf("Hello, World!\n");  /* Implicitly declared as: int printf() */
    return 0;
}
```

**Implementation** (expr.c case SYM):
- Save symbol name before gettoken() (which overwrites cur.v.name)
- Peek at next token to check for LPAR
- If symbol undefined and next token is LPAR, create implicit function:
  - Return type: int
  - Parameters: unknown (TF_FUNC with NULL elem)
  - Storage class: SC_EXTERN
  - Scope: level 1 (global)
- VERBOSE(V_SYM) outputs: "Implicit declaration: int name()"

**Benefits**:
- Allows compilation of K&R code without prototypes
- Classic "hello world" compiles without stdio.h
- Matches historical C compiler behavior

**Test**: tests/hello.c demonstrates implicit printf declaration

### Ternary Conditional Operator

The ternary conditional operator (? :) is fully implemented with proper
right-associativity and constant folding:

**AST Structure**:
- Tree: `QUES(condition, COLON(true_expr, false_expr))`
- COLON is not a binary operator, only used as part of ternary
- COLON removed from operator priority table to prevent standalone use

**Parsing**:
- Triggered by QUES token in binary operator loop
- Parse true expression: `parse_expr(0)` allows any expression
- Expect and consume COLON token
- Parse false expression: `parse_expr(0)` for right-associativity
- Nested ternary operators allowed

**Right-Associativity**:
- `a ? b : c ? d : e` parses as `a ? b : (c ? d : e)`
- False branch can contain another ternary at same precedence level
- Example: `1 ? 2 : 0 ? 3 : 4` -> `2` (not `3`)

**Constant Folding**:
- If condition is constant, fold to selected branch
- `1 ? 100 : 200` -> `100`
- `0 ? 100 : 200` -> `200`
- Handles nested constant ternary expressions

**Examples**:
```c
x > 0 ? 1 : -1          // (? (> x 0) (: 1 -1))
1 ? 10 : 20             // 10 (folded)
a ? b : c ? d : e       // (? a (: b (? c (: d e))))
```

**Label Compatibility**:
- Labels (`identifier:`) still work correctly
- Parsed at statement level by parse.c, not expression level
- Statement parser uses lookahead to detect label patterns
- Independent from expression operator handling

### Type Cast Operators

Type cast operator `(type)expr` is fully implemented with disambiguation from
parenthesized expressions:

**Disambiguation**:
- After `(`, check if next token is type keyword or typedef name
- If yes: parse as cast, otherwise parse as parenthesized expression
- `is_cast_start()` checks for type keywords (int, char, etc.) or typedef names

**Three Cast Operators** (only emit when runtime operation needed):
- **N** (NARROW): Truncate to smaller type (e.g., `long -> int`, `int -> char`)
- **W** (WIDEN): Zero-extend unsigned type to larger size
- **X** (SEXT): Sign-extend signed type to larger size

**AST Format**: `(op:width expr)` where width is destination type
- Width suffixes: `:b` (byte), `:s` (short), `:l` (long), `:p` (pointer)
- Same format as memory operations (M:width, =:width)

**Cast Selection Logic**:
- **Pointer -> Pointer**: No cast node (just type reinterpretation)
- **Same size**: No cast node (e.g., `int <-> unsigned int`)
- **Narrowing** (tgt < src): NARROW operator
- **Widening unsigned**: WIDEN operator (zero extend)
- **Widening signed**: SEXT operator (sign extend)
- **Pointer <-> Scalar**: NARROW or WIDEN based on size comparison

**Examples**:
```c
// Narrowing casts
(char) int_val      -> (N:b ...)      // truncate int to byte
(int) long_val      -> (N:s ...)      // truncate long to short

// Sign-extending casts
(int) signed_char   -> (X:s ...)      // sign-extend byte to short
(long) signed_int   -> (X:l ...)      // sign-extend short to long

// Zero-extending casts
(int) unsigned_char -> (W:s ...)      // zero-extend byte to short

// Pointer-to-pointer (no cast node)
(char *) int_ptr    -> Just type change in AST

// Mixed pointer/scalar
(int) ptr           -> (W:s ...)      // widen pointer to int
(char *) int_val    -> (W:p ...)      // widen int to pointer
```

**Implementation**:
- `parse_type_name()`: Parses type in cast (base type + pointers)
- Logic in expr.c selects appropriate operator based on type sizes and
  signedness
- outast.c emits single-char operator with width annotation

### Increment and Decrement Operators

Prefix and postfix increment/decrement operators (++, --) are fully implemented
with proper semantics:

**AST Operators** (single-character tokens):
- **PREINC** (0xcf, Ï): Prefix increment `++x` - increments lvalue, returns new
  value
- **POSTINC** (0xef, ï): Postfix increment `x++` - increments lvalue, returns
  old value
- **PREDEC** (0xd6, Ö): Prefix decrement `--x` - decrements lvalue, returns new
  value
- **POSTDEC** (0xf6, ö): Postfix decrement `x--` - decrements lvalue, returns
  old value

**Parsing**:
- Prefix forms: Parsed in prefix operator switch (unary precedence)
- Postfix forms: Parsed in postfix operator loop (highest precedence)
- E_POSTFIX flag distinguishes postfix from prefix in expression tree
- Lvalue unwrapping: DEREF is unwrapped to get address (like assignments)

**AST Format**: `(operator lvalue_address)`
```c
int x;
++x;        // (0xcf $_x)    - PREINC
x++;        // (0xef $_x)    - POSTINC
--x;        // (0xd6 $_x)    - PREDEC
x--;        // (0xf6 $_x)    - POSTDEC
```

**Pointer Arithmetic**:
```c
int *p;
*p++;       // (M:s (0xef $_p))         - post-increment p, then deref
*++p;       // (M:s (0xcf $_p))         - pre-increment p, then deref
++*p;       // (0xcf (M:s $_p))         - deref p, then increment value
(*p)++;     // (0xef (M:s $_p))         - deref p, then post-increment value
```

**Implementation Details**:
- Both forms unwrap DEREF to access lvalue address
- Postfix sets E_POSTFIX flag on expression node
- outast.c maps INCR/DECR + E_POSTFIX flag to single-char tokens
- Correct semantics: prefix returns new value, postfix returns old value

### Compound Assignment Operators

Compound assignment operators (+=, -=, etc.) are implemented with single lvalue
evaluation:

**Operators** (single-character tokens with width annotations):
- **PLUSEQ** ('P'): `x += y` - add and assign
- **SUBEQ** (0xdf, ß): `x -= y` - subtract and assign
- **MULTEQ** ('T'): `x *= y` - multiply and assign
- **DIVEQ** ('2'): `x /= y` - divide and assign
- **MODEQ** (0xfe, þ): `x %= y` - modulo and assign
- **ANDEQ** (0xc6, Æ): `x &= y` - bitwise AND and assign
- **OREQ** ('1'): `x |= y` - bitwise OR and assign
- **XOREQ** ('X'): `x ^= y` - bitwise XOR and assign
- **LSHIFTEQ** ('0'): `x <<= y` - left shift and assign
- **RSHIFTEQ** ('6'): `x >>= y` - right shift and assign

**Single Lvalue Evaluation**:
- Critical: lvalue is evaluated exactly once, not twice
- `*(foo()) += 1` calls `foo()` once, not twice
- Implementation keeps compound operators distinct (no desugaring to `x = x +
  y`)

**AST Format**: `(operator:width lvalue_address rvalue)`
```c
int i;
i += 5;         // (P:s $_i 5)
i -= 3;         // (0xdf:s $_i 3)
i *= 2;         // (T:s $_i 2)
```

**Automatic Type Conversion**:
- Compound assignments trigger automatic type conversion on rvalue
- Same conversions as simple assignment: NARROW/SEXT/WIDEN
```c
char c; int i;
c += i;         // (P:b $_c (N:b (M:s $_i)))  - NARROW int to char
```

**Implementation**:
- Lvalue unwrapping: DEREF is unwrapped to get address (like ASSIGN)
- `assign_type` variable tracks actual type being assigned
- Width annotation from lvalue type
- Type conversions inserted before compound operation

### String Literals

String literals are handled with proper escaping and output to a dedicated
literals section in the AST:

**Storage**:
- Stored as counted strings (cstring): first byte is length, followed by string
  data
- Not freed during tokenization to persist throughout compilation
- Each string gets a synthetic global name (_str0, _str1, etc.)

**Synthetic Names**:
- Created at global scope (level 1) to survive function scope cleanup
- Format: `_str0`, `_str1`, `_str2`, etc. (sequential counter)
- Stored in name table with STRING initializer expression

**AST Output Format**:
```
(L
  (s _str0 "hello world")
  (s _str1 "test\n")
  (s _str2 "quote\"and\\backslash")
)
```

**Escaping**:
Special characters are properly escaped in output:
- `\n` - newline
- `\t` - tab
- `\"` - quote
- `\\` - backslash
- `\r` - carriage return
- `\xNN` - hex escape for non-printable characters

**References**:
String literals in expressions reference synthetic names:
```c
char *s = "hello";      // (g $_s :ptr $___str0)
```

**Array Initialization**:
Arrays initialized with string literals automatically infer size:
```c
char foo[] = "string";  // :array:7 (6 chars + null terminator)
char bar[] = "test";    // :array:5 (4 chars + null)
char empty[] = "";      // :array:1 (0 chars + null)
```

**String Literal Concatenation**:
Adjacent string literals are automatically concatenated at parse time (standard
C feature):
```c
char *s = "Hello" "World";           // "HelloWorld"
char *msg = "Line1\n"
            "Line2\n"
            "Line3\n";               // "Line1\nLine2\nLine3\n"
char *code = "Before" /* comment */ "After";  // "BeforeAfter"
```

- Concatenation is handled in expr.c during expression parsing
- Lexer tokenizes individual strings; parser concatenates adjacent STRING tokens
- Comments and whitespace between strings are automatically handled by normal
  lexer processing
- Enables multi-line string formatting without requiring escape sequences at
  line ends

**Implementation**:
- expr.c: Creates synthetic name at level 1 when STRING token encountered
- parse.c: After initializer parsed, checks for array[-1] with STRING init, sets
  correct size
- outast.c: emit_literals() outputs all synthetic _str* names to literals
  section
- outast.c: emit_global_vars() skips _str* names (already in literals)
- type.c: get_type() compares array count to distinguish different array sizes

### Function and Array Decay

Functions and arrays automatically decay to pointers when used as values,
following standard C semantics:

**Function Decay**:
- Function names represent addresses (function pointers)
- Function names are NOT wrapped in DEREF (unlike variables)
- Direct use: `funcname` -> `$_funcname` (address)
- Assignment: `fptr = add;` -> `(= $_fptr $_add)`
- Call through pointer: `fptr(5, 3);` -> `(@ $_fptr 5 3)`
- Explicit address-of is redundant but allowed: `&add` -> `(& $_add)`

**Array Decay**:
- Array names decay to pointer to first element
- Array names are NOT wrapped in DEREF (unlike variables)
- Direct use: `arr` -> `$_arr` (address of first element)
- Assignment: `p = arr;` -> `(= $_p $_arr)`
- Array subscript: `arr[5]` -> `(M (+ $_arr 10))` (scaled offset, then deref)

**Variable Semantics (for comparison)**:
- Variables represent storage locations
- Variables ARE wrapped in DEREF to get their value
- Direct use: `x` -> `(M $_x)` (dereference to get value)
- Assignment: `x = 10;` -> `(= $_x 10)` (unwraps DEREF for lvalue)

**Implementation Details - Dual Flag Architecture**:
Arrays use BOTH `TF_ARRAY` and `TF_POINTER` flags to achieve proper decay
semantics while preserving array metadata:

- Array types have both flags set: `TF_ARRAY|TF_POINTER`
- `emit_type_info()` checks `TF_ARRAY` first → emits `:array:count:elemsize` in
  AST
- Type compatibility checks `TF_POINTER|TF_ARRAY` → treats arrays as pointers
- This allows arrays to:
  1. Emit full metadata (count, element size) for code generation
  2. Act as pointers in type compatibility and assignment checking

The SYM case in expression parser checks type flags:
- If `TF_FUNC`: return SYM directly (function address)
- If `TF_ARRAY`: return SYM directly (array base address, but type has both
  flags)
- Otherwise: wrap in DEREF (variable value)

**Array Subscripting**:
When subscripting, the parser saves the dereferenced type before unwrapping to
preserve correct element types:

```c
char *parms[32];    // Type: TF_ARRAY|TF_POINTER, sub=char*, count=32
parms[i]            // Returns: char* (correctly dereferenced from
                    // pointer array)

struct { char **p; } *s;
s->p[i]             // Returns: char* (saves DEREF type before unwrapping)
```

This prevents type information loss when subscripting struct members or complex
expressions.

**Function Pointer Arrays**:
Arrays of function pointers work with both syntaxes:

```c
int (*funcs[10])(int, int);

// Implicit syntax
result = funcs[0](5, 3);        // (@ (M (+ $_funcs 0)) 5 3)

// Explicit/preferred syntax
result = (*funcs[0])(5, 3);     // (@ (M (M (+ $_funcs 0))) 5 3)

// Variable index
result = (*funcs[i])(5, 3);     // (@ (M (M (+ $_funcs (* (M $i) 2)))) 5 3)

// Expression index
result = (*funcs[i+1])(5, 3);   // index expression evaluated and scaled
```

Both implicit and explicit syntaxes are supported and generate correct AST. The
explicit syntax `(*funcs[x])(args)` is preferred and more clearly shows the
dereference operation.

### Memory Width Annotations

The AST output includes memory width annotations on DEREF (M) and ASSIGN (=)
operators to provide size information for code generation. This architectural
decision enables several important benefits:

**Annotation Format**:
Size suffixes are appended to memory operations based on the type being
accessed:
- `:b` - byte (1 byte: char)
- `:s` - short (2 bytes: short, int)
- `:l` - long (4 bytes: long)
- `:p` - pointer (2 bytes: any pointer type)
- `:f` - float (4 bytes: float)
- `:d` - double (8 bytes: double)

**Examples**:
```c
char c;
int i;
long l;

c = 10;              // (=:b $_c 10)
i = c;               // (=:s $_i (M:b $_c))
l = i;               // (=:l $_l (M:s $_i))
```

**Benefit 1: Deferred Type Checking to Pass 2**:
Size annotations allow pass 1 (cc1) to remain simple and focused on parsing,
while pass 2 (cc2) handles all type width mismatches and conversions:

- Pass 1 just annotates the source and destination widths
- Pass 2 sees complete information: `(=:s $_i (M:b $a))` shows int<-byte
  assignment
- Type conversions, sign extensions, and warnings are deferred to code
  generation time
- Pass 1 doesn't need complex type checking logic
- Pass 2 has full context to generate optimal conversion code

This separation of concerns keeps the parser clean and moves complexity to where
it's needed (code generation).

**Benefit 2: Byte Arithmetic Optimization**:
Size annotations enable the code generator to use native byte arithmetic for
byte-sized variables instead of promoting to word size:

```c
char a, b, c;
a = b + c;           // (=:b $a (+:b (M:b $b) (M:b $c)))
```

Without annotations, the code generator would need to:
1. Load b as byte, promote to word
2. Load c as byte, promote to word
3. Add words
4. Truncate result to byte
5. Store byte

With annotations, the code generator can:
1. Load b as byte
2. Add c as byte (native byte arithmetic)
3. Store a as byte

This generates smaller, faster code and preserves correct overflow semantics for
byte-sized operations. On Z80 architecture, this is particularly important as
many instructions operate directly on 8-bit values.

**Implementation**:
The `get_size_suffix()` function in outast.c examines type flags and size
fields:
- Checks `TF_POINTER` flag for pointers
- Checks `TF_FLOAT` flag for float vs long disambiguation
- Uses `type->size` to determine width for other types

### Statement Parsing

Statement parsing is complete in parse.c:
- parsefunc() calls statement() to parse function bodies
- statement() processes statements recursively until END token
- Returns complete statement tree attached to function's body field
- All statement types supported: blocks, if/else, loops (while/do-while/for),
  switch/case, break/continue, return, goto/labels, expression statements, local
  declarations

### Token System

Tokens are encoded as single-byte values for efficient serialization:
- Most C keywords map to printable ASCII letters (IF='I', WHILE='W', etc.)
- Operators use their ASCII symbols where possible (PLUS='+', STAR='*')
- This allows cheap debugging and intermediate file I/O

### Debugging

Verbose debugging uses bitmask flags defined by VERBOSE() macro calls:
- Use -v flag with hex value to enable specific debug output
- ./makedebug.sh scans sources for VERBOSE(tag) and generates
  debug.h/debugtags.c
- Tags include: V_TRACE, V_LEX, V_PARSE, V_TYPE, etc.

## Development Notes

### Current State

**Pass 1 (cc1) - Complete**:
- Preprocessor: Full CPP with macros, includes, conditional compilation,
  stringify, token pasting
- Lexer: All C keywords, operators, literals
- Type System: All types (primitive, pointer, array, function,
  struct/union/enum), automatic conversions, compatibility checking
- Declarations: Variables, functions (ANSI/K&R), typedefs, bitfields, storage
  classes, initializers
- Expressions: All operators, constant folding, proper precedence/associativity
- Statements: All control flow, local declarations, proper scoping
- AST Output: S-expression format with memory width annotations for pass 2

**Self-hosting**: 100% complete - parses all 18 of its own source files (tagged
as **self-parse**)
**Tests**: 142 tests organized by category, all passing

**Not yet implemented**:
- Code generation (pass 2)

### Known Issues

- The 'signed' keyword is not supported (deliberate omission)
- Anonymous struct/union declarations don't work properly
- Single-bit bitfields in structs could be optimized better

### Testing

Tests are in tests/ directory (142 tests organized by category, all passing):
- **Expression tests** (EXPR_TESTS): Constant folding, simple expressions
- **Declaration tests** (DECL_TESTS): Variable and type declarations
- **Preprocessor tests** (CPP_TESTS): Macros, includes, conditional compilation,
  stringify
- **K&R function tests** (KR_FUNC_TESTS): K&R style function definitions
- **Modern function tests** (FUNC_TESTS): ANSI-style definitions and prototypes,
  forward declarations
- **Statement tests** (STMT_TESTS): All control flow statements
- **sizeof tests** (SIZEOF_TESTS): sizeof operator with various types
- **Typedef tests** (TYPEDEF_TESTS): Global and scoped typedefs, forward
  declarations
- **Local variable tests** (LOCAL_TESTS): Local declarations in functions
- **Scope tests** (SCOPE_TESTS): Lexical scoping
- **Struct tests** (STRUCT_TESTS): Struct declarations
- **Type cast tests** (CAST_TESTS): Pointer-to-pointer, scalar, and mixed casts
  with typedef support
- **String literal tests** (STRING_TESTS): String literals with escaping, array
  initialization (char[] = "string")
- **Increment/decrement tests** (INCR_DECR_TESTS): Prefix/postfix ++/-- on
  variables, pointers, expressions
- **Pointer compatibility tests** (PTR_COMPAT_TESTS): Pointer type checking and
  compatibility validation
- **Lvalue validation tests** (LVALUE_TESTS): Invalid lvalue detection for
  assignments and operators
- **Minimal/smoke tests**: Basic compiler functionality
- **Miscellaneous tests**: Regression tests

Unit tests are in unit_test/ directory:
- **test_kw**: Keyword lookup table testing
- **test_insertmacro**: Macro text insertion testing

See tests/Makefile for complete test organization and tests/README.md for
documentation.

Tests run with:
- `make test` - Run all integration tests
- `make test-typedef` - Run typedef tests only
- `make test-cast` - Run type cast tests only
- See tests/Makefile for all test categories
- `make unit-tests` - Run unit tests
- `cd tests && ./runtest.sh name.c` - Run single test
- Tests pass if cc1 completes without crashing

### Development History

Pass 1 (cc1) is complete with all major C language features implemented: full
preprocessor, type system, expression parsing, statement parsing, and AST
emission. The compiler is self-hosting (parses all 18 of its own source files)
and passes all 142 tests.

**Pass 1 (cc1) - Recent Enhancements**:
- K&R implicit function declarations: Undefined symbols followed by `(` are
  automatically declared as `int name()` (expr.c)
- Tests: Added tests/hello.c to demonstrate implicit printf declaration

**Pass 2 (cc2) - In Development**:
- Code refactoring: Split parseast.c into astio.c for low-level I/O operations
  - astio.c: Buffered input, whitespace/comment handling, token reading
  - parseast.c: Semantic AST parsing and tree building
  - Removed unnecessary forward declarations from parseast.c
- AST parser (parseast.c) implemented with critical bug fixes:
  - Fixed operator parsing in handle_block() to prevent semicolon consumption
  - Fixed BREAK statement handling (changed from 'B' to 'K')
  - Fixed XOREQ vs SEXT operator conflict (XOREQ='X', SEXT=0xab)
  - Added Break/Continue statement support in blocks and switches
  - Expanded switch statement handling for nested control flow
- Output file generation: strips .ast extension and appends .s, preserves
  directory paths
- Build system improvements:
  - selfcheck and fullcheck targets output to current directory
  - Preserve .ast and .s files on test failures for debugging
  - Clean target removes .ast and .s files

### Code Style

- Minimize code size, even at some cost to clarity
