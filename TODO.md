# TODO List

## Completed Features (Pass 1)

### Core Infrastructure
- + Full C preprocessor (macros, includes, conditional compilation, stringify, token pasting)
- + Nested macro expansion works correctly
- + Complete lexical analysis
- + Comprehensive test suite (110+ tests organized in 15 categories)

### Type System
- + Primitive types: char, short, int, long, unsigned variants, void, boolean, float, double
- + Pointer types (multi-level)
- + Array types (multi-dimensional)
- + Function types with parameter lists
- + Struct types with member lists
- + Union types
- + Enum constants as named integers in global namespace
- + Enum variables are simply unsigned char (_uchar_)
- + Forward struct/union declarations
- + Bitfield support in struct declarations
- + Tag namespace separate from variable namespace
- + Struct member namespace separation
- + Pointer type compatibility checking (validates assignments, reports ER_E_PT)
- + Automatic type conversions in assignments (NARROW/SEXT/WIDEN)

### Declarations
- + Variable declarations (global and local)
- + Function declarations (both ANSI and K&R style)
- + ANSI-style function definitions: `int foo(int x) { }` works correctly
- + Function type normalization: parameter names don't affect type compatibility
- + Forward declarations with different parameter names
- + Function parameters stored in namespace at level 2 with V_FUNARG flag
- + Typedef support including scoped typedefs inside functions
- + Local variable declarations inside functions
- + Static local variables emitted in global data section

### Expression Parsing
- + Constant folding for compile-time evaluation
- + Arithmetic operators: +, -, *, /, % with constant folding
- + Bitwise operators: &, |, ^, <<, >> with constant folding
- + Logical operators: &&, ||, ! with constant folding
- + Comparison operators: <, >, <=, >=, ==, != with constant folding
- + Unary operators: -, ~, !, *, &
- + Assignment operator: = with proper lvalue handling
- + Increment/decrement operators: ++, -- (prefix and postfix with distinct AST operators)
- + Compound assignment operators: +=, -=, *=, /=, %=, &=, |=, ^=, <<=, >>= (single lvalue evaluation)
- + Ternary conditional operator (? :) with right-associativity and constant folding
- + Type cast operator: (type)expr with typedef disambiguation
- + Address semantics: SYM nodes represent addresses, DEREF accesses values
- + Function and array names decay to pointers (correct C semantics)
- + Array subscript with scaled addition: arr[i] -> DEREF(ADD(arr, i * sizeof))
- + Struct member access: . and -> operators with offset addition
- + Function calls with argument lists
- + Function pointers: assignment, calls, arrays of function pointers
- + sizeof operator (for types and expressions)
- + Proper operator precedence and associativity

### Statements
- + Complete statement parsing (all control flow)
- + if/else statements
- + while, do-while, for loops
- + switch/case/default statements
- + break, continue, return statements
- + goto and labels
- + Block statements with proper scoping

### Memory Operations
- + String literals output to AST with escaped data
- + Array initialization with string literals (char[] = "string")
- + Memory copy operator (Y) for array/struct initialization
- + Struct assignment using COPY operator with block memory copy
- + Memory width annotations on DEREF and ASSIGN operators

## Known Limitations (Pass 1)

- The 'signed' keyword is deliberately not supported
- Anonymous struct/union declarations don't work properly

## Pending Tasks (Pass 1)

### High Priority - Type System Improvements

- [ ] Add full type compatibility checking (sametype function for all contexts)
- [ ] Implement type conversions and promotions in general expressions (not just assignments)
- [ ] Fix operator type propagation (currently copies left operand incorrectly)
- [ ] Add type checking validation for all operators
- [ ] Add lvalue validation for assignments and operators
- [ ] Function signature checking at call sites
- [ ] Pointer arithmetic type checking

### Lower Priority

- [ ] Improve error messages and recovery
- [ ] Better handling of edge cases

## TODO (Pass 2 - Code Generation)

- [ ] Implement code generator
- [ ] Implement assembler output
- [ ] Object file generation

## Future Optimizations (Ideas for Later)

### Lexer Optimization
Make a bitmap with 128 entries for character classification:
- 0x01 - character that may start a symbol name
- 0x02 - character internal to a symbol name
- 0x04 - whitespace
- 0x10 - operator
- 0x20 - += kind of operator
- 0x40 - ++ kind of operator

### Code Generation Optimization
Single-bit bitfields in structs for efficient flag handling.
Example: if (sym->visible) { emit("extern "); }
Could generate: ld iy, sym ; bit 4, (iy+9) ; jr foo

### Symbol Table Optimization
Faster/smaller scope and symbol space management:
- Store symbols in array indexed by number instead of pointers
- Index 0 is first global (grows up), locals are negative (grow down)
- Each symbol has scope level (increment on block open, decrement on close)
- Lookups start from most recent local to last global
- Store names as fixed-length strings with hash, length, and data
- Limit name storage to ~12 bytes to get symbol table down to 16 bytes per entry
- Statics: global storage but scoped, with synthetic global name for symbol table
