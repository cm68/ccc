# TODO List

## Pass 1 Status

**Complete**: Full preprocessor, type system, expression/statement parsing, AST emission. All 134 tests passing. See CLAUDE.md and README.md for details.

**Known Limitations**:
- The 'signed' keyword is deliberately not supported
- Anonymous struct/union declarations don't work properly

## Pending Tasks (Pass 1)

### High Priority - Type System Improvements

- [ ] Add full type compatibility checking (sametype function for all contexts)
- [x] Implement type conversions and promotions in binary expressions (automatic operand widening)
- [x] Fix operator type propagation (result type now uses larger operand type)
- [x] Add lvalue validation for assignments and operators
- [ ] Add type checking validation for all operators
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
