# CPP Filter Tests

Test suite for the cpp filter pipeline.

## Running Tests

```sh
./runtest.sh         # Run all tests
./runtest.sh -f      # Run only filter stress tests
./runtest.sh while   # Run single test
./runtest.sh -g      # Regenerate expected outputs
```

## Test Categories

### Loop Lowering Tests (xdump comparison)
- `while.c`, `for.c`, `do.c` - Basic loop tests
- `nested.c` - Nested loops
- `break.c`, `continue.c` - Break/continue handling
- `switch_in_loop.c` - Switch inside loops
- `dowhile_nested.c`, `mixed_loops.c` - Complex nesting
- `multiline_for.c` - Multi-line for expressions
- `spill_*.c` - Buffer spill tests

### Filter Stress Tests (pattern validation)

#### test_filtknr.c
K&R to ANSI function conversion:
- Multiple parameters with various types
- Pointer parameters, struct/union/enum types
- Function pointer parameters
- Already-ANSI functions (pass-through)

#### test_filtdecl.c
Declaration/initializer separation:
- Simple and compound initializers
- Multiple variables with mixed initializers
- Struct/union/enum definitions (preserved)
- Typedefs, array declarations, cast expressions

#### test_filtbrace.c
Brace insertion:
- Single-statement if/else/while/for/do
- Nested control structures, dangling else
- Switch case bodies, empty statements

#### test_filtctrl.c
Control flow lowering:
- While/for/do loop transformation
- Switch statement transformation
- Break/continue handling
- **Long conditions (BUF_MAX stress test)**
- Complex for loop expressions

## Known Issues Fixed

- **BUF_MAX overflow**: Conditions with >32 tokens were silently truncated.
  Fixed by increasing BUF_MAX from 32 to 64 in filtctrl.c.
