# Unit Tests for ccc Compiler

This directory contains unit tests for individual components of the ccc C compiler.

## Organization

Unit tests focus on testing specific modules or functions in isolation, without requiring the full compiler to be built or run.

## Available Tests

### test_kw - Keyword Lookup Test
Tests the keyword lookup functionality in `kw.c`. Validates that all C keywords, preprocessor keywords, and assembly keywords are correctly recognized and mapped to their token values.

**Dependencies**: `kw.o` from parent directory

**Run**:
```bash
make test-kw
```

### test_insertmacro - Macro Insertion Buffer Test
Tests the `insertmacro()` function in `io.c` which handles macro expansion buffer management. Tests both fast path (reusing existing buffer) and slow path (allocating new buffer) scenarios.

See `test_insertmacro_README.md` for detailed documentation.

**Dependencies**: `io.o` from parent directory

**Run**:
```bash
make test-insertmacro              # Normal mode
make test-insertmacro-verbose      # Verbose mode with buffer dumps
```

## Building and Running

```bash
# Build all unit tests
make

# Run all unit tests
make tests

# Run individual test
make test-kw
make test-insertmacro

# Clean build artifacts
make clean
```

## Running from Project Root

```bash
# Run all unit tests from project root
make unit-tests
```

## Adding New Unit Tests

To add a new unit test:

1. Create test source file `test_<name>.c` in this directory
2. Add build rule to `Makefile`:
   ```makefile
   test_<name>: test_<name>.c $(PARENT_OBJS)
       $(CC) $(CFLAGS) $(INCLUDES) -o test_<name> test_<name>.c $(PARENT_OBJS)
   ```
3. Add to `TESTS` variable in Makefile
4. Add test invocation to `tests` target
5. Document the test in this README

## Notes

- Unit tests link against object files from the parent directory
- The parent Makefile will automatically build required object files
- All generated headers (debug.h, error.h, etc.) must be built before unit tests
- Unit tests should be self-contained and not depend on external test files
