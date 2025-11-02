# test_insertmacro - Test Program for io.c Buffer Manipulations

## Overview

This test program exercises the `insertmacro()` function in `io.c`, which handles macro text insertion into the character stream. The function has two code paths with different buffer manipulation strategies.

## Building and Running

```bash
# Compile
gcc -g -DDEBUG -o test_insertmacro test_insertmacro.c io.c -I.

# Run tests
./test_insertmacro          # Normal output
./test_insertmacro -v       # Verbose (shows internal buffer dumps)
```

## What It Tests

### Test 1: Fast Path
**Condition:** `offset > macro_length`

When there's enough space in the already-read portion of the buffer, `insertmacro()` uses an optimization:
- Copies macro text backwards into the consumed portion of the buffer
- Updates `curchar` and `nextchar` to point to the start of the macro
- Reuses the same `textbuf` structure (no allocation)

**Example:** Buffer offset=10, macro="XYZ" (3 chars)
- Before: `ABCDEFGHIJ|KLMNOP` (| marks offset)
- After:  `ABCDEFGXYZ|KLMNOP` (macro inserted, offset moved back)

### Test 2: Slow Path
**Condition:** `offset <= macro_length`

When the macro doesn't fit in the consumed space:
- Allocates a new `textbuf` structure
- Pushes it onto the buffer stack
- Stores macro text in the new buffer
- Does NOT update `curchar` or `nextchar`

**Important:** Caller must manually initialize `nextchar` from the new buffer before calling `advance()`.

### Test 3: Edge Case
Tests the boundary condition where `offset == macro_length` (uses slow path since condition is `offset > length`).

### Test 4: Nested Macros
Tests multiple macro insertions creating a stack of buffers:
```
SECONDMACRO (top)
  ↓
FIRSTMACRO
  ↓
original buffer (bottom)
```

## Key Findings

### Buffer Offset Behavior

The slow path has an interesting quirk: when creating a new buffer with `offset=0`, the first call to `advance()` does `++offset` before reading, which accesses `storage[1]` instead of `storage[0]`, skipping the first character.

**Workaround:** Manually set `nextchar = tbtop->storage[0]` after `insertmacro()` in slow path.

### Character Stream Semantics

According to `io.c:174`, macros are inserted "BETWEEN curchar and nextchar":
- `curchar`: already consumed character
- `nextchar`: lookahead character
- Macro should be inserted after `curchar`, before `nextchar`

In the fast path, this works correctly because `curchar` and `nextchar` are updated.

In the slow path, `nextchar` is not updated, which can cause issues if not handled carefully by the caller.

## Test Output

All tests pass and verify:
- ✓ Correct code path taken (fast vs slow)
- ✓ Buffer manipulation is correct
- ✓ Characters can be read in the expected order
- ✓ Buffers are properly stacked and popped
- ✓ Memory is managed correctly (via the helper functions)

## Files

- `test_insertmacro.c` - Main test program
- `io.c` - Source file being tested (contains `insertmacro()`)
- `ccc.h` - Header with structure definitions

## Notes

This is a unit test for low-level buffer manipulation. Real usage in the compiler is more complex and involves the lexer's `gettoken()` function coordinating with `advance()` and the macro expander.
