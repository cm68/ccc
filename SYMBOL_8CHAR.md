# 8-Character Symbol Name Resolution

This document describes the symbol renaming performed to ensure all symbols are unique in their first 8 characters, as required by linkers with 8-character symbol limitations.

## Summary

**Status**: ✅ Complete - All conflicts resolved

**Symbols Renamed**: 17 symbols across 7 conflict groups

**Files Modified**: 11 files (cc1.h, cc1.c, cc2.h, expr.c, parse.c, parseast.c, outast.c, lex.c, io.c, macro.c, error.c, astio.c, unit_test/test_insertmacro.c, CLAUDE.md)

**Build Status**: ✅ All tests passing (21 self-hosting files)

## Conflict Resolution

### Group 1: emit_global_*
**Original Conflict**: `emit_glo` (8-char prefix)
```
emit_global_var  → emit_gv   (7 chars, unique)
emit_global_vars → emit_gvs  (8 chars, unique)
```
**Files**: cc1.h, outast.c, parse.c

### Group 2: makeexpr*
**Original Conflict**: `makeexpr` (8-char prefix)
```
makeexpr      → mkexpr    (6 chars, unique)
makeexpr_init → mkexpr_i  (8 chars, unique)
```
**Files**: cc1.h, expr.c, parse.c, CLAUDE.md

### Group 3: cpp_file*
**Original Conflict**: `cpp_file` (8-char prefix)
```
cpp_file      → cppfile   (7 chars, unique)
cpp_file_name → cppfname  (8 chars, unique)
```
**Files**: cc1.h, cc1.c, lex.c, io.c, unit_test/test_insertmacro.c

**Note**: Also fixed `write_cpp_file` → `write_cppfile` (was incorrectly affected by replacement)

### Group 4: skipwhite*
**Original Conflict**: `skipwhit` (8-char prefix)
```
skipwhite  → skipws   (6 chars, unique)
skipwhite1 → skipws1  (7 chars, unique)
```
**Files**: cc1.h, lex.c, astio.c, macro.c

### Group 5: asm_capture_*
**Original Conflict**: `asm_capt` (8-char prefix)
```
asm_capture_buf  → asm_cbuf  (8 chars, unique)
asm_capture_len  → asm_clen  (8 chars, unique)
asm_capture_size → asm_csiz  (8 chars, unique)
```
**Files**: cc1.h, lex.c, macro.c, parse.c

### Group 6: destroy_*
**Original Conflict**: `destroy_` (8-char prefix)
```
destroy_expr → fr_exp  (6 chars, unique)
destroy_stmt → fr_stm  (6 chars, unique)
```
**Files**: cc1.h, expr.c

**Note**: Additional rename to avoid new conflicts:
```
free_stmt → fr_stmt  (7 chars, unique)
```
**Files**: cc1.h, cc2.h, parse.c, parseast.c

### Group 7: token_history*
**Original Conflict**: `token_hi` (8-char prefix)
```
token_history       → tok_hist  (8 chars, unique)
token_history_index → tok_hidx  (8 chars, unique)
```
**Files**: cc1.h, lex.c, error.c

## Verification

All symbols verified unique in first 8 characters:
```bash
./find_conflicts.sh
# Output: No conflicts found!
```

## Testing

✅ **Build**: `make` completed successfully
✅ **Self-hosting**: All 21 compiler source files parse correctly
✅ **Functionality**: No regressions detected

## Files Changed

| File | Symbols Changed | Lines Modified |
|------|----------------|----------------|
| cc1.h | 11 | ~15 |
| cc1.c | 2 | ~3 |
| cc2.h | 1 | ~1 |
| expr.c | 4 | ~10 |
| parse.c | 7 | ~10 |
| parseast.c | 1 | ~4 |
| outast.c | 2 | ~3 |
| lex.c | 7 | ~15 |
| io.c | 2 | ~2 |
| macro.c | 4 | ~4 |
| error.c | 2 | ~2 |
| astio.c | 2 | ~2 |
| unit_test/test_insertmacro.c | 2 | ~2 |
| CLAUDE.md | 2 | ~4 |

## Impact on External Code

If you have external code that links with the compiler library or calls these functions, update the following symbol references:

### Function Calls
```c
// Old → New
emit_global_var()  → emit_gv()
emit_global_vars() → emit_gvs()
makeexpr()         → mkexpr()
makeexpr_init()    → mkexpr_i()
skipwhite()        → skipws()
skipwhite1()       → skipws1()
destroy_expr()     → fr_exp()
destroy_stmt()     → fr_stm()
free_stmt()        → fr_stmt()
```

### Variables
```c
// Old → New
cpp_file           → cppfile
cpp_file_name      → cppfname
asm_capture_buf    → asm_cbuf
asm_capture_len    → asm_clen
asm_capture_size   → asm_csiz
token_history      → tok_hist
token_history_index → tok_hidx
```

## Linker Compatibility

The codebase is now compatible with linkers that have 8-character symbol name limitations, including:
- Classic Unix linkers
- Embedded system toolchains with size constraints
- Legacy development environments
- Some retro computing platforms

All symbols are guaranteed unique in their first 8 characters, ensuring proper linking with no symbol collisions.

## Future Considerations

When adding new symbols:
1. Check first 8 characters don't conflict with existing symbols
2. Use the verification script: `./find_conflicts.sh`
3. Prefer short, distinctive prefixes (< 8 chars when possible)
4. Document any new conflicts and resolutions in this file

## Automation

The conflict detection script is available at `/tmp/find_conflicts.sh` and can be integrated into the build process for continuous verification.
