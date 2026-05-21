# Code Restriction Audit Report

Audit of ~/src/ccc/ccc for violations of Z80 self-hosting restrictions.

## Summary

| Restriction | Status | Notes |
|-------------|--------|-------|
| No structure copies | **FIXED** | pass2/expr.c - replaced with memcpy |
| No functions returning struct | **FIXED** | cpp/filttest.c - rewrote to use pointer-out |
| No struct arguments | **FIXED** | cpp/filttest.c - rewrote to use pointers |
| No `signed` keyword | OK | only in keyword tables/comments |
| No `const` storage class | **FIXED** | cpp/filttest.c, cpp/filtbrace.c, astpp.c |
| No UL suffixes | OK | none found |
| No LL suffixes | OK | none found |
| No C99 for-loop declarations | **FIXED** | cpp/filttest.c, cpp/mkkw.c |
| No auto aggregate initializers | **FIXED** | cpp/mkkw.c (made static) |

---

## Fixes Applied

### 1. pass2/expr.c:249 - Structure Copy
```c
// Before:
*n = *e;

// After:
memcpy(n, e, sizeof(Expr));
```

### 2. cpp/filttest.c - Complete Rewrite
- Changed filter function prototypes from `struct token func(void)` to `void func(struct token *out)`
- Changed `get_input()` from struct-returning to pointer-out style
- Changed `parse_tok()` from struct-returning to pointer-out style
- Changed `print_token()` from struct-by-value to pointer parameter
- Removed all `const` qualifiers
- Fixed C99 for-loop: moved `int i` declaration to function scope

### 3. cpp/filtbrace.c - Removed const (DEBUG only)
```c
// Before:
static const char *stname[] = {...}
static const char *ctrlname(...)

// After:
static char *stname[] = {...}
static char *ctrlname(...)
```

### 4. astpp.c - Removed const
```c
// Before:
static void prln(const char *s)
static void exprApp(const char *s)
const char *opn = ...
static void initApp(const char *s)

// After:
static void prln(char *s)
static void exprApp(char *s)
char *opn = ...
static void initApp(char *s)
```

### 5. cpp/mkkw.c - C89 Compliance
- Moved all for-loop variable declarations to function scope
- Changed auto aggregate initializer to static:
```c
// Before:
char *nonkw[] = { "foo", ... };  // auto aggregate - VIOLATION

// After:
static char *nonkw[] = { "foo", ... };  // static - OK
```
- Fixed mid-block declarations in emit(), printtable(), dumptrie(), main(), testtable()

---

## Files Verified Clean

- **~/src/ccc/tools/*** - All files clean (no violations)
- **cpp/filtutil.c** - Uses `tokcpy()` and `memcpy()` properly
- **All other production code** - No violations found

---

## Notes

### Keyword Tables (Not Violations)
The following files contain `signed` and `const` as keywords in lexer tables - this is required to recognize C keywords, not actual usage:
- cpp/mkkw.c - keyword generator
- cpp/xdump.c - debug token printer (DEBUG only)

### Build Tools
- cpp/mkkw.c is now C89 compliant and can be built natively on Z80
