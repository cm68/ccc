# Code Restriction Audit

Audit of the tree against the Z80 self-hosting restrictions in
[RESTRICTIONS.md](RESTRICTIONS.md).

## Current state: clean

Re-checked against the tree as it stands. No violations outside the documented
exemption.

| Restriction | Status |
|-------------|--------|
| No structure assignment | clean |
| No functions returning a struct | clean |
| No struct arguments by value | clean |
| No `const` | clean — only as the `CONST` keyword token in `cpp/xdump.c` |
| No `signed` | clean — only in comments and the `SIGNED` keyword token |
| No auto aggregate initializers | clean outside `cpp/test/` |
| No `UL` / `LL` suffixes | clean |
| No C99 for-loop declarations | clean; enforced by `-Werror=declaration-after-statement` |
| Declarations at the top of a block | enforced by the same flag |

The exemption is `ccc/cpp/test/*.c`, which is fed to cpp alone and never
compiled. Those files exist to exercise the declaration/initializer handling and
must keep using the constructs it has to survive. `test_filtdecl.c` is where the
auto aggregate initializers live.

## Re-running the audit

```bash
cd /vault/src/ccc

# const and signed outside comments and keyword tables
grep -rn '\bconst\b'  --include=*.c --include=*.h ccc/ tools/ libsrc/ | grep -v attic
grep -rn '\bsigned\b' --include=*.c --include=*.h ccc/ tools/ | grep -v unsigned

# structure assignment
grep -rnE '^\s*\*[a-zA-Z_][a-zA-Z_0-9]* = \*[a-zA-Z_]' --include=*.c ccc/ tools/ libsrc/

# auto aggregate initializers
grep -rnE '^\s+(char|int|short|long|unsigned char|struct [a-z]+) +[a-zA-Z_0-9]+\[[0-9]*\] *= *[{"]' \
    --include=*.c ccc/ tools/ libsrc/ | grep -v static
```

The C99-declaration and declaration-after-statement rules are enforced at build
time rather than by grep: the host Makefiles pass
`-Werror=declaration-after-statement`, because gcc accepts declarations anywhere
and pass1 does not.

---

## History

The fixes below were applied in the original audit pass. **Several of the files
named no longer exist** — `cpp/filttest.c`, `cpp/filtbrace.c`, and the rest of
the filter pipeline were folded into `norm.c` (see [cpp/NORM.md](cpp/NORM.md)).
The entries are kept because they record what the restrictions cost in practice.

### pass2/expr.c — structure copy

```c
*n = *e;                    /* before */
memcpy(n, e, sizeof(Expr)); /* after  */
```

### cpp/filttest.c — complete rewrite *(file since removed)*

- Filter prototypes changed from `struct token func(void)` to
  `void func(struct token *out)`
- `get_input()` and `parse_tok()` changed from struct-returning to pointer-out
- `print_token()` changed from struct-by-value to a pointer parameter
- All `const` qualifiers removed
- C99 for-loop declaration hoisted to function scope

### cpp/filtbrace.c — removed const *(file since removed)*

`static const char *stname[]` → `static char *stname[]`, and likewise
`ctrlname()`. DEBUG-only code.

### astpp.c — removed const

`prln()`, `exprApp()`, `initApp()` and a local `opn` all dropped `const`.

### cpp/mkkw.c — C89 compliance

- For-loop variable declarations hoisted to function scope
- `char *nonkw[] = {...}` made `static` — an auto aggregate initializer
- Mid-block declarations fixed in `emit()`, `printtable()`, `dumptrie()`,
  `main()`, `testtable()`

### Not violations

`cpp/mkkw.c` and `cpp/xdump.c` contain `signed` and `const` as **keyword table
entries** — required to recognize the C keywords, not uses of them.
