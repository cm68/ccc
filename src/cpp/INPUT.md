# CPP Input Language

This document defines the language accepted by `cpp`, the ccc preprocessor.
The input is C source text; `cpp` tokenizes it, runs the C preprocessor, and
feeds the result through the normalizer (`tdsrc.c` → `norm.c`, see
[NORM.md](NORM.md)). For the language `cpp` *produces*, see
[OUTPUT.md](OUTPUT.md).

The accepted dialect is the ccc subset of C. It deliberately omits several
standard-C features and preprocessor behaviours in exchange for a small
Z80-hostable implementation.

---

## 1. Source text

- Files are read through a unified text-buffer stack (`io.c`) shared by the
  main source, `#include` files, and macro expansions.
- Line endings: `\n` terminates a line; `\r` is treated as whitespace.
- A backslash immediately before a newline is a line continuation (used mainly
  in `#define` bodies).
- Tabs are 4 columns wide (project convention); column 0 is significant only
  for the `#` that introduces a directive.

## 2. Comments

Stripped before tokenization:

- `/* ... */` block comments. **Not nested** — the first `*/` closes the
  comment.
- `// ...` line comments, to end of line.

## 3. Identifiers and keywords

- An identifier starts with a letter or `_` and continues with letters,
  digits, or `_` (`issym()` in `lex.c`).
- **Maximum length is 14 characters.** Longer identifiers are truncated and
  produce warning `ER_W_SYMTRUNC`. (The object-file format allows 15 bytes; the
  leading `_` added by the compiler consumes one.)
- A trailing `:` after an identifier at statement position makes it a **label**
  rather than a symbol.
- Recognized keywords — the complete list, from the table in `mkkw.c` that
  generates `kwtab.c`:

  ```
  int char struct signed long unsigned union short void enum
  typedef auto extern static register
  goto return if while else switch case break continue do default for
  asm const volatile sizeof
  ```

  **`float` and `double` are not keywords.** ccc has no floating point, and
  they are deliberately left unreserved so a program can typedef them.

  `signed`, `const`, and `volatile` are accepted lexically (so existing headers
  parse) but see the restrictions below — `const`/`volatile` are dropped from
  the output, and `signed` must not appear in project code.

  Enums are accepted but lowered by cpp (see [NORM.md](NORM.md)): the constants
  become macro definitions — global, textual, `#define` semantics — so an enum
  constant's name must not be reused as any other identifier later in the file,
  and enum value expressions are limited to numbers, previously defined enum
  constants, unary `-`/`~`, parens, and `+ - *`.

## 4. Numeric constants

Handled by `isnumber()` / `getint()` in `lex.c`:

| Form | Example | Base |
|------|---------|------|
| Decimal | `123` | 10 |
| Hexadecimal | `0x1a`, `0XFF` | 16 |
| Octal | `0755` | 8 (leading `0`) |
| Explicit decimal | `0d99` | 10 (extension; a leading `0` otherwise means octal) |
| Binary | `0b1010`, `0B1010` | 2 (extension) |
| Character | `'a'`, `'\n'` | value of the char |

- A digit outside the base range is an error (`ER_C_BD` / `ER_C_CD`).
- Integers become `NUMBER` tokens. An `L`/`l` suffix makes the token
  `LNUMBER` instead — that suffix is the **only** thing that can make a
  constant long, because everything downstream sizes a constant by how big it
  is. (`5L` was a byte until `LNUMBER` was wired up, so passing one to a
  function put two bytes on the stack where the callee read four.)
- **Float literals are rejected.** `3.14`, `1.0e10`, `.5` and their `f`/`l`
  suffixes are all *scanned* — so the lexer stays in step with the source —
  and then raise `ER_C_CD` and yield 0. ccc has no floating point.
- A `.` is only treated as the start of a float when what follows is a digit,
  `e`/`E`, or a non-identifier character. That is what keeps `1.foo` a member
  access rather than a malformed number.

## 5. Character and string literals

- Character literal: `'c'` — a single (possibly escaped) character, value
  0–255.
- String literal: `"..."` — stored as a **counted string** (a 2-byte length
  prefix followed by the bytes) so embedded NULs are preserved.
- **Adjacent string literals are concatenated**, and the join happens *after*
  macro expansion (in `emit.c`), so two literals that only became adjacent
  when a macro expanded are joined too. That is most of them in this tree —
  pass2's rule table is built out of named fragments written side by side.
- Escape sequences (`getlit()` in `lex.c`), valid in both forms:

  | Escape | Value | Escape | Value |
  |--------|-------|--------|-------|
  | `\n` | 0x0a | `\\` | 0x5c |
  | `\r` | 0x0d | `\'` | 0x27 |
  | `\t` | 0x09 | `\"` | 0x22 |
  | `\b` | 0x08 | `\0`–`\7` | octal, ≤3 digits |
  | `\f` | 0x0c | `\xNN` | hexadecimal |
  | `\v` | 0x0b | `\BNN` | binary (extension) |
  | `\e` | 0x1b (extension) | `\DNN` | decimal (extension) |

  An invalid escape raises `ER_C_NX`.

## 6. Operators and punctuation

Recognized by the character tables in `lex.c`:

- **Single char:** `{ } , [ ] ( ) ; = . + - / * % & | ^ < > ! ~ ? :`
- **Doubled:** `++ -- || && == >> <<`
- **With `=` appended:** `+= -= *= /= %= &= |= ^= >= <= != >>= <<=`
- **Arrow:** `->`
- **Ellipsis:** `...`

## 7. `asm` blocks

The `asm` keyword introduces an inline-assembly block. The lexer captures the
block text verbatim and forwards it downstream as an `ASMSTR` payload (see
OUTPUT.md); braces are treated as parsing markers, and newlines inside the
block become statement separators.

---

## 8. Preprocessor directives

A `#` in **column 0** introduces a directive (`doCpp()`); a `#` elsewhere is an
ordinary token.

### 8.1 `#define`

```c
#define NAME              /* object-like, empty (or "1" via -D) */
#define NAME value        /* object-like */
#define NAME(a,b) a+b     /* function-like: '(' must touch NAME */
```

- **Function-like vs object-like** is decided by whether `(` *immediately*
  follows the name with no intervening whitespace. `#define BAR (x)` is
  object-like with value `(x)`.
- Up to `MAXPARMS` (16) parameters; more raises `ER_C_PC`. A call whose
  argument count does not match raises `ER_C_MA`.
- Body operators:
  - `#param` — stringify the argument.
  - `a##b` — token paste (the `##` is removed, tokens are joined).
- `\`-newline continues the body across lines.
- Argument parsing tracks parenthesis nesting; commas inside parentheses do not
  split arguments, and string/char literals are copied verbatim.

> **Limitation:** macro replacement text is expected to be short (typically a
> single token or small expression) — this is more restrictive than standard
> cpp.

### 8.2 `#undef`

```c
#undef NAME    /* removes the definition; silently succeeds if undefined */
```

### 8.3 `#include`

```c
#include <file.h>    /* system: search sysIncPath (-i), then -I paths */
#include "file.h"    /* user: current directory first, then -I paths */
```

Not found → fatal error.

### 8.4 Conditional compilation

```c
#if EXPR
#ifdef NAME
#ifndef NAME
#elif EXPR
#else
#endif
```

- Expression grammar (`readcppconst()`), evaluated in **ONELINE mode** so a
  newline ends the expression:
  - arithmetic `+ - * / %`
  - comparison `< > <= >= == !=`
  - logical `&& || !`
  - bitwise `& | ^ ~ << >>`
  - parentheses `( )`
  - `defined(NAME)` / `defined NAME` pseudo-function
- **Undefined identifiers evaluate to 0**, so `#if UNDEF` behaves as `#if 0`.
- Inactive branches are skipped: only nested conditional directives are
  tracked; `#define`/`#undef`/`#include` and all other tokens are discarded
  until the branch becomes active.
- Errors: `#elif`/`#else`/`#endif` without a matching `#if` → `ER_C_CU`;
  missing `#endif` at EOF → `ER_C_ME`.

---

## 9. Language restrictions (ccc dialect)

Because the compiler must self-host on the Z80, project source (and therefore
practical `cpp` input) must **not** use — see `../RESTRICTIONS.md`:

- Structure assignment (`struct a = b;`)
- Structure return (functions returning a struct by value)
- Auto aggregate initializers (`struct foo x = {...};`, `int a[] = {...};`)
- `const` qualifier (accepted lexically, then dropped)
- `signed` qualifier (use plain `int`/`char`)

Other known input limitations:

- Nested block comments are not supported (the first `*/` closes the comment).
- Macro replacement text is expected to be short (see §8.1).

---

## 10. Command line

```
cpp [options] <source.c>
```

| Option | Meaning |
|--------|---------|
| `-o <base>` | Output base name (writes `<base>.x` and `<base>.n`; `<base>.i` with `-p`) |
| `-I<dir>` | Add a user include directory (up to `MAX_INCLUDES`, 32) |
| `-i<dir>` | Set the system include directory |
| `-D<name>[=val]` | Define a macro (value `1` if no `=val`) |
| `-p` | Also emit a human-readable `<base>.i` (forks `xdump`) |
| `-E` | Preprocess and dump to stdout (forks `xdump`) |
| `-N` | Suppress line markers (`LINENO`/`NEWLINE`) in the `.x` stream |
| `-h` | Help |
| `-v <mask>` | Verbosity bitmask (DEBUG builds only) |

If `-o` is omitted, the output base is the source name with its extension
stripped.

`z80=1` is predefined, the way `zc3` predefines it. The headers guard
machine-specific shapes with `#if z80` — `jmp_buf`'s size, `cpm.h` wholesale —
and under a cpp that said nothing about its machine every one of those guards
failed shut.

<!-- vim: set tabstop=4 shiftwidth=4 noexpandtab: -->
