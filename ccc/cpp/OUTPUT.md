# CPP Output Language

This document defines the language `cpp` **produces**: the `.x` lexeme stream
consumed by pass1. For the language `cpp` accepts, see [INPUT.md](INPUT.md).

The output is *not* a byte-for-byte token image of the input. `cpp` emits a
**binary token stream** that encodes a **normalized subset of C** — the filter
pipeline rewrites several constructs so pass1 has less syntax to handle.

Two things therefore define the output language:

1. **The wire format** — how tokens are encoded as bytes (§1–§4).
2. **The normalized grammar** — which C constructs can appear, after the
   filters have rewritten the rest (§5).

The token values are defined in [`lexeme.h`](lexeme.h) and shared verbatim with
pass1. Emission lives in [`emit.c`](emit.c).

> **Note:** the stream is **raw little-endian binary**, not ASCII-hex. (Earlier
> `CPP.md` text describing a hex encoding is obsolete.)

---

## 1. Stream structure

A `.x` file is a flat sequence of tokens, in program order, terminated by a
single `E_O_F` (0x00) byte. Each token is either:

- a **simple token** — one byte whose value is the token code, or
- a **payload token** — a one-byte code followed by a fixed or
  length-prefixed payload.

All multi-byte integers are **little-endian**. There is no header and no
alignment/padding.

## 2. Token codes

From `lexeme.h`. Only the codes `cpp` can emit are listed; pass1/pass2 reserve
higher ranges (200+) for AST/synthetic nodes that never appear in a `.x`
stream.

### Delimiters (0–9)

| Code | Name | Char |
|-----:|------|------|
| 0 | `E_O_F` | (stream end) |
| 1 | `SEMI` | `;` |
| 2 | `BEGIN` | `{` |
| 3 | `END` | `}` |
| 4 | `LBRACK` | `[` |
| 5 | `RBRACK` | `]` |
| 6 | `LPAR` | `(` |
| 7 | `RPAR` | `)` |
| 8 | `COLON` | `:` |
| 9 | `COMMA` | `,` |

### Terminals with payload (20–25)

| Code | Name | Payload |
|-----:|------|---------|
| 20 | `SYM` | identifier (§3.1) |
| 21 | `NUMBER` | 4-byte LE integer (§3.2) |
| 22 | `STRING` | 2-byte LE length + bytes (§3.3) |
| 23 | `FNUMBER` | 4-byte LE IEEE-754 (§3.2) |

### Operators (30–92)

| Code | Name | | Code | Name | | Code | Name |
|-----:|------|-|-----:|------|-|-----:|------|
| 30 | `INCR` `++` | | 45 | `RSHIFT` `>>` | | 62 | `LE` `<=` |
| 31 | `DECR` `--` | | 46 | `LSHIFT` `<<` | | 63 | `LT` `<` |
| 34 | `BANG` `!` | | 47 | `AND` `&` | | 64 | `GE` `>=` |
| 35 | `AMPER` `&` | | 48 | `OR` `\|` | | 65 | `GT` `>` |
| 36 | `STAR` `*` | | 49 | `XOR` `^` | | 70 | `PLUSEQ` `+=` |
| 38 | `TWIDDLE` `~` | | 50 | `ARROW` `->` | | 71 | `SUBEQ` `-=` |
| 39 | `DOT` `.` | | 53 | `LAND` `&&` | | 72 | `MULTEQ` `*=` |
| 40 | `PLUS` `+` | | 54 | `LOR` `\|\|` | | 73 | `DIVEQ` `/=` |
| 41 | `MINUS` `-` | | 60 | `EQ` `==` | | 74 | `MODEQ` `%=` |
| 42 | `TIMES` `*` | | 61 | `NEQ` `!=` | | 75 | `RSHIFTEQ` `>>=` |
| 43 | `DIV` `/` | | | | | 76 | `LSHIFTEQ` `<<=` |
| 44 | `MOD` `%` | | | | | 77 | `ANDEQ` `&=` |
| | | | | | | 78 | `OREQ` `\|=` |
| | | | | | | 79 | `XOREQ` `^=` |
| | | | | | | 80 | `ASSIGN` `=` |
| 90 | `QUES` `?` | | 91 | `SIZEOF` | | 92 | `ELLIPSIS` `...` |

Note the lexer emits `STAR` (36) for `*` and `AND` (47) for `&`; `TIMES`/`AMPER`
are aliases used by later passes.

### Line tracking and misc (112–118)

| Code | Name | Payload |
|-----:|------|---------|
| 112 | `LABEL` | name (§3.1) |
| 116 | `LINENO` | 2-byte LE line + name (§4) |
| 117 | `NEWLINE` | none — "line += 1" (§4) |
| 118 | `ASMSTR` | 2-byte LE length + bytes (§3.3) |

### Keyword tokens (128–160)

Emitted as their single-byte code:

| Code | Name | | Code | Name | | Code | Name |
|-----:|------|-|-----:|------|-|-----:|------|
| 128 | `INT` | | 140 | `TYPEDEF` | | 150 | `SWITCH` |
| 129 | `CHAR` | | 141 | `AUTO` | | 151 | `CASE` |
| 130 | `FLOAT` | | 142 | `EXTERN` | | 152 | `BREAK`* |
| 131 | `DOUBLE` | | 143 | `STATIC` | | 153 | `CONTINUE`* |
| 132 | `STRUCT` | | 144 | `REGISTER` | | 154 | `DO`* |
| 133 | `SIGNED` | | 145 | `GOTO` | | 155 | `DEFAULT` |
| 134 | `LONG` | | 146 | `RETURN` | | 156 | `FOR`* |
| 135 | `UNSIGNED` | | 147 | `IF` | | 157 | `ASM` |
| 136 | `UNION` | | 148 | `WHILE`* | | 158 | `CONST`† |
| 137 | `SHORT` | | 149 | `ELSE` | | 159 | `VOLATILE`† |
| 138 | `VOID` | | | | | 160 | `SIZEOF_KW`‡ |
| 139 | `ENUM`* | | | | | | |

\* Rewritten away by the filters — see §5; `WHILE`/`FOR`/`DO`/`BREAK`/
`CONTINUE` do not survive to the output, and `ENUM` never does either:
filtenum lowers every enum to `unsigned char`, entering the constants
into the macro table so their uses emit as `NUMBER`.
† `CONST`/`VOLATILE` are **never emitted** (silently dropped in `emit.c`).
‡ `SIZEOF_KW` is normalized to `SIZEOF` (91) on emission.

## 3. Payload encodings

### 3.1 Name payloads — `SYM`, `LABEL`

```
<code> <len:1> <bytes...>
```

One length byte (0–255, clamped) followed by that many raw name bytes. No NUL
terminator. Used for identifiers (`SYM`) and statement labels (`LABEL`).

### 3.2 Fixed 4-byte payloads — `NUMBER`, `FNUMBER`

```
<code> <b0> <b1> <b2> <b3>      little-endian
```

- `NUMBER`: 32-bit integer value.
- `FNUMBER`: 32-bit IEEE-754 single-precision bit pattern.

### 3.3 Length-prefixed byte payloads — `STRING`, `ASMSTR`

```
<code> <len_lo> <len_hi> <bytes...>      length is 2-byte LE (0–65535, clamped)
```

- `STRING`: string-literal bytes; may contain embedded NULs.
- `ASMSTR`: verbatim inline-assembly text (from an `asm` block).

## 4. Line markers

Unless `-N` is given, `cpp` interleaves position information so pass1 can report
diagnostics against the original source:

- **`LINENO`** `<code> <line_lo> <line_hi> <namelen:1> <name...>` — an absolute
  line number plus source filename. Emitted at file start, on any file change,
  and on any non-`+1` line jump.
- **`NEWLINE`** — a single byte meaning "advance the current line by one".
  Emitted when the next token is exactly one line past the last.

With `-N` (`noLineMarkers`), neither `LINENO` nor `NEWLINE` is emitted.

## 5. Normalized grammar (what the filters guarantee)

Before emission, tokens pass through the pull-based filter pipeline
(`filtknr → filtdecl → filtbrace → filtctrl`, see [FILTERS.md](FILTERS.md)).
As a result the output stream obeys stronger invariants than arbitrary C —
pass1 can rely on all of these:

1. **No K&R function definitions.** Old-style parameter lists are rewritten to
   ANSI form; parameters with no declared type default to `int`.
   (`filtknr`)

2. **No initializers in local declarations.** A local `int x = 5;` becomes
   `int x; x = 5;`. Multiple declarators are split individually.
   (`filtdecl`, function scope only)

3. **All control bodies are braced.** `if`, `else`, `while`, `for`, and `do`
   bodies are wrapped in `{ }` if they were single statements.
   (`filtbrace`)

4. **No loops.** `while`, `for`, and `do`/`while` are lowered to `if` +
   `goto` + `LABEL` sequences; `switch` passes through but gains a trailing
   break label. `break`/`continue` become `goto`s. Thus `WHILE`, `FOR`, `DO`,
   `BREAK`, and `CONTINUE` tokens never appear in the output. (`filtctrl`)

   Lowering shapes (labels are `__<P><n><S>`; P = W/F/D/S, S = T/B/C; each
   `LABEL` is emitted with a following `;` so it always precedes a statement).
   The lowered sequence is spliced in place — no extra `{ }` wrapper is added
   around it:

   ```
   while (c) body   ->  __WnT: ; if (!(c)) { goto __WnB; } body goto __WnT; __WnB: ;
   for(i;c;u) body  ->  i; __FnT: ; if (!(c)) { goto __FnB; } body __FnC: ; u; goto __FnT; __FnB: ;
   do body while(c) ->  __DnT: ; body __DnC: ; if (c) { goto __DnT; } __DnB: ;
   switch (x) {...} ->  switch (x) {...} __SnB: ;
   ```

5. **No `const` / `volatile`** tokens (dropped in `emit.c`), and **`sizeof`**
   is always the single `SIZEOF` token.

6. **Balanced braces.** `emitChkBraces()` aborts with a diagnostic if `{`/`}`
   are unbalanced at EOF.

## 6. Example

Source:

```c
int x = 42;
```

Emitted `.x` stream (line markers omitted for clarity):

```
INT                 128
SYM "x"             20 01 'x'
ASSIGN              80
NUMBER 42           21 2a 00 00 00
SEMI                01
E_O_F               00
```

A larger example, showing loop lowering, is in `test/while.c` and its expected
`xdump` rendering `test/while.expected`.

## 7. Human-readable forms

The `.x` stream is binary. Two tools render it as text:

- **`xdump`** decodes a `.x` file back into token text (used by `cpp -E` and
  `cpp -p`, and by the test harness).
- **`cpp -p`** additionally writes a `<base>.i` file (via `xdump`).

These renderings are for humans/tests only; pass1 always consumes the binary
`.x`.

## 8. Output diversion (implementation note)

Loop lowering must emit an opening brace, label, and condition *before* the loop
body that the lexer has already started producing. `io.c` provides an
`outbufWrite`/output-buffer stack so `emit.c` can buffer body tokens and replay
them in the correct order, spilling to a temp file past `TBSIZE` (512 bytes).
This is an emission mechanism, not part of the output language; details are in
`CPP.md`.

<!-- vim: set tabstop=4 shiftwidth=4 noexpandtab: -->
