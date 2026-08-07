# CPP Output Language

This document defines the language `cpp` **produces**: the `.x` lexeme stream
consumed by pass1. For the language `cpp` accepts, see [INPUT.md](INPUT.md).

The output is *not* a byte-for-byte token image of the input. `cpp` emits a
**binary token stream** that encodes a **normalized subset of C** — the
normalizer rewrites several constructs so pass1 has less syntax to handle.

Three things therefore define the output language:

1. **The wire format** — how tokens are encoded as bytes (§1–§3).
2. **The sidecar and line markers** — how names and positions travel (§4).
3. **The normalized grammar** — which C constructs can appear, after the
   normalizer has rewritten the rest (§5).

The token values are defined in [`lexeme.h`](lexeme.h) and shared verbatim with
pass1. Emission lives in [`emit.c`](emit.c); the normalizer that decides what is
emitted is [`norm.c`](norm.c), described in [NORM.md](NORM.md).

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

### Terminals with payload (20–27)

| Code | Name | Payload | Emitted? |
|-----:|------|---------|----------|
| 20 | `SYM` | identifier (§3.1) | no — see `SYMID` |
| 21 | `NUMBER` | 4-byte LE integer (§3.2) | yes |
| 22 | `STRING` | 2-byte LE length + bytes (§3.3) | yes |
| 23 | `INUMBER` | 4-byte LE integer (§3.2) | yes |
| 25 | `LNUMBER` | 4-byte LE integer (§3.2) | yes |
| 26 | `SYMID` | 2-byte LE id (§3.4) | yes |
| 27 | `LABELID` | 2-byte LE id (§3.4) | yes |

- **`NUMBER`** is a spelled integer literal; pass1 types it by magnitude.
- **`INUMBER`** is a number `cpp` *folded* from a construct that C types as
  `int` — `sizeof`, today — so pass1 must type it `int` too, not by magnitude.
  Same 4-byte record. Code 23 was `FNUMBER` before floating point was removed.
- **`LNUMBER`** is a literal the source spelled with an `L` suffix. That suffix
  is the only thing that can make a constant long: everything downstream sizes
  a constant by how big it is, so `5L` was a byte and passing one to a function
  put two bytes on the stack where the callee read four.
- **`SYMID`/`LABELID`** replace `SYM`/`LABEL` in every stream. `cpp` never
  emits the name forms; the spellings live in the `.n` sidecar (§4.2). This is
  the only format — there is no flag.

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

| Code | Name | Payload | Emitted? |
|-----:|------|---------|----------|
| 112 | `LABEL` | name (§3.1) | no — see `LABELID` |
| 116 | `LINENO` | 2-byte LE line + name (§4.1) | yes |
| 117 | `NEWLINE` | none — "line += 1" (§4.1) | yes |
| 118 | `ASMSTR` | 2-byte LE length + bytes (§3.3) | yes |

### Keyword tokens (128–160)

Emitted as their single-byte code:

| Code | Name | | Code | Name | | Code | Name |
|-----:|------|-|-----:|------|-|-----:|------|
| 128 | `INT` | | 140 | `TYPEDEF`* | | 150 | `SWITCH` |
| 129 | `CHAR` | | 141 | `AUTO` | | 151 | `CASE` |
| 130 | *(vacant)*§ | | 142 | `EXTERN` | | 152 | `BREAK`* |
| 131 | *(vacant)*§ | | 143 | `STATIC` | | 153 | `CONTINUE`* |
| 132 | `STRUCT` | | 144 | `REGISTER` | | 154 | `DO`* |
| 133 | `SIGNED` | | 145 | `GOTO` | | 155 | `DEFAULT` |
| 134 | `LONG` | | 146 | `RETURN` | | 156 | `FOR`* |
| 135 | `UNSIGNED` | | 147 | `IF` | | 157 | `ASM` |
| 136 | `UNION` | | 148 | `WHILE`* | | 158 | `CONST`† |
| 137 | `SHORT` | | 149 | `ELSE` | | 159 | `VOLATILE`† |
| 138 | `VOID` | | | | | 160 | `SIZEOF_KW`‡ |
| 139 | `ENUM`* | | | | | | |

\* Rewritten away by the normalizer — see §5. `WHILE`/`FOR`/`DO`/`BREAK`/
`CONTINUE` do not survive to the output. Neither do `ENUM` or `TYPEDEF`: the
source layer (`tdsrc.c`) lowers every enum to `unsigned char`, entering the
constants into the macro table so their uses emit as `NUMBER`, and dissolves
each typedef by composing the use-site declarator into the typedef's hole.
† `CONST`/`VOLATILE` are **never emitted** (silently dropped in `emit.c`).
‡ `SIZEOF_KW` is normalized to `SIZEOF` (91) on emission.
§ 130 and 131 held `FLOAT` and `DOUBLE`. ccc has no floating point, and
`float`/`double` are deliberately not reserved words so a program can typedef
them. The numbers stay **vacant rather than reused**: `token_props` is a
positional table and `.x` streams carry these codes, so renumbering would
silently reinterpret both.

## 3. Payload encodings

### 3.1 Name payloads — `SYM`, `LABEL`

```
<code> <len:1> <bytes...>
```

One length byte (0–255, clamped) followed by that many raw name bytes. No NUL
terminator. **`cpp` does not emit this form** — it is the shape pass1 knows for
`SYM`/`LABEL`, kept here because the token codes are still reserved. What `cpp`
actually writes is the id form, §3.4.

### 3.2 Fixed 4-byte payloads — `NUMBER`, `INUMBER`, `LNUMBER`

```
<code> <b0> <b1> <b2> <b3>      little-endian
```

All three carry a 32-bit integer value; they differ only in how pass1 must
type it (see §2). There is no floating-point payload.

### 3.3 Length-prefixed byte payloads — `STRING`, `ASMSTR`

```
<code> <len_lo> <len_hi> <bytes...>      length is 2-byte LE
```

- `STRING`: string-literal bytes; may contain embedded NULs.
- `ASMSTR`: verbatim inline-assembly text (from an `asm` block).

Two bounds worth knowing:

- Lengths are **clamped to 32767**, not 65535. The field is 16 bits, but 65535
  is −1 in a 16-bit `int`, so the "too long" comparison fired for *every*
  string under the Z80 compilers. 32767 is representable everywhere.
- An `ASMSTR` record is **never more than 255 bytes**. pass1 copies every
  counted record with a byte counter, and asm text was the one record that
  could outrun it. Longer blocks are split at line boundaries into successive
  `ASMSTR` records — each becomes its own asm statement downstream, and since a
  slice ends on a newline the assembly reads back identically. The length field
  is still two bytes; only the guarantee is new.

Adjacent string literals are **joined in `emit.c`**, not in the lexer: the
lexer works on characters and cannot see two literals that only became adjacent
when a macro expanded. A `STRING` is therefore held back until the next token
arrives — another `STRING` extends it, anything else releases it.

### 3.4 Id payloads — `SYMID`, `LABELID`

```
<code> <id_lo> <id_hi>       2-byte LE id, 1-based; 0 means "no name"
```

The spelling is not in the stream. Ids are minted at **first emission**, not
first sight, so the sidecar holds only names the stream actually uses. See
§4.2 for the sidecar that maps them back.

## 4. Names and positions

### 4.1 Line markers

Unless `-N` is given, `cpp` interleaves position information so pass1 can report
diagnostics against the original source:

- **`LINENO`** `<code> <line_lo> <line_hi> <namelen:1> <name...>` — an absolute
  line number plus source filename. Emitted at file start, on any file change,
  and on any non-`+1` line jump.
- **`NEWLINE`** — a single byte meaning "advance the current line by one".
  Emitted when the next token is exactly one line past the last.

With `-N` (`noLineMarkers`), neither `LINENO` nor `NEWLINE` is emitted.

Line markers derive from **per-token line stamps** recorded at scan time, not
from the emitter's own position. This is what lets the normalizer synthesize
tokens (braces, labels, gotos) and still stamp them correctly — a synthesized
token carries the lexer's position at the moment it is made, which is why the
walker synthesizes at fixed stream offsets. See NORM.md, "Output-byte
discipline".

### 4.2 The `.n` sidecar

Written unconditionally alongside `<base>.x`, as `<base>.n`. It maps the ids in
`SYMID`/`LABELID` back to spellings:

```
2 bytes            count N, little-endian
N * 2 bytes        offset of name i (1-based) from file start, little-endian
(N+7)/8 bytes      score bitmap: bit i-1 set = id i was emitted exactly once
names              NUL-terminated, in pool-walk order (NOT id order)
```

Two seeks fetch any name; nothing has to hold the file. The *offset table* is
in id order — readers index it — but the names behind it sit in whatever order
the intern-pool walk visited them, each offset seeked into its slot.

The **score bitmap** is a real signal, not bookkeeping: a name the stream
mentions exactly once is mentioned only where it is declared, so nothing refers
to it and pass1 need not remember it at all. That is most of what pass1 holds.
Putting the scores at an offset the header alone gives — `2 + 2N` — is what
lets pass1 reach them without seeking to the end of the file, which the Z80
`lseek` is no good at.

Ids are 14 bits (`IDMASK` 0x3fff); the top two bits are the emit-count score in
memory and do not reach the file. Overflowing the id space is fatal, not a
wrapped id.

**Consumers:** pass1 never looks at the sidecar — its lookups are 16-bit
compares, which on the Z80 is also the difference between walking a 200-name
chain with `strcmp` and walking it with `sbc`. pass2 reads it to spell symbols
in assembly, and the driver uses it to translate `@{id}` markers in the passes'
diagnostics.

## 5. Normalized grammar (what the normalizer guarantees)

Before emission, tokens pass through the source layer and the recursive-descent
walker (`tdsrc.c` → `norm.c`, see [NORM.md](NORM.md)). As a result the output
stream obeys stronger invariants than arbitrary C — pass1 can rely on all of
these:

1. **No K&R function definitions.** Old-style parameter lists are rewritten to
   ANSI form; parameters with no declared type default to `int`, and an
   implicit return type is synthesized where pass1 requires one. (`knr.c`)

2. **No typedefs and no enums.** Every typedef is dissolved into the type it
   names, and every enum becomes `unsigned char` with its constants entered in
   the macro table (so their uses emit as `NUMBER`). `TYPEDEF` and `ENUM`
   tokens never appear. (`tdsrc.c`)

3. **A declaration is recognisable from its leading token**, which follows from
   (2): pass1 parses without a symbol table.

4. **No initializers in local declarations.** A local `int x = 5, y = f();`
   becomes `int x, y; x = 5; y = f();` — the declarator list first, then the
   assignments in order. **Statics and arrays keep theirs inline.**
   (function scope only)

5. **All control bodies are braced.** `if`, `else`, and loop bodies are wrapped
   in `{ }` if they were single statements. An if-body's closing `}` is
   deferred until the following token has shown whether an `else` is there.

6. **No loops.** `while`, `for`, and `do`/`while` are lowered to `if` +
   `goto` + `LABEL` sequences; `switch` passes through but gains a trailing
   break label. `break`/`continue` become `goto`s to the innermost matching
   target. Thus `WHILE`, `FOR`, `DO`, `BREAK`, and `CONTINUE` tokens never
   appear in the output.

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

   An **empty clause emits nothing**: `for(;;)` and `while()` produce no entry
   test, and a `for` with no init or no increment omits that fragment.
   `__DnC` precedes the do test so `continue` re-tests it, as C requires.

7. **No `const` / `volatile`** tokens (dropped in `emit.c`), and **`sizeof`**
   is always the single `SIZEOF` token — usually already folded to an
   `INUMBER` by `cfold.c`, and passed through only when the size registries
   cannot price it.

8. **Balanced braces.** `emitChkBraces()` aborts with a diagnostic if `{`/`}`
   are unbalanced at EOF.

## 6. Example

Source:

```c
int x = 42;
```

Emitted `.x` stream (line markers omitted for clarity):

```
INT                 128            80
SYMID #1            26 + 2-byte id 1a 01 00      ("x" lives in <base>.n)
ASSIGN              80             50
NUMBER 42           21 + 4-byte LE 15 2a 00 00 00
SEMI                1              01
E_O_F               0              00
```

A larger example, showing loop lowering, is in `test/while.c` and its expected
`xdump` rendering `test/while.expected`. Raw bytes: `od -An -tx1 <base>.x`.

## 7. Human-readable forms

The `.x` stream is binary. Two tools render it as text:

- **`xdump`** decodes a `.x` file back into token text (used by `cpp -E` and
  `cpp -p`, and by the test harness). It reads the `.n` sidecar to spell
  `SYMID`/`LABELID` tokens.
- **`cpp -p`** additionally writes a `<base>.i` file (via `xdump`).

These renderings are for humans/tests only; pass1 always consumes the binary
`.x`.

## 8. Validators

`validate_x.py` and `validate_i.py` are the executable form of this document —
`make langtest` runs every cpp source through `cpp` and checks the resulting
`.x` against the wire format and the normalized grammar above, and the `.i`
against its rendering. When this document and the validators disagree, the
validators are what the build enforces.

## 9. Emission is direct (implementation note)

`emit.c` writes straight to the `.x` file descriptor via `outbufWrite()` in
`io.c`. An output-buffer stack (`outbufPush`/`outbufPop`/replay, spilling to a
temp file) once lived there so loop lowering could emit a label and condition
*before* the body tokens the lexer had already produced — but nothing ever
pushed onto it. All reordering happens above emit, in the walker, which is why
the walker's synthesis points are fixed to specific stream offsets (§4.1).
The stack has been removed.

<!-- vim: set tabstop=4 shiftwidth=4 noexpandtab: -->
