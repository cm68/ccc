# CCC AST Binary Format

This document describes the binary AST format produced by cc1 (pass1) and
consumed by cc2 (pass2).

## Overview

The AST is a **compact binary format** using single-byte opcodes and
little-endian multi-byte values. Token values from
[`cpp/lexeme.h`](cpp/lexeme.h) are used directly as opcodes — there is no
separate AST opcode space, and the same header is shared by cpp, pass1, and
pass2. Names are length-prefixed with a single byte.

**Preprocessing happens in cpp, not cc1.** By the time cc1 reads its input, all
macros are expanded, conditionals resolved, includes processed, typedefs
dissolved, enums lowered, and loops lowered to labeled if/goto sequences. See
[cpp/OUTPUT.md](cpp/OUTPUT.md) for what cc1 is guaranteed to receive.

## Pass 1 writes two files

```
cc1  <base>.x  <base>.1  <base>.2
cc2  <base>.1  <base>.2  <base>.s
```

- **`<base>.1`** — the AST described by this document. Functions and their
  bodies, and nothing else.
- **`<base>.2`** — **assembly text**, not AST: global and static variable
  storage, string literal data, and static initializers, streamed directly as
  `.bss`/`.text`/`.ds`/`.db` lines. cc1 emits these while parsing rather than
  building tree nodes for them; cc2 copies the file through into `<base>.s`.

So a global `int g;` never appears in the AST at all — it shows up in `.2` as:

```
	.bss
_g::
	.ds 2
```

## Names are ids, resolved from the `.n` sidecar

Identifiers reach cc1 from cpp as 2-byte ids (`SYMID`/`LABELID`), and cc1 does
not have their spellings — it never needs them. A name emitted into the AST is
therefore written as the marker text `@<id>`, embedded in the ordinary
length-prefixed name string, with any prefix cc1 adds around it:

```
_@2     the global/extern form of id 2
S@7     a static
@5      a local's declaration name
```

cc2 reads cpp's `<base>.n` sidecar (see [cpp/OUTPUT.md](cpp/OUTPUT.md) §4.2) to
turn `@2` back into `main` when it writes assembly, and the driver uses the same
sidecar to translate `@{id}` markers in the passes' diagnostics. Nothing in the
AST itself carries a spelling.

## Encoding Primitives

### Numbers
- **1 byte**: `emit1(b)` - single unsigned byte
- **2 bytes**: `emit2(w)` - little-endian 16-bit
- **4 bytes**: `emit4(l)` - little-endian 32-bit signed (two's complement)

### Names
```
<1-byte-len><ascii-chars>
```
Emitted via `emitS(name)` - length byte followed by raw characters, no NUL.
Example: `\x03_@2` = `_@2`.

### Type/Size Suffixes

A single ASCII character after most opcodes, giving the operand/result type.
`typeSfx()` in `pass1/outh.c` produces exactly these:

| Char | Meaning |
|------|---------|
| `b` | signed byte |
| `B` | unsigned byte |
| `s` | signed short/int (2 bytes) |
| `S` | unsigned short/int |
| `l` | signed long (4 bytes) |
| `L` | unsigned long |
| `v` | void (size 0) |

Anything address-valued — pointer, array, or function — emits `s`, whatever its
element size says: the value is a 16-bit address. There are **no `f`/`d`
suffixes**; ccc has no floating point. (`format.h`'s `widthName()` still spells
`p`, `f`, and `d` for historical dumps, but `typeSfx` never emits them.)

---

## Opcode values

Statement and expression opcodes are the **token values from `lexeme.h`**, not
ASCII letters. The structural nodes that have no lexeme live at 221–225:

| Value | Name | Role |
|------:|------|------|
| 221 | `AST_FUNC` | function header |
| 222 | `AST_BLOCK` | block / compound statement |
| 223 | `AST_GLOBAL` | *reserved, unused* — globals go to `.2` |
| 224 | `AST_DECL` | parameter / local declaration |
| 225 | `AST_EMPTY` | null or absent expression |

Statement opcodes reused from the lexeme set:

| Value | Name | | Value | Name |
|------:|------|-|------:|------|
| 1 | `SEMI` (empty statement) | | 150 | `SWITCH` |
| 112 | `LABEL` | | 151 | `CASE` |
| 145 | `GOTO` | | 155 | `DEFAULT` |
| 146 | `RETURN` | | 157 | `ASM` |
| 147 | `IF` | | | |

Expression opcodes reused from the lexeme set:

| Value | Name | | Value | Name | | Value | Name |
|------:|------|-|------:|------|-|------:|------|
| 9 | `COMMA` | | 44 | `MOD` `%` | | 70–79 | compound assign |
| 20 | `SYM` | | 45 | `RSHIFT` `>>` | | 80 | `ASSIGN` `=` |
| 21 | `NUMBER` (constant) | | 46 | `LSHIFT` `<<` | | 90 | `QUES` `?:` |
| 30 | `INCR` (see below) | | 47 | `AND` `&` | | | |
| 31 | `DECR` (see below) | | 48 | `OR` `\|` | | | |
| 34 | `BANG` `!` | | 49 | `XOR` `^` | | | |
| 36 | `STAR` `*` (multiply) | | 53 | `LAND` `&&` | | | |
| 38 | `TWIDDLE` `~` | | 54 | `LOR` `\|\|` | | | |
| 40 | `PLUS` `+` | | 60 | `EQ` `==` | | | |
| 41 | `MINUS` `-` | | 61 | `NEQ` `!=` | | | |
| 43 | `DIV` `/` | | 62 | `LE` `<=` | | | |
| | | | 63 | `LT` `<` | | | |

Internal expression opcodes (200+):

| Value | Name | Description |
|------:|------|-------------|
| 201 | `DEREF` | memory dereference |
| 203 | `NEG` | unary minus |
| 204 | `NOT` | (emitted as a unary; see `outast.c`) |
| 205 | `CALL` | function call |
| 206 | `NARROW` | truncate to a smaller type |
| 207 | `WIDEN` | zero-extend |
| 208 | `SEXT` | sign-extend |
| 209 | `INITLIST` | initializer list — **vestigial**, see below |
| 210 | `PREINC` | `++x` |
| 211 | `POSTINC` | `x++` |
| 212 | `PREDEC` | `--x` |
| 213 | `POSTDEC` | `x--` |
| 214 | `BFEXTRACT` | bitfield extract |
| 215 | `BFASSIGN` | bitfield assign |
| 216 | `REGVAR` | register variable |
| 217 | `LOCALVAR` | local variable (IY-indexed) |
| 220 | `URSHIFT` | unsigned right shift |

`INCR`/`DECR` are the *parser's* tokens; `outast.c` always rewrites them to one
of `PREINC`/`POSTINC`/`PREDEC`/`POSTDEC` before emitting, so 30 and 31 never
appear in a `.1` file.

---

## Top-Level: Function Definition

```
AST_FUNC <rettype> <name> <param_count:1> <local_count:1> <frm_size:2> <savebase:1>
         <params...> <locals...> <body>
```

- `AST_FUNC` - byte 221 (0xDD)
- `rettype` - type suffix, or `v` if the function has no declared return type
- `name` - length-prefixed, `_@<id>` for externs, `S<n>` for statics
- `param_count` - 1 byte
- `local_count` - 1 byte, locals hoisted from all blocks
- `frm_size` - **2 bytes LE**, frame size in bytes for stack locals
- `savebase` - 1 byte, scalar area size. The callee-save slots sit at
  `(iy-savebase-2)` and `(iy-savebase-4)`, with arrays below them.
- `body` - always an `AST_BLOCK` statement

**Parameter declaration**: `AST_DECL <type> <name> <reg:1> <off:1>`
**Local declaration**:     `AST_DECL <type> <name> <reg:1> <off:2>`

The offset widths **differ**: a parameter's frame offset is one byte, a local's
is two. `reg` is a register number (see Register Allocation below); a
register-allocated variable has no meaningful offset.

A local's `name` reflects how it will be addressed rather than what it was
called: `S<n>` for a static local, `L<n>` for one renamed to avoid shadowing,
and `@<id>` otherwise.

Worked example — `short main(int a, char *b) { short c; ... }`:

```
dd                    AST_FUNC
73                    's' (returns short)
03 5f 40 32           name, len 3: "_@2"
02                    2 params
01                    1 local
00 00                 frame size 0 (2 bytes)
00                    savebase 0
e0 73 02 40 33 00 04  AST_DECL 's' "@3" reg=0 off=4      (param a)
e0 73 02 40 34 00 06  AST_DECL 's' "@4" reg=0 off=6      (param b)
e0 73 02 40 35 03 00 00   AST_DECL 's' "@5" reg=3 off=0  (local c, in BC)
de 00 03              AST_BLOCK 0 3
```

---

## Statements

### Block
```
AST_BLOCK 00 <stmt_count:1> <stmts...>
```
The `00` is always zero — locals are hoisted to the function prolog.

### If Statement
```
IF <nlabels:1> <cond> <then> <has_else:1> [<else>]
```
- `nlabels` - count of intermediate labels needed for `||`/`&&`
  short-circuiting, computed by `cntCondLbls()`
- `has_else` comes **after** the then-branch, not before

### Expression Statement
```
<expr>
```
**There is no wrapper opcode.** An expression statement is just the expression,
and a reader distinguishes it by the opcode not being one of the statement
opcodes. (`stmtname()` in pass2 calls this case `EXPR`.)

### Return Statement
```
RETURN <has_value:1> [<expr>]
```
The value is converted to the declared return type before emission, so the
`SEXT`/`WIDEN` is in the tree rather than left for pass2 to guess.

### Label / Goto
```
LABEL <name>
GOTO  <name>
```
These carry the labels cpp's loop lowering generated (`__W1T`, `__F2B`, …) as
well as source labels.

### Switch / Case / Default
```
SWITCH <has_label:1> <case_count:1> <expr> <cases...>
CASE    <stmt_count:1> <value_expr> <stmts...>
DEFAULT <stmt_count:1> <stmts...>
```
`has_label` is always 0 — cpp lowered `break` to `goto` and appended the
`__S<n>B` label itself. A case's value is an ordinary constant-folded
expression in the stream, not a field in a table.

### Inline Assembly
```
ASM <len:2> <text>
```
Inline asm **inside a function body** rides in the AST, because it must stay in
place relative to the generated code; pass2 copies the text through. A
**global** asm block does not — it goes straight to `.2`.

### Empty Statement
```
SEMI
```
Byte 0x01.

---

## Expressions

Prefix notation: opcode, type suffix, then operands.

### Constant
```
NUMBER <type> <value:4>
```
Byte 21 (0x15), type suffix, 4-byte little-endian value. Example:
`15 73 2a 00 00 00` = 42 as a short.

### Symbol Reference (global / extern / static / string literal)
```
SYM <name>
```
Byte 20 (0x14). **No type suffix.** The name is `_@<id>` for globals and
externs, `S<n>`/`L<n>` for statics, and the `strN` label for a string literal
whose data was already streamed to `.2`.

### Local Variable (stack)
```
LOCALVAR <type> <offset:2>
```
Byte 217 (0xD9). The offset is **2 bytes** LE, IY-relative — positive for
parameters, negative for locals.

### Register Variable
```
REGVAR <type> <reg:1>
```
Byte 216 (0xD8).

### Null / Absent Expression
```
AST_EMPTY
```
Byte 225 (0xE1).

### Unary Operators
```
<op> <type> <operand>
```
`DEREF`, `BANG`, `TWIDDLE`, `NEG`, `NOT`, `NARROW`, `WIDEN`, `SEXT`.

`NEG` and `NOT` promote their operand to the operator's width first — they are
done at the promoted width, so the operand has to get there too.

### Binary Operators
```
<op> <type> <left> <right>
```

Several normalizations are guaranteed, so a reader never sees the other form:

- **`GT` and `GE` never appear.** They are rewritten to `LT`/`LE` with the
  operands swapped. Both still get the promotion the comparison would have had.
- **Operands are converted in the tree.** A narrow operand is wrapped in
  `WIDEN` (unsigned) or `SEXT` (signed) to reach the operator's width; a
  constant just gets the wider type. An operand *wider* than the operator works
  at is demoted where that cannot change the answer.
- **`&&` and `||` do not converge.** Each side is tested against zero
  separately, so neither is converted and the node's own type is `uchar`.
- **A shift count is promoted on its own**, not to the width of the value being
  shifted.
- **Byte-valued equality stays a byte.** `EQ`/`NEQ` between two operands that
  provably fit in an unsigned byte are emitted at byte width.

### Assignment
```
ASSIGN <type> <lvalue> <rvalue>
```
Byte 80. Compound assignments (70–79) have the same shape. The left side is a
location and is never widened; the right side is converted to the target type,
and demoted first where only the low bytes were ever observable.

### Increment / Decrement
```
<op> <type> <target> <amount:2>
```
`op` is `PREINC` (210), `POSTINC` (211), `PREDEC` (212), or `POSTDEC` (213).
`amount` is 1 for scalars and the pointee size for pointers.

### Function Call
```
CALL <rettype> <argc:1> <func> <args...>
```
Byte 205. Arguments are already converted to the types the prototype declares.

### Ternary
```
QUES <type> <cond> <then> <else>
```
Byte 90. The parser's `COLON` node is flattened away — the two arms are direct
children of `QUES`.

### Comma
```
COMMA <type> <left> <right>
```
Byte 9. Emitted by the parser and also synthesized by `outast.c` to rewrite
`*++p` as `(++p, *p)`.

### Bitfields
```
BFEXTRACT <offset:1> <width:1> <addr>
BFASSIGN  <offset:1> <width:1> <addr> <value>
```
Bytes 214/215. Note these take **no type suffix** — the bit offset and width
follow the opcode directly.

---

## Initializers (vestigial)

`outast.c` can emit an `INITLIST` as

```
BEGIN  <count:1> <items...> END        aggregate
LBRACK <width> <count:1> <items...> RBRACK   array
```

and pass2 reads both, but **pass1 never constructs an `INITLIST` node**.
Global and static initializers stream directly to `.2` as assembly (`asmDb`,
`asmDbStr`, `asmDs`), and auto aggregate initializers are a documented language
restriction — see [RESTRICTIONS.md](RESTRICTIONS.md). This path is dead until
something builds those nodes again.

---

## What's NOT in the AST

Emitted as assembly into `<base>.2`, not into the AST:

1. **Global and static variable storage** — `.bss` / `.ds`
2. **String literal data** — emitted in phase 1 under a `strN` label; the AST
   references it with a `SYM`
3. **Static and global initializers** — streamed as `.db` while parsing
4. **Global (file-scope) `asm` blocks**

And absent because cpp already removed them: loops, `break`/`continue`,
typedefs, enums, `const`/`volatile`, local declaration initializers.

---

## Register Allocation

Register allocation is computed by pass1 (`regalloc.c`) from usage analysis and
communicated to pass2 via the `reg` field of each `AST_DECL`, and via `REGVAR`
nodes in expressions.

### Register Values
| Value | Register | Description |
|-------|----------|-------------|
| 0 | - | No register (on stack) |
| 1 | B | B register (byte) |
| 2 | C | C register (byte) |
| 3 | BC | BC register pair (word) |
| 4 | IX | IX index register (struct pointer) |

(pass2's `pass2.h` continues the numbering — 5 DE, 6 HL, 7 A, 8 IY — but only
0–4 can appear in an AST.)

### Allocation Strategy

Variables declared `register` are honoured first, then:

1. **IX** goes to the pointer with the highest `agg_refs`, and only if the
   pointer is actually used for field access — IX indexing is what makes
   `(ix+n)` addressing pay.
2. **BC** goes to the word variable with the highest reference count. Word
   before byte is the measured answer, not an accident of ordering.
3. **B/C** go to frequently referenced byte variables, if BC was not taken as a
   pair.

Never allocated: a variable whose address is taken, an aggregate or array (it
*is* its storage), a static local (it must survive calls, and a register does
not), and — if any parameter has its address taken — any parameter.

---

## Notes

1. **Loop lowering** happens in cpp. Labels are `__<P><n><S>` with P in
   `W`/`F`/`D`/`S` and S in `T`/`B`/`C`; see [cpp/NORM.md](cpp/NORM.md).

2. **Name mangling**:
   - globals and externs: `_` prefix, then the `@<id>` marker
   - statics: `S<n>`
   - shadow-renamed locals: `L<n>`
   - locals in expressions carry no name at all — `LOCALVAR`/`REGVAR`

3. **Width annotations**: most opcodes carry a type suffix byte. Lowercase
   (`b`,`s`,`l`) is signed, uppercase (`B`,`S`,`L`) unsigned. The exceptions
   that take **no** suffix are `SYM`, `BFEXTRACT`, and `BFASSIGN`.
