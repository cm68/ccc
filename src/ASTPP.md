# AST Pretty Printer

A standalone tool for formatting ccc compiler AST output in human-readable form.

> ## ⚠ Current status: astpp is behind the AST format
>
> `astpp` does not correctly decode a `.1` file produced by the current cc1. It
> reads the function header with the wrong field widths and desynchronises
> immediately, printing `(???: ...` for the whole body. Three things have moved
> under it (see [AST_FORMAT.md](AST_FORMAT.md)):
>
> | What | astpp reads | cc1 writes |
> |------|-------------|------------|
> | `AST_FUNC` frame size | 1 byte (`astpp.c:534`) | **2 bytes** |
> | `AST_FUNC` savebase | not read at all | **1 byte**, after frame size |
> | `AST_DECL` local offset | 1 byte (`astpp.c:263`, `:575`) | **2 bytes** (params are still 1) |
>
> A fourth difference is cosmetic but makes output hard to read: names in the
> AST are now `@<id>` markers, and astpp does not read cpp's `.n` sidecar, so it
> prints `_@2` where cc2 prints `_main`.
>
> Until that is fixed, use **`c1`'s own DEBUG trace** instead — pass2 annotates
> the assembly it writes with the statements and expressions it read:
>
> ```bash
> ccc -k -s prog.c        # keeps prog.1, prog.2, prog.s
> grep '^;' prog.s        # the AST, as pass2 understood it
> ```
>
> or read the bytes directly with `od -An -tx1 prog.1` against
> [AST_FORMAT.md](AST_FORMAT.md). The rest of this document describes what
> astpp is *meant* to do.

## Purpose

The AST pretty printer (`astpp`) converts the compact binary AST format into a
nicely formatted, indented, and annotated representation that's easier to read
and understand.

**Use cases:**
- **Debugging the parser**: Visually inspect the AST structure to verify correct
  parsing
- **Understanding code generation**: See exactly what the parser produces before
  code generation
- **Learning the AST format**: Study examples to understand the AST structure
- **Comparing ASTs**: Diff pretty-printed output to see changes between versions

## Installation

Built automatically with the compiler:

```bash
make            # Builds astpp along with cpp, cc1, cc2, and peep
make install    # Installs to root/bin/astpp
```

## Usage

```bash
astpp <base>.1
```

The AST file is **`<base>.1`**, not `<base>.ast`. cc1 writes two files: `.1`
holds the AST, `.2` holds assembly text for globals, strings, and static
initializers. Produce them with the driver's `-k`:

```bash
ccc -k -s prog.c     # keeps prog.x prog.n prog.1 prog.2 prog.s
astpp prog.1
```

astpp also reads stdin when given no filename, so it can sit in a pipeline.

The `ccc` driver has **no `-P` flag** and does not generate `.pp` files; that
was removed. `cc1` has no `-E` either — preprocessing is cpp's job, and cc1
takes three filename arguments (`c0 <.x> <.1> <.2>`).

The stage1 make targets do run astpp over every source, so `make stage1` in
`cpp/`, `pass1/`, or `pass2/` leaves a `stage1/<base>.pp` per file.

## Output Format

The pretty printer outputs:

1. **Function definitions** with parameters, return types, frame size, and
   register allocation
2. **Statement structure** with proper indentation showing nesting
3. **Expression trees** with operator names and type annotations

### Example Output

```
========================================
AST Pretty Printer Output (binary)
========================================

FUNCTION _main(argc:short@IY+4, argv:short@BC) -> short [frame=0]
  LOCALS: result:short@IY-2
{
  BLOCK 0 (2 stmts) {
    (ASSIGN:short (LOCALVAR:short -2) 42:short)
    RETURN (LOCALVAR:short -2)
  }
}

========================================
```

## Features

### Register Allocation Display

| Register | Name | Description |
|----------|------|-------------|
| `-` | REG_NONE | No register (stack only) |
| `B` | REG_B | B register |
| `C` | REG_C | C register |
| `BC` | REG_BC | BC register pair |
| `IX` | REG_IX | IX register |

Stack offsets are shown as `IY+N` or `IY-N`. `regName()` in `format.h` also
spells 5–8 (DE, HL, A, IY) and 16–21 (condition codes Z, NZ, C, NC, M, P), but
those are pass2's numbering and cannot appear in an AST.

### Operator Translation

`opName()` in `format.h` translates opcodes to readable names:

| Token | Pretty Name | Description |
|-------|-------------|-------------|
| 21 | CONST | Numeric constant |
| 20 | SYM | Symbol reference |
| 201 | DEREF | Dereference (memory read) |
| 80 | ASSIGN | Assignment |
| 40 | ADD | Addition |
| 41 | SUB | Subtraction |
| 36 | MUL | Multiplication |
| 43 | DIV | Division |
| 44 | MOD | Modulo |
| 47 | AND | Bitwise AND |
| 48 | OR | Bitwise OR |
| 49 | XOR | Bitwise XOR |
| 38 | NOT | Bitwise NOT |
| 46 | LSHIFT | Left shift |
| 45 | RSHIFT | Right shift |
| 220 | URSHIFT | Unsigned right shift |
| 63 | LT | Less than |
| 62 | LE | Less or equal |
| 60 | EQ | Equal |
| 61 | NE | Not equal |
| 53 | LAND | Logical AND |
| 54 | LOR | Logical OR |
| 34 | LNOT | Logical NOT |
| 9 | COMMA | Comma operator |
| 206 | NARROW | Narrow type conversion |
| 208 | SEXT | Sign extend |
| 207 | WIDEN | Widen type |
| 90 | TERN | Ternary conditional |
| 205 | CALL | Function call |
| 203 | NEG | Unary negation |
| 210 | PREINC | Pre-increment |
| 211 | POSTINC | Post-increment |
| 212 | PREDEC | Pre-decrement |
| 213 | POSTDEC | Post-decrement |
| 214 | BFEXT | Bitfield extract |
| 215 | BFSET | Bitfield set |
| 216 | REGVAR | Register variable |
| 217 | LOCALVAR | Local variable (stack) |

`opName()` still lists GT (65) and GE (64), but cc1 normalizes both to LT/LE
with the operands swapped, so neither reaches the AST.

### Compound Assignment Operators

| Token | Pretty Name |
|-------|-------------|
| 70 | ADDEQ |
| 71 | SUBEQ |
| 72 | MULEQ |
| 73 | DIVEQ |
| 74 | MODEQ |
| 75 | SHREQ |
| 76 | SHLEQ |
| 77 | ANDEQ |
| 78 | OREQ |
| 79 | XOREQ |

### Statement Types

Statement opcodes are **token values**, not ASCII letters:

| Opcode | Name | Description |
|--------|------|-------------|
| 221 | AST_FUNC | Function header |
| 222 | AST_BLOCK | Block statement |
| 224 | AST_DECL | Variable declaration |
| 225 | AST_EMPTY | Null expression |
| 147 | IF | If/else conditional |
| 146 | RETURN | Return statement |
| 150 | SWITCH | Switch statement |
| 151 | CASE | Case label |
| 155 | DEFAULT | Default label |
| 145 | GOTO | Goto statement |
| 112 | LABEL | Label definition |
| 157 | ASM | Inline assembly |
| 1 | SEMI | Empty statement |

An **expression statement has no opcode of its own** — astpp treats any
unrecognized opcode as the start of an expression.

Loop statements (WHILE, FOR, DO) and BREAK/CONTINUE are lowered to if/goto by
cpp and do not appear in the AST.

### Type Width Annotations

`widthName()` in `format.h` spells:

| Width | Name | Size | Type |
|-------|------|------|------|
| `b` | byte | 1 byte | signed char |
| `B` | ubyte | 1 byte | unsigned char |
| `s` | short | 2 bytes | short, int, **and every pointer** |
| `S` | ushort | 2 bytes | unsigned short/int |
| `l` | long | 4 bytes | long |
| `L` | ulong | 4 bytes | unsigned long |
| `v` | void | 0 bytes | void |

`widthName()` also spells `p` (ptr), `f` (float), and `d` (double), but cc1's
`typeSfx()` never emits them: ccc has no floating point, and pointers emit `s`.

## Implementation

The pretty printer is `astpp.c` in the `ccc/` directory (~720 lines), sharing
`format.h` with pass2 so operator and width names stay consistent. It:

1. **Reads the binary AST** using single-byte and multi-byte readers
2. **Handles length-prefixed names** (1 byte length + characters)
3. **Translates token values** to readable operator names via `format.h`
4. **Formats recursively** using indentation tracking
5. **Shows register allocation** for parameters and locals

It does *not* read the `.n` sidecar, so identifiers appear as their `@<id>`
markers rather than their spellings.

## Tips

### Reduce Verbosity

```bash
# Extract only function definitions
astpp prog.1 | grep -A 20 "FUNCTION"

# Find specific variable references
astpp prog.1 | grep "LOCALVAR"
```

### Compare ASTs

```bash
astpp old.1 > /tmp/old.txt
astpp new.1 > /tmp/new.txt
diff -u /tmp/old.txt /tmp/new.txt
```

## License

Same as the ccc compiler project.
