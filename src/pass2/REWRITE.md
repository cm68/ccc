# Expression Tree Rewriter

Table-driven bottom-up expression tree transformation with Sethi-Ullman
register allocation.

## Overview

The rewriter transforms expression trees into Z80 assembly through:
1. **Labeling** - bottom-up Sethi-Ullman pass counts registers needed
2. **Assignment** - top-down pass assigns target registers (HL/DE)
3. **Rewriting** - depth-first fixed-point rule application with code emission

## Rule Table

```c
struct rule {
    char *asmtpl;          /* asm template ($L/$R/... interpolation) */
    unsigned char op;      /* pattern letter of the root */
    unsigned char lop;     /* left operand, 0 if the root has no children */
    unsigned char rop;     /* right operand, 0 if unary */
    unsigned char subop;   /* the one grandchild pattern */
    unsigned char sfx;     /* width | dest<<3 | left child's width<<5 */
    unsigned char rep;     /* replacement op */
    unsigned char paths;   /* lsrc | rsrc<<2 | dsrc<<4 | RP_SUBR */
    unsigned char flags;
    unsigned char destval; /* result location: R_HL, R_A, F_Z, ... (0 = none) */
};
```

**Patterns are pre-decoded, not strings.** A rule used to carry the spelling it
is written as — `"=(D(H),N):s"` — and parse it afresh on every attempt; the
strings and their pointers were the largest single thing in the table. The
shape is small and fixed (an operator, up to two operands, at most one level
under either) so six bytes hold all of it. Rules are written through the `R()`
macro, which packs the fields:

```c
R(op, lop, rop, llop, rlop, sfx, rep, lsrc, rsrc, dsrc, flags, tpl, dest)
```

`llop` and `rlop` are the grandchild patterns, and **no rule has ever named
both** — a shape that deep on both sides has never been worth a rule — so they
share `subop`, with the `RP_SUBR` bit of `paths` saying which side it belongs
to. That one bit is 682 bytes of table.

DEBUG builds keep the spellings in `rulepat[]` for the trace to print;
`mkrulepat.py` regenerates that array from `rules[]` before every build.

### Pattern letters

These are the spellings `mkrulepat.py` produces and the trace prints; the table
itself stores opcode values.

| Char | Node | Char | Node | Char | Node |
|------|------|------|------|------|------|
| `=`  | ASSIGN    | `D` | DEREF     | `V` | REGVAR |
| `+`  | PLUS      | `W` | WIDEN     | `L` | LOCALVAR |
| `-`  | MINUS     | `X` | SEXT      | `I` | INDEX |
| `*`  | STAR      | `Z` | NARROW    | `S` | SYM |
| `/`  | DIV       | `!` | BANG      | `O` | SYMREF |
| `%`  | MOD       | `m` | NEG       | `G` | NUMBER |
| `&`  | AND       | `~` | NOT       | `H` | INHL |
| `\|` | OR        | `c` | CALL      | `E` | INDE |
| `^`  | XOR       | `;` | COMMA     | `A` | INA |
| `y`  | LSHIFT    | `Q` | QUES      | `B` | INBC |
| `w`  | RSHIFT    | `T` | TERNBRANCH| `K` | INE |
| `<`  | LT        | `i` | PREINC    | `C` | CODE |
| `>`  | GT        | `j` | POSTINC   | `R` | ARGNODE |
| `e`  | EQ        | `d` | PREDEC    | `F` | BFEXTRACT |
| `n`  | NEQ       | `k` | POSTDEC   | `f` | BFASSIGN |
| `q`  | LE        | `a` | LAND      | `U` | PLUSEQ |
| `p`  | GE        | `o` | LOR       | `l` | LABEL |

Note `<`/`>` are **LT/GT**, and the shifts are `y`/`w`. (An earlier revision of
this table had `<`/`>` as the shifts; they are not.)

### Pattern specials

Wildcards and constant classes, spelled as the lowercased tail of their `P_`
name:

| Spelling | Constant | Matches |
|----------|----------|---------|
| `any` | `P_ANY` (234) | any node |
| `0` | `P_NULL` (255) | no child |
| `num` | `P_NUM` (254) | any NUMBER |
| `pow2` | `P_POW2` (253) | a power of two |
| `zero` | `P_ZERO` (252) | the constant 0 |
| `smal` | `P_SMALL` (251) | 1–4: can use inc/dec |
| `eigh` | `P_EIGHT` (237) | the constant 8: a shift by a whole byte |
| `mul3` … `mul40` | `P_MUL3`…`P_MUL40` | 3, 5, 6, 7, 9, 10, 11, 12, 14, 15, 20, 24, 40 |
| `cmp` | `P_CMP` (236) | EQ, NEQ, LT or GE |
| `cmpx` | `P_CMPX` (235) | LE or GT — the same code, operands swapped |

Anything at or above `P_ANY` (234) is a looser match than a plain opcode, which
is how `tryrule` tells the two apart.

**`P_CMP`/`P_CMPX` are the comparison unification.** For one pair of operands
the six comparisons are two pieces of code, not six: EQ, NEQ, LT, and GE all
subtract and then read a different flag off the same subtraction, and LE and GT
are the same thing with the operands swapped. The table used to hold a row for
each, alike but for the flag it named — 89 rows saying what the operator
already says. Now one row each, with the result named `F_CC` and the real flag
worked out by `ccflag()`.

### Pattern syntax

```
op              match leaf node
op(child)       match unary with child pattern
op(left,right)  match binary with child patterns
:w              width suffix — b, s, or l
F               flag context (DEST_FLAGS)
V               value context (DEST_VALUE)
S               stack context (DEST_STACK)
```

The suffix byte packs three things: `SFX_W` the node's width, `SFX_D` the
destination context, and `SFX_LW` the **left child's** width — the one operand
that can carry a width of its own. Widths are `PW_B`, `PW_S`, `PW_L`, `PW_P`.

Width matching is case-insensitive: `b` matches both `byte` and `ubyte`.

### Examples

```
L               matches LOCALVAR
D(V)            matches DEREF(REGVAR)
+(D(V),num)     matches PLUS(DEREF(REGVAR), NUMBER)
*(any,pow2)     matches STAR(any, power of two)
=(I,num):b      matches byte ASSIGN(INDEX, NUMBER)
+(H,smal)       matches PLUS(INHL, 1..4)
e(A,num):bF     matches byte EQ(INA, NUMBER) in flag context
D(O):b          matches byte DEREF(SYMREF)
```

### Source Paths

Where a rule's replacement gets its children and its data. Only four values
exist, and the three paths share one byte (two bits each):

| Constant | Meaning |
|----------|---------|
| `P_NONE` | null/none |
| `P_L` | left child |
| `P_R` | right child |
| `P_LL` | left->left |

### Flags

The five modifiers are bits. The register requirements are **not** — a rule
cannot want two registers, so they are a 3-bit value in `RF_REG` (0xe0) rather
than a bit apiece. As bits they needed twelve, which meant a `short`, which
cost the table 474 bytes to say nothing more.

| Flag | Value | Effect |
|------|------:|--------|
| `RF_POW2`  | 0x01 | Transform the constant through log2 |
| `RF_NOTEQ` | 0x02 | NEQ→BANG(EQ): wrap children in an EQ node |
| `RF_INC1`  | 0x04 | Increment the right constant by 1 (GT→GE, LE→LT) |
| `RF_TDE`   | 0x08 | Require TARGET is DE (for the RHS of a binary op) |
| `RF_SIGNL` | 0x10 | Require the left operand has a signed width |
| `RF_IXIY`  | 0x20 | Require dsrc reg is IX or IY |
| `RF_BC`    | 0x40 | Require dsrc reg is BC |
| `RF_DE`    | 0x60 | Require dsrc reg is DE |
| `RF_HL`    | 0x80 | Require dsrc reg is HL |
| `RF_IX`    | 0xa0 | Require dsrc reg is IX |
| `RF_C`     | 0xc0 | Require dsrc reg is C (low byte of BC) |
| `RF_B`     | 0xe0 | Require dsrc reg is B (high byte of BC) |

### Preserved nodes

`preserve[]` in `rules.c` lists the node types a subtree is **not** reduced
past, so a parent rule can still match on their shape:

```
REGVAR LOCALVAR INDEX P_NUM SYM SYMREF INHL INDE INA INE INBC CODE
```

## Assembly Templates

Templates use `$` for interpolation. A path is any run of `L`/`R` naming a node
to reach, optionally followed by one modifier and any number of `+`s:

| Syntax   | Meaning                                    |
|----------|--------------------------------------------|
| `$L` `$R` `$LL` | Value of the node at that path      |
| `$RL`    | Right child's left child (special-cased)   |
| `$Rl` `$Rh` `$R2` `$R3` | The four bytes of a NUMBER, low to high |
| `$Lo`    | An INDEX's offset alone                    |
| `$Lr`    | An INDEX's register alone                  |
| `$L+`    | The same node with its offset + 1 (repeatable: `$L++`) |
| `$t`     | Target low register (`l` or `e`)           |
| `$u`     | Target high register (`h` or `d`)          |
| `$T`     | Target register pair (`hl` or `de`)        |
| `$$`     | A literal `$` — the assembler's "here"     |
| `$[` `$]`| Bracket a 16-bit helper call (see below)   |
| `%(text)`| Repeat text N times (N = right operand)    |

A path that reaches a node the emitter cannot print produces `?op<n>?`, and a
path that runs off the tree produces `?null?` — both of which the assembler
will reject loudly rather than emitting something plausible.

### `$[` and `$]` — saving BC across a helper

The 16-bit helpers take their second operand off the stack with a `pop bc` and
do not put it back. A register variable living in BC has to be saved across
that, and **only the emitter knows whether there is one** — the table cannot
say. `$[` emits `push bc` if `bcinuse()`, `$]` emits the matching `pop bc`.

Without it, `t = a * a` in a function with a register variable quietly
destroyed the variable — and when the variable was the loop subscript doing the
multiplying, the loop did not end.

### Shared template fragments

Templates are the largest single thing in the compiler and they repeat heavily
— `or a` alone appeared a hundred times, and nothing pools identical literals.
So common sequences live once in `fragtab[]` and a template names one with a
**single byte with the high bit set**, the low seven being the index.
`expandtpl()` expands them before anything is interpolated, so nothing
downstream has to know. This is worth about eight kilobytes.

`TPLMAX` (160) bounds a template once its fragments are put back.

### Repeat Syntax

The `%(text)` syntax repeats the enclosed text based on the right operand's
constant value. Used for small increments and shifts:

```c
R(PLUS,  INHL, P_SMALL, 0,0, 0, PLUS,  P_L, P_R, P_NONE, 0, "%(\tinc hl\n)", R_HL)
R(MINUS, INA,  P_SMALL, 0,0, 0, MINUS, P_L, P_R, P_NONE, 0, "%(\tdec a\n)",  R_A)
```

For `x + 3`, this emits `inc hl` three times.

## Sethi-Ullman Register Allocation

### Labeling (bottom-up)

Each node gets a `regs` count indicating registers needed:
- `0` - already in register (INHL, INDE, REGVAR HL/DE)
- `1` - needs one register to evaluate
- `2` - needs both HL and DE
- `3+` - needs spill to stack

Binary ops use Sethi-Ullman formula:
- If left.regs == right.regs: parent.regs = left.regs + 1
- Otherwise: parent.regs = max(left.regs, right.regs)

### Assignment (top-down)

Each node gets a `tgt` indicating where result should go:
- Binary ops with regs≥2: left→HL, right→DE
- Binary ops with regs<2: propagate parent's target
- ASSIGN: lvalue gets 0 (no target), rvalue gets parent's target

### Spill Handling

When regs≥3, the rewriter:
1. Evaluates left subtree (result in HL)
2. Pushes HL to stack
3. Evaluates right subtree (result in HL)
4. Pops to DE, exchanges: `pop de; ex de,hl`
5. Now left in HL, right in DE

## Normalization

Before pattern matching, commutative operators are normalized to put
constants on the right side:

Commutative: `+ * & | ^ == != && ||`
Non-commutative: `- / %`

## Comparison Lowering

Z80 `cp a, n` sets flags:
- Z if a == n
- C if a < n (unsigned)

Cheap comparisons (single cp + conditional jump):
- EQ: `cp n`, `jp z`
- NEQ: `cp n`, `jp nz` (via BANG(EQ))
- LT: `cp n`, `jp c`
- GE: `cp n`, `jp nc`

GT and LE rewritten to use cheap ops:
- GT(a,n) → GE(a,n+1): `a > 5` becomes `a >= 6`
- LE(a,n) → LT(a,n+1): `a <= 5` becomes `a < 6`

## Destination Context

`setdest()` marks an expression before rewriting:
- IF condition: `DEST_FLAGS` (result in the CPU flags)
- RETURN value: `DEST_VALUE` (result in a register)
- Expression statement: `DEST_NONE` (discard the result)
- Call argument: `DEST_STACK` (push it)

Pattern suffix `F` matches `DEST_FLAGS`, `V` matches `DEST_VALUE`, and `S`
matches `DEST_STACK`. There is no WHILE — cpp lowered it.

## Special Node Types

### INDEX

Created from LOCALVAR or REGVAR+offset. Represents indexed addressing:
`(ix+d)` or `(iy+d)`. The `+(I,N)` rule combines nested offsets.

### SYMREF

Symbol plus constant offset, resolved at link time. Created from:
- `S` (bare SYM) → SYMREF with offset 0
- `+(S,N)` → SYMREF with offset N
- `+(O,N)` → SYMREF with combined offset

### CODE

Represents emitted assembly code. Contains the result location (R_HL,
R_A, F_Z, etc.). After code emission, the matched tree is replaced
with a CODE node.

## Incomplete Code Detection

After rewriting, if the result is not CODE/INHL/INDE/INBC/INA, the
rewriter emits:
```
; XXXXXX incomplete: (pattern...)
```

This identifies patterns that need additional rules.

## Z80-Specific Notes

### Register Constraints

- Only HL and DE available as temps (BC reserved for register variables)
- IX used for register variable pointers
- IY used as frame pointer
- A used for byte operations

### IX Half-Register Access

The assembler supports undocumented instructions:
```asm
ld a,ixl        ; load low byte of IX
or a,ixh        ; OR with high byte (test IX for zero)
ld c,ixl        ; copy IX to BC
ld b,ixh
```

But NOT `ld l,ixl` - copying IX to HL requires `push ix; pop hl`.

### Depth-First Accumulation

The rewriter evaluates depth-first, accumulating into HL. For array
indexing like `wordarray[variable]`:
```asm
ld hl,(variable)    ; load index
add hl,hl           ; multiply by 2 (word size)
ld de,wordarray     ; load base address
add hl,de           ; compute final address
```

Constants load into DE at the last moment, avoiding register pressure.

## Multiply-by-Constant Strength Reduction

Multiplying by small constants with few set bits is converted to shift/add
sequences using `add hl,hl` (which doubles HL). The pattern saves HL to DE
first, then applies a sequence of doubles and adds:

| Constant | Formula      | Sequence (D=double, A=add de) |
|----------|--------------|-------------------------------|
| 3        | 2+1          | DA                            |
| 5        | 4+1          | DDA                           |
| 6        | 3×2          | DAD                           |
| 7        | 3×2+1        | DADA                          |
| 9        | 8+1          | DDDA                          |
| 10       | 5×2          | DDAD                          |
| 11       | 5×2+1        | DDADA                         |
| 12       | 3×4          | DADD                          |
| 14       | 7×2          | DADAD                         |
| 15       | 7×2+1        | DADADA                        |
| 20       | 5×4          | DDADD                         |
| 24       | 3×8          | DADDD                         |
| 40       | 5×8          | DDADDD                        |

Example: `x * 6` generates:
```asm
ld d,h          ; save HL to DE
ld e,l
add hl,hl       ; HL = 2x
add hl,de       ; HL = 3x
add hl,hl       ; HL = 6x
```

Powers of 2 (2, 4, 8, 16, ...) use pure shift sequences via the POW2 pattern.
