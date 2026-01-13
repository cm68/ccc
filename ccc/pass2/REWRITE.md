# Expression Tree Rewriter

Table-driven bottom-up expression tree transformation with Sethi-Ullman
register allocation.

## Overview

The rewriter transforms expression trees into Z80 assembly through:
1. **Labeling** - bottom-up Sethi-Ullman pass counts registers needed
2. **Assignment** - top-down pass assigns target registers (HL/DE)
3. **Rewriting** - depth-first fixed-point rule application with code emission

## Pattern Language

### Operator Codes

Single-character codes for matching expression nodes:

| Char | Operator   | Char | Operator   | Char | Special      |
|------|------------|------|------------|------|--------------|
| `+`  | PLUS       | `D`  | DEREF      | `N`  | NUMBER       |
| `*`  | STAR (mul) | `V`  | REGVAR     | `P`  | POW2 (2^n)   |
| `-`  | MINUS      | `L`  | LOCALVAR   | `Z`  | ZERO (0)     |
| `/`  | DIV        | `I`  | INDEX      | `M`  | SMALL (1-4)  |
| `%`  | MOD        | `H`  | INHL       | `_`  | any          |
| `&`  | AND        | `E`  | INDE       | `0`  | null         |
| `\|` | OR         | `A`  | INA        |      |              |
| `^`  | XOR        | `B`  | INBC       |      |              |
| `<`  | LSHIFT     | `C`  | CODE       |      |              |
| `>`  | RSHIFT     | `S`  | SYM        |      |              |
| `=`  | ASSIGN     | `O`  | SYMREF     |      |              |
| `Q`  | EQ         | `T`  | LT         |      |              |
| `U`  | NEQ        | `G`  | GT         |      |              |
| `W`  | LE         | `Y`  | GE         |      |              |
| `!`  | BANG       | `i`  | PREINC     |      |              |
| `o`  | OREQ       | `j`  | POSTINC    |      |              |
| `a`  | ARGNODE    | `k`  | PREDEC     |      |              |
|      |            | `m`  | POSTDEC    |      |              |

### Multiply Constants

Lowercase letters match specific constant values for strength-reduced multiply:

| Char | Value | Char | Value | Char | Value |
|------|-------|------|-------|------|-------|
| `3`  | 3     | `x`  | 10    | `n`  | 15    |
| `5`  | 5     | `e`  | 11    | `y`  | 20    |
| `6`  | 6     | `w`  | 12    | `q`  | 24    |
| `7`  | 7     | `f`  | 14    | `z`  | 40    |
| `9`  | 9     |      |       |      |       |

### Pattern Syntax

```
op              match leaf node
op(child)       match unary with child pattern
op(left,right)  match binary with child patterns
:w              width suffix (b/s/l/p/f or _ for any)
:F              flag context (DEST_FLAGS)
:V              value context (DEST_VALUE)
```

Width matching is case-insensitive: `b` matches both `byte` and `ubyte`.

### Examples

```
L               matches LOCALVAR
D(V)            matches DEREF(REGVAR)
+(D(V),N)       matches PLUS(DEREF(REGVAR), NUMBER)
*(_,P)          matches STAR(any, POW2)
=(I,N):b        matches byte ASSIGN(INDEX, NUMBER)
+(H,M)          matches PLUS(INHL, SMALL) where SMALL is 1-4
Q(A,N):F        matches EQ(INA, NUMBER) in flag context
D(O):b          matches byte DEREF(SYMREF)
```

## Rule Table

```c
struct rule {
    char *pat;      /* pattern string */
    char *rep;      /* replacement op char */
    char *lsrc;     /* left child source path */
    char *rsrc;     /* right child source path */
    char *dsrc;     /* data source path (for reg/off) */
    unsigned char flags;
    char *asmtpl;   /* assembly template (NULL = rewrite only) */
    unsigned char destval; /* result register (R_HL, R_A, F_Z, etc.) */
};
```

### Source Paths

Navigate the expression tree using L (left) and R (right):
- `""` - null/none
- `"L"` - left child
- `"R"` - right child
- `"LL"` - left->left
- `"LR"` - left->right

### Flags

| Flag       | Effect                                          |
|------------|-------------------------------------------------|
| `RF_POW2`  | Transform NUMBER value through log2             |
| `RF_IXIY`  | Require dsrc reg is IX or IY                    |
| `RF_BC`    | Require dsrc reg is BC                          |
| `RF_DE`    | Require dsrc reg is DE                          |
| `RF_HL`    | Require dsrc reg is HL                          |
| `RF_IX`    | Require dsrc reg is IX                          |
| `RF_NOTEQ` | NEQ→BANG(EQ): wrap children in EQ node          |
| `RF_INC1`  | Increment right constant by 1 (GT→GE, LE→LT)    |

## Assembly Templates

Templates use `$` for interpolation:

| Syntax   | Meaning                                    |
|----------|--------------------------------------------|
| `$L`     | Left child value                           |
| `$R`     | Right child value                          |
| `$LL`    | Left->left value                           |
| `$Rl`    | Low byte of right NUMBER                   |
| `$Rh`    | High byte of right NUMBER                  |
| `$L+`    | Left INDEX with offset+1                   |
| `$t`     | Target low register (l or e)               |
| `$u`     | Target high register (h or d)              |
| `$T`     | Target register pair (hl or de)            |
| `%(text)`| Repeat text N times (N = right operand)    |

### Repeat Syntax

The `%(text)` syntax repeats the enclosed text based on the right operand's
constant value. Used for small increments and shifts:

```c
{"+(H,M)", "+", "L", "R", "", 0, "%(\tinc hl\n)", R_HL}
{"-(A,M)", "-", "L", "R", "", 0, "%(\tdec a\n)", R_A}
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

Expressions marked with destination before rewriting:
- IF/WHILE condition: DEST_FLAGS (result in CPU flags)
- RETURN value: DEST_VALUE (result in register)
- Expression statement: DEST_NONE (discard result)

Pattern suffix `:F` matches DEST_FLAGS, `:V` matches DEST_VALUE.

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
