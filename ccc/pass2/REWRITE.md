# Expression Tree Rewriter

Table-driven bottom-up expression tree transformation.

## Pattern Language

Single-character operator codes:

| Char | Operator   | Char | Operator  |
|------|------------|------|-----------|
| `+`  | PLUS       | `D`  | DEREF     |
| `*`  | STAR (mul) | `V`  | REGVAR    |
| `-`  | MINUS      | `L`  | LOCALVAR  |
| `/`  | DIV        | `I`  | INDEX     |
| `%`  | MOD        | `H`  | INHL      |
| `&`  | AND        | `E`  | INDE      |
| `\|` | OR         | `N`  | NUMBER    |
| `^`  | XOR        | `P`  | POW2      |
| `<`  | LSHIFT     | `_`  | any       |
| `>`  | RSHIFT     | `0`  | null      |
| `=`  | ASSIGN     | `A`  | INA       |
| `Q`  | EQ         | `T`  | LT        |
| `U`  | NEQ        | `G`  | GT        |
| `W`  | LE         | `Y`  | GE        |
| `!`  | BANG       | `S`  | SYM       |
| `O`  | SYMREF     |      |           |

Pattern syntax:
- `op` - match leaf node
- `op(child)` - match unary with child pattern
- `op(left,right)` - match binary with child patterns
- `:w` suffix - match width (b=byte, s=short, l=long, p=ptr, _=any) or dest (f=flags)

Examples:
- `L` matches LOCALVAR
- `D(V)` matches DEREF(REGVAR)
- `+(D(V),N)` matches PLUS(DEREF(REGVAR), NUMBER)
- `*(_,P)` matches STAR(any, POW2)
- `=(I,N):b` matches byte ASSIGN(INDEX, NUMBER)
- `+(H,E):s` matches short PLUS(INHL, INDE)
- `==(A,N):f` matches flag-context EQ(INA, NUMBER)

## Rule Table

```c
struct rule {
    char *pat;      /* pattern string */
    char *rep;      /* replacement op char */
    char *lsrc;     /* left child source path */
    char *rsrc;     /* right child source path */
    char *dsrc;     /* data source path (for reg/off) */
    unsigned char flags;
};
```

Source paths use `L` and `R` to navigate:
- `""` - null/none
- `"L"` - left child
- `"R"` - right child
- `"LL"` - left->left
- `"LR"` - left->right
- `"RL"` - right->left

Flags:
- `RF_POW2` - transform NUMBER value through log2
- `RF_IXIY` - require data source reg is IX or IY
- `RF_NOTEQ` - NEQ→BANG(EQ): wrap children in EQ node
- `RF_INC1` - increment right constant by 1 (for GT→GE, LE→LT)

## Normalization

Before pattern matching, commutative operators are normalized to put
constants on the right side. This simplifies rules - only need one
version instead of two.

Commutative ops: `+ * & | ^ == != && ||`
Non-commutative (not swapped): `- / %`

## Comparisons

Z80 `cp a, n` sets flags:
- Z if a == n
- C if a < n (unsigned)

Cheap comparisons (single cp + conditional jump):
- EQ: `cp n`, `jp z`
- NEQ: `cp n`, `jp nz` (via BANG(EQ))
- LT: `cp n`, `jp c`
- GE: `cp n`, `jp nc`

GT and LE to constants rewritten to use cheap ops:
- GT(a,n) → GE(a,n+1): `cp n+1`, `jp nc`
- LE(a,n) → LT(a,n+1): `cp n+1`, `jp c`

## Destination Context

Expressions marked with destination before rewriting:
- IF condition: DEST_FLAGS (boolean result in flags)
- RETURN value: DEST_VALUE
- SWITCH expr: DEST_VALUE
- Expression statement: DEST_NONE (discard result)

DEST_FLAGS propagates through LAND, LOR, BANG.
Pattern suffix `:f` matches DEST_FLAGS context.

## Linker-Resolvable Addresses

`+(S,N)` → SYMREF: symbol plus constant offset folds into a single
node that code generation can emit as `symbol+N`, resolved at link time.
This handles array indexing with constant offsets, struct field access, etc.
