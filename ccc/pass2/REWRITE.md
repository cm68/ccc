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
| `%`  | MOD        | `N`  | NUMBER    |
| `&`  | AND        | `P`  | POW2      |
| `\|` | OR         | `_`  | any       |
| `^`  | XOR        | `0`  | null      |
| `<`  | LSHIFT     | `=`  | ASSIGN    |
| `>`  | RSHIFT     |      |           |

Pattern syntax:
- `op` - match leaf node
- `op(child)` - match unary with child pattern
- `op(left,right)` - match binary with child patterns

Examples:
- `L` matches LOCALVAR
- `D(V)` matches DEREF(REGVAR)
- `+(D(V),N)` matches PLUS(DEREF(REGVAR), NUMBER)
- `*(_,P)` matches STAR(any, POW2)

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

## Current Rules

```c
{"L",         "I", "",  "",  "",   0}
```
LOCALVAR -> INDEX: Convert frame-relative variable to indexed addressing.

```c
{"+(D(V),N)", "I", "",  "",  "LL", RF_IXIY}
```
ADD(DEREF(REGVAR), NUM) -> INDEX: Pointer+offset to indexed addressing.
Only when REGVAR is IX or IY. Data (reg) comes from left->left.

```c
{"*(_,P)",    "<", "L", "R", "",   RF_POW2}
```
MUL(x, POW2) -> LSHIFT(x, log2): Strength reduction.
Left child from L, right child from R (value transformed to shift count).

## Normalization

Before pattern matching, commutative operators are normalized to put
constants on the right side. This simplifies rules - only need one
version instead of two.

Commutative ops: `+ * & | ^ == != && ||`
Non-commutative (not swapped): `- / %`
