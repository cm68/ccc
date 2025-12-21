# cvopt - Code Generation Table Compiler

`cvopt.c` translates a compact DSL for specifying code generation patterns into C source code. It originates from the Ritchie C compiler.

## Usage

```bash
./cvopt [input] [output]
```

Reads pattern specifications from stdin (or input file), writes C source to stdout (or output file).

## Output Structure

The generated C file contains:

1. `#include "c1.h"` header
2. `#define` macros mapping pattern names to `&optab[N]`
3. `struct optab optab[]` array with pattern entries
4. String constants `L1`, `L2`, ... containing code templates
5. Verbatim C code from `%{...%}` blocks

## DSL Syntax

### Pattern Labels

A line ending with `:` defines a named entry point:

```
PLUS:
```

Generates: `#define PLUS &optab[N]`

### Pattern Rules

Lines starting with `%` define optimization rules:

```
%a,n
    template code
```

The format is `%<left-cond>,<right-cond>` followed by indented template lines.

### Condition Codes

| Code | Value | Meaning |
|------|-------|---------|
| `z`  | 4     | Zero |
| `c`  | 8     | Constant |
| `r`  | 9     | Register |
| `1`  | 5     | One |
| `i`  | 12    | Immediate |
| `a`  | 16    | Address |
| `e`  | 20    | Expression |
| `n`  | 63    | Any (match all) |

Append `*` to add 0100 octal (indirect flag).

### Type Flags

Type suffixes modify conditions:

| Flag | Value | Meaning |
|------|-------|---------|
| `w`  | 1     | Word |
| `i`  | 2     | Int |
| `b`  | 3     | Byte (signed) |
| `f`  | 4     | Float |
| `d`  | 5     | Double |
| `s`  | 6     | Short |
| `l`  | 8     | Long |
| `u`  | 9     | Unsigned word |
| `ub` | 10    | Unsigned byte |
| `ul` | 11    | Unsigned long |
| `p`  | +16   | Pointer (adds to other flags) |

### Operand Codes

These codes in templates get compressed to single characters:

| Input | Output | Meaning |
|-------|--------|---------|
| `A1`  | A      | Operand A1 |
| `A2`  | B      | Operand A2 |
| `A`   | O      | Operand A |
| `B1`  | C      | Operand B1 |
| `B2`  | D      | Operand B2 |
| `BE`  | L      | Operand BE |
| `BF`  | P      | Operand BF |
| `C1`  | E      | Operand C1 |
| `C2`  | F      | Operand C2 |
| `F`   | G      | Field |
| `H`   | H      | High |
| `R`   | I      | Register |
| `R1`  | J      | Register 1 |
| `S`   | K      | Stack |
| `I`   | M      | Immediate |
| `M`   | N      | Memory |
| `#1`  | #      | Literal 1 |
| `#2`  | "      | Literal 2 |

### Subtree Modifiers

After `F`, `H`, or `S`, these modifiers encode subtree properties:

| Modifier | Value | Meaning |
|----------|-------|---------|
| `*`      | +1    | Indirect |
| `S`      | +2    | Signed |
| `C`      | +4    | Constant |
| `1`      | +8    | First operand |
| `2`      | +16   | Second operand |

### Special Constructs

**Named definitions:**
```
%[NAME]
```
Generates: `#define NAME LN` (where N is current label number)

**Inline definitions:**
```
%[NAME:] pattern
```
Defines NAME and continues with pattern.

**Verbatim C code:**
```
%{
    /* C code here */
%}
```
Code is copied directly to output.

## Example

Input:
```
PLUS:
%aw,cw
    add hl,A1
%n,n
    call plus
```

Output (conceptual):
```c
#define PLUS &optab[0]

struct optab optab[]={
    {16,1,8,1,L1},  /* 0 */
    {63,0,63,0,L2}, /* 1 */
    {0},
};

static char L1[]="add hl,A\n";
static char L2[]="call plus\n";
```

## Implementation Notes

- Uses two temp files (`/tmp/cvoptaXXXXXX`, `/tmp/cvoptbXXXXXX`) to buffer output sections
- Tab characters after newlines in templates encode special escapes (`\ooo` format)
- Comments (`/* ... */`) are preserved in output
- The `X`, `Y`, `T` characters suppress newline emission in templates
