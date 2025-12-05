# AST Output Format

The compiler outputs a compact paren-free hex AST format for consumption by
the second pass (code generator).

## Format Overview

The AST uses a paren-free format with:
- Single-character opcodes
- Hex-encoded numbers terminated with `.`
- Hex-encoded symbol names
- Width suffixes (`:b`, `:s`, `:l`, `:p`)
- Counted children for compound statements

### Basic Syntax

- **Numbers**: Hex digits terminated by `.` (e.g., `a.` = 10, `ff.` = 255)
- **Symbols**: `$` followed by hex-encoded name (e.g., `$5f78` = `_x`)
- **Widths**: `:b` (byte), `:s` (short), `:l` (long), `:p` (pointer)
- **Operators**: Single character followed by width and operands

## Global Variables

Global variables are emitted with the `Z` opcode:
```
Z$<hex_name><type>[<init>]
```

**Examples:**
```c
int global_a = 10;     // Z$_global_a s 1. a.
int global_b;          // Z$_global_b s 0.
```

The format is:
- `Z` - global variable marker
- `$` + hex-encoded name
- type width (s=short, l=long, b=byte, p=pointer)
- init flag: `1.` if initialized, `0.` if not
- initializer value (if init flag is 1)

## Functions

Functions are emitted with the `F` opcode:
```
F<width>$<hex_name><param_count>.<body>
```

**Example:**
```c
int add(int x, int y) {
    return x + y;
}
```
Emits:
```
Fs$_add2.
  ds$x
  ds$y
  B0.1.R1.+sMs$xMs$y
```

## Expression Format

Expressions use prefix notation with single-character operators:

### Operators

| Op | Name | Format | Description |
|----|------|--------|-------------|
| `+` | ADD | `+<w><left><right>` | Addition |
| `-` | SUB | `-<w><left><right>` | Subtraction |
| `*` | MUL | `*<w><left><right>` | Multiplication |
| `/` | DIV | `/<w><left><right>` | Division |
| `%` | MOD | `%<w><left><right>` | Modulo |
| `<` | LT | `<<w><left><right>` | Less than |
| `>` | GT | `><w><left><right>` | Greater than |
| `L` | LE | `L<w><left><right>` | Less or equal |
| `g` | GE | `g<w><left><right>` | Greater or equal |
| `Q` | EQ | `Q<w><left><right>` | Equal |
| `n` | NE | `n<w><left><right>` | Not equal |
| `&` | AND | `&<w><left><right>` | Bitwise AND |
| `\|` | OR | `\|<w><left><right>` | Bitwise OR |
| `^` | XOR | `^<w><left><right>` | Bitwise XOR |
| `~` | NOT | `~<w><operand>` | Bitwise NOT |
| `y` | LSHIFT | `y<w><left><right>` | Left shift |
| `w` | RSHIFT | `w<w><left><right>` | Right shift |
| `j` | LAND | `j<left><right>` | Logical AND |
| `h` | LOR | `h<left><right>` | Logical OR |
| `!` | LNOT | `!<operand>` | Logical NOT |
| `M` | DEREF | `M<w><addr>` | Memory read |
| `=` | ASSIGN | `=<w><lval><rval>` | Assignment |
| `@` | CALL | `@<argc>.<func><args...>` | Function call |
| `?` | TERN | `?<cond><true><false>` | Ternary |
| `N` | NARROW | `N<w><expr>` | Truncate to smaller type |
| `W` | WIDEN | `W<w><expr>` | Zero-extend |
| `X` | SEXT | `X<w><expr>` | Sign-extend |
| `Y` | MCOPY | `Y<len>.<dest><src>` | Memory copy |
| `A` | ADDR | `A<expr>` | Address-of |

### Width Suffixes

- `b` - byte (1 byte: char)
- `s` - short (2 bytes: short, int)
- `l` - long (4 bytes: long)
- `p` - pointer (2 bytes)
- `f` - float (4 bytes)
- `d` - double (8 bytes)

**Examples:**
```c
char c; int i;
c = 10;     // =b$_c a.
i = c;      // =s$_i Xsb$_c     (sign-extend char to int)
```

### Increment/Decrement Operators

| Op | Name | Description |
|----|------|-------------|
| `0xcf` | PREINC | Prefix increment |
| `0xef` | POSTINC | Postfix increment |
| `0xd6` | PREDEC | Prefix decrement |
| `0xf6` | POSTDEC | Postfix decrement |

### Compound Assignment Operators

| Op | Name | Description |
|----|------|-------------|
| `P` | ADDASSIGN | += |
| `0xdf` | SUBASSIGN | -= |
| `T` | MULASSIGN | *= |
| `2` | DIVASSIGN | /= |
| `0xfe` | MODASSIGN | %= |
| `0xc6` | ANDASSIGN | &= |
| `1` | ORASSIGN | \|= |
| `X` | XORASSIGN | ^= |
| `0` | LSHASSIGN | <<= |
| `6` | RSHASSIGN | >>= |

## Statement Format

Statements use single-character codes with counted children:

| Op | Name | Format | Description |
|----|------|--------|-------------|
| `B` | BLOCK | `B<has_label>.<count>.<stmts...>` | Block |
| `E` | EXPR | `E<expr>` | Expression statement |
| `R` | RETURN | `R<has_expr>.[<expr>]` | Return |
| `I` | IF | `I<has_else>.<cond><then>[<else>]` | If statement |
| `W` | WHILE | `W<cond><body>` | While loop |
| `D` | DO | `D<body><cond>` | Do-while loop |
| `F` | FOR | See below | For loop |
| `S` | SWITCH | `S<has_label>.[<label>]<case_count>.<expr><cases...>` | Switch |
| `C` | CASE | `C<body_count>.<value><body_stmts...>` | Case label |
| `O` | DEFAULT | `O<body_count>.<body_stmts...>` | Default label |
| `K` | BREAK | `K` | Break |
| `N` | CONTINUE | `N` | Continue |
| `G` | GOTO | `G$<label>` | Goto |
| `L` | LABEL | `L$<label><stmt>` | Label |
| `d` | DECL | `d<width>$<name>` | Declaration |

### Block Statement

Format: `B<has_label>.<count>.<stmts...>`
- `has_label`: `1` if block has a label, `0` otherwise
- `count`: number of statements in hex
- If has_label=1, label name follows count

### If Statement

Format: `I<has_else>.<cond><then>[<else>]`
- `has_else`: `1` if has else branch, `0` otherwise
- If has_else=1, else statement follows then

### For Loop

For loops with labels are lowered to synthetic blocks:
```
B0.<count>.
  <init>           ; if present
  L$_top: ...      ; loop top
  I...             ; if (cond) body else break
  L$_continue: ... ; continue label
  <incr>           ; if present
  G$_top           ; goto top
  L$_break: (;)    ; break label
```

### Switch Statement

Format: `S<has_label>.[<label>]<case_count>.<expr><cases...>`

Each case: `C<body_count>.<value><body_stmts...>`
Each default: `O<body_count>.<body_stmts...>`

## Symbol Names

Symbols are hex-encoded and prefixed with `$`:

| Scope | Prefix | Example | Description |
|-------|--------|---------|-------------|
| Global/Extern | `_` | `$_global` | Global variable or function |
| Static | (mangled) | `$counter0` | Static variable with index |
| Local | (none) | `$x` | Local variable |
| String literal | `__` | `$__str0` | String constant |

**Static Variable Name Mangling:**

Static variables use simplified naming: `<name><index>`
- File-global counter increments for each static
- Example: `static int counter;` becomes `$counter0`

## String Literals

String literals are emitted in a literals section:
```
t<count>.
  <len>.<hex_data>
  ...
```

**Example:**
```c
char *s = "hello";
```
Emits:
```
t1.
  6.68656c6c6f00
```

## Type Conversions

The compiler automatically inserts type conversion operators:

- **N** (NARROW): Truncate larger to smaller (e.g., int to char)
- **X** (SEXT): Sign-extend signed types (e.g., char to int)
- **W** (WIDEN): Zero-extend unsigned types

**Examples:**
```c
char c; int i; long l;

c = i;      // =b$_c Nb Ms$_i     (narrow int to char)
i = c;      // =s$_i Xs Mb$_c     (sign-extend char to int)
l = i;      // =l$_l Xl Ms$_i     (sign-extend int to long)
```

## Complete Example

```c
int global_a = 10;

int main() {
    int x = 5;
    if (x > 0) {
        return 1;
    }
    return 0;
}
```

Emits:
```
Z$_global_as1.a.

Fs$_main0.
B0.3.ds$xE=s$x5.I0.>sMs$x0.B0.1.R1.1.R1.0.
```

Breaking down the function body:
- `B0.3.` - Block, no label, 3 statements
- `ds$x` - Declare short variable x
- `E=s$x5.` - Expression: assign 5 to x
- `I0.>sMs$x0.B0.1.R1.1.` - If (no else): condition `x > 0`, then block with return 1
- `R1.0.` - Return 0

## Implementation Notes

The format is designed for:
1. **Compact output** - Minimal bytes for constrained environments
2. **Easy parsing** - Single-character opcodes, counted children
3. **Self-delimiting** - Hex numbers end with `.`, symbols start with `$`
4. **No ambiguity** - Each construct has unique prefix

## Type Names

Type information uses width codes:
- `b` - char (1 byte)
- `s` - short/int (2 bytes)
- `l` - long (4 bytes)
- `p` - pointer (2 bytes)

Arrays: `:array:<count>` in type declarations
Structs: `:struct:<size>` in type declarations
