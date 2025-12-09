# CCC AST Binary Format

This document describes the binary AST format produced by cc1 and consumed by cc2.

## Overview

The AST is a compact binary format with minimal whitespace. Numbers are hexadecimal.
Names use length-prefixed format. All expressions carry type/size annotations.

**Important**: cc1 performs all preprocessing internally. The AST output contains
no preprocessor directives - all macros are expanded, conditionals resolved, and
includes processed before AST emission. String literals are hex-encoded to avoid
escape sequence issues in the binary format.

## Encoding Primitives

### Names
```
<2-hex-len><ascii-chars>
```
Example: `05hello` = "hello" (5 characters)

### Numbers
- 2-digit hex: `%02x` format (0-255)
- 4-digit hex: `%04lx` format (0-65535)
- 8-digit hex (constants): `#%08lx` format (32-bit signed, two's complement)

### Type/Size Suffixes
Single character after operators indicating operand type:
- `b` - signed byte (1 byte)
- `B` - unsigned byte
- `s` - signed short/int (2 bytes)
- `S` - unsigned short/int
- `l` - signed long (4 bytes)
- `L` - unsigned long
- `p` - pointer (2 bytes)
- `f` - float (4 bytes)
- `d` - double (8 bytes)
- `v` - void

---

## Top-Level Declarations

### Function Definition
```
F<rettype><hexname><param_count><params...><body>
```

- `rettype` - single char size suffix for return type
- `hexname` - hex-length-prefixed function name
- `param_count` - 2-digit hex count of parameters
- `params` - sequence of `d<type><hexname>` declarations
- `body` - statement (usually a Block)

Example:
```
Fs05_main02ds04argcdp04argv
```
= `int _main(int argc, char **argv)` - returns short (s), name "_main", 2 params

### Global Variable
```
Z$<hexname><type><has_init><init?>
```

- `type` - `p` (pointer), `a<count><elemsize>` (array), `r<size>` (aggregate), or size char
- `has_init` - `00` (no init) or `01` (has init)
- `init` - initializer expression if present

### String Literal
```
U<hexname><len><hexdata>
```

- `hexname` - synthetic name like "str0", "str1"
- `len` - 2-digit hex byte count
- `hexdata` - hex-encoded string bytes (each byte as 2 hex digits)

Example:
```
U04str00648656c6c6f00
```
= str0 containing "Hello\0" (6 bytes: 48='H', 65='e', 6c='l', 6c='l', 6f='o', 00='\0')

**Note**: String data is hex-encoded because the AST is a text format and raw
binary data (especially null bytes and control characters) would cause parsing
issues. cc2's readStr() decodes the hex data back to binary.

### Array Initializer (in global)
```
[<width><count><items...>
```

- `width` - element size suffix
- `count` - 2-digit hex element count
- `items` - constant expressions

---

## Statements

### Block
```
B<decl_count><stmt_count><decls...><stmts...>
```

- `decl_count` - 2-digit hex count of local variable declarations
- `stmt_count` - 2-digit hex count of statements
- `decls` - sequence of `d<type><hexname>` declarations
- `stmts` - sequence of statements

Example: `B0203` = block with 2 declarations, 3 statements

### If Statement
```
I<has_else><nlabels><cond><then>[<else>]
```

- `has_else` - `00` (no else) or `01` (has else)
- `nlabels` - 2-digit hex count of intermediate labels for ||/&& short-circuit
- `cond` - condition expression
- `then` - then-branch statement
- `else` - else-branch statement (only if has_else=01)

See CONDITIONS.md for details on label assignment for short-circuit evaluation.

### While Loop (lowered to labels)
```
B0005L<top>I01<nlabels><cond><body>B0001G<break>L<continue>G<top>L<break>
```

Loops are lowered to a block containing:
1. `L<label>_top` - loop top label
2. `I01<nlabels><cond><body>B0001G<break>` - if with else that breaks
3. `L<label>_continue` - continue label
4. `G<label>_top` - goto top
5. `L<label>_break` - break label

### While Loop (non-lowered form)
```
W<nlabels><cond><body>
```

- `nlabels` - 2-digit hex count of intermediate labels for ||/&& short-circuit
- `cond` - condition expression
- `body` - loop body statement

### Do-While Loop (non-lowered form)
```
D<nlabels><body><cond>
```

- `nlabels` - 2-digit hex count of intermediate labels
- `body` - loop body statement
- `cond` - condition expression

### For Loop (lowered)
```
B00<count>[E<init>]L<top>[I01<nlabels><cond><body>B0001G<break>|<body>]L<continue>[E<incr>]G<top>L<break>
```

### For Loop (non-lowered form)
```
F<nlabels><init><cond><incr><body>
```

- `nlabels` - 2-digit hex count of intermediate labels
- `init` - initializer expression (or `_` if none)
- `cond` - condition expression (or `_` if none)
- `incr` - increment expression (or `_` if none)
- `body` - loop body statement

### Expression Statement
```
E<expr>
```

### Return Statement
```
R<has_value>[<expr>]
```

- `has_value` - `00` (void return) or `01` (has value)

### Label
```
L<hexname>
```

### Goto
```
G<hexname>
```

### Switch Statement
```
S<has_label>[<hexlabel>]<case_count><expr><cases...>
```

- `has_label` - `00` or `01`
- `case_count` - 2-digit hex count of case/default labels

### Case Label
```
C<stmt_count><value><stmts...>
```

### Default Label
```
O<stmt_count><stmts...>
```

### Inline Assembly
```
A<len><hexdata>
```

### Empty Statement
```
;
```

### Break
```
K
```

### Continue
```
N
```

---

## Expressions

All expressions use prefix notation with type suffix after the operator.

### Constants
```
#<8-hex-digits>
```

32-bit signed value in two's complement.
Example: `#00000042` = 66, `#ffffffff` = -1

### Symbol Reference
```
$<hexname>
```

Example: `$05_main` = reference to _main

### Null/Empty Expression
```
_
```

### Memory Dereference
```
M<type><addr>
```

Example: `Mb$03foo` = byte load from foo

### Assignment
```
=<type><lvalue><rvalue>
```

Example: `=s$01x#00000005` = x = 5 (short)

### Binary Operators
```
<op><type><left><right>
```

Operators:
- `+` - add
- `-` - subtract
- `*` - multiply
- `/` - divide
- `%` - modulo
- `&` - bitwise AND
- `|` - bitwise OR
- `^` - bitwise XOR
- `y` - left shift
- `w` - right shift (arithmetic)
- `<` - less than
- `>` - greater than
- `Q` - equal (==)
- `n` - not equal (!=)
- `L` - less or equal (<=)
- `g` - greater or equal (>=)
- `h` - logical OR (||)
- `j` - logical AND (&&)

### Unary Operators
```
<op><type><operand>
```

- `M` - memory dereference
- `!` - logical NOT
- `~` - bitwise NOT
- `\` - address-of
- `'` - unary minus

### Type Conversions
```
N<type><expr>   - narrow (truncate to smaller type)
W<type><expr>   - widen (zero-extend unsigned)
x<type><expr>   - sign-extend (SEXT)
```

### Increment/Decrement
```
<op><type><expr><amount>
```

- `(` - pre-increment (++x)
- `)` - post-increment (x++)
- `{` - pre-decrement (--x)
- `}` - post-decrement (x--)
- `amount` - 4-digit hex increment value

### Function Call
```
@<rettype><argc><func><args...>
```

- `rettype` - single char return type suffix (b/s/l/p/etc)
- `argc` - 2-digit hex argument count
- `func` - function expression
- `args` - argument expressions wrapped in comma nodes

### Ternary Operator
```
?<type><nlabels><cond><then><else>
```

- `type` - result type suffix
- `nlabels` - 2-digit hex count of intermediate labels for ||/&& in condition

### Memory Copy
```
Y<length><dest><src>
```

- `length` - 4-digit hex byte count

### Bitfield Extract
```
e<offset><width><addr>
```

### Bitfield Assign
```
f<offset><width><addr><value>
```

### Comma Operator
```
,<type><left><right>
```

Used for function argument lists.

---

## Example

```
Fs05_main00
B0101ds01x
E=s$01x#0000000a
R01Ms$01x
```

Parses as:
```c
int _main() {
    int x;      // B0101ds01x - block with 1 decl, 1 stmt
    x = 10;     // E=s$01x#0000000a
    return x;   // R01Ms$01x
}
```

---

## Notes

1. **Loop Lowering**: Loops are fully lowered to labeled if/goto sequences by cc1.
   The labels use format `L<n>_top`, `L<n>_continue`, `L<n>_break`.

2. **Name Mangling**:
   - Global/extern symbols get `_` prefix
   - Static symbols use mangled names
   - Local variables have no prefix

3. **Operator Encoding**: All AST operators use ASCII (0x21-0x7e) to avoid
   encoding issues. Some operators reuse keyword character values since
   keywords never appear in expression-operator context:
   - `x` - sign-extend (SEXT) - reuses EXTERN
   - `(` - pre-increment - reuses LPAR
   - `)` - post-increment - reuses RPAR
   - `{` - pre-decrement - reuses BEGIN
   - `}` - post-decrement - reuses END
   - `e` - bitfield extract - reuses ENUM
   - `f` - bitfield assign - reuses FLOAT
   - `o` - subtract-equals - reuses AUTO
   - `a` - and-equals - reuses STRUCT
   - `m` - mod-equals - reuses UNION
   - `Y` - memory copy
   - `W` - widen (zero-extend)
   - `N` - narrow (truncate)

4. **Width Annotations**: Operators that produce values have a type suffix
   indicating the result size. This is used for code generation.

5. **Expression Flags**: The type suffix also encodes signedness:
   - Lowercase (b,s,l) = signed
   - Uppercase (B,S,L) = unsigned
   - `p` = pointer (unsigned)
