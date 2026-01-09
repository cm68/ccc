# CCC AST Binary Format

This document describes the binary AST format produced by cc1 and consumed by cc2.

## Overview

The AST is a **compact binary format** using single-byte opcodes and little-endian
multi-byte values. Token values from `lexeme.h` are used directly as opcodes.
Names are length-prefixed with a single byte.

**Important**: cc1 performs all preprocessing internally. The AST output contains
no preprocessor directives - all macros are expanded, conditionals resolved, and
includes processed before AST emission. Loops (while, do, for) are lowered to
labeled if/goto sequences by the preprocessor.

## Encoding Primitives

### Numbers
- **1 byte**: `emit1(b)` - single unsigned byte
- **2 bytes**: `emit2(w)` - little-endian 16-bit unsigned
- **4 bytes**: `emit4(l)` - little-endian 32-bit signed (two's complement)

### Names
```
<1-byte-len><ascii-chars>
```
Emitted via `emitS(name)` - length byte followed by raw characters.
Example: `\x05hello` = "hello" (5 characters)

### Type/Size Suffixes
Single ASCII character after operators indicating operand type:
- `b` - signed byte (1 byte)
- `B` - unsigned byte
- `s` - signed short/int (2 bytes)
- `S` - unsigned short/int
- `l` - signed long (4 bytes)
- `L` - unsigned long
- `p` - pointer (2 bytes, treated as short)
- `f` - float (4 bytes)
- `d` - double (4 bytes, same as float)
- `v` - void

Note: Pointers emit as `s` (short) since they're 16-bit on Z80.

---

## Token Values (from lexeme.h)

Key token values used in the AST:

### Delimiters
| Value | Name | Description |
|-------|------|-------------|
| 0 | E_O_F | End of file |
| 1 | SEMI | Semicolon |
| 2 | BEGIN | `{` left brace |
| 3 | END | `}` right brace |
| 4 | LBRACK | `[` left bracket |
| 5 | RBRACK | `]` right bracket |
| 9 | COMMA | Comma |

### Terminals
| Value | Name | Description |
|-------|------|-------------|
| 20 | SYM | Symbol reference |
| 35 (`#`) | AST_CONST | Numeric constant |

### Operators
| Value | Name | Description |
|-------|------|-------------|
| 30 | INCR | `++` |
| 31 | DECR | `--` |
| 34 | BANG | `!` logical NOT |
| 36 | STAR | `*` multiply |
| 38 | TWIDDLE | `~` bitwise NOT |
| 40 | PLUS | `+` add |
| 41 | MINUS | `-` subtract |
| 43 | DIV | `/` divide |
| 44 | MOD | `%` modulo |
| 45 | RSHIFT | `>>` right shift |
| 46 | LSHIFT | `<<` left shift |
| 47 | AND | `&` bitwise AND |
| 48 | OR | `\|` bitwise OR |
| 49 | XOR | `^` bitwise XOR |
| 53 | LAND | `&&` logical AND |
| 54 | LOR | `\|\|` logical OR |
| 60 | EQ | `==` equal |
| 61 | NEQ | `!=` not equal |
| 62 | LE | `<=` less or equal |
| 63 | LT | `<` less than |
| 80 | ASSIGN | `=` assignment |
| 90 | QUES | `?:` ternary |

### Compound Assignment
| Value | Name |
|-------|------|
| 70 | PLUSEQ `+=` |
| 71 | SUBEQ `-=` |
| 72 | MULTEQ `*=` |
| 73 | DIVEQ `/=` |
| 74 | MODEQ `%=` |
| 75 | RSHIFTEQ `>>=` |
| 76 | LSHIFTEQ `<<=` |
| 77 | ANDEQ `&=` |
| 78 | OREQ `\|=` |
| 79 | XOREQ `^=` |

### Internal Tokens (200+)
| Value | Name | Description |
|-------|------|-------------|
| 201 | DEREF | Memory dereference |
| 203 | NEG | Unary minus |
| 205 | CALL | Function call |
| 206 | NARROW | Narrow (truncate to smaller type) |
| 207 | WIDEN | Widen (zero-extend unsigned) |
| 208 | SEXT | Sign-extend |
| 210 | PREINC | Pre-increment `++x` |
| 211 | POSTINC | Post-increment `x++` |
| 212 | PREDEC | Pre-decrement `--x` |
| 213 | POSTDEC | Post-decrement `x--` |
| 214 | BFEXTRACT | Bitfield extract |
| 215 | BFASSIGN | Bitfield assign |
| 216 | REGVAR | Register variable |
| 217 | LOCALVAR | Local variable (stack) |
| 220 | URSHIFT | Unsigned right shift |

---

## Top-Level Declarations

### Function Definition
```
'F' <rettype> <name> <param_count> <local_count> <frm_size> <params...> <locals...> <body>
```

- `'F'` - literal byte 0x46
- `rettype` - single byte type suffix for return type
- `name` - length-prefixed function name
- `param_count` - 1-byte count of parameters
- `local_count` - 1-byte count of local variables (hoisted from all blocks)
- `frm_size` - 1-byte frame size (bytes for stack locals)
- `params` - sequence of parameter declarations
- `locals` - sequence of local variable declarations
- `body` - statement (always a Block)

**Declaration format**: `'d' <type> <name> <reg> <off>`
- `'d'` - literal byte 0x64
- `type` - single byte type suffix
- `name` - length-prefixed name
- `reg` - 1-byte register allocation (see Register Allocation below)
- `off` - 1-byte signed frame offset (params positive, locals negative)

Example binary (hex dump):
```
46 73 06 5f 6d 61 69 6e 02 01 04
```
= `F` `s` `\x06_main` `\x02` `\x01` `\x04` = function returning short, named "_main", 2 params, 1 local, 4-byte frame

---

## Statements

Statement opcodes use ASCII letters for readability in hex dumps.

### Block
```
'B' 00 <stmt_count> <stmts...>
```
- `'B'` - literal byte 0x42
- `00` - always zero (locals hoisted to function prolog)
- `stmt_count` - 1-byte count of statements

### If Statement
```
'I' <nlabels> <cond> <then> <has_else> [<else>]
```
- `'I'` - literal byte 0x49
- `nlabels` - 1-byte count of intermediate labels for ||/&& short-circuit
- `cond` - condition expression
- `then` - then-branch statement
- `has_else` - 1-byte: 0x00 (no else) or 0x01 (has else)
- `else` - else-branch statement (only if has_else=0x01)

**Note**: `has_else` comes AFTER the then block, not before.

### Expression Statement
```
'E' <expr>
```

### Return Statement
```
'R' <has_value> [<expr>]
```
- `has_value` - 0x00 (void return) or 0x01 (has value)

### Label
```
'L' <name>
```

### Goto
```
'G' <name>
```

### Switch Statement
```
'S' <has_label> <case_count> <expr> <cases...>
```
- `has_label` - always 0x00 (cpp handles break lowering)
- `case_count` - 1-byte count of case/default labels

### Case Label
```
'C' <stmt_count> <value_expr> <stmts...>
```

### Default Label
```
'O' <stmt_count> <stmts...>
```

### Empty Statement
```
';'
```
Literal byte 0x3B

---

## Expressions

All expressions use prefix notation with type suffix after the operator.

### Constants
```
'#' <type> <4-byte-value>
```
- `'#'` - literal byte 0x23 (AST_CONST)
- `type` - single byte type suffix
- `value` - 4-byte little-endian signed value

Example: `23 73 42 00 00 00` = `#` `s` `66` = 66 as short

### Symbol Reference (Global/Extern)
```
SYM <name>
```
- `SYM` - byte value 20 (0x14)
- `name` - length-prefixed symbol name (with `_` prefix for globals)

### Local Variable (Stack)
```
LOCALVAR <type> <offset>
```
- `LOCALVAR` - byte value 217 (0xD9)
- `type` - single byte type suffix
- `offset` - 1-byte signed frame offset

### Register Variable
```
REGVAR <type> <reg>
```
- `REGVAR` - byte value 216 (0xD8)
- `type` - single byte type suffix
- `reg` - 1-byte register number

### Null/Empty Expression
```
'_'
```
Literal byte 0x5F

### Memory Dereference
```
DEREF <type> <addr>
```
- `DEREF` - byte value 201 (0xC9)

### Assignment
```
ASSIGN <type> <lvalue> <rvalue>
```
- `ASSIGN` - byte value 80 (0x50)

### Binary Operators
```
<op> <type> <left> <right>
```
Type suffix followed by left and right operands.

**Note**: GT (>) and GE (>=) are normalized to LT (<) and LE (<=) by swapping operands.
The AST never contains GT or GE operators.

### Unary Operators
```
<op> <type> <operand>
```
- `DEREF` (201) - memory dereference
- `BANG` (34) - logical NOT
- `TWIDDLE` (38) - bitwise NOT
- `NEG` (203) - unary minus (negation)

### Type Conversions
```
NARROW <type> <expr>   - narrow (truncate to smaller type)
WIDEN <type> <expr>    - widen (zero-extend unsigned)
SEXT <type> <expr>     - sign-extend
```

### Increment/Decrement
```
<op> <type> <expr> <amount>
```
- `op` - PREINC (210), POSTINC (211), PREDEC (212), POSTDEC (213)
- `amount` - 2-byte little-endian increment value

### Function Call
```
CALL <rettype> <argc> <func> <args...>
```
- `CALL` - byte value 205 (0xCD)
- `rettype` - single byte return type suffix
- `argc` - 1-byte argument count
- `func` - function expression
- `args` - argument expressions (argc count)

### Ternary Operator
```
QUES <type> <cond> <then> <else>
```
- `QUES` - byte value 90 (0x5A)

### Bitfield Extract
```
BFEXTRACT <offset> <width> <addr>
```
- `BFEXTRACT` - byte value 214 (0xD6)
- `offset` - 1-byte bit offset (0-7)
- `width` - 1-byte bit width

### Bitfield Assign
```
BFASSIGN <offset> <width> <addr> <value>
```
- `BFASSIGN` - byte value 215 (0xD7)

### Comma Operator
```
COMMA <type> <left> <right>
```
- `COMMA` - byte value 9 (0x09)

---

## Initializers

### Array Initializer
```
LBRACK <width> <count> <items...> RBRACK
```
- `LBRACK` - byte value 4 (0x04)
- `width` - element type suffix
- `count` - 1-byte element count
- `items` - initializer expressions
- `RBRACK` - byte value 5 (0x05)

### Struct Initializer
```
BEGIN <count> <items...> END
```
- `BEGIN` - byte value 2 (0x02)
- `count` - 1-byte field count
- `items` - field initializer expressions
- `END` - byte value 3 (0x03)

---

## What's NOT in the AST

The following are emitted directly to the assembly output file, not to the AST:

1. **String literals** - emitted as assembly `.db` directives via `asmLabel`/`asmDb`
2. **Global variables** - emitted as assembly `.bss`/`.ds` directives
3. **Inline assembly** - emitted directly to assembly output
4. **Loop structures** - lowered to if/goto by preprocessor

---

## Register Allocation

Register allocation is computed by pass1 (cc1) based on variable usage analysis
and communicated to pass2 (cc2) via the AST.

### Register Values
| Value | Register | Description |
|-------|----------|-------------|
| 0 | - | No register (on stack) |
| 1 | B | B register (byte) |
| 2 | C | C register (byte) |
| 3 | BC | BC register pair (word) |
| 4 | IX | IX index register (struct pointer) |

### Allocation Strategy
Pass1 analyzes variable usage to determine optimal register allocation:

1. **IX register**: Allocated to pointer variables with struct member accesses
   (e.g., `ptr->field`). IX indexing enables efficient `(ix+n)` addressing.

2. **BC register**: Allocated to the most frequently referenced word variable.
   BC is preserved across function calls on Z80.

3. **B/C registers**: Allocated to frequently referenced byte variables if BC
   is not already allocated as a pair.

---

## Notes

1. **Loop Lowering**: Loops are fully lowered to labeled if/goto sequences by cpp.
   The labels use format `L<n>_top`, `L<n>_continue`, `L<n>_break`.

2. **Name Mangling**:
   - Global/extern symbols get `_` prefix
   - Static symbols use `S<id>` format
   - Local variables use LOCALVAR/REGVAR opcodes (no name in expression)

3. **Operator Normalization**: GT and GE are normalized to LT and LE by
   swapping operands. The AST never contains GT (65) or GE (64) operators.

4. **Width Annotations**: All operators have a type suffix byte indicating
   result type. Lowercase (b,s,l) = signed, Uppercase (B,S,L) = unsigned.
