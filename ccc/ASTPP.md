# AST Pretty Printer

A standalone tool for formatting ccc compiler AST output in human-readable form.

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
- **Documenting compiler behavior**: Generate readable examples for
  documentation
- **Comparing ASTs**: Diff pretty-printed output to see changes between versions

## Installation

Built automatically with the compiler:

```bash
make            # Builds astpp along with cc1 and cc2
make install    # Installs to root/bin/astpp
```

## Usage

### Basic Usage

```bash
# Generate AST from C source
make program.ast

# Pretty print the AST
./astpp program.ast
```

### Filter Mode (Reading from stdin)

The pretty printer can act as a Unix filter, reading from stdin if no filename
is provided:

```bash
# Use as filter in a pipeline
./cc1 -DCCC -i./include -I. -E program.c | ./astpp

# Combine with stderr (cc1 outputs AST to stdout, diagnostics to stderr)
./cc1 -DCCC -i./include -I. -E program.c 2>&1 | ./astpp
```

This is particularly useful for:
- **Quick inspection**: No intermediate file needed
- **One-liners**: Compile and view AST in a single command
- **Debugging**: Immediately see the AST without managing temporary files

### With ccc Driver

```bash
# Compile and generate .pp file automatically
./ccc -k -P -c program.c    # Creates program.ast, program.pp, program.s, program.o

# Or manually: compile to AST, then pretty print
./ccc -k -c program.c       # Creates program.ast
./astpp program.ast         # Pretty print to stdout
```

## Output Format

The pretty printer outputs:

1. **Function definitions** with parameters, return types, frame size, and register allocation
2. **Statement structure** with proper indentation showing nesting
3. **Expression trees** with operator names and type annotations
4. **Global variables** with types and initializers (if present in AST)

### Example Output

```
========================================
AST Pretty Printer Output (binary)
========================================

FUNCTION _main(argc:short@IY+4, argv:ptr@BC) -> short [frame=0]
  LOCALS: result:short@IY-2
{
  BLOCK {
    EXPR (ASSIGN:short $result 42:short)
    RETURN (DEREF:short $result)
  }
}

========================================
```

## Features

### Register Allocation Display

The pretty printer shows register assignments for parameters and locals:

| Register | Name | Description |
|----------|------|-------------|
| `-` | REG_NONE | No register (stack only) |
| `B` | REG_B | B register |
| `C` | REG_C | C register |
| `BC` | REG_BC | BC register pair |
| `IX` | REG_IX | IX register |

Stack offsets are shown as `IY+N` or `IY-N`.

### Operator Translation

The pretty printer translates binary token values to readable names:

| Token | Pretty Name | Description |
|-------|-------------|-------------|
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
| 206 | NARROW | Narrow type conversion |
| 208 | SEXT | Sign extend |
| 207 | WIDEN | Widen type |
| 90 | TERNARY | Ternary conditional |
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

**Note**: GT (>) and GE (>=) are normalized to LT (<) and LE (<=) by cc1,
so they never appear in the AST.

### Compound Assignment Operators

| Token | Pretty Name |
|-------|-------------|
| 70 | += |
| 71 | -= |
| 72 | *= |
| 73 | /= |
| 74 | %= |
| 75 | >>= |
| 76 | <<= |
| 77 | &= |
| 78 | \|= |
| 79 | ^= |

### Statement Types

| Opcode | Pretty Name | Description |
|--------|-------------|-------------|
| `B` | BLOCK | Block statement |
| `E` | EXPR | Expression statement |
| `R` | RETURN | Return statement |
| `I` | IF | If/else conditional |
| `S` | SWITCH | Switch statement |
| `C` | CASE | Case label |
| `O` | DEFAULT | Default label |
| `G` | GOTO | Goto statement |
| `L` | LABEL | Label definition |
| `d` | DECL | Variable declaration |

**Note**: Loop statements (WHILE, FOR, DO) are lowered to if/goto sequences
by the preprocessor and do not appear in the AST.

### Type Width Annotations

The pretty printer shows type widths on operations:

| Width | Name | Size | Type |
|-------|------|------|------|
| `b` | byte | 1 byte | signed char |
| `B` | ubyte | 1 byte | unsigned char |
| `s` | short | 2 bytes | short, int |
| `S` | ushort | 2 bytes | unsigned short/int |
| `l` | long | 4 bytes | long |
| `L` | ulong | 4 bytes | unsigned long |
| `p` | ptr | 2 bytes | pointer |
| `f` | float | 4 bytes | float |
| `d` | double | 4 bytes | double |
| `v` | void | 0 bytes | void |

## Examples

### Example 1: Simple Function

**Source (test.c):**
```c
int main() {
    int x = 10;
    return x + 5;
}
```

**Command:**
```bash
make test.ast
./astpp test.ast
```

**Output:**
```
FUNCTION _main() -> short [frame=2]
  LOCALS: x:short@IY-2
{
  BLOCK {
    EXPR (ASSIGN:short (LOCALVAR:short -2) 10:short)
    RETURN (ADD:short (LOCALVAR:short -2) 5:short)
  }
}
```

### Example 2: Function with Parameters

**Source:**
```c
int add(int a, int b) {
    return a + b;
}
```

**Output:**
```
FUNCTION _add(a:short@IY+4, b:short@IY+6) -> short [frame=0]
{
  BLOCK {
    RETURN (ADD:short (LOCALVAR:short 4) (LOCALVAR:short 6))
  }
}
```

## Implementation

The pretty printer is implemented in C (~800 lines) and:

1. **Reads the binary AST** using single-byte and multi-byte readers
2. **Handles length-prefixed names** (1 byte length + characters)
3. **Translates token values** to readable operator names
4. **Formats recursively** using indentation tracking
5. **Shows register allocation** for parameters and locals

Source: `astpp.c` in the ccc/ directory.

## Debugging with Pretty Printer

1. Generate AST: `make test.ast`
2. Pretty print: `./astpp test.ast`
3. Verify structure matches expected AST

## Tips

### Reduce Verbosity

For complex expressions, focus on specific parts:

```bash
# Extract only function definitions
./astpp program.ast | grep -A 20 "FUNCTION"

# Show only global variables
./astpp program.ast | grep "GLOBAL"

# Find specific variable references
./astpp program.ast | grep "LOCALVAR"
```

### Compare ASTs

```bash
# Compare two versions
./astpp old.ast > /tmp/old.txt
./astpp new.ast > /tmp/new.txt
diff -u /tmp/old.txt /tmp/new.txt
```

### Pipe to Editor

```bash
# Open in editor for exploration
./astpp program.ast | vim -
```

## License

Same as the ccc compiler project.
