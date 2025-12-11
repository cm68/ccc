# AST Pretty Printer

A standalone tool for formatting ccc compiler AST output in human-readable form.

## Purpose

The AST pretty printer (`astpp.py`) converts the compact paren-free hex AST
format into a nicely formatted, indented, and annotated representation that's
easier to read and understand.

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

Requires Python 3:

```bash
# Most systems already have Python 3 installed
python3 --version

# Debian/Ubuntu (if needed)
sudo apt-get install python3

# macOS (if needed)
brew install python3
```

The script is already executable and ready to use.

## Usage

### Basic Usage

```bash
# Generate AST from C source
make program.ast

# Pretty print the AST
python3 astpp.py program.ast
# or
./astpp.py program.ast
```

### Filter Mode (Reading from stdin)

The pretty printer can act as a Unix filter, reading from stdin if no filename
is provided:

```bash
# Use as filter in a pipeline
./cc1 -DCCC -i./include -I. -E program.c | ./astpp.py

# Combine with stderr (cc1 outputs AST to stdout, diagnostics to stderr)
./cc1 -DCCC -i./include -I. -E program.c 2>&1 | ./astpp.py
```

This is particularly useful for:
- **Quick inspection**: No intermediate file needed
- **One-liners**: Compile and view AST in a single command
- **Debugging**: Immediately see the AST without managing temporary files

### With ccc Driver

```bash
# Compile to AST (stops before code generation)
./ccc -E program.c

# Pretty print the generated AST
./astpp.py program.ast
```

## Output Format

The pretty printer outputs:

1. **Function definitions** with parameters, return types, frame size, and register allocation
2. **Statement structure** with proper indentation showing nesting
3. **Expression trees** with operator names and type annotations
4. **String literals** with their definitions
5. **Global variables** with types and initializers

### Example Output

```
========================================
AST Pretty Printer Output
========================================

FUNCTION main(argc:short@IY+4, argv:ptr@BC) -> short [frame=0]
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

The pretty printer translates single-character operators to readable names:

| AST | Pretty Name | Description |
|-----|-------------|-------------|
| `M` | DEREF | Dereference (memory read) |
| `=` | ASSIGN | Assignment |
| `+` | ADD | Addition |
| `-` | SUB | Subtraction |
| `*` | MUL | Multiplication |
| `/` | DIV | Division |
| `%` | MOD | Modulo |
| `&` | AND | Bitwise AND |
| `\|` | OR | Bitwise OR |
| `^` | XOR | Bitwise XOR |
| `~` | NOT | Bitwise NOT |
| `y` | LSHIFT | Left shift |
| `w` | RSHIFT | Right shift |
| `<` | LT | Less than |
| `>` | GT | Greater than |
| `L` | LE | Less or equal |
| `g` | GE | Greater or equal |
| `Q` | EQ | Equal |
| `n` | NE | Not equal |
| `j` | LAND | Logical AND |
| `h` | LOR | Logical OR |
| `!` | LNOT | Logical NOT |
| `N` | NARROW | Narrow type conversion |
| `X` | SEXT | Sign extend |
| `W` | WIDEN | Widen type |
| `?` | TERNARY | Ternary conditional |
| `@` | CALL | Function call |
| `Y` | COPY | Structure copy |
| `\` | NEG | Unary negation |
| `'` | ADDR | Address-of |
| `(` | PREINC | Pre-increment |
| `)` | POSTINC | Post-increment |
| `{` | PREDEC | Pre-decrement |
| `}` | POSTDEC | Post-decrement |
| `e` | BFEXT | Bitfield extract |
| `f` | BFSET | Bitfield set |

### Compound Assignment Operators

| AST | Pretty Name |
|-----|-------------|
| `P` | += |
| `T` | *= |
| `o` | -= |
| `m` | %= |
| `a` | &= |
| `0` | <<= |
| `1` | \|= |
| `2` | /= |
| `6` | >>= |

### Statement Types

| AST | Pretty Name | Description |
|-----|-------------|-------------|
| `B` | BLOCK | Block statement |
| `E` | EXPR | Expression statement |
| `R` | RETURN | Return statement |
| `I` | IF | If/else conditional |
| `W` | WHILE | While loop |
| `F` | FOR | For loop |
| `D` | DO | Do-while loop |
| `S` | SWITCH | Switch statement |
| `C` | CASE | Case label |
| `O` | DEFAULT | Default label |
| `K` | BREAK | Break statement |
| `N` | CONTINUE | Continue statement |
| `G` | GOTO | Goto statement |
| `L` | LABEL | Label definition |
| `A` | ASM | Inline assembly |
| `d` | DECL | Variable declaration |

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
./astpp.py test.ast
```

**Output:**
```
FUNCTION main() -> short [frame=2]
  LOCALS: x:short@IY-2
{
  BLOCK {
    EXPR (ASSIGN:short $x 10:short)
    RETURN (ADD:short (DEREF:short $x) 5:short)
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
FUNCTION add(a:short@IY+4, b:short@IY+6) -> short [frame=0]
{
  BLOCK {
    RETURN (ADD:short (DEREF:short $a) (DEREF:short $b))
  }
}
```

### Example 3: Globals and Strings

**Source:**
```c
int counter = 0;
char *msg = "hello";
```

**Output:**
```
GLOBAL counter : short = 0:short
STRING _str0 = "hello\x00"
GLOBAL msg : ptr = $_str0
```

## Implementation

The pretty printer is implemented in Python and:

1. **Reads the AST** using a custom parser for the paren-free hex format
2. **Handles hex-encoded names** with 2-digit length prefix
3. **Translates widths** to readable names (`:s` -> `:short`)
4. **Formats recursively** using indentation tracking
5. **Annotates operators** with human-readable names
6. **Shows register allocation** for parameters and locals

## Debugging with Pretty Printer

1. Generate AST: `make test.ast`
2. Pretty print: `./astpp.py test.ast`
3. Verify structure matches expected AST

## Tips

### Reduce Verbosity

For complex expressions, focus on specific parts:

```bash
# Extract only function definitions
./astpp.py program.ast | grep -A 20 "FUNCTION"

# Show only global variables
./astpp.py program.ast | grep "GLOBAL"

# Find specific variable references
./astpp.py program.ast | grep "\$myvar"
```

### Compare ASTs

```bash
# Compare two versions
./astpp.py old.ast > /tmp/old.txt
./astpp.py new.ast > /tmp/new.txt
diff -u /tmp/old.txt /tmp/new.txt
```

### Pipe to Editor

```bash
# Open in editor for exploration
./astpp.py program.ast | vim -
```

## License

Same as the ccc compiler project.
