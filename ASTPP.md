# AST Pretty Printer

A standalone tool for formatting ccc compiler AST output in human-readable form.

## Purpose

The AST pretty printer (`astpp.lisp`) converts the compact paren-free hex AST
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

Requires SBCL (Steel Bank Common Lisp):

```bash
# Debian/Ubuntu
sudo apt-get install sbcl

# macOS
brew install sbcl

# Arch Linux
sudo pacman -S sbcl
```

The script is already executable and ready to use.

## Usage

### Basic Usage

```bash
# Generate AST from C source
make program.ast

# Pretty print the AST
./astpp.lisp program.ast
```

### Filter Mode (Reading from stdin)

The pretty printer can act as a Unix filter, reading from stdin if no filename
is provided:

```bash
# Use as filter in a pipeline
./cc1 -DCCC -i./include -I. -E program.c | ./astpp.lisp

# Combine with stderr (cc1 outputs AST to stdout, diagnostics to stderr)
./cc1 -DCCC -i./include -I. -E program.c 2>&1 | ./astpp.lisp
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
./astpp.lisp program.ast
```

## Output Format

The pretty printer outputs:

1. **Function definitions** with parameters and return types
2. **Statement structure** with proper indentation showing nesting
3. **Expression trees** with operator names and type annotations
4. **String literals** with their definitions
5. **Global variables** with types and initializers

### Example Output (Pretty Printed)

```
FUNCTION main() -> _short_
{
  BLOCK {
    DECL result : _short_
    EXPR:
      (ASSIGN:short
        $result
        (NARROW:short 42))
    RETURN (DEREF:short $result)
  }
}
```

## Features

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

### Statement Types

| AST | Pretty Name | Description |
|-----|-------------|-------------|
| `B` | BLOCK | Block statement |
| `E` | EXPR | Expression statement |
| `R` | RETURN | Return statement |
| `I` | IF | If/else conditional |
| `W` | WHILE | While loop |
| `F` | FOR | For loop |
| `D` | DO-WHILE | Do-while loop |
| `S` | SWITCH | Switch statement |
| `C` | CASE | Case label |
| `O` | DEFAULT | Default label |
| `K` | BREAK | Break statement |
| `N` | CONTINUE | Continue statement |
| `G` | GOTO | Goto statement |
| `L` | LABEL | Label definition |
| `d` | DECL | Variable declaration |

### Type Width Annotations

The pretty printer shows type widths on operations:

| Width | Name | Size | Type |
|-------|------|------|------|
| `:b` | byte | 1 byte | char |
| `:s` | short | 2 bytes | short, int |
| `:l` | long | 4 bytes | long |
| `:p` | ptr | 2 bytes | pointer |
| `:f` | float | 4 bytes | float |
| `:d` | double | 8 bytes | double |

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
./astpp.lisp test.ast
```

**Output:**
```
FUNCTION main() -> _short_
{
  BLOCK {
    DECL x : _short_
    EXPR:
      (ASSIGN:short $x (NARROW:short 10))
    RETURN (ADD (DEREF:short $x) 5)
  }
}
```

### Example 2: Control Flow

**Source:**
```c
int factorial(int n) {
    if (n <= 1) {
        return 1;
    }
    return n * factorial(n - 1);
}
```

**Output shows:**
- IF statement with condition
- Nested blocks
- Recursive function call
- Type conversions

### Example 3: Switch Statement

**Source:**
```c
int classify(int x) {
    switch (x) {
        case 0: return 10;
        case 1: return 20;
        default: return 99;
    }
}
```

**Output shows:**
- SWITCH with expression
- CASE labels with values
- DEFAULT label
- BREAK statements

## Implementation

The pretty printer is implemented in Common Lisp and:

1. **Reads the AST** using a custom parser for the paren-free hex format
2. **Handles special characters** like `|` and escape sequences in strings
3. **Translates widths** to readable names (`:s` -> `:short`)
4. **Formats recursively** using indentation tracking
5. **Annotates operators** with human-readable names
6. **Displays type widths** on memory operations

## Debugging with Pretty Printer

1. Generate AST: `make test.ast`
2. Pretty print: `./astpp.lisp test.ast`
3. Verify structure matches expected AST

## Tips

### Reduce Verbosity

For complex expressions, focus on specific parts:

```bash
# Extract only function definitions
./astpp.lisp program.ast | grep -A 20 "FUNCTION"

# Show only global variables
./astpp.lisp program.ast | grep "GLOBAL"

# Find specific variable references
./astpp.lisp program.ast | grep "\$myvar"
```

### Compare ASTs

```bash
# Compare two versions
./astpp.lisp old.ast > /tmp/old.txt
./astpp.lisp new.ast > /tmp/new.txt
diff -u /tmp/old.txt /tmp/new.txt
```

### Pipe to Editor

```bash
# Open in editor for exploration
./astpp.lisp program.ast | vim -
```

## License

Same as the ccc compiler project.
