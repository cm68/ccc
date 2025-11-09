# AST Pretty Printer

A standalone tool for formatting ccc compiler AST output in human-readable form.

## Purpose

The AST pretty printer (`astpp.lisp`) converts the S-expression AST format into a nicely formatted, indented, and annotated representation that's easier to read and understand.

**Use cases:**
- **Debugging the parser**: Visually inspect the AST structure to verify correct parsing
- **Understanding code generation**: See exactly what the parser produces before code generation
- **Learning the AST format**: Study examples to understand the AST structure
- **Documenting compiler behavior**: Generate readable examples for documentation
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
./cc1 -E program.c > program.ast

# Pretty print the AST
./astpp.lisp program.ast
```

### With ccc Driver

```bash
# Compile to AST (stops before code generation)
./ccc -E program.c

# Pretty print the generated AST
./astpp.lisp program.ast
```

### Raw Mode (No Translation)

Use `--raw` or `-r` to keep original operators while still getting proper indentation:

```bash
# Pretty print with raw operators (M, =, +, etc.)
./astpp.lisp --raw program.ast

# Or short form
./astpp.lisp -r program.ast
```

**When to use raw mode:**
- **Reformatting AST files**: Output is valid S-expression AST that can be read back
- You're familiar with the AST format and don't need translation
- You want to minimize differences when comparing ASTs
- You're writing code that parses the output
- You want the most compact representation

**Raw mode outputs valid AST:**
```bash
# Reformat an AST file (canonicalize spacing/indentation)
./astpp.lisp --raw messy.ast > clean.ast

# The output can be fed back to the compiler/interpreter
sbcl --script interp.lisp clean.ast
```

**Comparison:**

Normal mode:
```
EXPR:
  (ASSIGN:short $a (NARROW:short 10))
IF ((GT (SEXT:long (DEREF:short $a)) 5))
  BLOCK {
```

Raw mode:
```
E:
  (=:short $a (N:short 10))
I ((> (X:long (M:short $a)) 5))
  B {
```

### One-Line Pipeline

```bash
# Generate and immediately view
./cc1 -E program.c | sbcl --script astpp.lisp /dev/stdin

# With raw mode
./cc1 -E program.c | sbcl --script astpp.lisp /dev/stdin --raw
```

## Output Format

The pretty printer outputs:

1. **Function definitions** with parameters and return types
2. **Statement structure** with proper indentation showing nesting
3. **Expression trees** with operator names and type annotations
4. **String literals** with their definitions
5. **Global variables** with types and initializers

### Example Input (AST)

```lisp
(f main () _short_
  (B (d result _short_) (E (=:s $result (N:s 42))) (R (M:s $result))))
```

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
./cc1 -E test.c | ./astpp.lisp /dev/stdin
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

1. **Reads the AST** using Lisp's reader with custom preprocessing
2. **Handles special characters** like `|` and escape sequences in strings
3. **Translates colons** to underscores for width annotations (`:s` → `_s`)
4. **Formats recursively** using indentation tracking
5. **Annotates operators** with human-readable names
6. **Displays type widths** on memory operations

## Debugging with Pretty Printer

### Workflow

1. **Write C code** with specific construct to test
2. **Generate AST** with `./cc1 -E test.c > test.ast`
3. **Pretty print** with `./astpp.lisp test.ast`
4. **Verify structure** matches expected AST
5. **Compare** with known-good examples

### Example Debugging Session

```bash
# Test if ternary operator works correctly
echo 'int main() { return 1 ? 10 : 20; }' > /tmp/test.c
./cc1 -E /tmp/test.c > /tmp/test.ast
./astpp.lisp /tmp/test.ast

# Look for TERNARY operator in output
# Verify condition, true branch, false branch are correct
```

### Common Debugging Patterns

**Check operator precedence:**
```bash
echo 'int main() { return 1 + 2 * 3; }' > /tmp/test.c
./astpp.lisp <(./cc1 -E /tmp/test.c)
# Should show MUL nested inside ADD
```

**Check type conversions:**
```bash
echo 'int main() { char c = 5; int i = c; return i; }' > /tmp/test.c
./astpp.lisp <(./cc1 -E /tmp/test.c)
# Look for SEXT (sign extend) from byte to short
```

**Check control flow:**
```bash
./cc1 -E tests/test_switch.c > /tmp/switch.ast
./astpp.lisp /tmp/switch.ast
# Verify case labels, fallthrough, default case
```

## Comparison with Interpreter

The AST pretty printer and interpreter (`interp.lisp`) serve different purposes:

| Tool | Purpose | Output |
|------|---------|--------|
| `astpp.lisp` | Format AST for reading | Human-readable structure |
| `interp.lisp` | Execute AST | Program exit code |

**Use pretty printer when:**
- You want to see AST structure
- Debugging parser output
- Learning the AST format
- Creating documentation

**Use interpreter when:**
- You want to test program behavior
- Validating parser produces correct semantics
- Testing without code generator

**Use both when:**
- AST looks correct but interpreter gives wrong result
- Pretty print to verify structure, then interpret to verify semantics

## Performance

The pretty printer is fast enough for interactive use:

- Small programs (<100 lines): Instant
- Medium programs (<1000 lines): <1 second
- Large programs (>1000 lines): Few seconds

For very large ASTs, you can pipe through `less` for paging:

```bash
./astpp.lisp large_program.ast | less
```

## Limitations

- **No expression simplification**: Shows AST as-is without folding constants
- **No type checking**: Displays types but doesn't validate correctness
- **No semantic analysis**: Only shows structure, not meaning
- **Verbose for complex expressions**: Deeply nested expressions may be hard to read

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
# Compare two versions (normal mode)
./astpp.lisp old.ast > /tmp/old.txt
./astpp.lisp new.ast > /tmp/new.txt
diff -u /tmp/old.txt /tmp/new.txt

# For minimal diff noise, use raw mode
./astpp.lisp --raw old.ast > /tmp/old_raw.txt
./astpp.lisp --raw new.ast > /tmp/new_raw.txt
diff -u /tmp/old_raw.txt /tmp/new_raw.txt
```

**Tip:** Raw mode produces more compact output with fewer long words to diff, making it easier to spot actual structural changes versus just reformatting.

### Pipe to Editor

```bash
# Open in editor for exploration
./astpp.lisp program.ast | vim -
```

## License

Same as the ccc compiler project.
