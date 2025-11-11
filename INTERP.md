# AST Interpreter for ccc

This is a Common Lisp interpreter that executes the S-expression AST output from the ccc compiler (pass 1) without needing pass 2 (code generation).

## Purpose

The interpreter is primarily a **debugging and validation tool** for the parser and AST generator:

- **Validate parser correctness**: If your C program executes correctly through the interpreter, the parser is producing valid AST
- **Debug AST issues**: Compare expected behavior with actual execution to identify parser bugs
- **Test parser changes**: Quick feedback loop when modifying the parser
- **Verify type conversions**: See if promotions and conversions are happening as expected
- **Check control flow**: Ensure loops, conditionals, and function calls work correctly
- **Prototype features**: Test new language features before implementing code generation

**Key benefit**: You can develop and test pass 1 (parser) completely independently of pass 2 (code generator).

## Requirements

- SBCL (Steel Bank Common Lisp) - recommended
- The ccc compiler (cc1) to generate AST

Note: Also works with CLISP, but SBCL is the primary supported implementation.

## Installation

SBCL is available on most systems:
```bash
# Debian/Ubuntu
sudo apt-get install sbcl

# macOS
brew install sbcl

# Arch Linux
sudo pacman -S sbcl

# Or download from http://www.sbcl.org/
```

## Usage

### Quick Start (Recommended)

Use the `ccc` driver with the `-x` flag:

```bash
# Compile and execute with interpreter
./ccc -x myprogram.c

# Output shows:
# === Pass 1: Parsing myprogram.c ===
# === Executing AST with interpreter ===
# Program exited with code: <exit_code>
# AST saved to: myprogram.ast
```

This automatically:
1. Runs cc1 to generate AST
2. Executes the AST with the interpreter
3. Reports the exit code
4. Saves the AST file for inspection

### Manual Usage

If you want more control:

1. Compile a C program to AST:
```bash
./cc1 -E test_interp.c > test_interp.ast
```

2. Run the interpreter:
```bash
sbcl --script interp.lisp test_interp.ast
```

Or make it executable:
```bash
chmod +x interp.lisp
./interp.lisp test_interp.ast
```

Note: Also works with CLISP if you prefer:
```bash
clisp interp.lisp test_interp.ast
```

### Debugging Workflow

1. Write test program with known expected output
2. Execute: `./ccc -x test.c`
3. Check exit code against expected value
4. If wrong, inspect saved AST file: `cat test.ast`
5. Verify AST structure matches expectations

## Features

### Supported Operations

**Expressions:**
- Arithmetic: `+`, `-`, `*`, `/`, `%`
- Bitwise: `&`, `|`, `^`, `~`, `<<`, `>>`
- Comparison: `<`, `>`, `<=`, `>=`, `==`, `!=`
- Logical: `&&`, `||`, `!`
- Assignment: `=`
- Compound assignments: `+=`, `-=`, `*=`, `/=`, `%=`, etc.
- Increment/decrement: `++`, `--` (prefix and postfix)
- Ternary conditional: `? :`
- Function calls
- Type conversions: NARROW, SEXT, WIDEN

**Statements:**
- Expression statements
- `if`/`else`
- `while` loops
- `for` loops
- `do-while` loops
- `return`
- `break`
- `continue`
- Block statements with scoping

**Declarations:**
- Global variables
- Local variables
- Function definitions
- Function parameters
- String literals with full escape sequence support

### Limitations

**Not Yet Implemented:**
- Pointers and pointer arithmetic (addresses are symbolic)
- Arrays (partially supported)
- Structs and unions
- Switch/case statements (basic support only)
- Goto and labels
- Type checking (assumes correct AST)
- Memory model is simplified (no actual byte-level memory)

## Implementation Notes

### Memory Model

The interpreter uses a simplified memory model:
- Global variables stored in a hash table
- Local variables in stack frames (one frame per scope)
- No actual byte-level memory or addresses
- Pointers are symbolic (values passed through)

### Variable Naming

The interpreter handles the AST symbol prefixes:
- `$_name` - Global/extern variables
- `$Sname` - Static variables
- `$Aname` - Function arguments
- `$name` - Local variables
- `$___name` - String literals

### Type Widths

The interpreter recognizes width annotations:
- `:b` - byte (1 byte)
- `:s` - short (2 bytes)
- `:l` - long (4 bytes)
- `:p` - pointer (2 bytes)
- `:f` - float (4 bytes)
- `:d` - double (8 bytes)

Type conversions (NARROW, SEXT, WIDEN) are implemented but simplified.

### Escape Sequence Preprocessing

C string literals in the AST contain escape sequences (`\n`, `\t`, `\"`, `\\`, etc.) that must be processed correctly by the Lisp reader. The interpreter preprocesses the AST text before passing it to the reader:

**Strategy:**
- Backslashes inside strings (except `\"`) are doubled: `\n` → `\\n`
- Escaped quotes stay as-is: `\"` → `\"` (reader produces quote in string)
- Vertical bars outside strings are escaped: `|` → `\|` (prevents Lisp multiple-escape interpretation)

**Example transformation:**
```
Input AST:  (s str0 "line1\nline2\ttab\"quote\\backslash")
After preprocessing: (s str0 "line1\\nline2\\ttab\"quote\\\\backslash")
Lisp reader produces: "line1\nline2\ttab"quote\\backslash"
process-escape-sequences converts: actual newline, tab, quote, backslash
```

**Why this works:**
1. AST contains C escape sequences as written in source
2. Preprocessing doubles backslashes (except before quotes) for Lisp reader
3. Lisp reader interprets `\"` as quote, `\\` as single backslash
4. `process-escape-sequences` converts `\n`, `\t`, etc. to actual characters
5. Final string has correct C semantics

**Implementation:** See preprocessing loop in `interpret-file` function (interp.lisp:443-473).

### Function Calls

Functions are called with proper stack frames:
1. New local frame created
2. Arguments evaluated and bound to parameters
3. Function body executed
4. Return value captured
5. Frame popped

Recursive calls work correctly.

## Debugging

To see what the interpreter is doing, you can add debug prints:

```lisp
;; In interp.lisp, add to eval-expr:
(format t "Evaluating: ~A~%" expr)
```

Or examine the loaded program:

```lisp
;; After loading AST
(maphash (lambda (k v) (format t "Global ~A = ~A~%" k v)) *globals*)
(maphash (lambda (k v) (format t "Function ~A~%" k)) *functions*)
```

## Testing

Test the interpreter with simple programs first:

```c
int main() {
    return 42;
}
```

Then try more complex features:
- Function calls
- Recursion (factorial, fibonacci)
- Loops and conditionals
- Local variables
- String literals with escape sequences

### Current Test Results

The following tests in `tests/` pass with the interpreter:
- **arith_widths.c** - Multi-width arithmetic (char, short, long)
- **array_init.c** - Array initialization
- **simple_arith.c** - Basic arithmetic operations
- **string_literals.c** - String literals with complex escape sequences

Run all interpreter tests:
```bash
bash /tmp/run_interp_tests.sh
```

Or test individual files:
```bash
./ccc -x tests/arith_widths.c
```

## Interactive REPL

You can also use the interpreter interactively in SBCL:

```lisp
$ sbcl
* (load "interp.lisp")
* (in-package :ccc-interp)
* (interpret-file "test_interp.ast")
Program exited with code: 120
* (gethash "main" *functions*)  ; Inspect main function
* (gethash "result" *globals*)   ; Check global values
```

## Extending the Interpreter

To add new operators or statements:

1. Add pattern matching in `eval-expr` or `eval-statement`
2. Implement the semantics
3. Test with a simple C program

Example - adding a new operator:
```lisp
;; In eval-expr, add:
((:NEW-OP)
 (my-implementation (eval-expr (second expr))))
```

## Architecture

The interpreter has these main components:

- **Memory model**: `*globals*`, `*locals*`, `*functions*`, `*strings*`
- **Expression evaluator**: `eval-expr` - recursive evaluation
- **Statement executor**: `eval-statement` - sequential execution
- **Function caller**: `funcall-ast` - manages stack frames
- **AST loader**: `load-ast` - populates memory from S-expressions
- **Type utilities**: Width handling, conversions

## Performance

This is an interpreter, not a compiler, so performance is limited:
- Suitable for testing small programs
- Recursive functions work but are slower than native code
- No optimization is performed
- Memory usage grows with program complexity

For production use, implement pass 2 (code generation) instead.

## License

Same as the ccc compiler project.
