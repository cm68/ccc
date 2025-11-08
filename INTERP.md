# AST Interpreter for ccc

This is a Common Lisp interpreter that executes the S-expression AST output from the ccc compiler (pass 1) without needing pass 2 (code generation).

## Purpose

The interpreter allows you to:
- Test C programs without implementing the code generator
- Verify that the AST is correctly formed
- Debug the parser by seeing actual execution results
- Prototype and test compiler features quickly

## Requirements

- SBCL (Steel Bank Common Lisp) or another Common Lisp implementation
- The ccc compiler (cc1) to generate AST

## Installation

SBCL is available on most systems:
```bash
# Debian/Ubuntu
sudo apt-get install sbcl

# macOS
brew install sbcl

# Or download from http://www.sbcl.org/
```

## Usage

### Basic Usage

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

### Example Session

```bash
# Compile test program
./cc1 -E test_interp.c > test_interp.ast

# Run interpreter
./interp.lisp test_interp.ast
Program exited with code: 120
```

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
- String literals (partial support)

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

The included `test_interp.c` tests recursion with factorial.

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
