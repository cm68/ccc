# AST Output Format

The compiler outputs a parseable AST format in S-expression notation for consumption by the second pass (code generator).

## Format Overview

Each function is emitted as:
```
; Function: function_name
(f name (param1:type param2:type ...) return_type
  statement)
```

## Expression Format

All expressions use prefix notation (Polish notation):

- **Constants**: Just the numeric value (e.g., `42`, `0`, `-1`)
- **Symbols/Variables**: Prefixed with `$` plus scope indicator:
  - **Extern/Global variables**: `$_name` (underscore prefix)
  - **Static variables**: `$Sname` (S prefix)
  - **Function arguments**: `$Aname` (A prefix)
  - **Local variables**: `$name` (no additional prefix)
- **String literals**: `S` followed by string table index (e.g., `S0`, `S1`)
- **Binary operators**: `(op left right)` (e.g., `(+ $x $y)`)
- **Unary operators**: `(op operand)` (e.g., `(NEG $x)`)

### Operators

All C operators are represented by their token character:
- Arithmetic: `+`, `-`, `*`, `/`, `%`
- Comparison: `<`, `>`, `L` (<=), `g` (>=), `Q` (==), `n` (!=)
- Bitwise: `&`, `|`, `^`, `~`, `y` (<<), `w` (>>)
- Logical: `j` (&&), `h` (||), `!`
- Assignment: `=`
- Function call: `@` - `(@ function arg1 arg2 ...)`

## Statement Format

Statements use single-character codes:

- **Block**: `(B stmt1 stmt2 ...)`
- **If**: `(I condition then-stmt [else-stmt])`
- **While**: `(W condition body-stmt)`
- **Do-while**: `(D body-stmt condition)`
- **For**: `(F init-expr cond-expr incr-expr body-stmt)`
- **Switch**: `(S expr body-stmt)`
- **Case**: `(C value body-stmt)`
- **Default**: `(O body-stmt)`
- **Return**: `(R [expr])`
- **Break**: `(K)`
- **Continue**: `(N)`
- **Goto**: `(G label)`
- **Label**: `(L label)`
- **Expression statement**: `(E expr)`
- **Empty statement**: `(;)`

## Examples

### Simple Addition
```c
int add(int x, int y) {
    return x + y;
}
```
Emits:
```
(f add (y:_short_ x:_short_) _short_
  (B (R (+ $Ax $Ay))))
```
Note: `$Ax` and `$Ay` have the `A` prefix because they are function arguments.

### Factorial with Recursion
```c
int factorial(int n) {
    if (n < 2) {
        return 1;
    }
    return n * factorial(n - 1);
}
```
Emits:
```
(f factorial (n:_short_) _short_
  (B (I (< $An 2) (B (R 1))) (R (* $An $_factorial)) (;)))
```
Note: `$An` has the `A` prefix (function argument), `$_factorial` has the `_` prefix (global function).

### Loop Example
```c
int loop_test(int n) {
    int i, sum;
    sum = 0;
    i = 0;
    while (i < n) {
        sum = sum + i;
        i = i + 1;
    }
    return sum;
}
```
Emits:
```
(f loop_test (n:_short_) _short_
  (B (E (= $sum 0)) (E (= $i 0))
     (W (< $i $An)
        (B (E (= $sum (+ $sum $i))) (E (= $i (+ $i 1)))))
     (R $sum)))
```
Note: `$An` has the `A` prefix (function argument), `$sum` and `$i` have no prefix (local variables).

### Function Call Examples

**Simple function call:**
```c
int test_simple_call() {
    return add(1, 2);
}
```
Emits:
```
(f test_simple_call () _short_
  (B (R (@ $_add 1 2))))
```
Note: `$_add` has the `_` prefix because it's a global function.

**Nested function calls:**
```c
int test_nested_call() {
    return add(multiply(2, 3), multiply(4, 5));
}
```
Emits:
```
(f test_nested_call () _short_
  (B (R (@ $_add (@ $_multiply 2 3) (@ $_multiply 4 5)))))
```

**Function call with expressions as arguments:**
```c
int test_expr_in_call(int n) {
    return add(n + 1, n * 2);
}
```
Emits:
```
(f test_expr_in_call (n:_short_) _short_
  (B (R (@ $_add (+ $An 1) (* $An 2)))))
```
Note: `$An` has the `A` prefix (function argument), `$_add` has the `_` prefix (global function).

### Variable Scope Prefixes

All variables are prefixed with `$` followed by a scope indicator:

| Scope/Storage Class | Prefix | Example | Description |
|---------------------|--------|---------|-------------|
| Extern variable     | `$_`   | `$_external_var` | Variable declared with `extern` keyword |
| Global variable     | `$_`   | `$_global_var` | Variable at file scope (level 1) |
| Static variable     | `$S`   | `$Sfile_funcname_varname_0` | Variable declared with `static` keyword (mangled name) |
| Function argument   | `$A`   | `$Ax` | Function parameter |
| Local variable      | `$`    | `$sum` | Local variable inside function |

**Static Variable Name Mangling:**

Static variables have global storage but lexical scope, requiring unique mangled names:

- **File-scoped static**: `$S<file_root>_<varname>`
  - Example: `static int counter;` in `test.c` → `$Stest_counter`

- **Function-scoped static**: `$S<file_root>_<funcname>_<varname>_<counter>`
  - Example: `static int local;` in function `foo()` in `test.c` → `$Stest_foo_local_0`
  - The counter increments for each static in the same function (0, 1, 2, ...)
  - Counter resets to 0 for each new function

This ensures that:
- Multiple files can have statics with the same name without collision
- Multiple functions can have statics with the same name without collision
- Multiple statics within one function each get unique names

**Complete example:**
```c
/* File: test_scopes.c */
int global_var;              // $_global_var
extern int extern_var;       // $_extern_var
static int static_global;    // $Stest_scopes_static_global

int test_all_scopes(int x, int y) {
    int local1;              // $local1
    static int static_local; // $Stest_scopes_test_all_scopes_static_local_0

    local1 = x + y;          // $local1 = $Ax + $Ay
    return local1 + global_var + static_local;
}
```
Emits:
```
(f test_all_scopes (y:_short_ x:_short_) _short_
  (B (E (= $local1 (+ $Ax $Ay)))
     (R (+ (+ $local1 $_global_var) $Stest_scopes_test_all_scopes_static_local_0))))
```

## Parser Implementation Notes

The format is designed for easy parsing:
1. S-expressions provide clear nesting
2. Single-character codes minimize parsing overhead
3. Prefix notation eliminates operator precedence concerns
4. All symbols prefixed with `$` for easy identification
5. Comments start with `;` (ignored by parser)

## Type Names

Type names from the compiler's type system:
- `_char_` - char type
- `_short_` - short/int type
- `_long_` - long type
- `_uchar_` - unsigned char
- `_ushort_` - unsigned short
- `_ulong_` - unsigned long
- `_void_` - void type
- `_float_` - float type
- `_double_` - double type

Pointer and array types are represented with modifiers.
