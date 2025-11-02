# AST Output Format

The compiler outputs a parseable AST format in S-expression notation for consumption by the second pass (code generator).

## Format Overview

Each function is emitted as:
```
; Function: function_name
(func name (param1:type param2:type ...) return_type
  statement)
```

## Expression Format

All expressions use prefix notation (Polish notation):

- **Constants**: Just the numeric value (e.g., `42`, `0`, `-1`)
- **Symbols/Variables**: Prefixed with `$` (e.g., `$x`, `$sum`)
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
(func add (y:_short_ x:_short_) _short_
  (B (R (+ $x $y))))
```

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
(func factorial (n:_short_) _short_
  (B (I (< $n 2) (B (R 1))) (R (* $n $factorial)) (;)))
```

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
(func loop_test (n:_short_) _short_
  (B (E (= $sum 0)) (E (= $i 0))
     (W (< $i $n)
        (B (E (= $sum (+ $sum $i))) (E (= $i (+ $i 1)))))
     (R $sum)))
```

### Function Call Examples

**Simple function call:**
```c
int test_simple_call() {
    return add(1, 2);
}
```
Emits:
```
(func test_simple_call () _short_
  (B (R (@ $add 1 2))))
```

**Nested function calls:**
```c
int test_nested_call() {
    return add(multiply(2, 3), multiply(4, 5));
}
```
Emits:
```
(func test_nested_call () _short_
  (B (R (@ $add (@ $multiply 2 3) (@ $multiply 4 5)))))
```

**Function call with expressions as arguments:**
```c
int test_expr_in_call(int n) {
    return add(n + 1, n * 2);
}
```
Emits:
```
(func test_expr_in_call (n:_short_) _short_
  (B (R (@ $add (+ $n 1) (* $n 2)))))
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
