# AST Output Format

The compiler outputs a parseable AST format in S-expression notation for consumption by the second pass (code generator).

## Format Overview

Global variables are emitted as:
```
; Global variable: var_name
(g $varname type [init-expr])
```

Each function is emitted as:
```
; Function: function_name
(f name (param1:type param2:type ...) return_type
  (d param1 type) (d param2 type) ...
  statement)
```

Declarations `(d varname type)` are emitted for:
- Function parameters (at function level, before the body)
- Local variables (inside each block where they're declared)

## Expression Format

All expressions use prefix notation (Polish notation):

- **Constants**: Just the numeric value (e.g., `42`, `0`, `-1`)
- **Symbols/Variables**: Prefixed with `$` plus scope indicator:
  - **Extern/Global variables**: `$_name` (underscore prefix)
  - **Static variables**: `$Sname` (S prefix)
  - **Function arguments**: `$Aname` (A prefix)
  - **Local variables**: `$name` (no additional prefix)
- **String literals**: `$__name` - reference to synthetic string name (e.g., `$___str0`, `$___str1`)
- **Binary operators**: `(op left right)` (e.g., `(+ $x $y)`)
- **Unary operators**: `(op operand)` (e.g., `(NEG $x)`)

### Operators

All C operators are represented by their token character:
- Arithmetic: `+`, `-`, `*`, `/`, `%`
- Comparison: `<`, `>`, `L` (<=), `g` (>=), `Q` (==), `n` (!=)
- Bitwise: `&`, `|`, `^`, `~`, `y` (<<), `w` (>>)
- Logical: `j` (&&), `h` (||), `!`
- Memory access: `M` (dereference) - `(M:width expr)` with width annotation
- Assignment: `=` - `(=:width lvalue rvalue)` with width annotation
- Memory copy: `Y` (0xbb «) - `(Y:length dest src)` with byte count annotation
- Address-of: `&` - `(& expr)`
- Function call: `@` - `(@ function arg1 arg2 ...)`
- Ternary conditional: `?` - `(? condition (: true_expr false_expr))`
- Type casts: `N` (narrow), `W` (0xb6 ¶ widen), `X` (0xab « sign-extend) - `(op:width expr)` with width annotation

**Increment/Decrement Operators** (single-character tokens):
- Prefix increment: `0xcf` (Ï) - `(0xcf lvalue)` - increments lvalue, returns new value
- Postfix increment: `0xef` (ï) - `(0xef lvalue)` - increments lvalue, returns old value
- Prefix decrement: `0xd6` (Ö) - `(0xd6 lvalue)` - decrements lvalue, returns new value
- Postfix decrement: `0xf6` (ö) - `(0xf6 lvalue)` - decrements lvalue, returns old value

**Compound Assignment Operators** (with width annotations):
- `P` (+=) - `(P:width lvalue rvalue)` - add and assign
- `0xdf` (ß -=) - `(0xdf:width lvalue rvalue)` - subtract and assign
- `T` (*=) - `(T:width lvalue rvalue)` - multiply and assign
- `2` (/=) - `(2:width lvalue rvalue)` - divide and assign
- `0xfe` (þ %=) - `(0xfe:width lvalue rvalue)` - modulo and assign
- `0xc6` (Æ &=) - `(0xc6:width lvalue rvalue)` - bitwise AND and assign
- `1` (|=) - `(1:width lvalue rvalue)` - bitwise OR and assign
- `X` (^=) - `(X:width lvalue rvalue)` - bitwise XOR and assign
- `0` (<<=) - `(0:width lvalue rvalue)` - left shift and assign
- `6` (>>=) - `(6:width lvalue rvalue)` - right shift and assign
- `J` (&&=) - `(J:width lvalue rvalue)` - logical AND and assign
- `H` (||=) - `(H:width lvalue rvalue)` - logical OR and assign

**Important**: Compound assignments evaluate the lvalue ONCE (not desugared). This ensures correct semantics when the lvalue has side effects (e.g., `*(foo()) += 1` calls `foo()` only once).

### Automatic Type Conversions

The compiler automatically inserts type conversion operators when assigning between different-sized scalar types. This applies to both simple assignments (`=`) and compound assignments (`+=`, `-=`, etc.).

**Conversion Rules:**
- **NARROW** (`N`): Used when assigning larger type to smaller type (e.g., `int -> char`, `long -> int`)
- **SEXT** (`X`): Sign extension for signed types (e.g., `char -> int`, `int -> long`)
- **WIDEN** (`W`): Zero extension for unsigned types (e.g., `unsigned char -> unsigned int`)

**Examples:**
```c
char c;
int i;
long l;
unsigned char uc;
unsigned int ui;

// Narrowing conversions
c = i;              // (=:b $_c (N:b (M:s $_i)))      - int to char
i = l;              // (=:s $_i (N:s (M:l $_l)))      - long to int

// Sign extension (signed types)
i = c;              // (=:s $_i (X:s (M:b $_c)))      - char to int
l = i;              // (=:l $_l (X:l (M:s $_i)))      - int to long

// Zero extension (unsigned types)
ui = uc;            // (=:s $_ui (W:s (M:b $_uc)))    - unsigned char to unsigned int

// Compound assignments with conversion
c += i;             // (P:b $_c (N:b (M:s $_i)))      - NARROW applied to rvalue
```

**Notes:**
- Only applies to scalar types (not pointers, arrays, functions, or aggregates)
- Conversions are based on type size and signedness
- Same conversions apply to both `=` and compound operators (`+=`, `-=`, etc.)
- Pointer assignments are checked for compatibility but don't insert conversions

### Pointer Type Compatibility

The compiler validates pointer type compatibility during assignments and reports error ER_E_PT ("incompatible pointer types") when incompatible pointers are assigned.

**Compatible Pointer Assignments:**
- Same base type: `int *p1 = p2;` (both point to int)
- Array decay: `char *p = arr;` (char[] decays to char*)
- Same struct type: `struct foo *fp1 = fp2;` (same struct)

**Incompatible Assignments (Error):**
- Different base types: `char *cp = int_ptr;` ❌
- Different struct types: `struct foo *fp = bar_ptr;` ❌
- Signed/unsigned mismatch: `unsigned int *up = signed_ptr;` ❌

**Checking Rules:**
1. Both pointers must have base types (type->sub)
2. For aggregates (structs/unions): Base types must be identical (pointer equality due to type unification)
3. For primitives: Must have same size and signedness
4. Arrays and pointers are considered compatible (array decays to pointer)

**Note:** Type casts can override compatibility checking. Use explicit casts for intentional type reinterpretation.

### Type Cast Operators

Type conversions that require runtime operations emit specific cast operators with destination width annotations:

**Cast Operators:**
- `N` - **Narrow**: Truncate to smaller type (e.g., `long -> int`, `int -> char`)
- `W` - **Widen**: Zero-extend unsigned type to larger size
- `X` - **Sign-extend**: Sign-extend signed type to larger size

**Format:** `(op:width expr)` where width is the destination type width

**Width Suffixes** (same as memory operations):
- `:b` - byte (char)
- `:s` - short (int)
- `:l` - long
- `:p` - pointer

**Examples:**
```c
char c;
int i;
long l;
unsigned char uc;

c = (char) i;        // (=:b $_c (N:b (M:s $_i)))      - narrow int to char
i = (int) l;         // (=:s $_i (N:s (M:l $_l)))      - narrow long to int
i = (int) c;         // (=:s $_i (X:s (M:b $_c)))      - sign-extend char to int
l = (long) i;        // (=:l $_l (X:l (M:s $_i)))      - sign-extend int to long
i = (int) uc;        // (=:s $_i (W:s (M:b $_uc)))     - zero-extend unsigned char
```

**No cast operator needed for:**
- Pointer-to-pointer casts: Just type reinterpretation
- Same-size conversions: e.g., `int <-> unsigned int`

### Memory Copy Operator

Block memory copies emit the `Y` operator with a byte count annotation:

**Format:** `(Y:length dest src)` where length is the number of bytes to copy

**Usage:**
- Local array initialization from string literals
- Structure assignment (both direct and dereferenced pointers)
- Static local array initialization (emitted in global data section)

**Examples:**
```c
// Array initialization
char a[] = "hello";     // (d a :array:6) (E (Y:6 $a $_str0))
char b[] = "test";      // (d b :array:5) (E (Y:5 $b $_str1))

// Struct assignment
struct point p1, p2;
p1 = p2;                // (E (Y:4 $p1 $p2))  - direct struct assignment

struct point *pp1, *pp2;
*pp1 = *pp2;            // (E (Y:4 (M:p $pp1) (M:p $pp2)))  - dereferenced pointer assignment
```

**Comparison with scalar assignment:**
```c
char *p = "hello";      // (d p :ptr) (E (=:p $p $_str0))     - pointer assignment
char a[] = "hello";     // (d a :array:6) (E (Y:6 $a $_str0)) - memory copy
```

The `Y` operator generates a block memory copy (like `memcpy`) rather than a scalar assignment. The destination remains a local array with proper `sizeof()` semantics, while the initialization copies bytes from the source. For struct assignments, the compiler automatically converts `=` to `Y` when the type has the TF_AGGREGATE flag.

### Memory Width Annotations

Memory operations (DEREF `M` and ASSIGN `=`) include width annotations showing the size of data being accessed:

**Width Suffixes:**
- `:b` - byte (1 byte: char)
- `:s` - short (2 bytes: short, int)
- `:l` - long (4 bytes: long)
- `:p` - pointer (2 bytes: any pointer type)
- `:f` - float (4 bytes: float)
- `:d` - double (8 bytes: double)

**Format:**
- `(M:width expr)` - Dereference (memory read) with width
- `(=:width lvalue rvalue)` - Assignment (memory write) with width

**Examples:**
```c
char c;
int i;
long l;
char *p;

c = 10;              // (=:b $_c 10)
i = c;               // (=:s $_i (M:b $_c))
l = i;               // (=:l $_l (M:s $_i))
p = &c;              // (=:p $_p (& $_c))
*p = 5;              // (=:b (M:p $_p) 5)
```

The width annotations enable:
1. **Type checking in pass 2** - Width mismatches visible in AST (e.g., `(=:s $_i (M:b $_c))` shows int<-byte)
2. **Optimization** - Code generator can use native byte arithmetic instead of promoting to word size

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
  (d y _short_) (d x _short_)
  (B (R (+ (M:s $Ax) (M:s $Ay)))))
```
Note:
- Parameters are declared with `(d y _short_) (d x _short_)`
- `$Ax` and `$Ay` have the `A` prefix in expressions because they are function arguments
- Variables are wrapped in `(M:s ...)` to dereference (read) their values

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
  (d n _short_)
  (B (I (< (M:s $An) 2) (B (R 1))) (R (* (M:s $An) (@ $_factorial (- (M:s $An) 1)))) (;)))
```
Note:
- `$An` has the `A` prefix (function argument), wrapped in `(M:s ...)` to read its value
- `$_factorial` has the `_` prefix (global function name, not wrapped in M)

### Loop Example with Local Variables
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
  (d n _short_)
  (B (d i _short_) (d sum _short_) (E (=:s $sum 0)) (E (=:s $i 0))
     (W (< (M:s $i) (M:s $An))
        (B (E (=:s $sum (+ (M:s $sum) (M:s $i)))) (E (=:s $i (+ (M:s $i) 1)))))
     (R (M:s $sum))))
```
Note:
- Parameter `n` declared with `(d n _short_)`
- Local variables `i` and `sum` declared with `(d i _short_) (d sum _short_)`
- `$An` has the `A` prefix in expressions (function argument)
- `$sum` and `$i` have no prefix in expressions (local variables)
- All assignments use `:s` width (short/int): `(=:s $sum 0)`
- Variables wrapped in `(M:s ...)` to read their values

### Global Variables
```c
int global_a = 10;
int global_b;
static int file_static = 42;

int test() {
    return global_a + global_b;
}
```
Emits:
```
; Global variable: global_a
(g $_global_a _short_ 10)

; Global variable: global_b
(g $_global_b _short_)

; Global variable: file_static
(g $Sfile_file_static _short_ 42)

; Function: test
(f test () _short_
  (B (R (+ (M:s $_global_a) (M:s $_global_b)))))
```
Note:
- Global variables with initializers: `(g $_global_a _short_ 10)`
- Global variables without initializers: `(g $_global_b _short_)`
- Static global variables use mangled names: `$Sfile_file_static`
- Global variables wrapped in `(M:s ...)` to read their values

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
  (d n _short_)
  (B (R (@ $_add (+ (M:s $An) 1) (* (M:s $An) 2)))))
```
Note: `$An` has the `A` prefix (function argument), wrapped in `(M:s ...)` to read value. `$_add` has the `_` prefix (global function name, not wrapped in M).

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
  - Example: `static int counter;` in `test.c` -> `$Stest_counter`

- **Function-scoped static**: `$S<file_root>_<funcname>_<varname>_<counter>`
  - Example: `static int local;` in function `foo()` in `test.c` -> `$Stest_foo_local_0`
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

    local1 = x + y;          // (=:s $local1 (+ (M:s $Ax) (M:s $Ay)))
    return local1 + global_var + static_local;
}
```
Emits:
```
(f test_all_scopes (y:_short_ x:_short_) _short_
  (d y _short_) (d x _short_)
  (B (d local1 _short_) (E (=:s $local1 (+ (M:s $Ax) (M:s $Ay))))
     (R (+ (+ (M:s $local1) (M:s $_global_var)) (M:s $Stest_scopes_test_all_scopes_static_local_0)))))
```

## Parser Implementation Notes

The format is designed for easy parsing:
1. S-expressions provide clear nesting
2. Single-character codes minimize parsing overhead
3. Prefix notation eliminates operator precedence concerns
4. All symbols prefixed with `$` for easy identification
5. Comments start with `;` (ignored by parser)

## Initializers

Global variables can have initializers:

**Scalar initializers:**
```c
int a = 42;
```
Emits: `(g $_a _short_ 42)`

**Array initializers:**
```c
int nums[3] = { 10, 20, 30 };
```
Emits: `(g $_nums :array:3 (list 10 20 30))`

**Struct array initializers:**
```c
struct point coords[2] = { {10, 20}, {30, 40} };
```
Emits: `(g $_coords :array:2 (list (, 10 20) (, 30 40)))`

Note: The `(list ...)` form is used for multi-element initializers that are linked via expr->next pointers.

## String Literals Section

String literals are output in a dedicated literals section before global variables and functions:

**Format:**
```
(L
  (s name "escaped_string_data")
  ...
)
```

**Example:**
```c
char *s1 = "hello";
char *s2 = "world\n";
char foo[] = "test";
```

Emits:
```
; String literals
(L
  (s _str0 "hello")
  (s _str1 "world\n")
  (s _str2 "test")
)

; Global variable: s1
(g $_s1 :ptr $___str0)

; Global variable: s2
(g $_s2 :ptr $___str1)

; Global variable: foo
(g $_foo :array:5 $___str2)
```

**Escaping:**
Special characters in strings are properly escaped:
- `\n` - newline
- `\t` - tab
- `\"` - quote
- `\\` - backslash
- `\r` - carriage return
- `\xNN` - hex escape for non-printable characters

**String References:**
- String literals are referenced by synthetic names: `$___str0`, `$___str1`, etc.
- The synthetic names are defined in the literals section
- They are NOT emitted as separate global variables (only in literals section)

**Array Initialization:**
Arrays initialized with string literals automatically get the correct size (string length + 1 for null terminator):
```c
char foo[] = "string";  // :array:7 (6 chars + null)
char bar[] = "test";    // :array:5 (4 chars + null)
char empty[] = "";      // :array:1 (0 chars + null)
```

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

Type information for variables includes:
- Primitive types: type name (e.g., `_short_`)
- Pointers: `:ptr`
- Arrays: `:array:count` (e.g., `:array:10`)
- Structs: `:struct:size` (e.g., `:struct:8`)
