# Code Duplication Analysis for Memory Optimization

**Goal**: Reduce code size to fit <64KB total (code + data)

**Codebase**: ~9,658 lines of C code across 22 source files

## High-Impact Opportunities

### 1. AST Output Helper Functions (outast.c)

**Current state**: Repetitive patterns for emitting expression trees

**Pattern A: Emit child expression with space prefix**
```c
// Appears 5+ times in emit_expr()
if (e->left) {
    fdprintf(ast_fd, " ");
    emit_expr(e->left);
}

if (e->right) {
    fdprintf(ast_fd, " ");
    emit_expr(e->right);
}
```

**Proposed helper**:
```c
static void emit_child(struct expr *e) {
    if (e) {
        fdprintf(ast_fd, " ");
        emit_expr(e);
    }
}

// Usage: emit_child(e->left); emit_child(e->right);
```

**Impact**:
- Pattern appears ~10 times in outast.c
- Each instance: ~4 lines × 20 bytes = 80 bytes
- Function call: ~5 bytes
- Total savings: ~750 bytes (10 × 80 - 50 function overhead - 20 function body)

**Pattern B: Single-character fdprintf calls**
```c
fdprintf(ast_fd, " ");   // 28 instances
fdprintf(ast_fd, ")");   // 20 instances
```

Currently these cannot be easily consolidated because they're interspersed with other logic. However, in some cases we could build format strings dynamically.

### 2. Symbol Name Duplication (Many strdup calls)

**Current state**: 32 strdup() calls across codebase

**Pattern**:
```c
// declare.c, macro.c, io.c, etc.
char *name = strdup(s);
```

**Issue**: strdup() requires:
1. malloc()
2. strlen()
3. strcpy()

For fixed-size compiler with limited strings, could use:
- String table with fixed-size entries
- String interning (share identical strings)

**Impact**:
- Each strdup: ~40-60 bytes of library code linked in
- String table approach: Single implementation, ~100 bytes total
- **Savings**: Significant if we implement string interning

### 3. Token Type Checking Patterns

**Current state**: Repeated pattern of checking cur.type and advancing

**Pattern A: Check and advance**
```c
if (cur.type == RPAR) {
    gettoken();
    // ...
}
```
Appears 70+ times across parse.c, declare.c, expr.c

**Pattern B: Expect token**
```c
// parseast.c has this, but cc1 doesn't
void expect(unsigned char tok) {
    if (cur.type != tok) {
        gripe(ER_xxx);
    }
    gettoken();
}
```

**Proposal**: Add expect() helper to cc1:
```c
// In error.c or parse.c
void expect(unsigned char tok, int err_code) {
    if (cur.type != tok) {
        gripe(err_code);
    }
    gettoken();
}
```

**Impact**:
- Pattern appears 50+ times
- Each instance: ~10 bytes (compare + conditional branch + call)
- Function call: ~5 bytes
- **Savings**: ~250 bytes (50 × 10 - 50 × 5 - 30 function body)

### 4. Error Reporting with Context

**Current state**: 73 gripe() calls across codebase

**Pattern**:
```c
if (condition) {
    gripe(ER_xxx);
    return NULL;
}
```

Most error codes don't provide context (filename, line number, etc.)

**Consideration**: The gripe() function is already small. Further consolidation would need to handle the varied control flow (return NULL vs continue vs break).

### 5. Memory Allocation Patterns

**Current state**: 13 malloc(sizeof(...)) calls

**Pattern**:
```c
struct type *t = malloc(sizeof(*t));
struct name *n = malloc(sizeof(*n));
struct macro *m = malloc(sizeof(*m));
```

**Proposal**: Type-specific allocators:
```c
static inline struct type *alloc_type(void) {
    return malloc(sizeof(struct type));
}
// Similar for name, macro, expr, stmt
```

**Impact**:
- Inline functions: Zero overhead if inlined
- Provides single point to add zero-initialization
- Potential to switch to arena allocator later
- **Savings**: Minimal immediate, but enables future optimization

## Medium-Impact Opportunities

### 6. Type Construction (type.c)

**Current state**: get_type() function handles type lookup/creation

The type.c code is already fairly well factored with get_type() doing the heavy lifting. Further consolidation opportunities are limited.

### 7. Output Format String Building

**Current state**: Many separate fdprintf calls for building S-expressions

**Pattern**:
```c
fdprintf(ast_fd, "(%c", e->op);
if (e->left) {
    fdprintf(ast_fd, " ");
    emit_expr(e->left);
}
fdprintf(ast_fd, ")");
```

**Alternative approach**: Build format strings dynamically
```c
char fmt[64];
sprintf(fmt, "(%c%s)", e->op, e->left ? " %s" : "");
// More complex than benefit
```

**Conclusion**: Current approach is fine. Building format strings would add complexity without clear benefit.

### 8. cpp_asm_out() Call Consolidation (lex.c)

**Completed**: output_token() already consolidates 2 calls to 1 for STRING/NUMBER cases by appending space to buffer.

**Remaining**: SYM/default cases still need 2 calls because buffers are not modifiable.

Could create wrapper:
```c
void cpp_asm_out_with_space(char *s, int len) {
    cpp_asm_out(s, len);
    cpp_asm_out(" ", 1);
}
```

**Impact**:
- Saves 1 function call per token
- Function wrapper adds indirection
- **Net savings**: ~50 bytes (4 call sites)

## Implementation Priority

### High Priority (Do First)
1. **emit_child() helper in outast.c** - Clear win, ~750 bytes saved
2. **expect() helper in parse/error.c** - ~250 bytes saved, improves readability
3. **cpp_asm_out_with_space() wrapper** - ~50 bytes saved, trivial change

### Medium Priority (Consider)
4. **Type-specific allocators** - Enables future arena allocator
5. **String interning** - Large impact if implemented, but significant refactor

### Low Priority (Defer)
6. Format string building - Complexity outweighs benefit
7. Further error.c consolidation - Control flow too varied

## Estimated Total Savings

Conservative estimate:
- emit_child(): 750 bytes
- expect(): 250 bytes
- cpp_asm_out_with_space(): 50 bytes
- **Total: ~1,050 bytes (~1.6% of typical 64KB)**

With string interning and allocator optimization:
- Potential additional 2,000+ bytes

## Next Steps

1. Implement emit_child() helper in outast.c
2. Add expect() function to error.c or parse.c
3. Wrap cpp_asm_out calls where beneficial
4. Run tests to verify correctness
5. Measure actual code size reduction with `size` command
6. Consider string interning for phase 2 optimization
