# CPP Filter Architecture

The cpp preprocessor uses a pull-based filter pipeline to normalize C syntax
before emitting the lexeme stream. Each filter is a function that returns the
next token, calling its upstream filter to get input tokens.

## Pipeline

```
lex -> filtknr -> filtdecl -> filtbrace -> filtctrl -> emit
```

Tokens flow right-to-left: `filtctrl()` is called by the main loop, which
calls `filtbrace()`, which calls `filtdecl()`, and so on up to `lex_get()`.

## Filter Descriptions

### filtknr.c - K&R Function Normalization

Transforms K&R style function definitions to ANSI style:

```c
// Input (K&R style):
int foo(a, b)
int a;
char *b;
{

// Output (ANSI style):
int foo(int a, char *b)
{
```

Only operates at file scope (brace_depth == 0) to avoid misinterpreting
local variable declarations.

### filtdecl.c - Declaration Initializer Separation

Separates initializers from local variable declarations. This simplifies
code generation since the backend doesn't need to handle interleaved
declarations and assignments.

```c
// Input:
int x = 5, y = foo();

// Output:
int x, y;
x = 5;
y = foo();
```

Only operates inside function bodies (brace_depth > 0). Handles:
- Multiple declarators with mixed initialization
- Pointer declarators (`int *p = &x`)
- Nested parentheses in initializers
- Brace-enclosed initializers for arrays/structs

Tracks typedef declarations at file scope to recognize user-defined types.

### filtbrace.c - Brace Insertion

Inserts braces around single-statement bodies of control structures:

```c
// Input:
if (x) foo();

// Output:
if (x) { foo(); }
```

Handles: IF, ELSE, WHILE, FOR, DO

This normalizes all control structures to have braced bodies, simplifying
the control flow filter.

### filtctrl.c - Control Flow Lowering

Lowers structured control flow to labels and gotos:

**WHILE loops:**
```c
// Input:
while (cond) body;

// Output:
{ __WnT: if (!(cond)) { goto __WnB; } body; goto __WnT; __WnB: }
```

**FOR loops:**
```c
// Input:
for (init; cond; incr) body;

// Output:
{ init; __FnT: if (!(cond)) { goto __FnB; } body; __FnC: incr; goto __FnT; __FnB: }
```

**DO-WHILE loops:**
```c
// Input:
do body; while (cond);

// Output:
{ __DnT: body; if (cond) { goto __DnT; } __DnB: }
```

**SWITCH statements:**
Pass through unchanged but add break label at end:
```c
// Input:
switch (x) { ... }

// Output:
switch (x) { ... } __SnB:
```

**break/continue:**
- `break` -> `goto __XnB` (X = W/F/D/S depending on context)
- `continue` -> `goto __XnT` (or `__FnC` for FOR loops)

Label naming: `__<prefix><number><suffix>`
- Prefix: W=while, F=for, D=do, S=switch
- Number: unique per control structure
- Suffix: T=top, B=bottom/break, C=continue (FOR only)

## Implementation Pattern

All filters share a common structure:

1. **State machine** - tracks parsing state (e.g., ST_NORMAL, ST_COND, ST_BODY)
2. **Token buffers** - accumulate tokens for deferred emission
3. **Pending buffer** - output queue for synthesized tokens
4. **Context stack** - handles nested control structures

### Common Functions (filtutil.c)

```c
void pend_init(struct pendbuf *p, struct token *buf, int max);
void pend_push(struct pendbuf *p, struct token *t);
int pend_has(struct pendbuf *p);
void pend_pop(struct pendbuf *p, struct token *out);
void pend_tok(struct pendbuf *p, unsigned char type);
void pend_tok_at(struct pendbuf *p, unsigned char type, struct token *ref);
void pend_buf(struct pendbuf *p, struct token *buf, int len);
int filt_entry(struct pendbuf *pb, struct token *out,
               void (*up)(struct token *), struct token *t);
void emit_label(struct pendbuf *p, char pfx, int num, char sfx);
void emit_goto(struct pendbuf *p, char pfx, int num, char sfx);
```

`filt_entry()` is the standard filter entry point: drain pending buffer first,
then get upstream token. Returns 1 if a pending token was returned.

### Type Recognition

```c
int is_type_kw(unsigned char type);   // Check if token is a type keyword
int is_type_tok(struct token *t);     // Check type keyword or typedef name
```

Typedef names are tracked by filtdecl at file scope and shared via a global
typedef table.

## Memory Constraints

The filter pipeline is designed for the Z80 target with limited memory:
- Fixed-size token buffers (BUF_MAX = 64, PEND_MAX = 32-64)
- Fixed-size context stacks (STK_MAX = 8-16)
- No dynamic allocation in hot paths

## Testing

The test harness (`runtest.sh`) validates filter output against expected
files. Test inputs are in `test/*.c`, expected outputs in `test/*.expected`.

Run tests: `make test` or `./test/runtest.sh`
