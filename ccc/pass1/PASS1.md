# Pass1: C Compiler Frontend

## Overview

Pass1 is the C compiler frontend that parses C source code and emits an intermediate AST representation for pass2 (code generation).

**Input preprocessing by cpp:** Before pass1 sees any code, the cpp preprocessor
has already performed:
- Macro expansion and conditional compilation
- K&R to ANSI function definition conversion
- Brace insertion around single-statement if/else bodies
- Loop lowering: while/for/do converted to if/goto/label sequences
- Break/continue resolution to goto statements
- Local declaration initializer splitting (`int x = 5;` → `int x; x = 5;`)

This means pass1 only handles `if` and `goto` for control flow (no loops),
all if/else bodies have explicit braces, and all declarations are separate
from their initializers.

## Architecture

### Two-Phase Parsing

Each function is parsed twice. The lexeme stream is rewound between phases via `lexRewind()`.

**Phase 1 (Discovery)**
- Uses `skipExpr()` - no expression tree allocation
- Builds symbol table with automatic variables
- Collects switch statement definitions (cases with stmt counts)
- Tracks if/else relationships in `ifHasElse[]`
- Counts reference usage for register allocation
- Counts statements for streaming emission in phase 2
- Emits function-local string literals immediately (with "fs" prefix)

**Phase 2 (Emission)**
- Streaming: emits AST immediately as parsed
- Builds expression trees per-statement
- Frees expressions immediately after emission
- Control structures (IF/WHILE/FOR/DO/SWITCH) emit directly inline
- No statement tree building for most constructs

### Phase 1 Statement Processing

The `statement()` function (parse.c) processes statements differently based on `phase`:

```c
if (phase == 1) {
    switch (cur.type) {
    case END: case E_O_F:
        // Finalize last case if in switch
        if (swDepth > 0)
            finishCase(stmt_count);
        // Push statement count for function bodies
        if (lexlevel == 2 && swDepth == 0)
            pushFuncCnt(stmt_count);
        // Push statement count for nested blocks
        else if (lexlevel > 2 && swDepth == 0)
            pushBlkCnt(stmt_count);
        block = 0;
        break;

    case BEGIN:
        gettoken();
        pushScope(blockname());
        statement(0);   // Recurse for nested block
        popScope();
        expect(END);
        stmt_count++;
        break;

    case IF:
        thisIf = ifCount++;  // Track this if's index
        gettoken();
        expect(LPAR);
        parseExpr(PRI_ALL, parent);
        expect(RPAR);
        parseBlock();  // Requires braces
        if (cur.type == ELSE) {
            ifHasElse[thisIf] = 1;
            gettoken();
            if (cur.type == IF)
                goto handle_if;  // else if
            parseBlock();
        } else {
            ifHasElse[thisIf] = 0;
        }
        stmt_count++;
        break;

    // Similar for WHILE, FOR, DO - all require braces via parseBlock()

    case SWITCH:
        pushSwitch();
        statement(0);  // Switch body - adds cases
        pushCount(swList[idx].count);
        popSwitch();
        break;

    case CASE:
        addCase(value, stmt_count);  // Tracks case value and stmt count
        break;
    }
    continue;  // Don't build statement tree
}
```

### Phase 1 Expression Processing

When `parseExpr(priority, parent)` is called in phase 1:

```c
if (phase == 1) {
    skipExpr(pri);
    return NULL;
}
```

**No data structures are built.** The `skipExpr()` function consumes tokens to keep the lexer synchronized but allocates nothing.

`skipExpr()` mirrors the structure of `parseExpr()`:
1. Handle prefix/primary tokens (NUMBER, SYM, LPAR, unary ops)
2. Handle postfix operators (function calls, array access, ++/--)
3. Handle binary operators based on precedence via `binopPri()`
4. For STRING tokens: creates and emits string literal immediately

### Data Structures Built in Phase 1

| Structure | Purpose | Lifetime |
|-----------|---------|----------|
| `struct name` chain | Symbol table entries | Until scope exit |
| `struct type` chain | Type definitions | Entire compilation |
| `funcCnts[]` | Statement counts per function | Read in phase 2 |
| `blkCnts[]` | Statement counts per block | Read in phase 2 (flipped) |
| `countBuf[]` | Misc counts (switch cases) | Read in phase 2 |
| `swList[]` | Switch tables with case info | Reset per function |
| `casePool[]` | Case values/stmt counts | Reset per function |
| `ifHasElse[]` | Has-else flag per if | Read in phase 2 |

### Statement Counting

**Function bodies**: When END is reached at lexlevel==2 (and not in switch), push count via `pushFuncCnt()`.

**Nested blocks**: When END is reached at lexlevel>2 (and not in switch body), push count via `pushBlkCnt()`. After phase 1, `flipBlkCnts()` reverses the order for LIFO access.

**Switch cases**: Each CASE/DEFAULT calls `addCase()`/`addDefault()` which tracks `stmts` (statement count since previous case). `finishCase()` is called before each new case and at switch END.

### Control Structure Requirements

After cpp preprocessing:
- `if (cond) { ... }` - braces guaranteed by cpp
- `if (cond) { ... } else { ... }` - both branches have braces
- Loops (while/for/do) are eliminated - converted to if/goto/label sequences
- break/continue statements are converted to goto

The cpp preprocessor's token filter (`knr.c`) handles all loop lowering and
brace insertion. Pass1 only needs to handle `if`, `goto`, and labels for
control flow.

### Declaration Initializer Handling

Local variable initializers are split by cpp:
```c
// cpp transforms:
int x = 5;          // → int x; x = 5;
char *p = "hello";  // → char *p; p = "hello";
```

This allows pass1 to handle all declarations uniformly without tracking
initializer expressions during declaration parsing.

### Expression Lifetime

Expressions are allocated, emitted, and freed within a single statement.

### Constant Folding

Pass1 performs limited compile-time constant folding for common C idioms:

**Binary operations** - When both operands are constants (E_CONST flag set):
- Arithmetic: `+`, `-`, `*`, `/`, `%`
- Bitwise: `&`, `|`, `^`
- Shifts: `<<`, `>>`

**Unary operations** - When operand is constant:
- Negation: `-x`
- Bitwise NOT: `~x`
- Logical NOT: `!x`

**Type casts** - E_CONST flag preserved through NARROW/WIDEN operations.

**Member access** - Struct member offset added to base is folded when base is constant.

This enables two important C idioms to resolve at compile time:

```c
/* Array element count */
int count = sizeof(arr) / sizeof(arr[0]);

/* Struct member offset (offsetof pattern) */
int off = (int)&((struct foo *)0)->member;
```

The `foldConst()` function in expr.c handles binary folding. Unary folding
is inline in the NEG/TWIDDLE/BANG case. Cast folding preserves E_CONST
through type conversions. Member access folding occurs after creating
the PLUS node for base + offset.

## Key Data Structures

### Switch Statement Tracking

```c
struct swcase {
    long value;              /* case constant value */
    unsigned char is_default; /* 1 if default, 0 if case */
    unsigned char stmts;     /* statement count for this case section */
};

struct swtab {
    struct swcase *cases;    /* pointer into global casePool */
    unsigned char count;     /* number of cases */
    unsigned char num;       /* switch number (for labels) */
    unsigned char base_stmts; /* stmt_count at start of current case */
};

/* Global arrays */
struct swcase casePool[MAX_ALLCASES];  /* shared case pool */
struct swtab swList[MAX_SWITCHES];     /* switch tables */
unsigned char swStack[MAX_SWDEPTH];    /* nesting stack */
```

### If/Else Tracking

```c
static unsigned char ifHasElse[MAX_IFS];  /* 1 if if #N has else */
static unsigned char ifCount = 0;         /* phase 1: count of ifs */
static unsigned char ifEmitIdx = 0;       /* phase 2: next if to emit */
```

### Phase 2 Stacks

**Label Stack** - for switch break resolution:
```c
struct lblfrm {
    int num;                 /* label number */
    unsigned char type;      /* SWITCH only (loops lowered by cpp) */
};
struct lblfrm lblStack[MAX_LBLDEPTH];
unsigned char lblDepth;
```

Labels are: `S<n>` for SWITCH. Break uses `_break` suffix.

Note: Loop labels (`__W<n>`, `__F<n>`, `__D<n>`) and break/continue resolution
are handled by cpp during loop lowering. Pass1 only tracks switch contexts.

## Statement Types

| Token | Char | Description |
|-------|------|-------------|
| BEGIN | `{`  | Block statement |
| IF    | `I`  | Conditional |
| SWITCH| `S`  | Switch statement |
| CASE  | `C`  | Case label |
| DEFAULT| `O` | Default label |
| RETURN| `R`  | Return |
| GOTO  | `G`  | Goto (includes lowered break/continue) |
| LABEL | `L`  | Label (includes cpp-generated loop labels) |
| EXPR  | `E`  | Expression statement |
| ASM   | `A`  | Inline assembly |

Note: WHILE, FOR, DO, CONTINUE are never seen by pass1 - they are
lowered to if/goto/label sequences by cpp. BREAK is still seen inside
switch statements (cpp only transforms loop breaks).

## Control Flow

Loops are lowered by cpp before pass1 sees the code. Pass1 receives:
- Labels (`__W<n>T`, `__F<n>B`, etc.) as regular LABEL tokens
- Gotos as regular GOTO tokens
- If statements with negated conditions for loop exit tests

Pass1 emits these constructs directly to the AST without special loop handling.

**SWITCH** (handled by pass1):
```
S has_label label case_count expr
  C stmt_count value_expr   ; Each CASE
  O stmt_count              ; DEFAULT
```

See `cpp/CPP.md` for details on the loop lowering transformations.

## Short-Circuit Evaluation

For `&&` and `||` operators, `cntCondLbls()` counts how many labels are needed and passes this to pass2 for proper branching.

## Memory Model

```
Per-function (reset at function start):
  - swList[8]           ~128 bytes
  - casePool[128]       ~384 bytes
  - ifHasElse[128]      128 bytes

Phase 2 stacks (static, reused):
  - lblStack[16]        ~48 bytes
  - forStack[8]         ~16 bytes

Count buffers (static):
  - funcCnts[32]        32 bytes
  - blkCnts[256]        256 bytes
  - countBuf[256]       256 bytes

Per-statement:
  - Expression tree     built, emitted, freed immediately
```

## Files

- `cc1.h` - Data structure definitions
- `pass1.c` - Main driver
- `lexread.c` - Lexical analysis
- `expr.c` - Expression parsing
- `parse.c` - Statement parsing and streaming emission
- `decl.c` - Declaration handling and top-level parse driver
- `declare.c` - Declarator parsing
- `type.c` - Type management
- `outast.c` - AST emission helpers
- `regalloc.c` - Register allocation analysis
- `error.c` - Error reporting
- `util.c` - Utilities
