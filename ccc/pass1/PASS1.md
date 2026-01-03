# Pass1: C Compiler Frontend

## Overview

Pass1 is the C compiler frontend that parses C source code and emits an intermediate AST representation for pass2 (code generation).

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

All control structures require braces:
- `if (cond) { ... }` - braces required
- `while (cond) { ... }` - braces required
- `for (...) { ... }` - braces required
- `do { ... } while (cond);` - braces required

This simplifies the streaming model since block boundaries are explicit.

### Expression Lifetime

Expressions are allocated, emitted, and freed within a single statement.

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

**Label Stack** - for break/continue resolution:
```c
struct lblfrm {
    int num;                 /* label number */
    unsigned char type;      /* WHILE, FOR, DO, SWITCH */
};
struct lblfrm lblStack[MAX_LBLDEPTH];
unsigned char lblDepth;
```

Labels are: `W<n>` for WHILE, `F<n>` for FOR, `D<n>` for DO, `S<n>` for SWITCH.
Break suffix: `B` (or `_break` for switch). Continue suffix: `T` (WHILE) or `C` (FOR/DO).

**FOR Stack** - saves increment expression during body:
```c
struct forctx {
    struct expr *incr;       /* increment expr, freed at loop end */
};
struct forctx forStack[MAX_FORDEPTH];
```

## Statement Types

| Token | Char | Description |
|-------|------|-------------|
| BEGIN | `{`  | Block statement |
| IF    | `I`  | Conditional |
| WHILE | (lowered) | While loop (emitted as labels/gotos) |
| DO    | (lowered) | Do-while loop (emitted as labels/gotos) |
| FOR   | (lowered) | For loop (emitted as labels/gotos) |
| SWITCH| `S`  | Switch statement |
| CASE  | `C`  | Case label |
| DEFAULT| `O` | Default label |
| BREAK | `G`  | Lowered to goto |
| CONTINUE| `G`| Lowered to goto |
| RETURN| `R`  | Return |
| GOTO  | `G`  | Goto |
| LABEL | `L`  | User label |
| EXPR  | `E`  | Expression statement |
| ASM   | `A`  | Inline assembly |

## Control Flow Lowering

Loops are lowered to labeled if/goto sequences in phase 2:

**WHILE loop:**
```
B 0 (4+body)              ; Block
  L W<n>T:                ; Top label
  I nlabels NOT(cond)     ; IF NOT(condition)
    B 0 1                 ; Then: break block
      G W<n>B             ; Goto break
    0                     ; has_else=0
  <body>                  ; Body in outer block
  G W<n>T                 ; Goto top
  L W<n>B:                ; Break label
```

**FOR loop:**
```
B 0 N                     ; Block (N = 4 + body + optionals)
  E <init>                ; Init (optional)
  L F<n>T:                ; Top label
  I nlabels NOT(cond)     ; IF NOT(condition) (optional)
    B 0 1
      G F<n>B
    0
  <body>
  L F<n>C:                ; Continue label
  E <incr>                ; Increment (optional)
  G F<n>T                 ; Goto top
  L F<n>B:                ; Break label
```

**DO loop:**
```
B 0 5                     ; Block
  L D<n>T:                ; Top (body start)
  <body>
  L D<n>C:                ; Continue (before test)
  I 0 nlabels cond        ; IF(condition)
    G D<n>T               ; Goto top if true
  L D<n>B:                ; Break label
```

**SWITCH:**
```
S has_label label case_count expr
  C stmt_count value_expr   ; Each CASE
  O stmt_count              ; DEFAULT
```

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
