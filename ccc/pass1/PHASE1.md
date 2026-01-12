# Phase 1: Symbol Table Building and Count Accumulation

## Overview

Phase 1 is the first pass over the lexeme stream. Its purpose is to discover
all declarations (variables, functions, types) and build the symbol table,
without building expression or statement trees. This allows phase 2 to have
complete type information when parsing expressions and to know counts
(case counts, statement counts, has-else flags) before emitting AST.

**Note:** The lexeme stream has already been preprocessed by cpp, which:
- Inserts braces around single-statement if/else bodies
- Lowers while/for/do loops to if/goto/label sequences
- Converts break/continue to goto statements
- Splits local declaration initializers (`int x = 5;` → `int x; x = 5;`)
- Converts K&R function definitions to ANSI style

Phase 1 therefore only handles `if` and `goto` for control flow (no loops),
and can assume all if/else bodies have explicit braces.

## Entry Point

In `pass1.c`, `process()` orchestrates both phases:

```c
phase = 1;
parse();          // Phase 1: build symbol table

lexRewind();      // Rewind to start of lexemes
lexlevel = 0;     // Reset scope level
resetLoopLbls();  // Reset label counter (must match phase 1)
resetFuncIdx();   // Reset function stmt count read pointer
flipBlkCnts();    // Reverse block counts for phase 2
globalStrCtr = 0; // Reset string counter so phase 2 matches

phase = 2;
parse();          // Phase 2: emit AST
```

## Key Behaviors

### 1. Expressions Are Skipped, Not Built

In `expr.c`, `parseExpr()` immediately returns NULL in phase 1:

```c
if (phase == 1) {
    skipExpr(pri);  // Consume tokens but build nothing
    return NULL;
}
```

The `skipExpr()` function consumes tokens matching expression syntax but
doesn't allocate any `struct expr` nodes. This saves memory and time.

**Exception: String literals**. All string literals (global and function-local)
are emitted during phase 1 with "str" prefix names. These are written to the
**.2 file** (assembly output via `asmFd`), not the .1 AST file. Phase 2 then
references these labels when building expressions for the AST.

```c
case STRING:
    /* Phase 1: emit string literal data to .2 file */
    size = (unsigned char)cur.v.str[0];
    symname = (char *)cur.v.str + 1;
    fmtstr(namebuf, "str%d", globalStrCtr++);
    setSeg(SEG_TEXT);
    asmLabel(namebuf);
    asmDbStr((unsigned char *)symname, size);
    gettoken();
    break;
```

String data is output as ASCII literals with hex for non-printable characters:
```
str0:
	.db 'hello world', 0x00
str1:
	.db 'line1', 0x0a, 'line2', 0x00
```

The .2 file contains all data that can be emitted as raw assembly without
needing pass2 processing: string literals, initialized globals, and
uninitialized global reservations.

### 2. Statements Are Counted, Not Built

In `parse.c`, `statement()` has a separate phase 1 path that:

- **Counts statements** in compound blocks
- **Counts cases** in switch statements with per-case stmt counts
- **Tracks if/else** relationships in `ifHasElse[]`
- **Tracks scopes** (pushScope/popScope for nested blocks)
- **Discovers declarations** (calls `declaration()` which builds symbol table)
- **Tracks variable references** (increments `ref_count` in `skipExpr()`)
- **Does NOT build statement trees** (no `makestmt()` calls)

```c
if (phase == 1) {
    switch (cur.type) {
    case BEGIN:
        gettoken();
        pushScope(blockname());
        statement(0);  // Recurse for nested block
        popScope();
        expect(END);
        stmt_count++;
        break;

    case IF:
        thisIf = ifCount++;  // Track this if's index
        gettoken();
        expect(LPAR);
        parseExpr(...);  // Returns NULL, just skips tokens
        expect(RPAR);
        parseBlock();    // Requires braces
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
    // ... similar for SWITCH (WHILE, FOR, DO handled by cpp)
    }
    continue;  // Skip tree-building path
}
```

### 3. Declarations Build Symbol Table

Unlike expressions, declarations ARE fully processed in phase 1:

- `parseSclass()` - parses storage class specifiers
- `getbasetype()` - parses type specifiers (int, struct, etc.)
- `declare()` - parses declarators and creates `struct name` entries
- `newName()` / `addName()` - add symbols to the symbol table

This is necessary because:
1. Type information must be available for expression parsing in phase 2
2. Function signatures must be known for type-checking calls
3. Struct/union layouts must be computed for member access

### 4. Functions Capture Locals

For function definitions, phase 1:

1. Pushes function scope
2. Installs parameters into symbol table at level 2
3. Parses function body (counting statements, tracking references)
4. Captures local variables via `capLocals()`
5. Stores locals in `f->u.body->locals` for phase 2

```c
if (phase == 1) {
    statement(0);  // Skips through body, builds symbol table
    f->u.body = makestmt(BEGIN, 0);  // Minimal stub
    f->u.body->locals = capLocals(); // Save locals for phase 2
}
```

This local list is used in phase 2 for:
- Register allocation (based on ref_count populated during phase 1 skipExpr)
- Frame offset assignment
- AST emission of variable declarations

### 5. Switch Case Tracking

Switch statements track cases with statement counts:

```c
struct swcase {
    long value;              /* case constant value */
    unsigned char is_default; /* 1 if default */
    unsigned char stmts;     /* statement count for this case section */
};

struct swtab {
    struct swcase *cases;    /* pointer into casePool */
    unsigned char count;     /* number of cases */
    unsigned char num;       /* switch number */
    unsigned char base_stmts; /* stmt_count at start of current case */
};
```

Processing:
```c
case SWITCH:
    pushSwitch();           // Start new switch table
    statement(0);           // Parse switch body - adds cases
    idx = swStack[swDepth - 1];
    pushCount(swList[idx].count);  // Save case count for phase 2
    popSwitch();
    break;

case CASE:
    addCase(value, stmt_count);  // Add case, tracks stmts since last
    break;

case DEFAULT:
    addDefault(stmt_count);
    break;
```

When each case is added, the previous case's `stmts` field is finalized based
on the difference between current `stmt_count` and `base_stmts`.

### 6. If/Else Tracking

If statements track whether they have an else clause:

```c
static unsigned char ifHasElse[MAX_IFS];
static unsigned char ifCount = 0;
static unsigned char ifEmitIdx = 0;
```

Phase 1 records:
```c
case IF:
    thisIf = ifCount++;
    // ... parse condition and then block
    if (cur.type == ELSE) {
        ifHasElse[thisIf] = 1;
        // ... parse else block
    } else {
        ifHasElse[thisIf] = 0;
    }
```

Phase 2 retrieves:
```c
case IF:
    hasElse = ifHasElse[ifEmitIdx++];
    // ... emit with has_else flag
```

### 7. Statement Count Storage

**Function body counts** - FIFO via `funcCnts[]`:
```c
static unsigned char funcCnts[32];
static unsigned char funcCntTop = 0;   // Write pointer (phase 1)
static unsigned char funcCntIdx = 0;   // Read pointer (phase 2)
```

**Block counts** - Stored LIFO, flipped to FIFO for phase 2:
```c
static unsigned char blkCnts[MAX_BLKCNTS];
static unsigned short blkCntTop = 0;
static unsigned short blkCntIdx = 0;

void flipBlkCnts(void) {
    // Reverse array so phase 2 reads in correct order
    for (i = 0, j = blkCntTop - 1; i < j; i++, j--) {
        tmp = blkCnts[i];
        blkCnts[i] = blkCnts[j];
        blkCnts[j] = tmp;
    }
    blkCntIdx = 0;
}
```

**Misc counts** - FIFO via `countBuf[]`:
```c
static unsigned char countBuf[MAX_COUNTS];
static unsigned char countTop = 0;   // Write pointer
static unsigned char countIdx = 0;   // Read pointer (FIFO)
```

Phase 1 pushes counts in parse order. Phase 2 pops them in the same order
since it parses the same source.

## What Phase 1 Produces

After phase 1 completes:

1. **Symbol table (`names`)** - All global and function-scoped names
2. **Type table (`types`)** - All declared types (structs, unions, enums)
3. **Function stmt count buffer** - `funcCnts[]` with per-function stmt counts
4. **Block stmt count buffer** - `blkCnts[]` with per-block stmt counts (flipped)
5. **Misc count buffer** - `countBuf[]` with switch case counts
6. **Switch tables** - `swList[]` with case values and per-case stmt counts
7. **If/else flags** - `ifHasElse[]` with has-else for each if
8. **Function stubs** - Each function has `u.body->locals` with local variables
9. **String literals** - All strings emitted with `str<n>` labels

## What Phase 1 Does NOT Produce

- No expression trees (`struct expr`) except for string literal emission
- No statement trees (`struct stmt`) except minimal function stubs
- No AST output for code (only string literals)

## Memory Efficiency

Phase 1 is designed to minimize memory usage on the Z80 target:

1. No expression node allocation (except transient string emission)
2. No statement node allocation (except function stubs)
3. Symbol table is shared between phases (no duplication)
4. Count buffers are small fixed size (32+256+256 bytes)
5. Switch tables are compact (8 switches, 128 cases max)

This allows the compiler to handle larger source files within the Z80's
limited memory (48KB total for code + data).

## File References

- `pass1.c:process()` - Phase control and initialization
- `parse.c:statement()` - Phase 1 statement skipping (lines 436-626)
- `expr.c:parseExpr()` - Phase 1 expression skipping
- `expr.c:skipExpr()` - Token consumption without tree building
- `decl.c:parsefunc()` - Phase 1 function handling
- `decl.c:declaration()` - Declaration parsing (runs in both phases)
- `parse.c:pushFuncCnt/popFuncCnt` - Function stmt count storage
- `parse.c:pushBlkCnt/popBlkCnt/flipBlkCnts` - Block stmt count storage
- `parse.c:pushCount/popCount` - Misc count storage (switch cases)
- `parse.c:pushSwitch/popSwitch/addCase/addDefault` - Switch table management
