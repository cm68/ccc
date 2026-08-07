# Phase 1: Symbol Table Building and Count Accumulation

## Overview

Phase 1 is the first pass over the lexeme stream. Its purpose is to discover
all declarations (variables, functions, types) and build the symbol table,
without building expression or statement trees. This allows phase 2 to have
complete type information when parsing expressions and to know counts
(case counts, statement counts, has-else flags) before emitting AST.

**Note:** The lexeme stream has already been preprocessed by cpp, which:
- Inserts braces around single-statement control bodies
- Lowers while/for/do loops to if/goto/label sequences
- Converts break/continue to goto statements (including in a switch)
- Splits local declaration initializers (`int x = 5;` → `int x; x = 5;`)
- Converts K&R function definitions to ANSI style
- Dissolves typedefs and lowers enums, so a declaration is recognisable from
  its leading token

Phase 1 therefore only handles `if` and `goto` for control flow (no loops),
and can assume all control bodies have explicit braces.

## Entry Point

The two phases do **not** run over the whole file. `process()` in `pass1.c`
runs them over one **span** at a time — everything from the end of the previous
function through the end of the next one — so that only one function's locals
are ever live:

```c
pushScope("global");
for (;;) {
    long spanBase = lexTell();              /* where this span starts */
    unsigned short spanStr = globalStrCtr;
    int hitEof;

    resetSpanCnts();
    phase = 1;
    parseSpan();                            /* discover */
    hitEof = cur.type == E_O_F;

    lexSeek(spanBase);                      /* rewind just this span */
    globalStrCtr = spanStr;                 /* so strN line up again */
    lexlevel = 1;
    resetFuncIdx();
    flipBlkCnts();
    resetCountIdx();

    phase = 2;
    parseSpan();                            /* emit */

    drainGraves();                          /* free this span's names */
    if (hitEof) break;
}
popScope();
```

`parseSpan()` (in `decl.c`) returns when `spanStop` is set, which
`parsefunc()` does after consuming a function body's closing brace.

A span starts *after* the previous function rather than at this one's brace,
because the globals between two functions need both phases: phase 1 emits the
string a pointer initializer refers to, and phase 2 is where an array's extent
is finally known.

There is no `lexRewind()` in this path and no loop-label counter to reset —
cpp owns the loop labels now.

## Key Behaviors

### 1. Expressions Are Skipped, Not Built

In `expr.c`, `parseExpr()` immediately returns NULL in phase 1:

```c
if (phase == 1) {
    skipExpr(pri);  // Consume tokens but build nothing
    return NULL;
}
```

`skipExpr()` (in `eutil.c`) consumes tokens matching expression syntax but
doesn't allocate any `struct expr` nodes. This saves memory and time — and it
is also where `ref_count` and `agg_refs` are accumulated, which is what lets
register allocation run at the *start* of phase 2.

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
- **Tracks variable references** (increments `ref_count`/`agg_refs` in `skipExpr()`)
- **Does NOT build statement trees** — there is no `struct stmt` in pass1 at
  all, in either phase

```c
if (phase == 1) {
    switch (cur.type) {
    case BEGIN:
        gettoken();
        pushScope(blockname());
        statement();   /* recurse for nested block */
        popScope();
        expect(END);
        stmt_count++;
        break;

    case IF:
        thisIf = ifCount++;             /* track this if's index */
        gettoken();
        expect(LPAR);
        parseExpr(PRI_ALL);             /* returns NULL, just skips tokens */
        expect(RPAR);
        parseBlock();                   /* braces guaranteed by cpp */
        if (cur.type == ELSE) {
            ifHasElse[thisIf >> 3] |= 1 << (thisIf & 7);
            gettoken();
            if (cur.type == IF)
                continue;               /* else if: run the arm again */
            parseBlock();
        } else {
            ifHasElse[thisIf >> 3] &= ~(1 << (thisIf & 7));
        }
        stmt_count++;
        break;
    /* ... similar for SWITCH (WHILE, FOR, DO handled by cpp) */
    }
    continue;  /* skip the emitting path */
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

1. Snapshots the name graveyard (`gravemark = deadNames`)
2. Pushes function scope
3. Installs parameters into the symbol table at level 2, preserving `register`
4. Parses the function body (counting statements, tracking references)
5. Captures local variables **by value** via `capLocals()`
6. Stores them in `f->u.locals` for phase 2, and marks `f->kind = kfdef`

```c
if (phase == 1) {
    statement();                 /* skips through body, builds symbol table */
    f->kind = kfdef;
    f->u.locals = capLocals();   /* capture before popScope */
}
```

There is no `f->u.body` and no statement stub — nothing builds `struct stmt`.

This local list is used in phase 2 for:
- Register allocation (from the `ref_count`/`agg_refs` phase 1 accumulated)
- Frame offset assignment
- AST emission of variable declarations

Because the copies are by value, the names themselves can be freed as soon as
the function is done: on the way out, `parsefunc()` frees everything the
graveyard collected above `gravemark`.

### 5. Switch Case Tracking

Switch statements track cases with statement counts:

```c
struct swcase {
    unsigned char is_default; /* 1 if default */
    unsigned char stmts;      /* statement count for this case section */
};

struct swtab {
    struct swcase *cases;     /* this switch's own array, grown on demand */
    unsigned char count;      /* number of cases */
    unsigned char capacity;   /* allocated size of cases */
    unsigned char num;        /* switch number */
    unsigned char base_stmts; /* stmt_count at start of current case */
    unsigned char final_cnt;  /* stmt_count when the body ended */
    unsigned char emitIdx;    /* phase 2: current case being emitted */
    unsigned char cslot;      /* reserved count-queue slot */
};
```

A case's **value is not stored.** Phase 2 re-parses it as an ordinary
expression and emits it into the AST, so the table only carries what phase 2
cannot recompute. `swList` itself is malloc'd and grown, not a fixed pool.

Processing:
```c
case SWITCH:
    pushSwitch();                       /* start a new switch table */
    sw->cslot = reserveCount();         /* count not known yet - hold a slot */
    statement();                        /* parse the body; cases add themselves */
    patchCount(sw->cslot, sw->count);   /* fill it in */
    popSwitch();
    break;

case CASE:
    addCase(stmt_count);                /* tracks stmts since the last case */
    break;

case DEFAULT:
    addDefault(stmt_count);
    break;
```

When each case is added, the previous case's `stmts` field is finalized from
the difference between the current `stmt_count` and `base_stmts`. The case
count needs `reserveCount`/`patchCount` rather than a plain push because it is
only known after the body has been walked, and the count queue is FIFO.

### 6. If/Else Tracking

If statements track whether they have an else clause:

A **bitmap**, so 4096 ifs cost 512 bytes rather than 4K:

```c
#define MAX_IFS 4096
unsigned char ifHasElse[MAX_IFS / 8];  /* bit N set: if #N has an else */
unsigned short ifCount   = 0;          /* phase 1 write index */
unsigned short ifEmitIdx = 0;          /* phase 2 read index */
```

Phase 1 records:
```c
case IF:
    thisIf = ifCount++;
    /* ... parse condition and then block */
    if (cur.type == ELSE) {
        ifHasElse[thisIf >> 3] |= 1 << (thisIf & 7);
        /* ... parse else block */
    } else {
        ifHasElse[thisIf >> 3] &= ~(1 << (thisIf & 7));
    }
```

Phase 2 retrieves (`stIf2()` in `pblock.c`):
```c
hasElse = (ifHasElse[ifEmitIdx >> 3] >> (ifEmitIdx & 7)) & 1;
ifEmitIdx++;
```

### 7. Statement Count Storage

All three live in `swcnt.c`.

**Function body counts** - FIFO via `funcCnts[]`:
```c
#define MAX_FUNCCNTS 128    /* the largest source here has 46 */
static unsigned char funcCnts[MAX_FUNCCNTS];
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

**Misc counts** - FIFO via `countBuf[]` (`MAX_COUNTS` = 256):
```c
static unsigned char countBuf[MAX_COUNTS];
static unsigned char countTop = 0;   // Write pointer
static unsigned char countIdx = 0;   // Read pointer (FIFO)
```

Phase 1 pushes counts in parse order. Phase 2 pops them in the same order
since it parses the same source. A switch's case count cannot be pushed when
its header is reached, so it uses `reserveCount()` to take a slot and
`patchCount()` to fill it once the body is walked — the queue order stays
correct either way.

All three are reset per span by `resetSpanCnts()`; `resetCountIdx()` rewinds
only the read pointer for phase 2.

## What Phase 1 Produces

After phase 1 completes for a span:

1. **Symbol table (`names`)** - All global and function-scoped names
2. **Type table (`types`)** - All declared types (structs, unions)
3. **Function stmt count buffer** - `funcCnts[]` with per-function stmt counts
4. **Block stmt count buffer** - `blkCnts[]` with per-block stmt counts (flipped)
5. **Misc count buffer** - `countBuf[]` with switch case counts
6. **Switch tables** - `swList[]` with per-case stmt counts (not values)
7. **If/else bitmap** - `ifHasElse[]`, one bit per if
8. **Per-function locals** - `f->u.locals`, captured by value
9. **String literal data** - written to `.2` under `str<n>` labels

## What Phase 1 Does NOT Produce

- No expression trees (`struct expr`) — `skipExpr()` allocates nothing
- No statement trees — there is no `struct stmt` type in pass1
- No AST output at all; `.1` is written entirely by phase 2

## Memory Efficiency

Phase 1 is designed to minimize memory usage on the Z80 target:

1. No expression node allocation
2. No statement node allocation
3. Symbol table is shared between the phases of a span (no duplication)
4. Count buffers are fixed size (128 + 1024 + 256 bytes)
5. Switch tables are allocated on demand, one case array per switch
6. The span loop bounds the live set to **one function's** names and locals,
   rather than the whole file's

This lets the compiler handle larger sources within the Z80's memory.

## File References

- `pass1.c:process()` - The span loop, phase control and initialization
- `decl.c:parseSpan()` - One span's worth of top-level parsing
- `parse.c:statement()` - The statement machine, both phases
- `pblock.c:parseBlockEx()` - Braced block bodies, scope push/pop
- `expr.c:parseExpr()` - Diverts to `skipExpr` in phase 1
- `eutil.c:skipExpr()` - Token consumption without tree building
- `decl.c:parsefunc()` - Function handling, graveyard mark, `spanStop`
- `decl.c:declaration()` - Declaration parsing (runs in both phases)
- `decl.c:drainGraves()` - End-of-span name freeing
- `name.c:capLocals()` - By-value capture of a function's locals
- `swcnt.c:pushFuncCnt/popFuncCnt` - Function stmt count storage
- `swcnt.c:pushBlkCnt/popBlkCnt/flipBlkCnts` - Block stmt count storage
- `swcnt.c:pushCount/popCount/reserveCount/patchCount` - Misc count storage
- `swcnt.c:pushSwitch/popSwitch/addCase/addDefault/finishCase` - Switch tables
- `swcnt.c:resetSpanCnts()` - Per-span reset of all of the above
