# Pass1: C Compiler Frontend

## Overview

Pass1 (`c0`) is the C compiler frontend. It reads the binary lexeme stream cpp
produced and emits an intermediate AST for pass2 (code generation).

```
c0 <base>.x <base>.1 <base>.2
```

- **`<base>.x`** — input, cpp's lexeme stream (see [cpp/OUTPUT.md](../cpp/OUTPUT.md))
- **`<base>.1`** — output, the binary AST (see [AST_FORMAT.md](../AST_FORMAT.md))
- **`<base>.2`** — output, **assembly text**: global and static storage, string
  literal data, and static initializers, streamed directly rather than built
  as tree nodes

Pass1 does the semantic work: type resolution, operator precedence, expression
parsing, symbol table management, and register allocation.

**Input preprocessing by cpp.** Before pass1 sees any code, cpp has already
performed:
- Macro expansion and conditional compilation
- Typedef dissolution and enum lowering (so a declaration is recognisable from
  its leading token — pass1 parses without a symbol table)
- K&R to ANSI function definition conversion
- Brace insertion around single-statement control bodies
- Loop lowering: while/for/do converted to if/goto/label sequences
- Break/continue resolution to goto statements
- Local declaration initializer splitting (`int x = 5;` → `int x; x = 5;`)
- `sizeof` folding where cpp's size registries can price it

This means pass1 only handles `if` and `goto` for control flow (no loops), all
control bodies have explicit braces, and all local declarations are separate
from their initializers.

**Identifiers arrive as 2-byte ids**, not names. Pass1 never learns the
spellings — its lookups are 16-bit compares — and emits `@<id>` markers into
the AST for pass2 to resolve from cpp's `.n` sidecar.

## Architecture

### Two phases, run per span

Each function is parsed **twice**. But the two phases do not run over the whole
file: they run over one **span** at a time, where a span is everything from the
end of the previous function through the end of the next one.

```c
pushScope("global");
for (;;) {
    long spanBase = lexTell();
    unsigned short spanStr = globalStrCtr;

    resetSpanCnts();
    phase = 1;  parseSpan();          /* discover */

    lexSeek(spanBase);                /* rewind to the span's start */
    globalStrCtr = spanStr;
    lexlevel = 1;
    resetFuncIdx(); flipBlkCnts(); resetCountIdx();

    phase = 2;  parseSpan();          /* emit */

    drainGraves();                    /* free this span's names */
    if (hitEof) break;
}
popScope();
```

`parseSpan()` returns when `spanStop` is set, which `parsefunc()` does at the
end of a function body.

**Why spans rather than the whole file.** The phases used to run file-wide:
phase 1 discovered every function's locals and phase 2 freed them one at a
time, so the entire file's worth was live at the turn between passes. Per-span,
phase 2 frees a function's locals before phase 1 goes looking for the next
one's, and the live set is one function.

A span **starts after the previous function rather than at this one's brace**,
because the globals between two functions need both phases: phase 1 emits the
string a pointer initializer refers to, and phase 2 is where an array's extent
is finally known. The string counter is rewound to where the span began so the
`strN` labels phase 1 emitted are the ones phase 2 refers to.

**Phase 1 (Discovery)**
- Uses `skipExpr()` — consumes expression tokens, allocates nothing
- Builds the symbol table, including function-scope locals
- Collects switch definitions (cases with per-case statement counts)
- Records if/else relationships in the `ifHasElse[]` bitmap
- Counts reference usage (`ref_count`, `agg_refs`) for register allocation
- Counts statements for streaming emission in phase 2
- Emits string literal data to `.2` immediately, under `strN` labels

**Phase 2 (Emission)**
- Streams: emits AST immediately as each construct is parsed
- Builds expression trees per statement, frees them right after emission
- Control structures emit inline using phase 1's counts
- **No statement trees are built in either phase** — there is no `struct stmt`

### Phase 1 statement processing

`statement()` in `parse.c` has a phase 1 path that counts rather than emits:

- `END`/`E_O_F` — finalize the last case if in a switch; push the statement
  count with `pushFuncCnt()` at function level or `pushBlkCnt()` for a nested
  block
- `BEGIN` — push scope, recurse, pop scope
- `IF` — take `thisIf = ifCount++`, skip the condition, parse the braced body,
  then set or clear bit `thisIf` of `ifHasElse[]`
- `SWITCH` — `pushSwitch()`, parse the body (which adds cases), reserve a count
  slot and patch it with the case count, `popSwitch()`
- `CASE`/`DEFAULT` — `addCase()`/`addDefault()`, tracking statements since the
  previous case

### Phase 1 expression processing

`parseExpr()` in `expr.c` diverts immediately:

```c
if (phase == 1) {
    skipExpr(pri);
    return NULL;
}
```

**No data structures are built.** `skipExpr()` (in `eutil.c`) mirrors the shape
of `parseExpr()` — prefix/primary, postfix, then binary operators by precedence
— consuming tokens to keep the lexer synchronized while allocating nothing. It
is also where `ref_count` and `agg_refs` are incremented, which is why register
allocation can run at the *start* of phase 2.

The one exception is `STRING`: a string literal's data is emitted to `.2`
during phase 1, under a `strN` label that phase 2 references.

### Data structures built in phase 1

| Structure | Purpose | Lifetime |
|-----------|---------|----------|
| `struct name` chain | Symbol table entries | Until scope exit, then the graveyard |
| `struct type` chain | Type definitions | Entire compilation |
| `struct local` list | Per-function locals, by value | `f->u.locals`, freed after phase 2 |
| `funcCnts[]` | Statement counts per function | Read in phase 2 (FIFO) |
| `blkCnts[]` | Statement counts per block | Read in phase 2 (flipped to FIFO) |
| `countBuf[]` | Misc counts (switch case counts) | Read in phase 2 (FIFO) |
| `swList[]` | Switch tables, dynamically grown | Reset per function |
| `ifHasElse[]` | Has-else bit per if | Read in phase 2 |

### Statement counting

**Function bodies**: at `END` with `lexlevel == 2` and not inside a switch,
push via `pushFuncCnt()`.

**Nested blocks**: at `END` with `lexlevel > 2`, push via `pushBlkCnt()`. Block
counts are pushed LIFO; `flipBlkCnts()` reverses the array between phases so
phase 2 reads them in parse order.

**Switch cases**: each `CASE`/`DEFAULT` calls `addCase()`/`addDefault()`, which
finalizes the previous case's `stmts` from the difference between the current
`stmt_count` and `base_stmts`. `finishCase()` runs before each new case and at
the switch's `END`.

**Switch case counts** use `reserveCount()`/`patchCount()` rather than a plain
push: the count is not known until the body has been walked, so a slot is
reserved in `countBuf[]` at the header and filled in afterwards.

### The name graveyard

`popScope()` cannot free a name on the spot, because pending AST may still
point at it — so it parks the name on the `deadNames` list instead.

Two things drain that list. `parsefunc()` snapshots `deadNames` on entry and
frees everything parked above the mark on exit: the graveyard is LIFO, so those
entries are all the function's. Then `drainGraves()` at the end of the span
frees the rest. Without this, both phases' names pile up until exit — the whole
file's worth, on the machine with the least room for it.

`capLocals()` keeps copies **by value** of everything a later pass needs, which
is what makes the freeing safe.

### Declaration initializer handling

Local variable initializers are split by cpp:

```c
int x = 5;          /* → int x; x = 5; */
char *p = "hello";  /* → char *p; p = "hello"; */
```

Statics and arrays keep theirs inline, and those are handled by `init.c` /
`istream.c`, which stream the initializer straight to `.2` as assembly rather
than building tree nodes.

### Expression lifetime

Expressions are allocated, emitted, and freed within a single statement.

### Constant folding

Pass1 performs compile-time constant folding for common C idioms — in
`fold.c`, split out of `expr.c`:

**Binary operations** — when both operands are constants (`E_CONST` set):
arithmetic (`+ - * / %`), bitwise (`& | ^`), shifts (`<< >>`), and relationals.

**Unary operations** — negation `-x`, bitwise NOT `~x`, logical NOT `!x`.

**Type casts** — `E_CONST` is preserved through `NARROW`/`WIDEN`.

**Member access** — a struct member offset added to a constant base is folded.

Together these resolve two important idioms at compile time:

```c
int count = sizeof(arr) / sizeof(arr[0]);   /* array element count */
int off   = (int)&((struct foo *)0)->member; /* offsetof pattern */
```

`foldNode()` folds a single node; `foldTree()` folds bottom-up and is called at
the root of every statement's expression before emission. Note that cpp now
folds `sizeof` itself wherever its size registries can price the type, so many
of these arrive already reduced to an `INUMBER`.

## Key Data Structures

### Switch statement tracking

The switch list is **dynamically allocated and grown**, not a fixed pool:

```c
struct swcase {
    unsigned char is_default; /* 1 if default, 0 if case */
    unsigned char stmts;      /* statement count for this case section */
};

struct swtab {
    struct swcase *cases;     /* this switch's own case array */
    unsigned char count;      /* number of cases */
    unsigned char capacity;   /* allocated size of cases */
    unsigned char num;        /* switch number */
    unsigned char base_stmts; /* stmt_count at start of current case */
    unsigned char final_cnt;  /* stmt_count when the body ended */
    unsigned char emitIdx;    /* phase 2: current case being emitted */
    unsigned char cslot;      /* reserved count-queue slot (phase 1) */
};

struct swtab *swList;                   /* grown as needed */
unsigned char swCount, swCapacity;
unsigned char swStack[MAX_SWDEPTH];     /* nesting stack */
unsigned char swStmtDepth[MAX_SWDEPTH]; /* statement() depth per switch */
unsigned char swEmitStack[MAX_SWDEPTH]; /* phase 2 nesting stack */
```

A case's **value is not stored**. It is re-parsed in phase 2 as an ordinary
expression and emitted into the AST after the case's statement count, so the
table only has to carry what phase 2 cannot recompute.

### If/else tracking

```c
#define MAX_IFS 4096
unsigned char ifHasElse[MAX_IFS / 8];  /* bit N set: if #N has an else */
unsigned short ifCount;                /* phase 1: count of ifs */
unsigned short ifEmitIdx;              /* phase 2: next if to emit */
```

A bitmap, not a byte array: 4096 ifs in 512 bytes.

### There is no label stack

Pass1 keeps **no** loop or break label stack. cpp lowers loops, resolves
break/continue to gotos, and appends the switch's own `__S<n>B` label — so
pass1 emits `has_label = 0` on every `SWITCH` and tracks only which switch and
which case it is currently emitting (`swEmitStack`, `swtab.emitIdx`).

## Statement Types

| Token | Value | Description |
|-------|------:|-------------|
| BEGIN | 2 | Block statement (emitted as `AST_BLOCK`) |
| IF | 147 | Conditional |
| SWITCH | 150 | Switch statement |
| CASE | 151 | Case label |
| DEFAULT | 155 | Default label |
| RETURN | 146 | Return |
| GOTO | 145 | Goto (includes lowered break/continue) |
| LABEL | 112 | Label (includes cpp-generated loop labels) |
| ASM | 157 | Inline assembly |
| SEMI | 1 | Empty statement |

An expression statement has **no opcode of its own** — the expression is
emitted directly.

WHILE, FOR, DO, BREAK, and CONTINUE are never seen by pass1: cpp lowers all of
them, including `break` inside a switch.

## Control Flow

Loops are lowered by cpp before pass1 sees the code. Pass1 receives:
- Labels (`__W<n>T`, `__F<n>B`, …) as regular LABEL tokens
- Gotos as regular GOTO tokens
- If statements with negated conditions for loop exit tests

Pass1 emits these constructs directly to the AST without special loop handling.

**SWITCH** (still handled by pass1):
```
SWITCH 0 <case_count> <expr>
  CASE    <stmt_count> <value_expr>   ; each case
  DEFAULT <stmt_count>                ; the default
```

See [cpp/NORM.md](../cpp/NORM.md) for the lowering transformations.

## Short-Circuit Evaluation

For `&&` and `||`, `cntCondLbls()` counts how many intermediate labels pass2
will need for branching, and that count is emitted as the `IF` node's
`nlabels` field.

## Memory Model

```
Per-span (reset by resetSpanCnts):
  - swList[]            grown on demand, one case array per switch
  - ifHasElse[512]      512 bytes (4096 ifs, one bit each)

Count buffers (static):
  - funcCnts[128]       128 bytes   (the largest source here has 46)
  - blkCnts[1024]      1024 bytes
  - countBuf[256]       256 bytes

Per-function:
  - struct local list   captured by value in phase 1, freed after phase 2

Per-statement:
  - Expression tree     built, emitted, freed immediately
```

## Files

Pass1 is split into many small translation units on purpose: the compiler has
to compile *itself* on a 64K machine, and cpp's tables for one translation unit
are paid per unit — so the biggest source in the tree is the one that stops
fitting first.

| File | Purpose |
|------|---------|
| `cc1.h` | Data structure definitions |
| `token.h` | Token numbering (generates `enumlist.h`) |
| `pass1.c` | Main driver, span loop, phase control |
| `error.c` | Error reporting |
| `lexread.c` | Lexeme stream reader (`.x` decoding, `lexTell`/`lexSeek`) |
| `expr.c` | Expression parsing, precedence table |
| `eutil.c` | Expression node constructors, `skipExpr` |
| `pfx.c` | Prefix and primary expression parsing |
| `post.c` | Postfix arms: subscripts, calls, member selection, `x++` |
| `fold.c` | Constant folding |
| `parse.c` | Statement parsing and streaming emission |
| `pblock.c` | Block scaffolding: scope, locals, asm text, `stIf2`/`stRet2`/… |
| `swcnt.c` | Switch bookkeeping and the phase-1/phase-2 statement counters |
| `decl.c` | Declaration parsing, `parsefunc`, `parseSpan`, the graveyard |
| `declare.c` | Declarator parsing |
| `init.c` | Static/global initializer parsing |
| `istream.c` | Initializer value streaming to `.2` |
| `type.c` | Type tables |
| `tparse.c` | Type construction and parsing |
| `name.c` | Name and scope management |
| `outast.c` | Expression emission (`emitExpr`) |
| `outh.c` | AST-writer helpers (`typeSfx`, local lookup, demotion tests) |
| `outfn.c` | Function- and file-level emitters (`emitFuncPre`, `emitGv`) |
| `regalloc.c` | Register allocation and frame offset assignment |
| `util.c` | Utilities |

Generated: `enumlist.h` (from `token.h`), `error.h` (from `errorcodes` via
`makeerror.awk`), `debug.h` and `debugtags.c` (from `makedebug.sh`).

`io.c` is an empty file, not in `SOURCES`, and can be removed.
