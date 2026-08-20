# Phase 2: Streaming AST Emission

## Overview

Phase 2 is the second pass over the lexeme stream. It parses expressions,
emits AST directly as each construct is parsed, and immediately frees
expression nodes. Phase 2 relies on:

1. The symbol table built by phase 1
2. Pre-computed counts pushed by phase 1 (case counts, statement counts)
3. If/else flags recorded by phase 1 in a bitmap
4. Per-function locals captured by value in `f->u.locals`
5. Switch tables with per-case statement counts
6. String literals already emitted to .2 file by phase 1

**Output files:** Pass1 produces two output files:
- **.1 file** (`astFd`): Binary AST for functions, emitted by phase 2
- **.2 file** (`asmFd`): Assembly for data (strings, globals), emitted by both phases

String literals are emitted to the .2 file during phase 1. Phase 2 emits
references to these `str<n>` labels in the AST when it encounters string
expressions.

**Note:** The cpp preprocessor has already:
- Inserted braces around single-statement control bodies
- Lowered while/for/do loops to if/goto/label sequences
- Converted break/continue to goto statements — **including in a switch**
- Split local declaration initializers (`int x = 5;` → `int x; x = 5;`)
- Dissolved typedefs and lowered enums

This means phase 2 only handles `if`, `goto`, and labels for control flow.
All loop constructs have been eliminated before pass1 sees the code.

## Entry Point

Phase 2 runs per **span**, immediately after phase 1 has covered the same span
(see [PHASE1.md](PHASE1.md)). `process()` rewinds only that span and resets the
read pointers:

```c
lexSeek(spanBase);     /* rewind to where this span began */
globalStrCtr = spanStr;/* so the strN phase 1 emitted line up */
lexlevel = 1;          /* back to file scope */
resetFuncIdx();        /* function stmt count read pointer */
flipBlkCnts();         /* block counts were pushed LIFO; make them FIFO */
resetCountIdx();       /* misc count read pointer */

phase = 2;
parseSpan();           /* emit AST for this span */

drainGraves();         /* the span's names are unreferenced now */
```

`drainGraves()` after emission is what keeps the live set to one function: the
span has been emitted and its expressions freed, so nothing points at the
names `popScope()` parked on the way out.

## Key Behaviors

### 1. Expressions ARE Built

In phase 2, `parseExpr()` fully parses expressions and builds `struct expr`
trees. These trees are used for:

- Type checking and implicit conversions
- Constant folding
- AST emission
- Then immediately freed after emission

### 2. Streaming Emission (No Statement Trees)

Phase 2 emits AST directly as it parses, without building statement trees.
Control structures emit inline using pre-computed counts from phase 1:

`stIf2()` in `pblock.c`:

```c
for (;;) {
    hasElse = (ifHasElse[ifEmitIdx >> 3] >> (ifEmitIdx & 7)) & 1;
    ifEmitIdx++;
    gettoken();
    expect(LPAR, ER_S_NP);
    e1 = parseExpr(PRI_ALL);
    expect(RPAR, ER_S_NP);
    /* Emit: IF nlabels cond then has_else [else] */
    e1 = foldTree(e1);
    emit1(IF);
    emit1(cntCondLbls(e1));
    emitExpr(e1);
    FreeExpr(e1);
    parseBlock();          /* emits the then-body inline */
    emit1(hasElse);        /* has_else comes AFTER the then block */
    if (cur.type == ELSE) {
        gettoken();
        if (cur.type == IF)
            continue;      /* else if: run the arm again */
        parseBlock();      /* emits the else-body inline */
    }
    return;
}
```

No statement node is created — there is no `struct stmt` in pass1.

Note `foldTree()` before `emit1(IF)`: `emitExpr()` folds internally and may
replace the root node, so folding first is what keeps `e1` valid for
`FreeExpr()`.

### 3. Control Flow (No Loop Lowering)

Loops are lowered by cpp before pass1 sees the code. Phase 2 simply emits:
- IF statements with their conditions and bodies
- GOTO statements (including cpp-generated loop gotos)
- LABEL statements (including cpp-generated loop labels like `__W1T`, `__F1B`)

Pass1 has no special handling for WHILE, FOR, DO, or CONTINUE tokens
since they are never present in the input stream. BREAK is only seen
inside switch statements.

### 4. Break/Continue (Handled entirely by cpp)

Break and continue are resolved to gotos by cpp. Pass1 **never** sees a `BREAK`
or `CONTINUE` token — not in a loop and not in a switch. A switch is not
lowered, but cpp still appends its `__S<n>B` label and rewrites the `break`s
inside it to `goto __S<n>B`.

Consequently pass1 keeps no label stack of any kind, and emits `has_label = 0`
on every `SWITCH`.

### 5. Function Processing

For each function, phase 2:

1. Looks up the function's phase 1 `kfdef` entry to get `u.locals`
2. Calls `emitFuncPre()`, which calls `analyzeFunc()` for register allocation
   and frame layout, then emits:
   - Function header (name, return type)
   - Parameter count, local count, frame size (2 bytes), savebase
   - Parameter declarations with types, registers, and 1-byte frame offsets
   - Local declarations with registers and **2-byte** frame offsets
   - The block prefix for the function body, with its statement count
3. Calls `statement()`, which streams the body
4. Each statement is parsed, emitted, then freed
5. On the way out, frees the phase 1 local copies — they have served register
   allocation and nothing else needs them

```c
if (phase == 2) {
    emitFuncPre(f);   /* header, params, locals, block prefix */
    statement();      /* streams: parses, emits, frees each stmt */
}
```

### Register Allocation Policy

The Z80 has limited registers available for local variables:

- **IX** - Index register, used for struct/union field access (`p->field`)
- **BC** - General 16-bit register pair
- **B, C** - Individual 8-bit registers

Allocation priority:

1. **Explicit `register` keyword** - Variables marked `register` get first choice
2. **IX for pointers with field access** - Only pointers actually used for
   member access (`p->field` or `s.member`) are allocated to IX. A struct
   pointer that's just passed around without field access goes to BC instead.
   This is tracked via `agg_refs` count during expression parsing.
3. **BC for high-use word variables** - 16-bit variables with highest `ref_count`
4. **B/C for byte variables** - if BC was not taken as a pair

Word before byte is the **measured** answer, not an accident of ordering:
three attempts to be cleverer (weighting loop references by depth, among
others) all made the compiler bigger.

Never eligible, whatever the counts say:
- a variable whose address is taken
- an array or aggregate — it *is* its storage, so giving one IX made
  `stack[0].l = 11` assign to the register
- a static local — it must survive a call, and a register does not
- any parameter, if *any* parameter has its address taken

The `agg_refs` field is incremented during phase 1 (`skipExpr`) when DOT or
ARROW operators are encountered on a local variable. This ensures the count
is available when `analyzeFunc()` runs at the start of phase 2.

Example: In `foo(struct bar *p) { other(p, 0); }`, `p` has no field access
so `agg_refs=0` and it goes to BC. In `foo(struct bar *p) { return p->x; }`,
`p` has `agg_refs=1` and goes to IX.

### 6. Count Retrieval

Phase 2 retrieves counts pushed by phase 1:

**Function body stmt counts:**
```c
stmt_count = popFuncCnt();
emit1(AST_BLOCK);
emit1(0);
emit1(stmt_count);
```

**Block stmt counts:**
```c
cnt = popBlkCnt();
emit1(AST_BLOCK);
emit1(0);
emit1(cnt);
```

**Switch case counts:**
```c
emit1(SWITCH);
emit1(0);           // has_label: always 0, cpp lowered break
emit1(popCount());  // the slot phase 1 reserved and patched
```

**If has-else flags** (a bitmap):
```c
hasElse = (ifHasElse[ifEmitIdx >> 3] >> (ifEmitIdx & 7)) & 1;
ifEmitIdx++;
/* ... emit then block ... */
emit1(hasElse);
```

### 7. Switch Emission

Switch statements use the tables built in phase 1. There is **no label stack**
and no `S<n>` label: cpp already appended `__S<n>B` and rewrote the `break`s.

`stSwitch2()` in `pblock.c`:

```c
gettoken();
expect(LPAR, ER_S_NP);
e1 = parseExpr(PRI_ALL);
expect(RPAR, ER_S_NP);
expect(BEGIN, ER_S_SB);
sw_idx = swEmitIdx++;                  /* this switch's table */
swEmitStack[swEmitDepth++] = sw_idx;
swList[sw_idx].emitIdx = 0;            /* case cursor lives in the table */
/* Emit: SWITCH has_label case_count expr */
e1 = foldTree(e1);
emit1(SWITCH);
emit1(0);                              /* no label - cpp lowered break */
emit1(popCount());
emitExpr(e1);
FreeExpr(e1);
statement();                           /* CASE/DEFAULT emit themselves */
swEmitDepth--;
expect(END, ER_S_CC);
```

`nextCase()` (in `pblock.c`) advances the innermost switch's `emitIdx` and
returns the `struct swcase`, so a case emits:

```c
/* CASE: the value is re-parsed, not stored */
e1 = parseExpr(13);
expect(COLON, ER_S_NL);
sc = nextCase();
e1 = foldTree(e1);
emit1(CASE);
emit1(sc->stmts);       /* pre-computed in phase 1 */
emitExpr(e1);
FreeExpr(e1);

/* DEFAULT */
sc = nextCase();
emit1(DEFAULT);
emit1(sc->stmts);
```

## AST Binary Format

Fully specified in [AST_FORMAT.md](../AST_FORMAT.md). In summary:

### Statement Opcodes
- `AST_FUNC` (221) - Function definition
- `AST_BLOCK` (222) - Block (compound statement)
- `AST_DECL` (224) - Variable/parameter declaration
- `AST_EMPTY` (225) - Null/empty expression
- `IF` (147), `GOTO` (145), `LABEL` (112), `RETURN` (146)
- `SWITCH` (150), `CASE` (151), `DEFAULT` (155)
- `SEMI` (1) - Empty statement
- `ASM` (157) - Inline assembly, with a 2-byte length and the text

An **expression statement has no opcode** — the expression is emitted directly.

`AST_GLOBAL` (223) is reserved but unused: globals go to `.2` as assembly, not
into the AST. `STRING` (22) is likewise not a statement opcode — a string
literal's data goes to `.2` in phase 1 and the AST refers to it with a `SYM`.

### Expression Format
- `NUMBER` (21) - Constant (type suffix, then 4-byte LE value)
- `SYM` (20) - Symbol reference (counted string; **no type suffix**)
- `LOCALVAR` (217) - type suffix, then a **2-byte** IY-relative offset
- `REGVAR` (216) - type suffix, then a register number
- `DEREF` (201) - type suffix, then the address expression
- `AST_EMPTY` (225) - null/empty expression placeholder
- Operators use their token values with a type suffix

### Type Suffixes

`typeSfx()` in `outh.c` emits exactly seven characters:

- `b` / `B` - signed / unsigned byte
- `s` / `S` - signed / unsigned short-int
- `l` / `L` - signed / unsigned long
- `v` - void

**Everything address-valued — pointer, array, function — emits `s`**, because
the value is a 16-bit address whatever its element size. There are no `f`,
`d`, `p`, `a`, or `r` suffixes; ccc has no floating point.

## Label Naming Conventions

**All** control-flow labels are generated by cpp; pass1 just passes them
through as `LABEL`/`GOTO` tokens:

| Construct | Labels | Purpose |
|-----------|--------|---------|
| WHILE | `__W<n>T`, `__W<n>B` | Top, Break |
| FOR | `__F<n>T`, `__F<n>C`, `__F<n>B` | Top, Continue, Break |
| DO | `__D<n>T`, `__D<n>C`, `__D<n>B` | Top, Continue, Break |
| SWITCH | `__S<n>B` | Break exit point |

Pass1 generates **no** labels. A `break` inside a switch has already become
`goto __S<n>B` before pass1 sees it.

## Variable Naming Conventions

Identifiers reach pass1 as 2-byte ids, so what pass1 writes is an `@<id>`
marker with whatever prefix it adds; pass2 resolves the marker from cpp's `.n`
sidecar.

- Global public: `_@<id>` (underscore prefix)
- Static global: `S<id>` (S0, S1, ...)
- Static local: `S<id>` (same sequence as global statics)
- Shadowed local: `L<id>` (when a local shadows an outer name)
- String literal: `str<n>` (str0, str1, ...) — all strings share one counter,
  rewound to the span's start between phases so both phases agree

## Memory Management

Phase 2 manages memory carefully for the Z80 target:

1. **Expression trees:** Built, emitted, then freed via `FreeExpr()`
2. **No statement trees:** every control structure emits inline
3. **Symbol table:** Shared with phase 1 of the same span, then drained
4. **Per-function locals:** freed at the end of `parsefunc()` in phase 2
5. **Count buffers:** Fixed size, reset per span by `resetSpanCnts()`

The span loop is the memory story: phase 2 frees a function's locals and names
before phase 1 goes looking for the next function's, so the live set is one
function rather than the whole file.

Debug counters track allocations:
- `exprAllocCnt`, `exprCurCnt`, `exprHighWater` - Expression node tracking
- `nameAllocCnt`, `nameCurCnt`, `nameHighWater` - Name node tracking

## File References

- `pass1.c:process()` - The span loop, phase control and initialization
- `decl.c:parseSpan()` - One span's worth of top-level parsing
- `parse.c:statement()` - The statement machine, both phases
- `pblock.c:stIf2/stRet2/stSwitch2/stExpr2/stGoto2` - The phase 2 statement arms
- `pblock.c:parseBlockEx()` - Braced block bodies with the block header
- `pblock.c:nextCase()` - The switch case cursor
- `expr.c:parseExpr()` - Expression tree building
- `fold.c:foldTree()/foldNode()` - Constant folding before emission
- `decl.c:parsefunc()` - Phase 2 function processing and teardown
- `outast.c:emitExpr()` - Expression emission
- `outfn.c:emitFuncPre()` - Function header, params, locals, block prefix
- `outfn.c:emitGv()` - Uninitialized global emission (to `.2`)
- `outfn.c:emitAsmStmt()` - In-body inline assembly (into the AST)
- `outh.c:typeSfx()` - Type suffix characters
- `regalloc.c:analyzeFunc()` - Register allocation and frame layout
