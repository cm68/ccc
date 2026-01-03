# Phase 2: Streaming AST Emission

## Overview

Phase 2 is the second pass over the lexeme stream. It parses expressions,
emits AST directly as each construct is parsed, and immediately frees
expression nodes. Phase 2 relies on:

1. The symbol table built by phase 1
2. Pre-computed counts pushed by phase 1 (case counts, statement counts)
3. If/else flags recorded by phase 1
4. Local variable information captured in function stubs
5. Switch tables with per-case statement counts

## Entry Point

After phase 1 completes, `process()` resets state and runs phase 2:

```c
lexRewind();         // Rewind lexeme stream
lexlevel = 0;        // Reset scope level
resetLoopLbls();     // Reset label counter (must match phase 1)
resetFuncIdx();      // Reset function stmt count read pointer
flipBlkCnts();       // Reverse block counts for phase 2
funcStrCtr = 0;      // Reset string counter (phase 2 uses same sequence)

phase = 2;
parse();             // Phase 2: emit AST
```

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

```c
case IF: {
    unsigned char hasElse = ifHasElse[ifEmitIdx++];
    gettoken();
    expect(LPAR);
    cond = parseExpr(PRI_ALL, parent);
    expect(RPAR);
    /* Emit: I nlabels cond then has_else [else] */
    emit1('I');
    emit1(cntCondLbls(cond));
    emitExpr(cond);
    FreeExpr(cond);
    parseBlock();         /* Emits body inline */
    emit1(hasElse);       /* has_else comes after then block */
    if (cur.type == ELSE) {
        gettoken();
        if (cur.type == IF)
            goto handle_if2;  /* else if */
        parseBlock();     /* Emits else inline */
    }
    st = NULL;  /* No statement node created */
    break;
}
```

### 3. Control Flow Lowering

Loops are lowered to labeled if/goto sequences during emission:

**WHILE loop:**
```c
case WHILE: {
    gettoken();
    expect(LPAR);
    cond = parseExpr(PRI_ALL, parent);
    expect(RPAR);
    pushLabel(WHILE);
    num = lblStack[lblDepth - 1].num;
    sprintf(lbl, "W%d", num);
    body_cnt = popBlkCnt();  /* get body stmt count from phase 1 */
    /* Wrap condition in NOT for "if NOT(cond) goto break" */
    notcond = mkexpr(BANG, cond);
    /* Emit: B 0 (4+body) L<T> I(NOT cond){goto B} body G<T> L<B> */
    emit1('B');
    emit1(0);
    emit1(4 + body_cnt);
    emitLabel(lbl, "T");
    emit1('I');
    emit1(cntCondLbls(notcond));
    emitExpr(notcond);
    FreeExpr(notcond);
    emit1('B');
    emit1(0);
    emit1(1);
    emitGoto(lbl, "B");
    emit1(0);  /* has_else=0 */
    parseBlockEx(0);  /* body (no header - already emitted) */
    emitGoto(lbl, "T");
    emitLabel(lbl, "B");
    popLabel();
}
```

**FOR loop:**
```c
case FOR: {
    /* Parse init, cond, incr expressions */
    /* Emit structure:
     *   B 0 N
     *     [E init]
     *     L F<n>T
     *     [I nlabels NOT(cond) { B 0 1 G F<n>B } 0]
     *     <body>
     *     L F<n>C
     *     [E incr]
     *     G F<n>T
     *     L F<n>B
     */
}
```

**DO loop:**
```c
case DO: {
    /* Emit structure:
     *   B 0 5
     *     L D<n>T
     *     <body>
     *     L D<n>C
     *     I 0 nlabels cond G D<n>T
     *     L D<n>B
     */
}
```

### 4. Break/Continue Resolution

In phase 2, break/continue are resolved to goto statements:

```c
case BREAK: {
    char lbl[16];
    int i;
    gettoken();
    expect(SEMI);
    /* Find innermost loop or switch */
    for (i = lblDepth - 1; i >= 0; i--) {
        char prefix = 'W';
        if (lblStack[i].type == FOR) prefix = 'F';
        else if (lblStack[i].type == DO) prefix = 'D';
        else if (lblStack[i].type == SWITCH) prefix = 'S';
        sprintf(lbl, "%c%d", prefix, lblStack[i].num);
        break;
    }
    /* Switch uses _break suffix */
    if (lblStack[i].type == SWITCH)
        emitGoto(lbl, "_break");
    else
        emitGoto(lbl, "B");
}

case CONTINUE: {
    /* Find innermost loop (skip switches) */
    for (i = lblDepth - 1; i >= 0; i--) {
        if (lblStack[i].type == SWITCH)
            continue;  /* continue doesn't apply to switch */
        // ...
    }
    /* WHILE: continue goes to top (T), others go to C */
    emitGoto(lbl, type == WHILE ? "T" : "C");
}
```

### 5. Function Processing

For each function, phase 2:

1. Looks up the function's phase 1 definition to get locals
2. Calls `analyzeFunc()` to perform register allocation
3. Calls `emitFuncPre()` to emit:
   - Function header (name, return type)
   - Parameter count, local count, frame size
   - Parameter declarations with types, registers, and frame offsets
   - Local variable declarations with register/offset info
   - Block prefix for function body with stmt count
4. Calls `statement(0)` which streams the body
5. Each statement is parsed, emitted, then freed

```c
if (phase == 2) {
    emitFuncPre(f);   // Emit header, params, locals, block prefix
    statement(0);     // Streams: parses, emits, frees each stmt
}
```

### 6. Count Retrieval

Phase 2 retrieves counts pushed by phase 1:

**Function body stmt counts:**
```c
stmt_count = popFuncCnt();
emit1('B');
emit1(0);
emit1(stmt_count);
```

**Block stmt counts:**
```c
cnt = popBlkCnt();
emit1('B');
emit1(0);
emit1(cnt);
```

**Switch case counts:**
```c
case_cnt = popCount();
emit1('S');
emit1(1);  // has_label
emitS(lbl);
emit1(case_cnt);
```

**If has-else flags:**
```c
hasElse = ifHasElse[ifEmitIdx++];
// ... emit then block ...
emit1(hasElse);
```

### 7. Switch Emission

Switch statements use the tables built in phase 1:

```c
case SWITCH: {
    /* Get this switch's index and push onto emit stack */
    idx = swEmitIdx++;
    swEmitStack[swEmitDepth++] = idx;
    caseEmitIdx[idx] = 0;
    pushLabel(SWITCH);
    num = lblStack[lblDepth - 1].num;
    sprintf(lbl, "S%d", num);
    /* Emit switch header */
    emit1('S');
    emit1(1);  /* has label */
    emitS(lbl);
    case_cnt = popCount();
    emit1(case_cnt);
    emitExpr(e);
    FreeExpr(e);
    /* Parse body - CASE/DEFAULT emit themselves */
    statement(parent);
    popLabel();
    swEmitDepth--;
}

case CASE: {
    sw_idx = swEmitStack[swEmitDepth - 1];
    c_idx = caseEmitIdx[sw_idx]++;
    c = &swList[sw_idx].cases[c_idx];
    /* Emit: C stmt_count value_expr */
    emit1('C');
    emit1(c->stmts);  /* pre-computed in phase 1 */
    emitExpr(e);
    FreeExpr(e);
}

case DEFAULT: {
    sw_idx = swEmitStack[swEmitDepth - 1];
    c_idx = caseEmitIdx[sw_idx]++;
    c = &swList[sw_idx].cases[c_idx];
    /* Emit: O stmt_count */
    emit1('O');
    emit1(c->stmts);  /* pre-computed in phase 1 */
}
```

## AST Binary Format

The AST is emitted as a compact binary format:

### Statement Opcodes
- `F` - Function definition
- `B` - Block (compound statement)
- `I` - If statement
- `G` - Goto
- `L` - Label
- `E` - Expression statement
- `R` - Return
- `S` - Switch
- `C` - Case
- `O` - Default (Originally "Otherwise")
- `;` - Empty statement
- `A` - Inline assembly
- `U` - String literal (emitted in phase 1)
- `Z` - Global variable

### Expression Format
- `#` - Constant (followed by type suffix and 4-byte value)
- `$` - Symbol reference (followed by counted string)
- `M` - Memory dereference (followed by type and address expr)
- Operators use their token values with type suffix

### Type Suffixes
- `b` - byte (char, unsigned: `B`)
- `s` - short/int (unsigned: `S`)
- `l` - long (unsigned: `L`)
- `f` - float
- `v` - void
- `p` - pointer
- `a` - array
- `r` - aggregate (struct/union)

## Label Naming Conventions

| Loop Type | Labels | Purpose |
|-----------|--------|---------|
| WHILE | `W<n>T`, `W<n>B` | Top, Break |
| FOR | `F<n>T`, `F<n>C`, `F<n>B` | Top, Continue, Break |
| DO | `D<n>T`, `D<n>C`, `D<n>B` | Top, Continue, Break |
| SWITCH | `S<n>_break` | Break only |

Break statements emit `G <label>B` (or `G <label>_break` for switch).
Continue statements emit `G <label>T` (WHILE) or `G <label>C` (FOR/DO).

## Variable Naming Conventions

- Global public: `_name` (underscore prefix)
- Static global: `S<id>` (S0, S1, ...)
- Static local: `S<id>` (same sequence as global statics)
- Shadowed local: `L<id>` (when local shadows outer name)
- Function-local string: `fs<n>` (fs0, fs1, ...)
- Global string: `str<n>` (str0, str1, ...)

## Memory Management

Phase 2 manages memory carefully for the Z80 target:

1. **Expression trees:** Built, emitted, then freed via `FreeExpr()`
2. **No statement trees:** Most control structures emit inline
3. **Symbol table:** Shared with phase 1, freed at end of compilation
4. **Count buffers:** Fixed size, reset between files

Debug counters track allocations:
- `exprAllocCnt`, `exprCurCnt`, `exprHighWater` - Expression node tracking
- `nameAllocCnt`, `nameCurCnt`, `nameHighWater` - Name node tracking

## File References

- `pass1.c:process()` - Phase control and initialization
- `parse.c:statement()` - Phase 2 statement parsing (lines 631-1083)
- `expr.c:parseExpr()` - Expression tree building
- `decl.c:parsefunc()` - Phase 2 function processing
- `outast.c:emitExpr()` - Expression emission
- `outast.c:emitFuncPre()` - Function header emission
- `outast.c:emitGv()` - Global variable emission
- `outast.c:emitStrLit()` - String literal emission
- `regalloc.c:analyzeFunc()` - Register allocation and frame layout
