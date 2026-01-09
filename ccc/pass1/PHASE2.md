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

**Note:** The cpp preprocessor has already:
- Inserted braces around single-statement if/else bodies
- Lowered while/for/do loops to if/goto/label sequences
- Converted break/continue to goto statements
- Split local declaration initializers (`int x = 5;` → `int x; x = 5;`)

This means phase 2 only handles `if`, `goto`, and labels for control flow.
All loop constructs have been eliminated before pass1 sees the code.

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
    /* Emit: IF nlabels cond then has_else [else] */
    emit1(IF);
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

### 3. Control Flow (No Loop Lowering)

Loops are lowered by cpp before pass1 sees the code. Phase 2 simply emits:
- IF statements with their conditions and bodies
- GOTO statements (including cpp-generated loop gotos)
- LABEL statements (including cpp-generated loop labels like `__W1T`, `__F1B`)

Pass1 has no special handling for WHILE, FOR, DO, or CONTINUE tokens
since they are never present in the input stream. BREAK is only seen
inside switch statements.

### 4. Break/Continue (Handled by cpp)

Break and continue statements in loops are resolved to goto statements by cpp
during loop lowering. Pass1 never sees BREAK or CONTINUE tokens for loops.

For switch statements, break is still handled by pass1 since switches are not
lowered. Switch break tracking uses the label stack to find the innermost
switch context.

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
case_cnt = popCount();
emit1(SWITCH);
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
    emit1(SWITCH);
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
    /* Emit: CASE stmt_count value_expr */
    emit1(CASE);
    emit1(c->stmts);  /* pre-computed in phase 1 */
    emitExpr(e);
    FreeExpr(e);
}

case DEFAULT: {
    sw_idx = swEmitStack[swEmitDepth - 1];
    c_idx = caseEmitIdx[sw_idx]++;
    c = &swList[sw_idx].cases[c_idx];
    /* Emit: DEFAULT stmt_count */
    emit1(DEFAULT);
    emit1(c->stmts);  /* pre-computed in phase 1 */
}
```

## AST Binary Format

The AST is emitted as a compact binary format:

### Statement Opcodes
- `AST_FUNC` (221) - Function definition
- `AST_BLOCK` (222) - Block (compound statement)
- `IF` (147) - If statement
- `GOTO` (145) - Goto
- `LABEL` (112) - Label
- `EXPR` (202) - Expression statement
- `RETURN` (146) - Return
- `SWITCH` (150) - Switch
- `CASE` (151) - Case
- `DEFAULT` (155) - Default
- `SEMI` (1) - Empty statement
- `ASM` (157) - Inline assembly
- `STRING` (22) - String literal (emitted in phase 1)
- `AST_GLOBAL` (223) - Global variable
- `AST_DECL` (224) - Variable/parameter declaration
- `AST_EMPTY` (225) - Null/empty expression

### Expression Format
- `NUMBER` (21) - Constant (followed by type suffix and 4-byte value)
- `SYM` (20) - Symbol reference (followed by counted string)
- `DEREF` (201) - Memory dereference (followed by type and address expr)
- `AST_EMPTY` (225) - Null/empty expression placeholder
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

Loop labels are generated by cpp during loop lowering (pass1 just passes them through):

| Loop Type | Labels | Purpose |
|-----------|--------|---------|
| WHILE | `__W<n>T`, `__W<n>B` | Top, Break |
| FOR | `__F<n>T`, `__F<n>C`, `__F<n>B` | Top, Continue, Break |
| DO | `__D<n>T`, `__D<n>C`, `__D<n>B` | Top, Continue, Break |

Switch labels are generated by pass1:

| Type | Label | Purpose |
|------|-------|---------|
| SWITCH | `S<n>_break` | Break exit point |

Break inside switch emits `G S<n>_break`.

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
