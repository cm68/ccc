# Condition and Control Flow Labels

This document describes label generation for control flow in pass2.

## Overview

Pass2 generates numeric labels for conditions and control flow. All labels
follow the pattern `<prefix><num>_<fnIndex>` where:
- `prefix` identifies the label type
- `num` is a local counter within the function
- `fnIndex` is the function index for global uniqueness

## Label Types

| Prefix | Purpose | Example |
|--------|---------|---------|
| `no` | False/skip target (if condition false) | `no3_2` |
| `ht` | True target for OR short-circuit | `ht4_2` |
| `el` | Else branch start | `el3_2` |
| `te` | Ternary else branch | `te5_2` |
| `tn` | Ternary end | `tn5_2` |
| `ja` | AND short-circuit merge | `ja6_2` |
| `jz` | AND result zero test | `jz7_2` |
| `hz` | OR result zero test | `hz8_2` |
| `ln` | Logical not result | `ln9_2` |
| `eq` | Equality result | `eq10_2` |
| `sg` | Sign test result | `sg11_2` |
| `ni` | Long increment overflow | `ni12_2` |
| `swc` | Switch case | `swc13_2` |
| `swd` | Switch default | `swd14_2` |
| `swe` | Switch end | `swe15_2` |
| `sw` | Switch table | `sw16_2` |

## If Statement Processing

AST format: `I <nlabels> <condition> <then-body> <has_else> [<else-body>]`

The `nlabels` field tells pass2 how many intermediate labels the condition
needs for short-circuit evaluation.

```c
// In dumpStmt() case 'I':
nlabels = read1();
lbl = labelCnt++;
labelCnt += nlabels;  // reserve intermediate labels
```

### Simple If

```c
if (x) { body }
```

Generated:
```asm
    ; evaluate x
    ld a,h
    or l
    jp z,no0_1        ; skip if false
    ; body
no0_1:
```

### If-Else

```c
if (x) { then } else { else }
```

Generated:
```asm
    ; evaluate x
    jp z,no0_1        ; skip to else if false
    ; then body
    jp no1_1          ; skip else
no0_1:
el0_1:
    ; else body
no1_1:
```

## Condition Flag Propagation

The `cond` field marks expressions used as conditions. When `cond=1`, the
expression emits conditional jumps instead of computing a value.

The `aux2` field encodes the jump target:
- Positive: FALSE jump to `no{aux2}_{fnIndex}`
- Negative: TRUE jump to `ht{-aux2}_{fnIndex}`

```c
// setCondLbl2() propagates through condition tree
if (inOr)
    e->aux2 = -(lbl + 1);  // TRUE jump to ht{lbl+1}
else
    e->aux2 = lbl;         // FALSE jump to no{lbl}
```

## Short-Circuit AND (&&)

Both sides must be true. FALSE jumps out early.

```c
if (a && b) { then }
```

```asm
    ; evaluate a
    jp z,no0_1        ; a false -> skip
    ; evaluate b
    jp z,no0_1        ; b false -> skip
    ; then body
no0_1:
```

When AND is nested inside OR, it uses a local merge label:
```asm
    ; evaluate a
    jp z,ja5_1        ; a false -> local merge
    ; evaluate b
    jp z,ja5_1        ; b false -> local merge
ja5_1:                ; merge point, Z flag set if either false
```

## Short-Circuit OR (||)

Either side true is enough. TRUE jumps to merge, then falls into body.

```c
if (a || b) { then }
```

```asm
    ; evaluate a
    jp nz,ht1_1       ; a true -> take then
    ; evaluate b
    jp z,no0_1        ; b false -> skip
ht1_1:                ; merge for true path
    ; then body
no0_1:
```

## Nested Conditions

### `(a || b) && c`

```asm
    ; evaluate a
    jp nz,ht2_1       ; a true -> skip b
    ; evaluate b
    jp z,no0_1        ; b false -> skip all
ht2_1:
    ; evaluate c
    jp z,no0_1        ; c false -> skip
    ; then body
no0_1:
```

### `(a && b) || c`

```asm
    ; evaluate a
    jp z,ja3_1        ; a false -> try c
    ; evaluate b
    jp nz,ht2_1       ; b true -> take then
ja3_1:
    ; evaluate c
    jp z,no0_1        ; c false -> skip
ht2_1:
    ; then body
no0_1:
```

## Comparison Operators

Comparisons with `cond=1` emit conditional jumps via `emitCondJmp()`:

| Operator | FALSE jump (aux2 > 0) | TRUE jump (aux2 < 0) |
|----------|----------------------|---------------------|
| EQ (==)  | `jp nz,no{n}` | `jp z,ht{n}` |
| NEQ (!=) | `jp z,no{n}` | `jp nz,ht{n}` |
| LT (<)   | `jp nc,no{n}` | `jp c,ht{n}` |

Pass1 normalizes `>`, `<=`, `>=` to `<`, `==`, `!=` by rearranging operands.

## Ternary Operator

```c
x ? then : else
```

```asm
    ; evaluate x
    ld a,h
    or l
    jp z,te0_1        ; if false, go to else
    ; evaluate then
    jp tn0_1          ; skip else
te0_1:
    ; evaluate else
tn0_1:
```

## Logical Not (!)

When used as condition (`cond=1`), just emits child and IF handler flips sense.

When used for value (`cond=0`):
```asm
    ; evaluate child
    ld a,h
    or l
    ld hl,0
    jp nz,ln0_1       ; if non-zero, result is 0
    inc l             ; if zero, result is 1
ln0_1:
```

## Switch Statements

Switch uses a runtime helper with inline jump table:

```asm
    ; evaluate expr (result in HL)
    ld a,l            ; get low byte
    ld hl,sw0_1       ; table address
    jp switch         ; runtime dispatch
    ; case bodies...
swe0_1:               ; switch end
foo_break:            ; break alias (if labeled switch)
sw0_1:                ; jump table
    .db 3             ; case count
    .db 10            ; case value 0
    .dw swc1_1        ; case label 0
    .db 20            ; case value 1
    .dw swc2_1        ; case label 1
    .db 30            ; case value 2
    .dw swc3_1        ; case label 2
    .dw swd4_1        ; default (or swe0_1 if no default)
```

Cases are recorded in `swstack[swdepth]` during parsing, then emitted after
all case bodies are processed.

## Label Counter Management

```c
int labelCnt;    // per-function counter, reset at function start
int fnIndex;     // global function counter, incremented each function

// In parseFunc():
labelCnt = 0;
fnIndex++;

// In if statement:
lbl = labelCnt++;
labelCnt += nlabels;  // reserve for short-circuit
```

This ensures unique labels across the entire compilation unit.
