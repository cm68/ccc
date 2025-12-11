# Condition and Loop Label Schemes

This document describes label generation for control flow statements.

## Overview

There are two distinct label schemes in ccc:

1. **Loop labels** (cc1): Text labels like `L0_top`, `L0_break` for loops/switch
2. **Condition labels** (cc2): Numeric labels like `yes0_1`, `el0_2` for if/ternary

## Loop Labels (cc1)

Generated in `parse.c` by `genLoopLabel()` using a global counter:

```c
static int loopLblCnt = 0;
static char *genLoopLabel(const char *prefix) {
    sprintf(label, "%s%d", prefix, loopLblCnt++);
    return label;
}
```

**Prefixes:**
- `L` - while/for loops
- `D` - do-while loops
- `S` - switch statements

**Suffixes added by outast.c:**
- `_top` - loop start (condition test)
- `_continue` - continue target
- `_break` - break target
- `_test` - do-while condition (before test)

**Example:** A while loop generates:
```
L0_top:
    ; condition test
    jp z, L0_break
    ; body
L0_continue:
    jp L0_top
L0_break:
```

All loops are lowered to labeled if/goto sequences by cc1. The labels are
stored as text in `stmt->label` and emitted directly.

## Condition Labels (cc2)

For if statements and ternary operators, cc2 uses numeric labels with a
two-part scheme: function index + label number.

**Label format:** `<type><fnIndex>_<labelNum>`

**Types:**
- `yes` - then-branch start (true path)
- `el` - else-branch start
- `no` - if-end (after else, or end if no else)
- `orE` - OR short-circuit end
- `anE` - AND short-circuit end
- `tF` - ternary false branch
- `tE` - ternary end

**Example:** Function 0, if statement with label 3:
```
    jp z, el0_3
yes0_3:
    ; then body
    jp no0_3
el0_3:
    ; else body
no0_3:
```

### Label Counter

The `nlabels` field in the AST IF statement tells cc2 how many intermediate
labels are needed for short-circuit `||` and `&&` evaluation:

```
I<has_else><nlabels><condition><then>[<else>]
```

cc2's `parseast.c` maintains a `labelCounter` that is incremented for each
label slot needed. The `fnIndex` resets to 0 at each function.

## Short-Circuit Evaluation

### OR (||)
```c
if (a || b) { then } else { else }
```
- `a` true → fall through to `yes`
- `a` false → evaluate `b`
- `b` true → fall through to `yes`
- `b` false → jump to `el`

### AND (&&)
```c
if (a && b) { then } else { else }
```
- `a` false → jump to `el`
- `a` true → evaluate `b`
- `b` false → jump to `el`
- `b` true → fall through to `yes`

### Nested: `(a || b) && c`

The `||` needs an intermediate `orE` label:
```
    ; eval a
    jp nz, orE0_3     ; a true -> skip b
    ; eval b
    jp z, el0_1       ; b false -> else
orE0_3:
    ; eval c
    jp z, el0_1       ; c false -> else
yes0_1:
    ; then body
```

### Counting Intermediate Labels

`cntCondLbls()` in `outast.c` walks the condition tree:

- `||` inside `&&` right side or at top → needs `orE` label (+1)
- `&&` inside `||` right side or at top → needs `anE` label (+1)
- Chained `||` or `&&` share parent's target (no extra label)

The count is emitted as `nlabels` in the AST so cc2 knows how many labels
to pre-allocate.

## Integration

1. **cc1** generates loop labels as text (`L0_top`, etc.) and embeds them
   directly in the lowered if/goto sequences

2. **cc1** counts intermediate condition labels and emits the count in AST

3. **cc2** uses `fnIndex` + `labelCounter` to generate unique condition
   labels per function

This separation keeps cc1 simple (text labels for loops) while allowing
cc2 to manage condition labels efficiently (numeric indices).
