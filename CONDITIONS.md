# Condition Label Scheme

This document describes how labels are assigned for conditional statements
(if/while/for) with short-circuit evaluation of `||` and `&&` operators.

## Overview

Each IF statement needs labels for control flow:
- **yes**: Start of then-body (usually fall-through, but `||` jumps here on true)
- **el**: Start of else-body (jump here on condition false, if else exists)
- **no**: End of IF statement (then-body jumps here to skip else)

Short-circuit `||` and `&&` operators may need intermediate labels when nested.

## Label Assignment

Labels are assigned sequentially starting from 0 for each function:
```
Label 0: yes  (then-body start)
Label 1: el   (else-body start, or no if no else)
Label 2: no   (if-end, when else exists)
Label 3+: intermediate labels for nested ||/&&
```

## Short-Circuit Evaluation

### OR (||)
```c
if (a || b) { then } else { else }
```
- `a` true → jump to `yes`
- `a` false → fall through, evaluate `b`
- `b` true → fall through to `yes`
- `b` false → jump to `el`

### AND (&&)
```c
if (a && b) { then } else { else }
```
- `a` false → jump to `el`
- `a` true → fall through, evaluate `b`
- `b` false → jump to `el`
- `b` true → fall through to `yes`

### Chained OR: `a || b || c`
```
      ||
     /  \
    ||   c
   /  \
  a    b
```
All true cases share the same `yes` label:
- `a` true → jump to `yes`
- `a` false → fall through to `b`
- `b` true → jump to `yes`
- `b` false → fall through to `c`
- `c` true → fall through to `yes`
- `c` false → jump to `el`

No intermediate labels needed.

### Chained AND: `a && b && c`
```
      &&
     /  \
    &&   c
   /  \
  a    b
```
All false cases share the same `el` label:
- `a` false → jump to `el`
- `a` true → fall through to `b`
- `b` false → jump to `el`
- `b` true → fall through to `c`
- `c` false → jump to `el`
- `c` true → fall through to `yes`

No intermediate labels needed.

### Nested: `(a || b) && c`
```
       &&
      /  \
    ||    c
   /  \
  a    b
```
The `||` needs an intermediate label for its "end" (where true case goes):
- `a` true → jump to `orEnd` (skip `b`, continue to `c`)
- `a` false → fall through to `b`
- `b` true → fall through to `orEnd`
- `b` false → jump to `el` (whole && fails)
- `orEnd`: evaluate `c`
- `c` false → jump to `el`
- `c` true → fall through to `yes`

**Intermediate label needed: 1 (orEnd)**

### Nested: `(a && b) || c`
```
       ||
      /  \
    &&    c
   /  \
  a    b
```
The `&&` needs an intermediate label for its "end" (where false case goes):
- `a` false → jump to `anEnd` (skip `b`, continue to `c`)
- `a` true → fall through to `b`
- `b` false → fall through to `anEnd`
- `b` true → jump to `yes` (whole || succeeds)
- `anEnd`: evaluate `c`
- `c` true → fall through to `yes`
- `c` false → jump to `el`

**Intermediate label needed: 1 (anEnd)**

### Complex: `(a || b) && (c || d)`
```
           &&
          /  \
        ||    ||
       / \   / \
      a   b c   d
```
Left `||`:
- `a` true → jump to `orEnd1`
- `a` false → fall through to `b`
- `b` true → fall through to `orEnd1`
- `b` false → jump to `el`

`orEnd1`: evaluate right `||`:
- `c` true → jump to `yes`
- `c` false → fall through to `d`
- `d` true → fall through to `yes`
- `d` false → jump to `el`

**Intermediate labels needed: 1 (orEnd1)**

Note: Right `||` doesn't need an intermediate label because its true case
goes directly to `yes`.

## Counting Intermediate Labels

Walk the condition tree and count labels needed:

```
countLabels(node, context):
    if node is ||:
        if context is AND_RIGHT or TOP:
            // || inside && needs orEnd label
            return 1 + countLabels(left, OR_LEFT) + countLabels(right, OR_RIGHT)
        else:
            // chained || shares parent's target
            return countLabels(left, OR_LEFT) + countLabels(right, context)

    if node is &&:
        if context is OR_RIGHT or TOP:
            // && inside || needs anEnd label
            return 1 + countLabels(left, AND_LEFT) + countLabels(right, AND_RIGHT)
        else:
            // chained && shares parent's target
            return countLabels(left, AND_LEFT) + countLabels(right, context)

    if node is !:
        // NOT just inverts, pass context through
        return countLabels(child, context)

    // leaf node (comparison, variable, etc)
    return 0
```

## AST Format

IF statement in AST:
```
I flags nlabels condition then [else]
```

Where:
- `flags`: bit 0 = has_else
- `nlabels`: number of intermediate labels needed (beyond yes/el/no)
- `condition`: the condition expression with label indices on ||/&& nodes
- `then`: then-body statements
- `else`: else-body statements (if has_else)

Each `||` and `&&` node that needs an intermediate label gets annotated:
```
h labelidx left right    // || with intermediate label
j labelidx left right    // && with intermediate label
```

Label indices are assigned:
- 0 = yes
- 1 = el (or no if no else)
- 2 = no (if else exists)
- 3+ = intermediate labels in tree-walk order

## Pass2 Usage

Pass2 reads the label count and pre-allocates that many labels for the IF.
When emitting `||`/`&&`, it reads the label index from the AST instead of
calling `newLabel()`.

This eliminates the "255 means no label" hack - every jump target has an
explicit label index in the AST.
