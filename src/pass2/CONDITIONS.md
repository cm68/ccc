# Conditions and Control Flow Labels

This document describes how pass2 lowers conditions and names the labels it
generates.

## Label Naming

Two schemes, both ending in the **function index** — `labelcnt` starts again at
zero in every function, so without it the first label of one function and the
first of the next collide.

| Form | Purpose |
|------|---------|
| `no<n>_<fn>` | An `if`'s false target, and (as `no<n+1>_<fn>`) its end target |
| `_C<n>` | A short-circuit join inside a condition |
| `_D<id>_<fn>` | A switch's dispatch |
| `_K<id>_<fn>_<m>` | Case *m* of switch *id* |
| `_N<id>_<fn>` | A switch's "nothing matched" |
| `_F<id>_<fn>` | A switch's default |
| `_X<id>_<fn>` | Past the whole switch |
| `X<name>` | A function's exit (see [STACK.md](STACK.md)) |

Loop labels (`__W<n>T`, `__F<n>B`, …) are **not** pass2's — cpp generated them
and they arrive as ordinary `LABEL` statements.

`labelcnt` is per function, reset in the `AST_FUNC` case; `fnindex` increments
there and never resets.

## If Statement

AST format: `IF <nlabels> <condition> <then-body> <has_else> [<else-body>]`

```c
lbl = labelcnt;
labelcnt += 2 + n;      /* lbl, lbl+1, and the short-circuits */
```

**Two numbers, not one.** An `if` with an else emits `no<lbl>` for the false
branch and `no<lbl+1>` to jump over the else — but only one used to be
reserved, and whether there *is* an else is not known until the then-body has
been read, by which time any `if` inside it has already taken the next number.
So `no<lbl+1>` was defined twice and every jump to it went to whichever the
assembler kept. In an else-if chain the body of a branch was simply skipped:
cpp built this way read its own `-o` and did nothing with the name after it.
One number wasted per `if` without an else costs nothing.

`nlabels` is pass1's count of the intermediate labels the condition needs for
short-circuiting.

### Simple if

```asm
	; evaluate x, leaving a condition
	jp z,no0_1
	; body
no0_1:
```

### If-else

```asm
	; evaluate x
	jp z,no0_1
	; then body
	jp no1_1
no0_1:
	; else body
no1_1:
```

## Branch-Chained Conditions

The condition of an `if` is not evaluated to a value and then tested. It is
lowered by `condfalse()` → `condgo()` in `lower.c`, which emits code that jumps
to a label when the expression is false — **the short-circuit *is* the branch**,
and nothing is materialised in between.

The old path rewrote `&&` and `||` to a nought-or-one in A and then tested A:
six bytes of join and a retest per operator, six hundred `xor a`s across this
pass alone.

```c
condgo(e, lbl, wf)   /* jump to lbl when e is false (wf=1) or true (wf=0) */
```

- **`&&` / `||` where every operand agrees with the jump** — chain them: emit
  each operand's test straight to the same label.
- **Otherwise** — the left operand short-circuits *past* the test instead, to a
  fresh `_C<n>` join emitted after the right:

  ```c
  fmtstr(sc, "_C%d", labelcnt++);
  condgo(l, sc, !wf);
  condgo(r, lbl, wf);
  outf("%s:\n", sc);
  ```

- **`!`** — drop the node and flip `wf`. No code at all.
- **A leaf** — `condleaf()`: mark it `DEST_FLAGS`, label, assign, rewrite, then
  one conditional jump.

### `a && b` (jump when false)

Both operands agree with the jump, so both go straight to the false label:

```asm
	; evaluate a
	jp z,no0_1
	; evaluate b
	jp z,no0_1
	; then body
no0_1:
```

### `a || b` (jump when false)

The operands disagree with the jump, so the left one short-circuits past:

```asm
	; evaluate a
	jp nz,_C5
	; evaluate b
	jp z,no0_1
_C5:
	; then body
no0_1:
```

## Condition Codes

`falsecc(e)` and `truecc(e)` in `lower.c` turn a reduced condition into a Z80
condition code, **emitting a zero test first if the answer came back as a value
rather than a flag**:

| Reduced to | `falsecc` | `truecc` | Test emitted first |
|------------|-----------|----------|--------------------|
| `F_Z` | `nz` | `z` | — |
| `F_NZ` | `z` | `nz` | — |
| `F_C` | `nc` | `c` | — |
| `F_NC` | `c` | `nc` | — |
| `F_M` | `p` | `m` | — |
| `F_P` | `m` | `p` | — |
| `R_A` | `z` | `nz` | `or a` |
| `R_HL` | `z` | `nz` | `ld a,l` / `or h` |
| `R_DE` | `z` | `nz` | `ld a,e` / `or d` |
| `R_BC` | `z` | `nz` | `ld a,c` / `or b` |

**A long in HL answers with all four bytes voting.** The pair test alone read
only half of it, and `if (n & 1)` was false for every odd long. A is not banked,
so the fold carries on across the `exx`:

```asm
	ld a,l
	or h
	exx
	or l
	or h
	exx
```

`ccguard()` runs first and emits `; XXXXXX unreduced condition` if the
condition did not reduce to a flag or a register — a missing rule, made loud.

## Comparison Operators

Pass1 normalizes `>` and `>=` to `<` and `<=` by swapping operands, so only
`EQ`, `NEQ`, `LT`, and `LE` arrive. The rewriter re-creates `GT`/`GE`
internally when it flips a comparison against a constant (`a > 5` → `a >= 6`,
via `RF_INC1`).

In the rule table all six are **two** rows, not six: `P_CMP` matches EQ, NEQ,
LT and GE — all of which subtract and read a different flag off the same
subtraction — and `P_CMPX` matches LE and GT, the same code with the operands
swapped. The result is named `F_CC`, and `ccflag()` works out the real flag
from the operator and its signedness. See [REWRITE.md](REWRITE.md).

## Switch Statements

Switch is emitted **bodies first, dispatch after**. The stream is sequential —
`SWITCH`, then each `CASE` with its value and body — so the values are not all
known before the first body has to be emitted. The control value is worked out,
the bodies are jumped over, and the comparison or table goes after them where
every case label is known. Nothing falls into that block: the last body jumps
past it, and the bodies were jumped over rather than run, so the control value
is still live when the dispatch is reached.

`swdispatch()` picks one of **three** shapes by counting, not by taste. With
*n* cases spanning *span* values:

| Shape | Size | Form |
|-------|------|------|
| chain | 5n | `cp v` / `jp z,L` per case |
| `swtab` | 4 + 3n | call, count byte, then a value and a label per case |
| `swidx` | 5 + 2·span | call, low bound, span, then a label per slot |

The chain wins to n=2. Above that it is `swidx` when the values are dense
enough to beat the pair table — `2*span < 3n-1`, a little over two thirds — and
`swtab` when they are not. Over the tree's own 85 switches that is 4175 bytes
of dispatch down to 2705.

Both are needed and they live at opposite ends: every switch here with more
than about twenty cases is sparse (the largest, 125 cases in `wsnm.c`, spans
the whole byte at 48%) and would want a 517-byte index against a 379-byte pair
table, while the dense ones are nearly all small — and for those `swidx` is not
just smaller but constant time instead of a scan.

In the sparse form the values are emitted together so the scan is one `cpir`,
with the labels after them **and backwards**, which is what lets the helper find
the slot from what `cpir` leaves in HL and BC.

A count or span of 256 would store as a zero byte, so anything that large stays
on the chain. `MAXSWCASE` bounds it at 256; `MAXSWNEST` (8) bounds nesting.

Case values are **bytes**. A control expression need not be one — a state
machine over an `int` is the usual shape — so a word control tests its high byte
once before comparing the low one, and any value that does not fit a byte cannot
match and goes to the default.

`break` never appears: cpp lowered it to `goto __S<n>B` and appended the label.
