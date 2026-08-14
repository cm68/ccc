# First Argument in HL: the projection

What HLARG.md beside this argues for, measured before it was written.

Every number here comes from the **stock master toolchain** compiling
its own front end - the 45 sources of cpp, c0 and c1, **123,175 bytes
of text** - and from the hand-written assembly in libu and libc.
Nothing is measured on the new code generator.  That is what makes
this a projection: it says what the current code spends, and what of
that spending the change removes.

## The shape of a call today

Arguments push right to left, so the first is pushed **last** and is
already in HL when the call is reached.  That is why the change is
cheap at the site - nothing moves, a `push` and a `pop` stop being
emitted:

```asm
	ld	hl,(_stdout)	; second arg
	push	hl
	ld	hl,32		; FIRST arg - already in HL
	push	hl		; 1 byte, 11 T
	call	_fputc
	pop	af		; 1 byte, 10 T
	pop	af
```

## How many arguments calls actually take

2,941 calls to C functions, by argument count:

| args | calls | share |
|---|---|---|
| 0 | 564 | 19.2% |
| **1** | **1,505** | **51.2%** |
| 2 | 545 | 18.5% |
| 3 | 243 | 8.3% |
| 4 | 58 | 2.0% |
| 5 | 8 | 0.3% |

HLARG.md says single-argument calls are the most common shape in the
tree.  They are half of every call made, and they are the ones that
lose their argument sequence entirely.

Where the first argument already sits:

| | count | share |
|---|---|---|
| already in HL (`push hl` immediately before the call) | **2,159** | 73.4% |
| in DE or BC (`push de`, `push bc`) | 205 | 7.0% |
| no arguments | 577 | 19.6% |

| | calls | each | total |
|---|---|---|---|
| first argument in HL | 2,159 | 2 bytes | 4,318 |
| first argument in DE/BC, `push de` becomes `ex de,hl` | 205 | 1 byte | 205 |
| | | | **4,523** |

## Half the callees want no spill

HLARG.md puts the spill in the shared helper, one place.  The corpus
uses all five entries in `csv.s`:

```
fentbx 169    fentb 128    fentx 86    fenter 59    fentn 13
```

455 of 500 functions enter through one of them, so the spill is 30 to
50 bytes in libc against 373 functions that would each have paid six.

And half of those do not want it.  pass1 already assigns registers to
parameters and ships the answer in the AST; `parseast.c` reads it and
stages frame parameters into registers at entry.  Counting the first
parameter of every function that has one:

| | count | share |
|---|---|---|
| assigned a register (`r != 0`) - no spill wanted | **177** | 47.5% |
| frame-resident (`r = 0`) - spill wanted | 196 | 52.5% |

For the 177 the value arrives in HL, goes to its register, and the
load that stages it out of the frame today disappears as well.  The
question of whether the helper can be told which to do is the one
HLARG.md leaves open, and this is its size: it decides the time result
for half the functions in the tree.

## The frame that stops being necessary

Not visible from the call sites, and the largest single item.

| entry | functions | with parameters |
|---|---|---|
| `fentbx` | 169 | 142 |
| `fentb` | 128 | 88 |
| `fentx` | 86 | 77 |
| `fenter` | 59 | 58 |
| `fentn` | 13 | 8 |
| **no frame at all** | **45** | **0** |

**Not one frameless function takes a parameter.**  A parameter forces
a frame, because a parameter is reachable only through IY.

240 functions take exactly one parameter, and **102 have no locals and
frame size zero** - their whole reason for a frame is to reach that
one argument.  In HL, they need no IY, and stop paying:

```asm
	call	fentb		; 3 bytes
	.dw	-N		; 2 bytes
```

and the matching exit, and about **116 T-states** for `fentb` alone:
`pop hl`, two `ld r,(hl)` with `inc hl` between, `push iy`, `ld iy,0`,
`add iy,sp`, `ex de,hl`, `add hl,sp`, `ld sp,hl`, `push bc`, `jp (hl)`
- with the teardown still to come.

## The libraries

Small absolutely; in every binary that links them.

**libu** - 44 wrappers, 47 objects, 2,288 bytes, mean 48.  Four of
`close`'s bytes exist only to reach one argument and put the stack
back:

```asm
_close:
	pop	de		; ret addr
	pop	hl		; fd
	push	hl
	push	de
```

`write` pops three arguments into statics, readjusts SP by hand, and
loads `fd` out of E twice because it could not stay in HL - about
**127 T-states** of argument handling.

**libc's hand-written assembly** - 93 objects, 3,549 bytes, mean 38.
`strlen` is the sharpest case in the tree:

```asm
_strlen:	pop	hl
	pop	de
	push	de
	push	hl
	ld	hl,0
```

Fourteen bytes, four of them the fetch: **29% of the function**, and
42 T-states before the loop starts.  The loop runs about 28 T per
character, so on a five-character string the fetch is a quarter of the
call.  `strcmp` and `strcpy` each spend seven bytes on `ld hl,4 / add
hl,sp / ld e,(hl) / inc hl / ld d,(hl)` where `ex de,hl` would do.

## The total

| | bytes | time, per call |
|---|---|---|
| call sites, 2,364 with an argument | ~4,500 | 21 T |
| frames no longer needed, 102 functions | ~500-800 | ~200 T |
| libu, 44 wrappers | ~250 | 42-127 T |
| libc hand-written, 93 objects | ~400 | 42 T |
| entry helpers, once | +40 | |
| | **~5,600-5,900** | |

**4.6% of the corpus text**, and against the passes:

| | text | projected |
|---|---|---|
| c0 | 46,020 | ~2,100 |
| c1 | 45,832 | ~2,100 |
| cpp | 42,216 | ~1,950 |
| asz | 30,007 | ~1,380 |
| ld | 25,474 | ~1,170 |

For scale, on the same machine: making `-O` the default bought c0 883
bytes and was the difference between compiling declare.c and not;
deleting the linker's archive walk bought ld 1,086.  This is larger
than either, and like both it is image rather than allocation - it
lands on every input and cannot be handed back on a different one.

## What the projection does not settle

**Whether the spill can be conditional.**  The caller saves 21 T; a
helper that spills unconditionally spends 38 - `ld (iy+4),l` and `ld
(iy+5),h` at 19 each.  For the 52.5% that are frame-resident the size
still improves and the time may not.

**Whether dropping the frame is cheap to decide.**  "This parameter
has a register" is a byte pass1 already computed.  "This function
needs no frame" is a harder question and the analysis costs c1 text -
the largest image in the tree at 56,426.  The 102 candidates are one
parameter, no locals, frame size zero, which suggests a cheap test.
159 have one parameter and frame size zero without the no-locals
condition, so a cheap test captures most but not all.

**Whether `ex de,hl` covers all 205** of the non-HL sites, or some
need a two-byte reload, which would cost that 205 back.

**Whether the 605 reads of `iy+4`/`iy+5`** shrink further where a
single-use parameter can stay in HL.  That is register allocation
rather than calling convention, and it is where the estimate could
improve rather than erode.

## The code budget for smart frame allocation

Three cases are worth telling apart at entry: **no frame**, **frame
but no HL save**, and **everything**.  Whether pass2 can afford to is
a question about c1, which is the largest image in the tree.

What it has to spend:

```
c1 image      text 45,832   data 9,310   bss 1,284   total 56,426
headroom      5,225 bytes on c1-lower.ast, its worst input
```

Largest image, but not the tightest at run time - c0 at 1,812 and ld
at 1,593 are both worse off.

**The change is substantially self-funding.**  It hands c1 about 2,100
bytes back, so the question is not whether c1 can afford the analysis
but whether the analysis costs less than the change returns.

The selection is already a two-bit decision over a name, in
`parseast.c`:

```c
	h = savesbc() ?
	      ((regsused & REGBIT(R_IX)) ? "fentbx" : "fentb") :
	      ((regsused & REGBIT(R_IX)) ? "fentx" : "fentn");

	if (!savesbc() && !(regsused & REGBIT(R_IX)) && savebase == 0)
		out("\tcall\tfenter\n");
```

Two predicates choosing one of four names, plus a special case.  A
third dimension is another predicate over the same shape - more names
and one more test, not a new pass - **provided the predicate is
already computable**.  For two of the three cases it is:

- **no HL save.**  `reg != 0` on the first parameter is a byte pass1
  already computes and ships in the AST, and `parseast.c` already
  reads it to drive staging.  Nearly free, and it covers 47.5% of
  functions.
- **everything.**  The fallback.  No new information.
- **no frame.**  The expensive one.  `savebase == 0 && !savesbc() &&
  !(regsused & REGBIT(R_IX))` already identifies the `fenter` case,
  but a frameless function additionally needs no `(iy+d)` access
  anywhere in its body.  Whether that is knowable where the entry is
  emitted, without a walk, is the question that decides the cost.

That suggests taking them in that order.  The no-HL-save case is
nearly free and settles the **time** result for the 52.5% of functions
that are frame-resident, where an unconditional spill costs 38 T
against the caller's 21 saved.  The frameless case is worth
**~500-800 bytes and ~200 T-states** across 102 functions, and is the
one that could overrun the budget - so it should be attempted second,
against a measured c1 rather than a projected one.

Both passes already self-compile, so the budget can be measured rather
than estimated: c0 builds natively at 46,014 bytes of text and c1 at
60,451, both producing byte-identical output to the cross build.

## Method

```
ccc -m micronix -O -s        over the 45 sources of cpp, c0 and c1
```

Call sites, argument counts and entry helpers were counted from that
assembly; parameter registers from the `; param name:t r=N o=M`
annotations pass2 emits; text sizes from `wssize` on the built
objects; T-states from the Z80 timings.  The corpus is the compiler
itself, which is the largest C this system compiles and the code most
likely to be representative of what it will compile.
