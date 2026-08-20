# Stack Frame and Calling Convention

This document describes the stack layout and calling convention pass2 emits.

**IY is the frame pointer.** IX is left free for the code generator to use as a
pointer register variable. This is the difference from Hi-Tech's convention,
where IX is the frame pointer and both index registers are saved — which is why
ccc cannot share Hi-Tech's `csv`/`cret` and has its own `fent*`/`fex*` pair.

## Stack Frame Layout

```
High addresses
    +--------------------+
    | arg N              |  IY + 4 + ...
    | ...                |
    | arg 1              |  IY + 4
    +--------------------+
    | return address     |  IY + 2
    +--------------------+
    | saved IY           |  IY + 0   <- IY points here
    +--------------------+
    | scalar area        |  IY - 1 .. IY - savebase
    |  (locals reached   |
    |   by (iy+d))       |
    +--------------------+
    | saved BC           |  IY - savebase - 2   (if used)
    | saved IX           |  IY - savebase - 4   (if used)
    +--------------------+
    | arrays and the     |  reached by 16-bit arithmetic,
    | rest of the frame  |  not by (iy+d)
    +--------------------+  <- SP
Low addresses
```

**The order is deliberate.** Frame pointer, then the scalar area, then the
callee saves just under it — so the saves stay inside the **7-bit `(iy+d)`
window** — and the bulk last, where big arrays live and are addressed with
16-bit arithmetic anyway. Pass1 computes `savebase`, the scalar area size, and
puts it in the `AST_FUNC` header; pass2 emits it as the word after the prologue
call.

## Prologue

The whole prologue is one call plus a word:

```asm
_foo::
	call	fentb
	.dw	-4          ; -savebase
	ld	hl,-200     ; only if there is more frame below the saves
	add	hl,sp
	ld	sp,hl
```

The helper is chosen by what has to be saved, crossed with what the
first argument is - it arrives in HL (HL':HL when long, see below),
and the `w`/`q` variants spill it into its `(iy+4)` slot as part of
frame setup, so a function with parameters costs the same five bytes
of prologue it always did:

| Saves | no args | word first arg | long first arg |
|-------|---------|----------------|----------------|
| BC and IX | `fentbx` | `fentbxw` | `fentbxq` |
| IX only | `fentx` | `fentxw` | `fentxq` |
| BC only | `fentb` | `fentbw` | `fentbq` |
| neither, but a scalar area | `fentn` | `fentnw` | `fentnq` |
| neither, and no scalar area | `fenter` | `fenterw` | `fenterq` (no word) |

The `w` family additionally promises that **HL reaches the body
intact and equal to `(iy+4)`**, which is what lets peep's `r_hlarg`
delete a reload of the first parameter.  See HLARG.md for the whole
story.

Even the bare frame-pointer case is **called** rather than written out: the
inline sequence is eight bytes (`push iy` 2, `ld iy,0` 4, `add iy,sp` 2) against
three for a call, and leaving it to the peephole meant paying the eight in every
build that does not run `-O`.

### BC is always saved

`savesbc()` returns 1 unconditionally, and that is not laziness. The
register-variable homes are callee-saved, so a function keeping a variable in BC
must save it — and that used to be the whole test. It is not enough, because the
code generator also uses **BC as scratch** in functions that have no variable
there at all: `ld bc,4` for an offset, `ld c,l / ld b,h` to move a pair. 366 of
the tree's functions do it, and none of them were saving anything.

While callers saved BC around every call, that did not matter. Now that they do
not, it is the difference between a caller's variable surviving a call and not.
The prologue is emitted before the body, so pass2 cannot know whether the
scratch will be used — and since nearly every function uses it, the answer that
costs least to be sure of is *always*.

## Epilogue

Every `return` is a `jp` to the function's own exit label, `X<name>`:

```asm
	jp	Xfoo
Xfoo:
	call	fexb
	.dw	-4          ; offset of the LOWER save from IY
```

| Restores | no args | word first arg | long first arg |
|----------|---------|----------------|----------------|
| IX and BC | `fexbx` | `fexbxw` | `fexbxq` |
| IX only | `fexx` | `fexxw` | `fexxq` |
| BC only | `fexb` | `fexbw` | `fexbq` |
| neither | `jp fexit` | `jp fexitw` | `jp fexitq` |

The `fex*` helpers end with the unwind themselves, so there is no `jp fexit`
after them. The `w`/`q` variants discard the spilled first argument on the
way out - it sits above the return address, in stack the caller does not
know exists, so the exit is what drops it, carrying the return address
over the slot through DE. DE is dead at every return - a long comes back
in HL':HL - and none of these touch HL, A, or the shadow set, so a return
value and a condition ride through.

## Argument Passing

**The first argument travels in HL** - in HL':HL when it is a long - and
every argument after it is pushed right-to-left before the call. The arg
chain is built last-to-first, so the first argument is evaluated
immediately before the call, exactly where its value is already sitting.
A byte is promoted to a word (`ld l,a / ld h,0`, or a sign extension -
never `push af`). A long on the stack is pushed low word first, so its
high word lands at the lower address, the way a long lies in memory.

The callee's prologue helper spills the first argument back into the
`(iy+4)` slot the caller used to push it into, so the frame is laid out
exactly as the stack convention's was, and `&arg1` arithmetic - walking
included - sees the memory it always saw. The caller drops only what it
pushed; the spill is the callee's, and the `fex*` w/q exit discards it.

Calls through a pointer with arguments cannot carry the address in HL any
more, so the address is worked out after the pushed arguments and parked
on the stack while the first argument is evaluated, then popped into DE -
dead at every call - and reached through `trampde` (`push de / ret`). A
zero-argument indirect call still goes through `tramp` (`jp (hl)`).

Register-variable parameters are **staged** in the prologue: pass1 assigns them
a register, and pass2 emits the loads from their stack slots right after the
frame is set up. When the staged parameter is the first one, peep's
`r_hlarg` rewrites the load to come from HL (`ld c,l / ld b,h`) or deletes
it outright - the w helpers guarantee HL still holds it. See HLARG.md.

```asm
	ld	c,(iy+4)      ; a byte parameter into C
	ld	c,(iy+6)      ; a word parameter into BC
	ld	b,(iy+7)
	ld	l,(iy+8)      ; a pointer parameter into IX
	ld	h,(iy+9)
	push	hl
	pop	ix
```

### Interop warning: zc3/hitech uses the opposite byte convention

The Hi-Tech compiler (`zc3`) puts a prototyped byte argument in the **low** byte
of the stack word, with a junk high byte (it emits `ld l,(hl)` / `push hl`);
unprototyped calls widen to a full 16-bit value. A `push af` convention would
put the value in the high byte. The two cannot share hand-written callees that
take byte arguments.

The `src/libu` syscall wrappers implement the **ccc** convention now:
the first argument — which is where every byte argument in the set sits —
arrives promoted in HL, so the old question of which byte of a stack word
to trust does not arise for it. The zc3 build of these wrappers is
history along with the zc3 build area; if it ever comes back, keep the
two conventions in separate source trees rather than conditional
assembly — `asz` stays a minimal back-end assembler.

## Return Values

| Type | Location |
|------|----------|
| byte | HL — pass2's RETURN widens even a byte |
| word | HL |
| long | HL':HL (high word in HL', low in HL) |

Every return goes through an assignment to HL, which is why the `fex*` helpers
need not preserve A or the flags.

## Local Variable Access

Scalars in the scalar area are reached with `(iy-d)`:

```asm
	ld	a,(iy-1)      ; a byte local
	ld	l,(iy-4)      ; a word local, low byte
	ld	h,(iy-3)      ; high byte
```

Arrays sit below the callee saves and are reached by arithmetic:

```asm
	push	iy
	pop	hl
	ld	de,-200
	add	hl,de         ; -> the array
```

Pass1 assigns the offsets — scalars first so they stay inside the `(iy+d)`
window, arrays after the saves. `assignFrmOff()` gripes with `ER_D_FL` if the
scalar area passes 120 bytes, and pass2 emits a `.error` if the frame grows
large enough to push a callee-save slot past −128.

## Register Variables

| Register | Type | Assigned to |
|----------|------|-------------|
| B | byte | a byte local |
| C | byte | a byte local |
| BC | word | the word local with the highest reference count |
| IX | pointer | the pointer with the most field accesses |

Register variables consume no frame space. Pass1 decides; see
[../pass1/PHASE2.md](../pass1/PHASE2.md) for the policy and the exclusions.

## Stack Cleanup

Pass2 does not emit explicit cleanup after a call. The frame teardown resets SP
from IY, discarding the frame and any accumulated argument pushes at once, so
argument space accumulates during nested calls within a function and is
reclaimed when the function returns.

## Example: Complete Function

```c
short main(int a, char *b)
{
	short c;
	c = a + 42;
	return c;
}
```

`c` is allocated to BC, so there is no scalar area at all.  `a`
arrives in HL; `fentbw` spills it to `(iy+4)` and hands HL through,
and peep has deleted the reload the body used to open with:

```asm
_main::
	call	fentbw
	.dw	0             ; no scalar area
	ld	de,42         ; a is already in HL
	add	hl,de
	ld	c,l           ; -> c, which lives in BC
	ld	b,h
	ld	l,c           ; return c
	ld	h,b
Xmain:
	call	fexbw
	.dw	-2            ; the saved BC, just under an empty scalar area
```
