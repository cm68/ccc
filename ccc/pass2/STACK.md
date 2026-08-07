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

The helper is chosen by what has to be saved:

| Saves | Helper |
|-------|--------|
| BC and IX | `fentbx` |
| IX only | `fentx` |
| BC only | `fentb` |
| neither, but a scalar area | `fentn` |
| neither, and no scalar area | `fenter` (takes no word) |

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

| Restores | Helper |
|----------|--------|
| IX and BC | `fexbx` |
| IX only | `fexx` |
| BC only | `fexb` |
| neither | `jp fexit` |

The `fex*` helpers end with the unwind themselves, so there is no `jp fexit`
after them. None of them touch HL, the flags, DE, or the shadow set — so they
are correct with a return value already in HL':HL and with a condition already
set up.

## Argument Passing

Arguments are pushed right-to-left before the call. A long is pushed **high word
first**, so it lands on the stack the same way round as in memory.

Register-variable parameters are **staged** in the prologue: pass1 assigns them
a register, and pass2 emits the loads from their stack slots right after the
frame is set up.

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

The `libsrc/libu` syscall wrappers implement the **ZC3** convention (low byte),
because the running native toolchain is zc3-compiled — a `pop af` wrapper under
zc3 reads the junk byte. This was a real bug: `close()` closed garbage
descriptors and leaked fds. When ccc becomes the system compiler, the
byte-argument wrappers (`close`, `dup`, `read`, `write`, `seek`, `gtty`,
`fstat`, `stty`) need a ccc-convention variant tree. Keep the two conventions in
separate source trees rather than conditional assembly — `asz` stays a minimal
back-end assembler.

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

`c` is allocated to BC, so there is no scalar area at all:

```asm
_main::
	call	fentb
	.dw	0             ; no scalar area
	ld	l,(iy+4)      ; a
	ld	h,(iy+5)
	ld	de,42
	add	hl,de
	ld	c,l           ; -> c, which lives in BC
	ld	b,h
	ld	l,c           ; return c
	ld	h,b
	jp	Xmain
Xmain:
	call	fexb
	.dw	-2            ; the saved BC, just under an empty scalar area
```
