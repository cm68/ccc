# First Argument in HL

The rework: the first argument to a function travels in HL — in HL':HL
when it is a long — instead of on the stack.  Every other argument stays
where it was.  This is a change of calling convention: every compiled
caller, every compiled callee, and every hand-written routine that is
called from C moves together or not at all.

## Why it pays

The result of every expression lands in HL.  That is the compiler's one
law — `pusharg` wraps each argument in `ASSIGN(INHL, value)` precisely
to reuse the `=(H,...)` rules — and today the very last thing done with
that carefully-placed value is to push it so the callee can dig it back
out of the frame:

```asm
	; f(x) today			; f(x) after
	ld	l,(iy-4)		ld	l,(iy-4)
	ld	h,(iy-3)		ld	h,(iy-3)
	push	hl			call	_f
	call	_f
	pop	af
```

Two bytes gone per call site — the push and its share of the drop — and
the arg chain is already built last-to-first, so the first argument is
already the one evaluated immediately before the call.  No evaluation
order changes.

The compounding wins:

- **`f(g(x))`**: g's result comes back in HL, which is exactly where f
  wants it.  The call becomes `call _g` / `call _f` with no stack
  traffic at all.
- **Single-argument calls** — `strlen(s)`, `free(p)`, `fclose(fp)`,
  `putchar(c)` — are the most common shape in the tree, and they lose
  their entire argument sequence.
- **A long first argument** is already sitting in HL':HL when its
  evaluation finishes.  Today that costs `push hl / exx / push hl /
  exx` at the site plus two `pop af` after: eight bytes, every time.
  After: zero.

## The calling sequence

**Caller.**  Arguments after the first are pushed right-to-left as
today.  The first argument is evaluated last and left in HL (a byte is
promoted to a word, as it always was) or HL':HL (a long).  Then the
call.  The caller's drop no longer counts the first argument.  A call
with no arguments is unchanged.

**Indirect calls.**  Today the function address is loaded into HL after
the pushes and reached through `tramp` (`jp (hl)`).  HL is now occupied.
The address is evaluated after args N..2 and pushed; then the first
argument is evaluated into HL; then:

```asm
	pop	de		; the function address back
	call	trampde		; push de / ret — a jump to DE
```

DE':DE is the second accumulator and is dead at a call, so it is free to
carry the address.  `trampde` is two bytes of library beside `tramp`,
which remains for the zero-argument indirect call.

## The callee: spill in the helper

The prologue is already one call.  The whole callee side of this
convention is choosing the variant of that call.

A function that takes arguments spills HL back into its old slot as
part of frame setup, **inside the `fent*` helper** — the callee's text
does not grow by a byte, and the helper is then free to clobber HL as it
always has:

```
High addresses
	arg N .. arg 2		caller-pushed, as today
	arg 1			IY + 4   <- spilled by the helper
	return address		IY + 2
	saved IY		IY + 0
	scalar area, saves	as today
```

This is **today's frame layout exactly**.  The helper lifts its own and
the caller's return addresses off the stack (the caller's rides in AF —
`pop af / push af`, and nothing between them reads a flag), pushes the
argument into the vacated slot, and puts the return address back under
it.  Nothing is ever below SP, so an interrupt cannot eat anything.
For a long, both halves go in, high word first: low at IY+4, high at
IY+6, and args 2..N from IY+8 — again exactly where `assignFrmOff`
puts them today.

Because the layout is unchanged, `assignFrmOff` is untouched, register
staging reads the offsets it always read, and — the reason this shape
wins outright — a function that takes the **address** of its first
parameter, even one that walks from it, sees precisely the memory it
saw yesterday.  The tree has two dozen `&first-param` functions
(`_flsbuf`, `_pnum`, `alpha`, `tok2reg`, ...); none of them can tell.
An earlier draft spilled *below* the return address to save two bytes
per helper; contiguity everywhere costs those bytes once per helper and
buys today's layout for every function, which on a size budget is the
better trade by a wide margin.

The prologue variants, `w` for a word first argument, `q` for a long
(the letter the 32-bit set already owns):

| First argument | none | word | long |
|----------------|------|------|------|
| BC and IX      | `fentbx` | `fentbxw` | `fentbxq` |
| IX only        | `fentx`  | `fentxw`  | `fentxq`  |
| BC only        | `fentb`  | `fentbw`  | `fentbq`  |
| neither        | `fentn`  | `fentnw`  | `fentnq`  |
| no scalar area | `fenter` | `fenterw` | `fenterq` |

Sketch of `fentbw` — against `fentb`, the cost is one pop and three
pushes at the top:

```asm
fentbw:	pop	de		; -> the word after the call
	pop	af		; the caller's return address, parked in AF
	push	hl		; spill arg 1 into its old slot
	push	af		; the return address back under it
	push	iy
	ld	iy,0
	add	iy,sp		; new frame pointer
	ex	de,hl		; hl -> word, and hl is free: arg1 is home
	ld	e,(hl)
	inc	hl
	ld	d,(hl)
	inc	hl		; hl = body, de = -scalars
	ex	de,hl
	add	hl,sp
	ld	sp,hl		; scalar area
	ex	de,hl
	push	bc
	jp	(hl)
```

The `q` form pushes the low word first (`push hl / exx / push hl /
exx`), the order `pusharg` always used, which lands the high word at
the lower address the way `qld` expects a long in memory.
`fenterq`/`fenterw` have no word to fetch and are shorter still.

**The w family promises more than the spill**: HL reaches the function
body intact and equal to `(iy+4)`.  That costs the w helpers their
sharing - the plain bodies carry their jump target in HL, so the w
bodies ferry it through DE' and read HL back from the slot - and it is
what the peephole rule below stands on.  The q family makes no such
promise: a long's high word is in HL' and the reload is not worth the
juggling for as rare as long first parameters are.

**Epilogue.**  `fexit` today ends `ld sp,iy / pop iy / ret`, which
leaves SP pointing at the spilled arg 1 — a slot the caller does not
know exists, so the caller cannot drop it.  Matching variants carry the
return address over it:

```asm
fexitw:	ld	sp,iy
	pop	iy
	pop	de		; the return address
	inc	sp
	inc	sp		; discard the spilled arg 1
	push	de
	ret
```

`fexitq` steps four.  `fexbw`, `fexxw`, `fexbxw` and the `q` set read
the same offset word as today and `jr` to the right tail.  DE is dead
at every return — a long comes back in HL':HL — and none of these touch
HL, A, or the shadow set.  The one new liberty taken is the flags
(`pop af` in the prologue, and nothing more): every return goes through
an assignment to HL, so there is no condition to preserve.

**Functions with no parameters** keep the old helpers and the old
`noframe` path untouched.  This is not a second convention; a function
with no first argument has nothing to place.

## Varargs

There is no `...` in this language; the pattern is K&R:

```c
printf(f, a)
	char *f;
	int a;
{
	return (_doprnt(stdout, f, &a));
}
```

`_doprnt` walks upward from `&a` — the address of the **second**
parameter — and with the spill landing arg 1 in its old slot, the
memory from `&f` on up is byte-for-byte what it was yesterday.  The
printf family, and any function that walks from any parameter's
address, works with no special case and no analysis.

## What must move together

Everything below is one flag day; the tree does not link old against
new.

**Compiler:**

- Frame offsets: **unchanged** — the spill reproduces today's layout,
  so `assignFrmOff` is not touched.
- pass1 marks the function header with the spill kind (none / word /
  long), which is nothing but the width of the first parameter.  The
  AST format version bumps; `astpp` learns the field.
- `pass2/parseast.c` `emitprolog`/`emitepilog`: choose the helper
  variant from the header.
- `pass2/lower.c` `docall`: skip the push for the final ARGNODE (the
  first argument), leaving it in HL / HL':HL; drop-count excludes it;
  indirect calls with arguments push the address and go through
  `trampde`.

**Runtime:**

- `libc/csv.s`: the ten new prologue helpers and eight new epilogue
  tails.
- `libc/tramp.s`: add `trampde`.
- Every hand-written routine **called from C** takes its first argument
  in HL now: the string set (`strcmp`, `strcpy`, ...), stdio's
  assembly (`fgetc`, `fputc`, ...), `setjmp`/`longjmp`, `bmove`,
  `abs`, `atoi`, the lot.  Many get *shorter*: their first act today is
  digging the argument out from behind the saves.  Full inventory at
  conversion time; the tell is any `.s` file with a global label that C
  calls.
- Every hand-written routine that **calls into C** places the first
  argument: `crt0` (argc to `main`), the signal delivery glue, any
  `.s`-to-C callbacks.
- `libu` syscall wrappers: the ccc-convention set takes its first
  argument in HL.  (The zc3-convention interop warning in STACK.md is
  about a dead tree — the zc3 build area is gone from `src/libc` — but
  the wrappers themselves are live and move.)
- `libcpm`: converted so the CP/M leg still assembles and links, but
  micronix is the gate; the CP/M leg stays dormant.

**Not touched:**

- The `q*` 32-bit helpers, `amul`/`adiv`/`lmod` and friends, `swtab`/
  `swidx`: reached from rule templates with register operands, not C
  calls.
- The Hi-Tech `ladd`/`csv`/`cret` fossils: nothing emits them.
- Return values: HL / HL':HL as today.
- The `$[ $]` BC guard brackets.
- BC-preservation rules for hand-written assembly (RUNTIME.md): intact,
  but the *pattern* changes — "read the arguments where they lie" now
  means the first one lies in HL.

## The second wave: the reload is peep's to delete

HLARG-PROJECTION.md beside this measured, before any of it was built,
that 190 functions reload the first parameter into HL as their first
act, 53 stage it into BC and 40 read it as a byte - about 1,430 bytes
of loads that are dead the moment the argument arrives in the
register.  The projection's answer, taken whole: pass2 keeps its one
unconditional case, and **peep deletes the redundancy**, paid for out
of the pass with 21K of headroom rather than the one at 5K.

`r_hlarg` in peep/rules.c: after a `call` to one of the five w-family
names, a read of `(iy+4)` reached before anything writes H, L or IY -
no stores, no branches, no calls crossed; pushes are fine, the slot is
above the frame pointer - is a read of what HL already holds:

| pattern | becomes | saves |
|---------|---------|-------|
| `ld l,(iy+4)` / `ld h,(iy+5)` | nothing | 6 bytes, 38 T |
| `ld c,(iy+4)` / `ld b,(iy+5)` | `ld c,l` / `ld b,h` | 4 bytes, 30 T |
| `ld a,(iy+4)` | `ld a,l` | 2 bytes, 15 T |

This is also what repairs the time story: the frame-resident majority
pay the helper's 38 T store, and the functions that reloaded at once
get 38 T straight back.  `r_outi` moved with the convention too: the
`out(str)` site is now `ld hl,S / call _out`, and the inline
`call oarg / .dw S` form still beats it by a byte at the most repeated
call in the code generator.

## The third wave: the frame that stops being necessary

After r_hlarg has run, `f(x) { return x + 1; }` is `call fenterw / inc
hl / jp fexitw` - a frame built, torn down, and never once reached
through.  The projection counted 102 one-parameter functions in that
shape, their no-parameter cousins whose only frame use is the BC save
are the same case wearing `fentb`, and the tree's frame-free functions
run past a hundred significant lines - so this is not a window rule,
and it is not buffering either.

`nfemit()` in peep.c decides it in the stream: the entry call is
written normally and its file position remembered (a byte count peep
keeps itself - see below); every line on the way out feeds a dirt bit
- any key containing `iy` or `sp` means the frame is real; and a clean
matching exit is replaced with `pop`s and a `ret` while the entry is
overwritten in place, through the file descriptor, with pushes padded
to exactly the bytes the call and its `.dw` occupied.  One held line
of state, any function length, and the same decision on host and
target because it is made from normalised keys the DEBUG commentary
never reaches - which also required comment lines to stop occupying
window slots (they stream straight through now, and every window rule
became deterministic across DEBUG and non-DEBUG builds in the
bargain).

`f(x){return x+1;}` compiles to `inc hl / ret`.  The `bx` entries
keep their frames - two pushes do not fit over one call line - and so
do the `q` entries: a long first argument spills two words and earns
its frame.

**Two defects this wave surfaced, worth remembering:**

- The DEBUG comment bypass writes comments at read time while
  significant lines drain up to a window later, so in the *output* a
  comment can sit between two lines c1 wrote adjacently.  The entry's
  two lines are therefore patched as two independent regions, each at
  the position recorded when it was actually written.
- **The target stdio's ftell/fseek on a buffered write stream is
  broken**: the first seek's flush padded the file out with a block
  of zeros, and native peep diverged from the host on any file big
  enough to cross a buffer boundary.  peep now counts its own output
  bytes and patches with `fflush` + raw `lseek`/`write`, which the
  syscall wrappers track correctly.  The stdio defect itself is still
  there for the next caller and wants its own fix.

## Measured

All three waves, cross-built at -O, against the stock master toolchain
HLARG-PROJECTION.md measured:

| | stock | now | saved |
|---|---|---|---|
| c0 | 46,020 | 44,026 | 1,994 (4.3%) |
| c1 | 45,832 | 44,234 | 1,598 (3.5%) |
| cpp | 42,216 | 40,192 | 2,024 (4.8%) |
| asz | 30,007 | 28,817 | 1,190 (4.0%) |
| ld | 25,474 | 24,379 | 1,095 (4.3%) |

Seven point nine kilobytes across the five images.  For scale: making
-O the default bought c0 883 bytes, and deleting the linker's archive
walk bought ld 1,086; each pass here got more than both together.

The self-host stands: all 51 sources compiled natively agree with the
host cross-build byte for byte, and the simulated c0/c1 are
byte-identical to the host over the whole corpus.  Worst-case memory,
from the footprint gate's gap table - free bytes at the tightest
moment of compiling the compiler's own sources:

| pass | worst gap | on |
|------|-----------|-----|
| cpp | 19,338 | pass2/expr.c |
| c0 | 16,081 | pass1/declare.c |
| c1 | 9,453 | pass1/expr.c |
| peep | 8,975 | cpp/cpp.c |

Every pass clears its worst input by nearly nine kilobytes, where the
campaign started with c1's headroom at 5,225.  `tests/run` 112 of
112, `tests/gen` 2,242 checks native and ccc, no missing productions,
footprint clean, `make selfhost` 51 of 51 on the mx leg.

## Still on the table

A register round-trip survives in the frameless BC shape - `ld c,l /
ld b,h` staging followed at once by `ld l,c / ld h,b` - which a small
bounce rule could halve.  And a single-use parameter could stay in HL
outright with no staging at all, which is register allocation rather
than calling convention, and where the next win of this size probably
lives.

## Risks worth naming

- **The helper spill is load-bearing for setjmp/longjmp and signals**:
  everything stays on the one real stack, above SP at every instant, so
  the existing guarantees hold by construction.  Any cleverness that
  parks a value below SP or in a static is wrong here for the same
  reasons it was wrong in the side-stack experiment.
- **`fent*` variants clobber HL only after the spill**; the old
  helpers' "HL holds nothing on entry" comment becomes false in
  general and must be rewritten, not trusted.
- **Unprototyped calls** (all of them, this is K&R): caller and callee
  agree through the declared parameter and the promoted argument, the
  same handshake the stack convention relied on.  A caller passing an
  int where the callee declares a long was broken yesterday and is
  broken today, in a different register.
- **astpp and the AST format docs** drift easily (see the 2026-08-07
  validation sweep); the header field lands in the same commit as the
  format bump.
