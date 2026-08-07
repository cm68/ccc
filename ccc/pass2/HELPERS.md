# Runtime Helper Functions

The routines pass2 emits calls to, implemented in `libsrc/libc`. These are the
operations too big to inline on a Z80 — 32-bit arithmetic, 16-bit multiply and
divide, frame setup and teardown, and switch dispatch.

> **There are no floating-point helpers here.** ccc has no floating point:
> `float` and `double` are not keywords, and a float literal is rejected by the
> lexer. `libsrc/libc` still contains float routines (`float.s`, `asfloat.s`,
> `ftol.s`, …) because they are archived for the Hi-Tech compiler `zc3`, but
> pass2 cannot emit a call to any of them.
>
> **The Hi-Tech long helpers are not these either.** `ladd`, `almul`, `aldiv`,
> `allsh`, `lrelop` and the rest take their operands in HL:DE and on the stack.
> That is Hi-Tech's convention; `zc3` emits calls to those names, and
> `libsrc/libc/*.s` is archived into both `root/lib/zc3/` and `root/lib/ccc/`,
> so they stay exactly as they are. ccc uses the parallel `q*` set below.
> Nothing links both.

---

## 32-bit (long) — the `q*` set

**A long lives in HL':HL** — HL' is the high word, HL the low. The second
operand lives in DE':DE the same way round. A function returning a long returns
it in HL':HL. In memory the low word is at the lower address, and a long
argument is pushed high word first so it lands on the stack the same way round
as in memory.

The low halves are in the main bank because that is what makes the arithmetic
cheap: one `exx` brings both high words into place at once, and `exx` touches
no flags, so the carry crosses it:

```asm
	add	hl,de		; low words
	exx
	adc	hl,de		; high words, with the carry
	exx
```

**BC' is free** — nothing in the convention uses it, so a helper may treat the
whole shadow pair as scratch once it has its operands. **BC is not free**: it
is the caller's register variable, and a helper that wants it saves it on the
stack. **DE':DE is the second accumulator**, so a helper that clobbers it may
only be called where pass2 knows it is dead.

| Helper | Operation |
|--------|-----------|
| `qadd` `qsub` | HL':HL += / −= DE':DE |
| `qand` `qor` `qxor` | the same, bitwise |
| `qcom` | HL':HL = ~HL':HL |
| `qcmp` `qucmp` | compare; signed answers in the sign flag, unsigned in carry, both set Z on equal |
| `qshl` `qsar` `qshr` | shift HL':HL by the count in A |
| `qinc` `qdec` | step the long at (HL), hand back what was there |
| `qld` `qldde` `qst` | load and store through a pointer |
| `qmul` | HL':HL *= DE':DE |
| `qdiv` `qmod` `qudiv` `qumod` | divide and remainder, signed and unsigned |

Sources: `qadd.s`, `qsub.s`, `qand.s`, `qor.s`, `qxor.s`, `qcom.s`, `qcmp.s`,
`qshl.s`, `qsar.s`, `qshr.s`, `qinc.s`, `qldst.s`, `qmul.s`, `qdiv.s`.

**What the convention is worth.** With a single HL:DE accumulator, `dolongbin`
had to work the right operand out first and push it, save BC around the call
because the helper popped into it, and ask `longable()` in advance whether both
operands could reach HL:DE at all. Two accumulators remove the push, the save,
and the question. At the call site a long add went from `push hl` / `push de` /
`push bc` / `call ladd` / `pop bc` to `call qadd`; in the runtime, `ladd` was
fifteen bytes of stack shuffling and `qadd` is six.

The authoritative statement is
[../../libsrc/libc/QLONG.md](../../libsrc/libc/QLONG.md).

---

## 16-bit multiply and divide

Operands in HL and DE, result in HL. These are Hi-Tech's names and Hi-Tech's
convention — signed spelled `a*`, unsigned `l*`:

| Helper | Operation |
|--------|-----------|
| `amul` | HL * DE |
| `adiv` `ldiv` | HL / DE, signed / unsigned |
| `amod` `lmod` | HL % DE, signed / unsigned |

These **take their second operand in DE and clobber BC**, which is why every
rule that calls one wraps the call in `$[ … $]` — the emitter's "save BC if a
register variable lives there" bracket (see [REWRITE.md](REWRITE.md)).

---

## Frame setup and teardown

Implemented in `libsrc/libc/csv.s`, alongside — but distinct from — Hi-Tech's
`csv`/`cret`. Hi-Tech points IX at the frame and saves both index registers;
**ccc points IY at the frame and leaves IX free** for the code generator to use
as a pointer, so the two cannot share the routines.

The function-particular part — how big the scalar area is — rides in the word
after the call:

| Helper | Prologue |
|--------|----------|
| `fenter` | frame pointer only (no scalar area, no saves) — takes no word |
| `fentn` | frame pointer + scalar area |
| `fentb` | + save BC |
| `fentx` | + save IX |
| `fentbx` | + save BC and IX |

| Helper | Epilogue |
|--------|----------|
| `fexit` | `ld sp,iy` / `pop iy` / `ret` — the plain unwind |
| `fexb` | restore BC, then unwind |
| `fexx` | restore IX, then unwind |
| `fexbx` | restore IX and BC, then unwind |

The `fex*` helpers take the offset of the **lower** save from the frame pointer
in the word after the call, and the unwind is the same code, so there is no
`jp fexit` after them.

**Why helpers and not inline.** The prologue written out is eleven bytes, of
which only the constant is particular to the function; through a helper it is
five. The epilogue is worse: restoring IX inline costs twelve bytes, because it
has to come back through A — HL is the return value, and DE is the rest of it
when the value is long — plus six more for BC. Two thousand bytes of that in
`c1` alone.

`fenter` clobbers HL, which holds nothing on entry to a function whose
arguments came in on the stack. `fexit` touches neither HL nor the flags, so it
can be jumped to with a return value or a condition already set up. None of the
`fex*` helpers touch BC', DE' or HL', which is what lets a long live in HL':HL.

### The rule for hand-written assembly

There is deliberately **no helper for saving BC in a hand-written routine**.
One was tried; it parked the saved BC in a static side stack, because the
bodies there read their arguments by popping and a save pushed at entry would
come back as an argument. But a static side stack is state the real stack does
not know about: a `longjmp` past a frame midway through such a routine leaves
it pointing at dead slots, and a signal handler that touches stdio has the same
problem.

The rule instead: **a hand-written routine that needs BC saves it on the real
stack and reads its arguments where they sit**, indexed past the saves and the
return address. `strcmp`, `strcpy`, `fgetc` and `fputc` are the pattern.
Everything lives on the one stack, so `setjmp` owes nothing to anybody.

---

## Switch dispatch

Two shapes, chosen per switch by size (see [README.md](README.md)):

| Helper | Cost | Form |
|--------|------|------|
| `swtab` | 4 + 3n | `call`, a count byte, then a value and a label per case |
| `swidx` | 5 + 2·span | `call`, a low bound, a span, then a label per slot |

A comparison chain wins up to n=2; above that it is `swidx` when the values are
dense enough and `swtab` when they are not.

---

## Indirect calls

`tramp` (`tramp.s`) — a trampoline for calling through a computed address when
no register is free to hold it.

---

## Notes

1. **Signed vs unsigned:** for addition, subtraction, and bitwise operations
   the two are identical. Division, modulo, and right shifts differ, and have
   separate entry points.
2. **Stack cleanup:** the `q*` helpers take their second operand in registers,
   not on the stack, so there is nothing to clean up. The 16-bit `a*`/`l*`
   helpers follow Hi-Tech's convention.
3. **Negation** of a long is done inline, as a two's complement of HL':HL.
4. **Source files** are in `../../libsrc/libc/`.
