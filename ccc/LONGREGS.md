# Longs in HL':HL and DE':DE

A 32-bit value lives in HL with its high word in HL', and the second one
lives in DE with its high word in DE'.  Two 32-bit accumulators where
there is currently one, and a function returning a long returns it in
HL':HL.

This is a different proposition from the shadow-register-variable idea
in SHADOW.md, and a better-shaped one: that idea grows c1, this one
shrinks it.

## What the single accumulator costs today

`pass2/lower.c:dolongbin` says it outright:

> Both operands want HL:DE and there is only one of those, so the right
> one is worked out first and pushed - which is also what the helpers
> expect: left in HL:DE, right on the stack with its high word pushed
> first.

Everything awkward about the long path follows from that sentence.

- The right operand is pushed and the helper pops it back.
- BC has to be saved around the call, because the helper pops into it:
  "Save it underneath the operand: the helper consumes exactly the two
  words it was passed, so the copy is on top again when it returns."
- A 32-bit constant operand costs `pushlongc` - four instructions,
  eight bytes - where a register operand would be two `ld`s.
- The shift path parks its count through the stack and `ld b,c`,
  because reducing the value may call a helper that uses BC.
- And `longable()`: a pre-flight walk asking whether both operands are
  shapes that *can* end up in HL:DE, "settled before anything is
  emitted - once the right operand has been pushed there is no way
  back."

Measured over cpp, pass1, pass2 and peep - which barely use longs at
all - there are 201 long helper call sites, and in the eight
instructions before them, 295 pushes and 26 pops of HL, DE or BC.  Some
320 bytes of nothing but parking operands, in a program that does almost
no 32-bit work.

## The pre-flight gate is the real damage

`longable()` is not just a cost, it is a bug generator, and its own
comments are the evidence.  A long-valued ternary was missing from the
list, so `(c ? a : b) + 10` "was declined here, refused by the width
guard above the rules, and the whole statement left as an XXXXXX
marker: no code at all."  A long assignment used as a value was missing,
so `if ((pos = off - ftell(f)) == 0)` "was declined here and nothing
else could compare a long - the condition never reduced."

Both are the same failure: the marshalling is irreversible, so the
compiler has to predict success, and anything it fails to predict
silently emits nothing.  Register operands are reversible.  The gate
does not need to exist, and neither does that class of bug.

## The arithmetic is inline, and `exx` does not touch the flags

That last point is the linchpin.  `exx` exchanges BC, DE and HL with
their primes and affects no flag, so a carry survives it:

```
	add	hl,de		; low words          1 byte
	exx			;                    1
	adc	hl,de		; high words         2
	exx			;                    1
```

Five bytes, inline, for a 32-bit add - against `push hl` / `push de`,
a `call ladd`, and fifteen bytes of helper that does its own `exx`
dance to stash the return address.  Subtract is the same with
`or a` / `sbc hl,de` twice, seven bytes.  Shift left by one is
`add hl,hl` / `exx` / `adc hl,hl` / `exx`.  A 32-bit compare is the
subtract, and it leaves the flags where the caller wants them instead
of going through `arelop` - 37 sites in this corpus alone.

`asz` already encodes all of it: `adc hl,rr` via the CARRY path in
`tools/asmz80.c`, `exx` as 0xD9, and `ld l,(iy+d)` is emitted today.

Not everything wants to be inline.  And, or and xor have no 16-bit
form, so four bytes through A is about 24 bytes against a 3-byte call -
they stay helpers, as do multiply, divide, remainder and the variable
shifts.  But their convention becomes register to register, so the
`push hl` / `push de` / `push bc` at the call sites goes away even
where the call itself does not.

## The return ABI solves the problem it looks like it creates

Returning a long in HL':HL means the exit helpers can no longer use HL'
as scratch - and they use it deliberately.  `csv.s`:

> fexit and the fexbx family touch neither HL nor the flags, and not DE
> either: a long comes back in HL:DE. ... This is why the IX restore
> used to go through A, and why the helpers use the shadow registers
> now.

But that is the whole argument, and reversing the premise reverses the
conclusion: once a long comes back in HL':HL, **DE and DE' are free at
return time** instead.  The helper reads the saves through `(hl)`
rather than pointing SP at them, and parks the return value on the
stack with `ex (sp),hl`:

```
fexbx:
	ex	af,af'
	pop	de		;-> the word after the call
	ld	a,(de)
	inc	de
	ld	d,(de)
	ld	e,a		;de = offset of the lower save
	push	iy
	ex	(sp),hl		;hl = iy, return value now on the stack
	add	hl,de		;-> the lower save
	ld	e,(hl)
	inc	hl
	ld	d,(hl)
	push	de
	pop	ix
	inc	hl
	ld	c,(hl)
	inc	hl
	ld	b,(hl)
	pop	hl		;return value back
	ex	af,af'
	jr	fxdone
```

HL' is never touched, so a long return survives.  About ten bytes more
than the present body, paid once in the runtime rather than at 526 call
sites - the five-bytes-against-twenty-one bargain that makes the whole
`fent`/`fex` scheme worth having is untouched.

A useful side effect: with this rewrite the exit helpers stop
clobbering BC' and HL' altogether, which strikes `csv.s` off the
blocker list in SHADOW.md.

The rest of the ABI surface is small.  Only three library functions
return a long - `atol`, `ftell`, `lseek` - and one more, `labs`; all
four are compiled C and follow the compiler.  No hand-written routine
returns a long except the long helpers themselves.

## What has to be rewritten

| | |
|---|---|
| `pass2/lower.c` | `dolongbin` largely dissolves; `longable`, `pushlongc` and the BC-save logic go entirely |
| `pass2/rules.c` | inline rules for long +, -, comparison, shift-by-small; the helper-calling rules change convention |
| `libsrc/libc/csv.s` | the three exit helpers as above |
| `libsrc/libc/ladd.s` `lsub.s` | deleted - inline now |
| `libsrc/libc/lrelop.s` `lcom.s` | deleted or reduced |
| `libsrc/libc/land.s` `lor.s` `lxor.s` | operands in DE':DE instead of the stack; the `exx`/`pop hl`/`exx` return-address dance goes |
| `libsrc/libc/lmul.s` (59 lines) | working set is BC' DE' HL' today; must be reworked to take DE':DE |
| `libsrc/libc/ldiv.s` (272 lines) | the same, and this is the bulk of the work |
| `libsrc/libc/lldst.s` | `lld`/`lstde` load and store HLDE; new layout |
| `libsrc/libc/float.s` `fbcd.s` `frelop.s` `finc.s` | share the HLDE convention |
| `peep/regs.c` | `exx` in `reads`/`writes`, as ever |

`ldiv.s` and `lmul.s` are the real cost: they use the shadow set as
their working registers, which is exactly the space the new convention
claims.  Everything else on the list is small or is a deletion.

What they get back is BC'.  Nothing else in the new scheme uses it -
the two accumulators are HL':HL and DE':DE, and BC is the caller's
register variable - so inside a helper the whole shadow pair is free
scratch.  `djnz` counts in B of whichever bank is current, so a
shift-and-add loop running in the shadow half gets its counter without
spending a main-bank register; `mult8b`'s `ld b,8` becomes B'.

## Why this one is worth doing and the other is not

The shadow-register-variable idea buys about 3.2K of emitted code and
spends several hundred bytes of c0 and c1 to do it, in a compiler that
is already over budget.  This idea:

- **shrinks c1** - `dolongbin`, `longable`, `pushlongc` and the BC-save
  logic are procedural special-case code, replaced by ordinary rules
  and the Sethi-Ullman machinery that is already there for 16-bit
  values.  Two 32-bit accumulators are exactly what `label()` and
  `assign()` already assume they have;
- **shrinks the runtime** - `ladd`, `lsub`, `lrelop` and `lcom` go away;
- **closes a bug class** - no irreversible marshalling, so no
  predict-or-emit-nothing gate;
- **is much faster** - no call, no ret, no stack traffic for operands.

The two ideas do conflict: this one claims DE' and HL' as the high
halves of the accumulators, and SHADOW.md wanted them as bank-1
scratch.  They cannot both apply inside one function.  But the
exclusion is one that already exists for other reasons - a function
doing long arithmetic was never going to get a shadow register
variable, because the long helpers destroy BC'.

## Order

1. `peep/regs.c` knows `exx`.  Independent, small.
2. The `csv.s` exit helpers off the shadow set (the body above).  This
   is correct and testable *before* anything else changes, since it
   preserves the current HL:DE ABI too.
3. Pick the halves: HL' high / HL low makes the add cheapest and is
   what "returns in HL',HL" reads as.  Write it down before anything
   depends on it - `lldst.s` has a comment recording what happened last
   time a file claimed one order and implemented the other.
4. `ladd`/`lsub`/compare inline in `rules.c`, HL':HL only, second
   operand still on the stack.  Measurable on its own.
5. DE':DE as the second accumulator; delete the marshalling in
   `dolongbin` and the `longable` gate.
6. `lmul.s`, `ldiv.s`, `float.s` to the new convention.
7. The long return ABI last, once everything else is on the new layout.
