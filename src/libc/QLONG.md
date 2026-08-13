# The 32-bit register convention

This is the authoritative statement of how ccc keeps a `long` in
registers. Everything in `q*.s` and everything pass2 emits follows what
is written here.

    A long lives in HL':HL.          HL' is the HIGH word, HL the LOW.
    The second one lives in DE':DE.  DE' is the HIGH word, DE the LOW.
    A function returning a long returns it in HL':HL.
    In memory the HIGH word is at the LOWER address.
    A long argument is pushed low word first, so it lands on the
    stack the same way round as in memory.

That fourth line used to say the LOW word was at the lower address,
and everything obeyed it, and it was wrong. Micronix keeps a long the
other way up: the seconds of a stat time are in the word at the higher
address. Every long that crossed between a ccc-built program and the
kernel, an on-disk structure, or a Whitesmiths-built program arrived
with its halves swapped - `ls -l` printed 2015 for a 1987 file, and a
one-second difference read as nineteen hours, because promoting the
low word to the high multiplies it by 65536. See NUXI.

The bytes *inside* each half were never in dispute, and neither is the
register convention below: only the pairing of the halves moved. The
push rule is not an independent choice - it is written to match the
memory layout, so it moved with it.

The low halves are in the main bank because that is what makes the
arithmetic cheap. One `exx` brings BOTH high words into place at once,
and `exx` touches no flags, so the carry crosses it:

```
	add	hl,de		;low words
	exx
	adc	hl,de		;high words, with the carry
	exx
```

**BC' is free.** Nothing in this convention uses it, so a helper may
treat the whole shadow pair as scratch once it has its operands - and
`djnz` counts in B of whichever bank is current, so a loop that runs in
the shadow half gets its counter for nothing.

**BC is not free.** It is the caller's register variable. A helper that
wants it saves it on the stack, as everything else in libc does.

**DE':DE is the second accumulator**, so a helper that clobbers it may
only be called where pass2 knows it is dead. Each routine says.

## Why these are not the Hi-Tech helpers

`ladd`, `arelop`, `almul`, `aldiv`, `allsh`, `lrelop` and the rest take
their left operand in HL:DE with the high word in HL, and the right one
on the stack. That is Hi-Tech's convention, and zc3 - which this tree
still uses as a reference compiler and as the oracle for
`tests/run` - emits calls to those names from its own code generator.
`libsrc/libc/*.s` is assembled once and archived into both `zc3/` and
`ccc/`, so changing them in place would silently break every
zc3-compiled program that touches a long.

Hence a parallel set under its own prefix. The Hi-Tech routines stay
exactly as they were and remain correct for zc3; nothing links both.

## What the convention is worth

The single HL:DE accumulator is why `dolongbin` in pass2 had to work
the right operand out first and push it, save BC around the call
because the helper popped into it, and ask `longable()` in advance
whether both operands were shapes that could reach HL:DE at all -
"settled before anything is emitted - once the right operand has been
pushed there is no way back". Two accumulators remove the push, the
save and the question.

At the call site a long add went from `push hl` / `push de` /
`push bc` / `call ladd` / `pop bc` to `call qadd`. In the runtime,
`ladd` was fifteen bytes of stack shuffling and `qadd` is six.

## The routines

| | |
|---|---|
| `qadd` `qsub` | HL':HL += / -= DE':DE |
| `qand` `qor` `qxor` | the same, bitwise |
| `qcom` | HL':HL = ~HL':HL |
| `qcmp` `qucmp` | compare; signed answers in the sign flag, unsigned in carry, both set Z on equal |
| `qshl` `qsar` `qshr` | shift HL':HL by the count in A |
| `qinc` `qdec` | step the long at (HL), hand back what was there |
| `qld` `qldde` `qst` | load and store through a pointer |
| `qmul` | HL':HL *= DE':DE |
| `qdiv` `qmod` `qudiv` `qumod` | divide and remainder, signed and unsigned |
