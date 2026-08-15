# What the compiler costs on the machine it targets

Wall clock on the host measures the host.  The number that matters is
what a pass costs on a 4MHz Z80 with 64K, and the simulator is the only
thing that can answer it: usersim counts the cycles every instruction
takes and reports the total an image spent, at its exit.

    sim -S -v 0x400 <pass> <args>

`-S` gives the footprint - stack low-water, final break, and the gap
between them, which is the heap that is left.  V_CYCLE, bit 10, gives
the cycles.  The count is taken from `exec`, beside the stack and
text-write counters that reset there, so a program is billed for itself
and not for whatever ran before it.

Run the passes directly.  The driver forks them and its own cost is not
theirs.

## Baseline: 2026-08-14

Compiling `pass1/declare.c`, 46K of source and the largest thing the
compiler compiles, through the whole chain.  Toolchain at ccc 09816ad.

| pass  | cycles      | sec @ 4MHz | heap gap | share |
|-------|------------:|-----------:|---------:|------:|
| pass0 |  81,944,111 |       20.5 |   20,197 | 23.7% |
| c0    |  19,144,419 |        4.8 |   17,064 |  5.5% |
| c1    | 121,915,130 |       30.5 |    9,392 | 35.2% |
| peep  |  32,119,329 |        8.0 |   29,098 |  9.3% |
| asz   |  90,913,885 |       22.7 |   32,489 | 26.3% |
| total | 346,036,874 |       86.5 |          |       |

Intermediates: declare.x 15,319 bytes, declare.o 5,782.

A minute and a half a file on the real machine.  Three things this says
that were not obvious before it was measured:

**c1 is the expensive pass**, at over a third of the total, and it is
also the one with the least heap left - 9,392 bytes.  It is the only
pass that is tight in both dimensions at once, and it is where time
spent will show.

**asz costs more than c0 and peep together.**  A quarter of a compile
goes into the assembler.  It also has the most headroom of anything
here, 32,489 bytes, so it is the one place where memory could be spent
to buy time without a fight.

**c0 is cheap** - 4.8 seconds, 5.5%.  Most of the fitting work this
tree has done was about c0's memory, which was the right worry: it is
not the time.

peep's 8.0 seconds is what it costs having had 94ea75b and f80f3f8,
which took its per-line work from about 1.47M operations to 70K - the
nftab walk from 439,794 string comparisons to 3,888, and the rule
dispatch from 1,027,880 invocations to 66,492.

## asz: the table walk, and the fold that was thrown away

2026-08-14.  asz 90,913,885 -> 61,619,963, a third of it, and a
quarter of a compile becomes a fifth.

| pass  | cycles      | sec @ 4MHz | heap gap | share |
|-------|------------:|-----------:|---------:|------:|
| pass0 |  81,944,111 |       20.5 |   20,197 | 25.9% |
| c0    |  19,144,419 |        4.8 |   17,064 |  6.0% |
| c1    | 121,915,130 |       30.5 |    9,392 | 38.5% |
| peep  |  32,119,329 |        8.0 |   29,098 | 10.1% |
| asz   |  61,619,963 |       15.4 |   32,533 | 19.5% |
| total | 316,742,952 |       79.2 |          |       |

Two changes, both output-identical over all 51 files:

`match` folded a to lower case for every character it compared and
then compared the unfolded one.  The fold was dead - and so is the
case-insensitivity the comment claims, which nothing has ever had.

`asm_instr` walked all 69 entries of the instruction table for every
line and called match on each to be told no.  The first character
settles 68 of them, and that is the test match makes first anyway.

**Host profiling under-predicted this.**  callgrind said 17.8% -
16,935,584 instructions down to 13,927,099 - and the machine says
32.2%.  A call is nearly free on the host and out-of-order execution
hides what is left of it; on a Z80 it is neither.  Work measured on
the host carries across, but removing CALLS carries across better
than the host will tell you, which is worth knowing when choosing
what to do next.

## Keeping it

Add a row when the number moves, and say what moved it.  A baseline
with no history is a number; a baseline with history is a record of
what worked.
