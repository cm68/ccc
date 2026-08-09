# cpm3 — a CP/M 3 machine, just big enough to run this compiler

`cpm3` loads a `.com` at 0100 and runs it on an emulated Z80 with
enough of a CP/M 3 BDOS behind it that the compiler's own passes work:
`cpp`, `c0` and `c1` all run under it and produce the same bytes the
host compiler does.

```
cpm3 [options] program.com [args...]

  -v          trace the interesting bdos calls; twice traces every one
  -u          fold the command tail to upper case, the way a CCP does
  -d dir      the directory drive A maps to (default .)
  -d X=dir    the directory drive X maps to
  -t addr     put the bdos here, and so size the TPA (default fe00)
  -M          on exit, report the heap and stack high-water marks
  -p          on exit, report the busiest addresses
  -w          report writes to page zero; twice to stop on the first
  -l n        stop after n instructions
```

The exit status is the program's own: `cpm3` returns what bdos
function 108 was last set to, which is what `exit()` sets. A pass that
gripes and stops fails the script that ran it. Without that, `cpp`
reporting "out of memory" and stopping looks exactly like `cpp`
succeeding, and a survey of what does and does not compile counts the
failures as passes - which it did, until this was wired up.

## What it is

The Z80 is [z80.h](z80.h), Andre Weissflog's, vendored unmodified
under its zlib licence — the same core the Micronix simulator in this
tree runs on, which is worth something: it has already been proven
against this compiler's code generation.

Everything else is about 700 lines in `cpm3.c` and `bdos.c`.

The BDOS is not code. `0005` jumps to a trap address holding a single
`ret`; the run loop notices when the processor is about to execute
there, services the call out of the registers in C, and lets the `ret`
carry the program back. A real banked CP/M 3 keeps its BDOS in a bank
the TPA cannot see, which is exactly as visible as this.

That check has to happen between instructions, not inside a memory
cycle. `z80.h` keeps the registers in locals for the length of a
`z80_exec` and writes them back at the end of an instruction, so a
call serviced from inside a memory access reads whatever was in the
struct beforehand — which is how the first version of this asked for
BDOS function 255.

## The memory map

```
0000  jp WBOOT        program exit lands here
0005  jp BDOS         the trap
0006  BDOS address    what a program reads to size the TPA
005c  fcb 1           parsed from the first argument
006c  fcb 2           its 16 name bytes only - see below
0080  command tail, and the default DMA buffer
0100  the program
 ...  the TPA: heap up from the end of bss, stack down from BDOS
fe00  the two trap bytes
```

The BDOS sits at `fe00` by default and `-t` moves it, which is the
only knob that matters for asking whether something fits: everything
below follows from it. Nothing had to be told the number — the startup
code reads `0006` and puts its stack below whatever it finds, and
`sbrk` grows the heap up towards it.

`fe00` is more memory than any real machine has once the system is in
it. A banked CP/M 3 gives about 62K, which is `-t 0xf900`, and that is
the figure worth quoting: the whole compiler self-hosts there, all
fifty sources agreeing with the host byte for byte. Aiming at CP/M 3
rather than 2 is what makes that possible at all — `c1` ends at `e555`
and would not fit under a 2.2 BDOS at `e406` whatever else was done.

The two default FCBs overlap, which is how CP/M laid page zero out:
the first is a whole 36-byte FCB at `005c` running to `007f`, and the
second is only its 16 name bytes at `006c`, sitting on top of the
first one's allocation area. Writing a full FCB at `006c` instead
runs to `008f` and takes the command tail with it.

## Files

An FCB names a file in one host directory and the record number in it
says where to read or write. There is no directory, no allocation
map and no handle kept across calls — a program is free to copy an
FCB around and CP/M would not have noticed either, because the
position lives in the FCB, which is the one place both sides agree
on. Open host files are cached by name so a compiler reading a source
a record at a time does not pay an open for each one.

Names are matched case-insensitively and created lower case. CP/M is
upper case and a Unix directory usually is not.

**The byte count is the reason this targets CP/M 3.** CP/M 2 could
only count 128-byte records, so a file came back rounded up and the
reader had to find the end itself — which works for text ending in a
`^Z` and not at all for the compiler's binary intermediates. CP/M 3
keeps the byte count of the last record; `cpm3` reports it in the
FCB's `s1` on open and honours it on close by cutting the file back to
the length the program claims. Without that, `c0` reads the padding
after the lexemes and takes it for more of them.

## Where it is not a CP/M 3

- **The command tail keeps its case** unless `-u` says otherwise. A
  real CCP folds it, and there is no way to get the original back,
  which is why CP/M programs take upper case options. This compiler's
  are lower case and `-o` and `-O` mean different things to it, so
  folding by default would mean it could not be driven at all.
- **No directory search.** Functions 17 and 18 answer "no more files".
  Nothing in the compiler asks.
- **No banking.** Memory is flat. Nothing here looks at the system
  bank, so there is nothing for the switch to do.
- **No date, no passwords, no disk parameters.** Function 105 answers
  with zeroes.
- **One drive.** The `-d` directory is all of it.

## Running the compiler

`make selfcheck` compiles a source three ways — the host compiler, and
the CP/M passes under this machine — and compares. What it proves is
that `cpp` and `c0` produce byte-identical output, and that `c1` does
too once the host build's `; stmt` commentary is accounted for: the
host is a `-DDEBUG` build and the CP/M one is not.

By hand, the same thing:

```
cpm3 -d work cpp.com -DCCC -o t t.c      # t.c -> t.x, t.n
cpm3 -d work c0.com  t.x t.1 t.2         # -> the AST and the string pool
cpm3 -d work c1.com  t.1 t.2 t.s         # -> Z80 assembly
```

## Asking whether something fits

`-M` reports two marks on exit: how far up the heap got and how far
down the stack got. A write counts as heap if it lands well below the
deepest the stack has ever been — the stack sets a new low-water by
writing there, so a plain "below the stack pointer" test calls every
push a heap write and the mark climbs to meet the stack. No
instruction moves the stack 64 bytes in one step, which is the margin
used.

```
cpm3 -M -t 0xf900 -d work c0.com t.x t.1 t.2
cpm3: heap high f045  stack low f980  gap 2363  depth 1152  fits under f4c5
```

**"Fits under" is for ranking, not for quoting.** It is the heap top
plus the stack depth, and it does not know what `sbrk` keeps in
reserve, so it always reads low — by about 210 bytes for `c0`, 270 for
`c1`, and as much as 900 for `cpp`. The error is not a constant and
not the same per pass. To get a number you can put in a commit
message, bisect `-t` until the run both succeeds and produces the same
bytes as the host:

```
l=0xf000; h=0xfe00
while [ $((h-l)) -gt 64 ]; do
    m=$(( (l+h)/2 & 0xffc0 ))
    if cpm3 -t $m ... && cmp -s out ref; then h=$m; else l=$m; fi
done
```

Two things that will waste an afternoon if they are not known:

- **Bisect below `fe00`.** Higher is not always better. The BDOS needs
  room above it, so a run can fail at `ff80` and succeed at `fe00`,
  and a bisect that assumes monotonicity over the whole range reports
  the ceiling and calls it the answer.
- **Compare the output, not just the exit status.** `c0` writes its
  `.1` and `.2` even when it has diagnosed an error, and `c1` reads
  them without complaint. A check that only asks whether the file
  appeared will call a broken build green.

`selfhost.sh` takes `TPA=` to run the whole survey at one BDOS
address:

```
TPA=0xf900 sh selfhost.sh          # every source, at a real 62K TPA
```

## What building this found

Nothing under `src/libcpm` had ever been linked, let alone run, so
every latent bug in it was still there. In rough order of how badly
each one hurt:

- `csv`/`cret` are HiTech's frame helpers and save IY and IX, not BC.
  ccc keeps register variables in BC and expects a callee to hand the
  caller's back. `bdos`, `bdoshl` and `getuid` all load C with a
  function number, and the BDOS destroys BC on its own account, so
  every library call quietly corrupted a register variable. A loop
  counter in BC never reached zero.
- `crtcpm` read the command tail length from `(80)`, which this
  assembler takes for decimal — address `0050`, which is zero. Every
  program saw `argc` 1 and an empty `argv[0]`. The very next
  instruction to name the same buffer spells it `80h`.
- `perror` went through `putc` and `stderr`, dragging the whole stdio
  machinery into anything that mentioned it and colliding with the
  empty `_cleanup` that `cpp` defines to keep it out.
- `fgetc.s` ended `ld hl,EOF` with no `equ` for it. `fseek.c` called a
  `clreof()` that exists nowhere in the tree.
- `getuid.s` — a BDOS call — was sitting in `libc`, the
  system-independent half, and was in neither library's source list,
  so it was not built at all.

Two of those were the assembler emitting wrong code silently, and are
fixed there rather than worked around: `cp a,32` assembling as `cp a`
with the operand dropped, and `jp 0` relaxing into a relative jump
that lands on the program's entry point instead of the reboot vector.
