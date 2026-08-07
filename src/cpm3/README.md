# cpm3 — a CP/M 3 machine, just big enough to run this compiler

`cpm3` loads a `.com` at 0100 and runs it on an emulated Z80 with
enough of a CP/M 3 BDOS behind it that the compiler's own passes work:
`cpp`, `c0` and `c1` all run under it and produce the same bytes the
host compiler does.

```
cpm3 [-v] [-v] [-u] [-d dir] program.com [args...]

  -v       trace the interesting bdos calls; twice traces every one
  -u       fold the command tail to upper case, the way a CCP does
  -d dir   the directory the drive maps to (default .)
```

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

The TPA runs to `fe03`, which is the point of aiming at CP/M 3 rather
than 2: `c1` ends at `ea55` and does not fit under a 2.2 BDOS at
`e406`. Nothing had to be told this number — the startup code reads
`0006` and puts its stack below whatever it finds, and `sbrk` grows
the heap up towards it.

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
