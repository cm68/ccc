# Where things stand

Notes on the state of the compiler and, more usefully, on what kinds of
bug keep turning up and what finds them.

## The compiler compiles everything in the tree

`make stage1` is green end to end: 16 cpp sources, 11 pass1, 6 pass2,
and all 8 tools (assembler, linker, nm, lib, size).  That was reached
by fixing three things, none of which were where the error pointed:

* cpp joined adjacent string literals in its lexer, which works on
  characters and so can only see literals that were already neighbours.
  Two that became adjacent when a macro expanded were never joined.
  C puts the joining after expansion for exactly that reason.  The
  pass2 rule table is written as named instruction fragments set side
  by side, so this surfaced as pass1 rejecting one arbitrary-looking
  line three hundred lines into `rules.c`.
* `lexread` shifted its token lookahead with a structure assignment,
  which is on the list of things this tree may not use.
* A byte operator with two computed operands put both in A.

## Sizes

Measured with `make sizecheck ZCC=ccc` (and `ZCC=zc3`) in each pass
directory.  Directories are keyed to the compiler that produced them -
they hold objects, and one directory shared between two compilers means
whatever ran last wins, which cost an afternoon once.

| | ccc | zc3 | zc3 -O |
|---|---:|---:|---:|
| cpp | 44824 | 48688 | 39106 |
| c0 | 46501 | 54600 | 43251 |
| c1 | 46921 | 57926 | *cannot build* |

All three fit, with 17-20KB spare.  Against zc3 **without** `-O` ccc is
about 13% smaller; against `zc3 -O` it is 8-19% larger, which is the
peephole gap.  `sizecheck`'s zc3 build carries no `-O` - the number to
compare against is the third column, not the second.

`zc3 -O` cannot build `pass2/rewrite.c` at all: `optim: Out of memory in
_rewrite1`.  The same limit forces test files here to split `main()`
once they get long.

`rules.o` is 13721 bytes of it - about a third of c1 - and comes out
identical from every compiler, being a table and not code.  It went
15143 -> 13721 by packing the rule struct from 11 bytes to 8.  What is
left in there, measured: ~1200 bytes of duplicated pattern and template
strings, and ~3800 bytes held by rules that never match.

## The kinds of bug that keep turning up

Worth reading before adding anything, because the same shapes recur.

**A table that is read back by position.**  pass1 counts things in
phase 1 and reads them back by position in phase 2 - statements per
function, statements per block.  Both tables were fixed size, both
dropped silently when full, and both read back zero.  A zero is not a
missing entry, it is a wrong answer: it says the body is empty, so
pass2 stops reading that body's statements and takes what follows for
whatever record it decodes as.  `rewrite.c` lost thirteen of its
forty-six functions, `rewrite()` among them, and everything said ok.
Overflowing either is fatal with a message now.

**A rule that matches nothing.**  A third of the rule table matches
nothing over every line of C in this tree.  A rule nothing reaches has
never had its output run, and one sat there emitting `bit n,(iy+d)` -
correct, and unreachable, because an AND reduces its left operand
before any rule can see it.  Every test passed the whole time, because
what ran instead was right, only longer.  `make coverage` answers this
now; check a new rule actually fires before believing in it.

**A check that only looks at the root.**  A tree that reduced is a
single register node with nothing under it, so children left standing
mean a rule was missing somewhere below.  Only the root used to be
looked at, and a parent that still matched hid the failure:
`arr[i] = i * a` with `i` in a register emitted no multiply at all and
put the wrong number in the array.

**A lookup by name where names are not unique.**  Every local of a
function is in one list, so a name declared in a nested block sits
beside the one it shadows and both answer to the same string - the
`L<n>` renaming is for what gets emitted, not for what gets looked up.
`findInLocals` matched on the name alone and returned whichever came
first, so both `v`s took the outer one's register.  The level and the
block tell them apart; two variables of the same name cannot be
declared in the same block.

**A flag set after it is read.**  A variable whose address is taken
cannot live in a register.  The flag saying so was set as phase 2
walked the statements - after phase 2 had handed the registers out at
the top of the function.  It was always zero when read.

**A width taken from the wrong node.**  The spill path stamped the
parent's width on both operands, and a comparison's width is `ubyte`
whatever it compared, so `RF_SIGNL` never held and every signed
comparison between two computed operands read the carry.

**A trick that has nowhere to go at the end of the range.**  `x <= n`
compiled as `x < n+1`.  Fixed for bytes at 255, left alone for words,
where it wraps at 65535: `u <= 0xffff` was false for every unsigned
short.

**A cost that is not the truth.**  Sethi-Ullman is what avoids
defensive spilling, and it works by knowing which side is dearer.  A
shape whose rules can only put the answer in HL costs two registers,
not one, because it cannot be held in DE while the other side is
worked out.  Costing it one does not avoid the spill, it moves the
collision somewhere the tables cannot see.  Calls were already costed
that way; steps were not, and "buf[pos++]" added HL to itself.

The table says which shapes are flexible: a template writing through
`$t` and `$T` lands wherever it was asked, one naming `l` and `h` does
not.  Cost from that, not from the storage class - the first attempt
at this reasoned from "is it a global" and missed the register
variable.

**A rule for HL and DE but not BC.**  A register variable lives in BC,
so any rule that takes an operand in HL or DE usually needs a third
form.  Three separate gaps in a row were this: adding a subscript to a
symbol, to an index register, and to a frame slot's address.  When
writing one, ask whether BC needs it too.

**Registers clobbered by a helper.**  The 16-bit helpers take their
second operand off the stack with `pop bc` and do not put it back.  A
register variable lives in BC.  The long helpers had always saved it;
the short ones never did.  `$[` and `$]` in a template now bracket a
call and expand to a push and pop only when BC holds something.

## What finds them

In rough order of how much they have earned:

* **`tests/run`** - the runtime suite.  21 files, run under three
  toolchains: native (the reference), zc3, and ccc.  A disagreement
  between ccc and either other is a bug in ccc.  This finds what
  assembly inspection does not, because most of these bugs generate
  code that assembles, links and runs.
  `make one MODE=ccc T=rt_x.c` narrows one file.
* **`make asmsnap DIR=x`** - snapshot the generated assembly for a
  corpus, change something, snapshot again, diff.  Byte-identical
  output is a stronger claim than a passing test because it covers the
  paths the tests do not reach.  Every refactor here should be able to
  show it.
* **`make coverage`** (in `ccc/pass2`) - which rules ever match.  Needs
  `c1` built with `-DDEBUG`; the counters are host-side and the z80
  build has never seen them.
* **The `XXXXXX incomplete` markers** - 68 over the tree's own
  sources.  Count them with
  `grep -rh '^; XXXXXX' ccc/*/stage1 tools/stage1 | wc -l`.
* **`make regression`** - 365 baselines of cpp's lexeme output.  Catches
  unintended changes to what cpp emits; `REGRESS_FLAGS=--bless` to
  rebless after an intended one.

## Keep the probes

A probe written to reproduce a marker is worth more than it looks and
should go into `tests/run` rather than being deleted.  Reproducing one
is usually the expensive part: the failing shape tends to be a
particular combination of storage class, width and register that the
obvious test does not reach, and two or three attempts compile cleanly
before one matches what the real source does.  Thrown away, that work
is spent again the next time something near it moves.

`rt_shape.c` is where they go, each with a note saying which file it
came from, so a regression leads back to the original.

## Traps in the harness itself

* **A simulator fault reports as a check number.**  "FAILED at check 4"
  with only two checks in the file means the program crashed, not that
  check 4 failed.  Read `out/<name>.ccc.log`: `cp/m system call from
  fffd - call: RESET` and a wild `sp` is a crash.  This has cost hours
  more than once.
* **A new rule can pass the whole suite while being dead or wrong.**
  Aim a fixture at it and check the instruction actually appears.
* **`ccc -s` writes `<base>.s` beside the source**, and some
  directories hold hand-written assembly under a name a `.c` file also
  uses - `libcpm/getargs.s` next to `libcpm/getargs.c`.  A script that
  compiles a corpus must put back what it found.

## Open, roughly by how much they matter

* **153 rules of 485 never match.**  Some is float, which is unstarted.
  The rest is shapes no source here takes, and each is code that has
  never run.
* **68 markers** over the tree's own sources.
* **Float** is not started.
* **c0 could be single-phase.**  The two-phase structure is what forces
  the file-wide tables above, and `resetSwitch()` is declared "reset for
  new function" but is never called, because calling it would break the
  scheme.  The seam is the counts in the `.1` format: phase 1 walks the
  whole file so phase 2 can stream a block without holding it.  Going
  single-phase relocates that work rather than removing it - either c0
  buffers a function and backpatches, or the counts leave the format
  and c1 discovers block structure itself.  That is the c0-versus-c1
  size trade.

## Conventions worth keeping

* Verify before committing: runtime suite, `make stage1`, `make test`,
  `make regression`, and `asmsnap` for anything that should not change
  code.
* A fix gets a fixture aimed at it, in `tests/run`, checked against the
  native build.
* Say in the commit message what was wrong and how it presented, not
  just what changed.  Most of these bugs are invisible at the point of
  failure and the note is what makes the next one findable.
* Restrictions this tree compiles under are in `RESTRICTIONS.md` and are
  not negotiable, since it has to compile itself: no structure
  assignment or return, no auto aggregate initialisers, no `const` or
  `signed`, symbols 14 characters or fewer.

<!-- vim: set tw=72: -->
