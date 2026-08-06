# Shadow registers as a home for variables

Notes from measuring the idea before writing any of it.  The proposal:
give register variables somewhere else to live by using BC', DE' and
HL', and let each expression tree decide which bank it runs in.  IX and
IY are not banked, so arguments at `(iy+d)` and struct fields at
`(ix+d)` read the same in either bank - which is what makes a bank
choice per tree thinkable at all.

Everything below was measured by compiling cpp, pass1, pass2 and peep
for the Z80 - 543 functions - with a temporary `REGSTAT` block in
`pass1/regalloc.c` reporting what the allocator handed out and what it
had to leave on the stack.

## What the allocator is short of

| | |
|---|---|
| functions | 520 |
| with at least one variable it could not place | 172 (33%) |
| variables left on the stack | 601 |
| functions that used all three slots (IX + B + C) | 3 |

The variables it could not place, by shape: 255 word pointers, 180
bytes, 166 words.  What it did place:

| got | functions | | got | functions |
|---|---|---|---|---|
| BC only | 127 | | IX only | 80 |
| BC + IX | 141 | | nothing | 118 |
| a byte, or two | 25 | | B + C together | 6 |

So the shortage is not that the three slots are full - they almost never
are.  It is that the slots are shaped: one pointer may have IX, one word
may have BC, and a second word of either kind has nowhere to go.

If every starved function got one more word register, and it went to
that function's most-referenced missing word, 814 reference sites move
off the stack.  A word reference is `ld l,(iy+d)` + `ld h,(iy+d+1)`
against `ld l,c` + `ld h,b`, so four bytes a site: **about 3.2K gross**.

## What a bank actually buys

One word variable.  Not three.

In the shadow bank the rewriter still needs two scratch pairs, and they
are DE' and HL' - the same Sethi-Ullman machinery, the same rules, the
same templates.  What is left over is BC', which is the shadow of the
one pair already given to variables.  B' and C' instead of BC', for two
bytes, the same way B and C work today.

Nor is there a cheaper version of this in the main bank.  DE looks free
in principle - a function all of whose trees label out at one register
never needs it - and it is not: 445 of 544 functions touch DE, and of
the starved ones, **every single one** does.  The rewriter's second
scratch pair is genuinely in use, which is exactly why a second *bank*
is the interesting idea and a second *register* is not.

## Per-tree banking looks cheap

The cost of the scheme is `exx` (one byte) whenever consecutive trees
want different banks, plus a spill whenever one tree wants both - the
accumulator does not survive `exx`, so crossing mid-tree costs a
`push hl` / `exx` / `pop hl` and more if DE is live too.

Counting distinct stack scalars per statement across the same corpus,
over the 2287 statements that touch one at all:

| distinct scalars in the statement | | |
|---|---|---|
| 1 | 2150 | 94.0% |
| 2 | 131 | 5.7% |
| 3 | 6 | 0.3% |

Statements are overwhelmingly about one variable at a time.  A bank
choice made once per statement holds for the whole of it 94% of the
time, and the mid-tree crossing is a path that has to be correct but
will hardly ever be taken.

The machinery still has to exist, and there is no fallback to soften it:
a register variable here has `frm_off = 0` and no stack slot at all, so
a statement naming variables in both banks *must* cross.  It cannot
spill one of them instead.

## The part that is not cheap: everything already uses the shadow set

`exx` is one byte and no ordinary register, so the runtime reached for
it wherever it wanted scratch.  Reading each routine's alternate-bank
writes:

| routine | destroys |
|---|---|
| csv.s - `fexb` `fexx` `fexbx` | BC' HL' |
| bmove, blkclr | BC' DE' HL' |
| lmul (`almul` `llmul`) | BC' DE' HL' |
| ldiv (`aldiv` `almod` `lldiv` `llmod`), iregset | BC' DE' HL' |
| lrelop (`arelop` `lrelop`) | BC' DE' HL' |
| linc (`lainc` `llinc` `ladec` `lldec`) | HL' |
| float, finc | BC' DE' HL' |
| ladd, lsub, land, lor, lxor | HL' |
| frelop, doprnt_ws | HL' |
| longjmp | BC' |
| start2, `_signal` | BC' DE' HL' |

`_signal` saving the whole shadow set around a handler is already
right and stays right.

Two observations turn this from a wall into a list:

**Only BC' needs a calling convention.**  DE' and HL' are scratch, and
scratch is caller-saved here already - HL and DE do not survive a call
today either.  The bank at a call has to be canonical: a callee names
BC meaning its own variable, and if the caller had switched, the callee
would be handed the caller's BC' instead.  So a tree returns to bank 0
before any call, and the caller's bank-1 accumulator is spilled around
it by the same machinery that spills HL today.  Only BC' has to be
preserved, exactly as BC and IX are.  That means the routines above
that touch only HL'
- ladd, lsub, land, lor, lxor, linc, frelop, doprnt_ws - **need no
change at all**.

**The routines that do clobber BC' are enumerable, and the functions
that call them are few.**  Classifying all 526 emitted functions by
what they call:

| | functions | |
|---|---|---|
| calls something that destroys BC' | 28 | 5% |
| calls only compiled C and BC'-safe helpers | 413 | 79% |
| no calls at all | 85 | 16% |

Excluding the first row costs a twentieth of the functions - joined
against the starvation data, about a fifth of the missed variables,
since long arithmetic and register pressure go together - and requires
no runtime changes at all.  The 413 in the second row are the prize,
and they are reachable as soon as BC' is callee-saved.

(The join between the two tables is only 86% complete: a static
function is emitted as `S<n>` and its C name never reaches the
assembly, so 77 of the 520 starvation rows cannot be matched to an
emitted body.  The classification above covers all 526; the
starved-per-class split does not.)

`csv.s` is the real work.  `fexb`/`fexx`/`fexbx` use the shadow set as
scratch *because* it was free, which is exactly the assumption being
withdrawn - and that scheme is not negotiable, since 526 call sites at
five bytes against twenty-one is some eight kilobytes.  It can be
reworked: the helper's scratch use finishes before it pops, so the
shadow restores go last.  But the entry and exit families grow from
{n,b,x,bx} to eight and seven variants.

## The peephole is safe but goes blind

`peep/regs.c` does not know `exx`, so `reads()` returns `~0` and
`writes()` returns 0 for it.  That is the pessimistic direction in both
places - `isdead()` refuses at the first `exx` - so nothing is wrong
today and nothing becomes wrong.  What happens instead is that every
`exx` becomes a wall the peephole will not look past.  Teaching it the
instruction is optional for correctness and probably necessary for the
scheme to pay.

## Where the code would change

| | |
|---|---|
| `pass1/regalloc.c` | bank assignment; `REG_BCX`/`REG_BX`/`REG_CX` classes |
| `pass1/cc1.h`, `pass2/pass2.h` | the new register codes |
| `pass1/outast.c` | the FUNC header carries the bank |
| `pass2/parseast.c` | prologue/epilogue variant choice; statement-level bank state |
| `pass2/lower.c` | bank at `rewrite()`, and the mid-tree crossing |
| `peep/regs.c` | `exx` in `reads`/`writes` |
| `libsrc/libc/csv.s` | eight entry and seven exit variants |

Note that `pass2/rules.c` is **not** on the list, and that is the whole
attraction of doing it per tree: a template that says `bc` says BC' once
the bank is switched, so three thousand lines of rules are reused
verbatim.  The emission added is the `exx` itself.

`rewrite()` in `lower.c` is not quite the right hook, though it looks
like it.  It is re-entered for sub-expressions while a partial result is
live - from `docompound`, from the argument path, from the for-loop
step.  The bank has to be chosen at the statement roots in
`parseast.c` and held for the statement.

## The arithmetic

Gross, if every starved function gains one word register: ~3.2K.

Against it: saving and restoring BC' in the functions that use it (a
`fent`/`fex` variant, so about five bytes each way, times ~143
functions - call it 1.4K); an `exx` at each bank change; and the growth
in c0 and c1 themselves for the allocator, the bank state and the
epilogue variants.

That last one is the problem.  c1 is already over budget on both
targets.  A scheme that is roughly break-even on emitted code and costs
several hundred bytes in the compiler that emits it is a net loss until
c1 has room - and `regalloc.c` already carries the record of three
attempts to be cleverer about allocation, every one of which made the
compiler bigger.

## If it is done anyway, the order

1. Teach `peep/regs.c` about `exx`.  Independent, small, useful either way.
2. `csv.s`: the entry and exit variants that save and restore BC'.
   Testable on its own by hand-writing a function that uses BC'.
3. `pass1`: the new register classes and the exclusion rule (no shadow
   allocation in a function that calls a BC'-clobbering helper).  Note
   that pass1 does not know which helpers pass2 will emit; the long and
   float widths are the proxy, plus struct assignment and `setjmp`.
4. `pass2`: statement-level bank state, `exx` on change, the crossing.
5. Measure.  `make sizecheck ZCC=ccc` against the numbers above.

Step 5 before step 4 is not possible, which is the uncomfortable part:
most of the cost is spent before the benefit can be read.
