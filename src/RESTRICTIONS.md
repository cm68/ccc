# Code Restrictions

This compiler must be self-hosting on the Z80 target. The following C features
are NOT supported and must NOT be used anywhere in this project:

- **Structure assignment** - no `struct a = b;`
- **Structure return** - no functions returning structs by value
- **Auto aggregate initializers** - no `struct foo x = {...};`, `int arr[] = {...};`
  or `char s[] = "text";` on a local. This covers any aggregate with
  automatic storage, including the `char buf[N] = "";` idiom for zeroing a
  buffer - declare it and assign `buf[0] = 0;` instead. File-scope and
  `static` aggregates initialize fine and are used throughout.
- **const qualifier** - no `const` anywhere
- **signed qualifier** - no `signed` anywhere (use plain `int` or `char`)

These restrictions apply to every source in the tree - cpp/, pass1/, pass2/,
shared headers, tools/, libsrc/ and tests/. Auto aggregate initialization is a
restriction, not a gap: do not write tests that expect it to work.

The one deliberate exception is `ccc/cpp/test/*.c`, which is fed to cpp alone
and never compiled. Those files exist to exercise the declaration/initializer
filter and must keep using the constructs it has to survive.

# Patterns the codegen rewards

Not rules - measured habits.  Each of these was worth real bytes on the
Z80; when two shapes say the same thing, prefer the one below.

## Prefer --x and ++x wherever the value is not consumed

A post form promises the value from BEFORE the step, and the compiler
has to keep that promise - read the old value out, step the location,
carry the old value forward.  The pre form has nothing to juggle.
The statement-context rules rewrite `x++;` and `++x;` alike for the
common shapes, but the pre form needs no rule to be cheap, works at
every width, and says what is meant.  Reach for `x++` only when the
expression actually uses the old value.

## Count down, pre-decrement, byte counter

```c
	unsigned char n = count + 1;
	while (--n) { ... }		/* runs count times */
```

A byte counter in a frame slot (or reached through a register struct
pointer) steps and tests in ONE instruction: `dec (iy+d)` sets Z
itself, so the loop head is six bytes.  The same loop as
`while (n--)` must read the old value first - ten bytes - and
`for (i = 0; i < n; i++)` pays a load and a compare every trip and
holds two live variables.  Swizzle the counter (start at count+1) to
fit the pre-decrement test; where the body needs "is this the last
one", n counts the current element plus the rest, so the test is
`n > 1`.

The counter must be a BYTE with a guaranteed range under 255, and the
form only wins where the counter is not otherwise consumed - a loop
that indexes with it keeps its index.  Word counters gain nothing
(`dec hl` sets no flags); leave them alone.

## Walk a pointer, bound by the counter

```c
	register struct ent *p = tab;
	unsigned char n = count + 1;
	while (--n) { ... p->field ... ; p++; }
```

`tab[i].field` rescales i by the struct size at every mention.  The
walking pointer pays the scale never, the counter check beats
computing `tab + count` at the loop head - BUT only when the pointer
lands in a register home (IX for struct access, BC otherwise).  If
both homes are taken the walking pointer lives in the frame and LOSES
to indexing (measured +57 in macexpand): count the function's
register pressure before converting.

## Read a global byte once per basic block

`ld a,(nn)` and `ld a,(iy-d)` are both three bytes: copying a byte
global into a plain local is a wash, and in a short helper the
register staging costs more than the body saves (measured +57 over
five small functions).  The cache pays only when the local gets B or
C and the function is long enough to amortize the save/restore -
gettoken, not skipws.

# Invariants

Things the compiler and its runtime hold to.  Breaking one of these
does not fail to build; it produces a program that is wrong somewhere
far from the change.

## The callee saves BC.  We never caller-save.

BC and IX are the register-variable homes.  A function saves them in
its prologue and restores them in its epilogue - the fent* and fex*
families in libc/csv.s do both, and every function ccc compiles goes
through them.  A caller therefore never writes push bc around a call,
and no rule, no helper and no future optimisation should reintroduce
one.

BC is saved unconditionally, not only when a variable lives there:
the code generator also uses BC as SCRATCH, in 366 of the tree's
functions, and none of them were saving anything.  See pass2/STACK.md.

The obligation is on anything a compiled program can call:

  - Hand-written .s routines that use BC as scratch save it
    THEMSELVES, on the stack, and restore it before every ret.
    strcpy, strcmp, atoi, xtoi, wait, time, execv, execl and sbrk all
    do.  rcsv does it for the eight string routines that share it,
    and they exit through rcret rather than cret so the pop happens.

  - The save goes on the STACK, never in a static.  A static is
    smaller and was tried; a chain of these routines or any recursion
    treads on it - fputc calls itself to put a carriage return before
    a newline - and it is silently wrong when it overflows.

  - Where a routine's own entry shuffle pops into BC before a save
    could happen, read the arguments where they lie (ld hl,4 / add
    hl,sp) instead of popping them.

  - A routine that uses the shadow set for its body needs nothing:
    exx puts the caller's BC out of reach.  bmove is the example.

The exception, and the only one, is the arithmetic helpers - amul,
adiv, ldiv, amod, lmod.  They take their second operand in DE and
clobber BC, they are reached by name from rule templates rather than
through a call node, and the rules that name them carry $[ and $] so
the save happens at the few places that need it.

The long helpers are ccc's own q* set (qadd, qsub, qmul, qdiv, qcmp,
qld, qst and the rest - see libsrc/libc/QLONG.md), and BC is not free
to them either: a q* routine that wants BC saves it on the stack, as
everything else in libc does.  BC' IS free - nothing in the long
convention uses it.

The Hi-Tech names - ladd, arelop, lrelop, almul, aldiv, allsh, lld,
lstde - are still in libsrc/libc because zc3 emits calls to them and
the same sources are archived into root/lib/zc3/.  ccc never calls
them.  Nothing links both sets.

## The exits do not disturb the return value

fexit and the fex* family touch neither HL nor the flags, nor DE, nor
the shadow set - a long comes back in HL':HL, so all four of HL, HL',
DE and DE' have to survive.  This is why the IX restore used to go
through A.  A long-returning function that restored IX through DE
handed back the saved IX's address as its low word, and "int a[5]"
reserved .ds <heap pointer> bytes.

The helpers briefly did their arithmetic in the shadow set, on the
grounds that every ordinary register was live.  Two of the three
premises were wrong - A and the flags are NOT live (every return goes
through an assignment to HL), and the saves need not be popped
(ex (sp),hl parks the return value and hands over the pointer in one
byte).  Nothing there touches BC', DE' or HL' any more, which is
exactly what lets a long live in HL':HL.

## Declarations go at the top of a block

pass1 accepts them nowhere else, and it has to compile itself.  gcc
accepts them anywhere, so the host Makefiles pass
-Werror=declaration-after-statement to make the tree tell the truth.
