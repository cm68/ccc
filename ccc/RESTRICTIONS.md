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
