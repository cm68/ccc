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
