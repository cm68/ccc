# Code Restrictions

This compiler must be self-hosting on the Z80 target. The following C features
are NOT supported and must NOT be used anywhere in this project:

- **Structure assignment** - no `struct a = b;`
- **Structure return** - no functions returning structs by value
- **Auto aggregate initializers** - no `struct foo x = {...};` or `int arr[] = {...};`
- **const qualifier** - no `const` anywhere
- **signed qualifier** - no `signed` anywhere (use plain `int` or `char`)

These restrictions apply to all code in cpp/, pass1/, pass2/, and shared headers.
