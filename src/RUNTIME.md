# Runtime Libraries

The Z80 runtime: startup code, the C library, and the OS interface layers.
Built by the toolchain that `make` in the repository root has already
installed, and archived into `root/lib/`.

## Layout

| Directory | Archive | Contents |
|-----------|---------|----------|
| `include/` | — | System headers for the target |
| `libc/` | `libc.a` | The C library: stdio, malloc, string, long arithmetic, frame helpers |
| `libu/` | `libu.a` | Micronix syscall wrappers — see [libu/README.md](libu/README.md) |
| `libcpm/` | `libcpm.a` | CP/M support |
| `old/` | — | Superseded sources, kept for reference |

Startup code sits at this level: `crt0.s` for Micronix, `crtcpm.s` for CP/M.

`make` here builds `libu libc libcpm` in that order and installs the archives
and the startup objects.

## Two compilers, two library areas

`root/lib` is split per compiler:

```
root/lib/crt0.o      root/lib/crtcpm.o
root/lib/ccc/        libc.a libu.a libcpm.a libccc.a  built for ccc
root/lib/zc3/        the same names, built for Hi-Tech C v3.09
```

The two calling conventions are **incompatible** — they disagree about where a
byte argument sits in its stack word, and about where a long lives — so nothing
links both. Reading one compiler's objects and calling them the other's is an
easy mistake to make and a hard one to see, which is why the areas are separate
directories rather than one shared one.

`ld -L$(ROOTDIR)/lib/ccc` or `-L$(ROOTDIR)/lib/zc3` picks the area.

`make` here builds only the **ccc** area now — the zc3 area dates from when
Hi-Tech C was bootstrapping this compiler's runtime, and that is finished. It
stays because the `com-zc3` footprint build in `ccc/cpp` still links against
it, and because zc3 remains the reference compiler for `tests/run`.

## Assembly sources are shared, deliberately

`libsrc/libc/*.s` is assembled once and archived into **both** areas. That is
why the Hi-Tech long helpers (`ladd`, `almul`, `aldiv`, `allsh`, `lrelop`, …)
are still here untouched: zc3's code generator emits calls to those names, and
changing them in place would silently break every zc3-compiled program that
touches a long.

ccc uses its own parallel set under the `q*` prefix — see
[libc/QLONG.md](libc/QLONG.md) for the 32-bit register convention and why it
exists.

## Key documents

| Document | Covers |
|----------|--------|
| [libc/QLONG.md](libc/QLONG.md) | The 32-bit register convention (HL':HL) — authoritative |
| [libu/README.md](libu/README.md) | The Micronix syscall interface and `_fdpos` tracking |
| [../ccc/pass2/HELPERS.md](../ccc/pass2/HELPERS.md) | Which of these routines pass2 actually calls |
| [../ccc/pass2/STACK.md](../ccc/pass2/STACK.md) | The frame layout `csv.s` implements |
| [../ccc/RESTRICTIONS.md](../ccc/RESTRICTIONS.md) | The BC-saving rule hand-written assembly must follow |

## The rule for hand-written assembly

A routine that uses BC as scratch **saves it itself, on the real stack**, and
restores it before every `ret`. Never in a static: a chain of such routines, or
any recursion, treads on it — `fputc` calls itself to put a carriage return
before a newline — and it is silently wrong when it overflows. A `longjmp` past
a frame midway through such a routine has the same problem.

Where a routine's entry shuffle pops into BC before a save could happen, read
the arguments where they lie (`ld hl,4` / `add hl,sp`) instead of popping them.
`strcmp`, `strcpy`, `fgetc`, and `fputc` are the pattern.

A routine that runs its body in the shadow set needs nothing — `exx` puts the
caller's BC out of reach. `bmove` is the example.

## Float

`libc` contains floating-point routines (`float.s`, `asfloat.s`, `ftol.s`,
`ltof.s`, `frelop.s`, and the `.c` math functions). **ccc cannot call any of
them**: it has no floating point, `float` and `double` are not keywords, and a
float literal is rejected by the lexer. They are here for zc3.
