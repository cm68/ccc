# ccc

A self-hosting C compiler for the Z80, with a disjoint Z280 variant. The
compiler is written in C and must compile *itself on the target*, so every
design decision — separate passes, small source files, byte-sized data
structures — follows from that one constraint. It must fit, and run, in a
64 KB address space with heap and stack sharing what is left.

## The passes

One driver (`ccc`) runs a chain of separate programs. Each is a program of
its own because none of them can afford to live beside the others in 64 KB.

```
.c ──cpp──> .x .nam ──c0──> .ast .dat ──c1──> .s ──[peep]──> .s ──asz──> .o ──ld──> a.out
```

| stage | program | reads | writes |
|---|---|---|---|
| preprocess, normalize | `cpp` (pass0) | `.c` | `.x` lexemes, `.tok` readable dump, `.nam` name sidecar |
| parse, analyze | `c0` (pass1) | `.x` | `.ast` AST, `.dat` data assembly |
| generate code | `c1` (pass2) | `.ast` `.dat` | `.s` assembly |
| peephole (`-O`) | `peep` | `.s` | `.s` |
| assemble | `asz` | `.s` | `.o` |
| link | `ld` | `.o` `.a` | `a.out` |

The `.dat` file is not tree data — it is assembly: globals, string
constants, and static initializers, which pass1 can emit directly. Names
travel through cpp and pass1 as 2-byte ids; the `.nam` sidecar carries the
spellings, and pass2 turns the ids back into symbols. See
`src/README.md` for a per-file map of each pass.

The language is the K&R C the compiler is written in, with an ANSI front
end: sources mix K&R function definitions with ANSI declarations. The heavy
lifting is in `cpp`, whose normalizer simplifies the language before pass1
sees it — typedefs dissolved, enums lowered, `sizeof` folded, K&R definitions
rewritten as ANSI, braces inserted, loops lowered, and `break`/`continue`
turned into `goto` — so pass1 parses without a symbol table and the code
generator meets only `if` and `goto` for control flow.

## Calling sequence

The passes take positional arguments, not `-o`, and will do nothing useful
without all of them — do not run them by hand. The driver (`src/tools/ccc.c`)
runs them in order, resolves each from its own location, and unlinks the
temporaries one pass at a time:

```
cpp  [opts] -o <base> <src.c>      # writes <base>.x, <base>.tok, <base>.nam
c0   <base>.x <base>.ast <base>.dat
c1   <base>.ast <base>.dat <out>.s
peep <in>.s <out>.s                 # only under -O
asz  -o <out>.o <in>.s
ld   <objects and archives> ...     # to a.out
```

The `.x`/`.tok`/`.nam`/`.ast`/`.dat` intermediates go in `/tmp`; only what the
user asked to keep is written beside the source. `-s` stops after `c1` (keeps
`.s`), `-c` after `asz` (keeps `.o`), `-k` keeps every intermediate, and `-H`
feeds pass1 the readable `.tok` dump instead of the binary `.x`.

## The Z280 variant

`-m z280` selects a Z280 code generator; `-m micronix` and `-m cpm` keep
their Z80 meaning. The two variants coexist — the divergent passes are
separate programs, installed under `280` names, and the runtimes are split
the same way:

| | Z80 | Z280 |
|---|---|---|
| c0 | `src/pass1` → `c0` | `src/pass1z280` → `c0280` |
| c1 | `src/pass2` → `c1` | `src/pass2z280` → `c1280` |
| peep | `src/peep` → `peep` | `src/peep280` → `peep280` |
| asz | `asmz80.c`/`asm.c` → `asz` | `asmz280.c`/`asm280.c` → `asz280` |
| runtime | `libc.a libu.a libf.a crt0.o` | `libc280.a libu280.a libf280.a crt0280.o` |

The reason for the split is the frame convention: the Z80 runtime uses IY
as the frame pointer (`csv.s`); the Z280 runtime unwinds with the SR mode
(`csv280.s`), which frees IY as a second register-variable pointer. The Z280
assembler implements the full unprivileged Z280 instruction set (Appendix C);
`zsim` (`src/tools/zsim.c`) is a host-only simulator for the instructions the
compiler emits. Details are in `PROGRESS.md`.

## Limitations

The compiler compiles itself, so the sources of this tree may not use what
the compiler does not implement. The full, non-negotiable list is
`src/RESTRICTIONS.md`; the essentials:

- **No structure assignment or structure return** (`struct a = b;`, functions
  returning structs by value).
- **No auto aggregate initializers** — no `struct f x = {...};`, no
  `int a[] = {...};`, no `char s[] = "..."` on a local. File-scope and
  `static` aggregates are fine.
- **No `const` or `signed`** qualifiers.
- **No union initializers**, in any spelling.
- **`switch` case labels must fit in an `int` (sixteen bits).** Values 0..255
  take the byte dispatch; any wider value widens the whole switch to a 16-bit
  pair table (`swtabw`). Only a value outside the 16-bit range — a `long`
  case label — is refused.
- **Symbol names are 14 characters or fewer** (the object format's 15-char
  limit, minus the leading underscore).
- **`float` and `double` are ordinary words, not types.** There is no float
  type, no float constants, and no float helpers. `libf` (`src/libf/`) is a
  software float a program links when it wants one — work in progress, see
  `src/libf/README` for the known defect.
- **Arguments are not converted to the parameter type** (K&R definitions), so
  `f(0)` into `f(v) long v;` passes two bytes to a four-byte parameter.

The runtime convention, which anything a compiled program calls must honor:
the **callee saves BC** (BC and IX are the register-variable homes); a
long returns in `HL':HL`; and exits must not disturb HL, the flags, DE, or
the shadow set. See `src/pass2/STACK.md` and the "Invariants" section of
`src/RESTRICTIONS.md`.

## Building and testing

```sh
make            # host passes into desthost/, then the Z80 runtime into destmicronix/
make test       # assembler and generated-code suites
make selfhost   # compile the compiler with itself; host vs simulated output must match
make prodtest   # full production-coverage suite (complete + correct + cover + sim + footprint)
```

The three staging trees are `desthost/` (host binaries), `destmicronix/` and
`destcpm/` (Z80). `make install` copies into `desthost/`; `make sysinstall`
copies that onto `$(PREFIX)`. The CP/M target is parked — see the `cpm`
target in `GNUmakefile`. Every directory answers to `all`, `install`, `clean`
and `clobber`.

## Further reading

| document | covers |
|---|---|
| `src/README.md` | per-pass detail and the full pipeline |
| `src/RESTRICTIONS.md` | the C subset the tree compiles under, and the codegen invariants |
| `PROGRESS.md` | where the project stands, and the bug shapes that recur |
| `src/AST_FORMAT.md` | the `.ast` binary, byte by byte |
| `src/pass1/PASS1.md` | pass1 overall |
| `src/pass2/README.md`, `REWRITE.md`, `STACK.md`, `CONDITIONS.md`, `HELPERS.md` | codegen, the rule engine, frames, conditions, helpers |
| `src/tools/README.md`, `ASZ.md`, `WS.md` | the assembler, linker, and object format |
| `src/libf/README` | the software float library and its known defect |
| `tests/README.md` | the test suites |
