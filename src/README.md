ccc - full native C compiler

A C compiler for the Z80, written in C, that must compile itself **on** the
Z80. Every design decision in the tree is downstream of that: the passes are
separate programs, the sources are split small, and the data structures are
sized in bytes.

## Pipeline

Four programs, driven by `ccc`:

```
  .c ──cpp──> .x + .n ──c0──> .1 + .2 ──c1──> .s ──[peep]──> .s ──asz──> .o ──ld──> a.out
```

| Stage | Program | Reads | Writes |
|-------|---------|-------|--------|
| Preprocess and normalize | `cpp` | `.c` | `.x` lexeme stream, `.n` name sidecar |
| Parse and analyze | `c0` (pass1) | `.x` `.n` | `.1` AST, `.2` data assembly |
| Generate code | `c1` (pass2) | `.1` `.2` `.n` | `.s` assembly |
| Peephole (with `-O`) | `peep` | `.s` | `.s` |
| Assemble | `asz` | `.s` | `.o` |
| Link | `ld` | `.o` `.a` | executable |

The `.2` file is assembly, not AST: globals, string data, and static
initializers, which pass1 can write directly. The `.n` sidecar carries the
identifier spellings — names travel through cpp and pass1 as 2-byte ids, and
pass2 turns them back into symbols.

## Project Status

All four passes self-host, and the tree's own sources compile with the tree's
own compiler. `PROGRESS.md` at the top level tracks where that stands.

**cpp** — C preprocessor plus normalizer
- Macro expansion, conditional compilation, includes
- Typedef dissolution and enum lowering, so pass1 parses without a symbol table
- `sizeof` folded where the size registries can price it
- K&R → ANSI, brace insertion, loop lowering, break/continue → goto
- Outputs a binary lexeme stream
- See [cpp/CPP.md](cpp/CPP.md), [cpp/NORM.md](cpp/NORM.md),
  [cpp/INPUT.md](cpp/INPUT.md), [cpp/OUTPUT.md](cpp/OUTPUT.md)

**c0 (pass1)** — two-phase recursive descent parser
- Phase 1 discovers declarations and counts; phase 2 streams the AST
- Both phases run over one **span** — one function — at a time, so the live
  set is a function's names rather than the file's
- Bottom-up constant folding before emission
- Register allocation: IX for struct pointers, BC/B/C for hot locals
- Only `if` and `goto` for control flow; loops were lowered by cpp
- See [pass1/PASS1.md](pass1/PASS1.md), [pass1/PHASE1.md](pass1/PHASE1.md),
  [pass1/PHASE2.md](pass1/PHASE2.md)

**c1 (pass2)** — code generator targeting Z80
- Streaming: one statement's tree at a time, built, rewritten, freed
- Table-driven expression-tree rewriting with a compact pattern language
- Sethi-Ullman labeling for evaluation order
- Strength reduction for multiply by small constants
- Longs in HL':HL with the `q*` runtime helpers
- See [pass2/README.md](pass2/README.md), [pass2/REWRITE.md](pass2/REWRITE.md),
  [pass2/CONDITIONS.md](pass2/CONDITIONS.md), [pass2/STACK.md](pass2/STACK.md),
  [pass2/HELPERS.md](pass2/HELPERS.md)

**peep** — peephole optimizer
- A sliding window of lines; rules match at its head, and a rewrite re-runs the
  window from the top so one substitution can expose another
- A window rather than the whole file because it too has to run on the Z80, and
  the largest thing the compiler compiles produces a `.s` over 200KB
- Worth a few percent of text — 610 bytes off cpp, 1109 off c0, 1665 off c1 —
  which at this budget is the difference between a source compiling and not
- `peep [-v] in.s out.s`; the driver runs it under `-O`

**Whitesmith's object tools** (in `../tools/`)
- **asz** — Z80 assembler producing relocatable objects
- **ld** — linker for objects and libraries
- **nm** — symbol table and disassembly
- **wslib** / **size** — library manager, size reporter
- See [../tools/README.md](../tools/README.md), [../tools/ASZ.md](../tools/ASZ.md),
  [../tools/WS.md](../tools/WS.md)

**Debugging tools**
- **xdump** — renders a `.x` lexeme stream as text
- **astpp** — AST pretty printer. **Currently out of date with the AST format**
  — see [ASTPP.md](ASTPP.md)

## Memory Constraints

The compiler must fit, and run, within 64KB. On CP/M 2.2 the TPA on a 64KB
system is 56KB, which has to hold text, data, bss, heap, and stack — so a
static footprint around 48KB leaves 8–10KB for heap and stack together. The
Micronix target is a little more generous; CP/M is the tighter ceiling by
roughly 7.5KB.

This is why sources are split the way they are: cpp's per-translation-unit
tables are paid per unit, so the largest source in a directory is the one that
stops fitting first. `fold.c`, `pfx.c`, and `post.c` came out of pass1's
`expr.c`; `lower.c` came out of pass2's `rewrite.c`; `tdsrc.c`, `knr.c`, and
`cfold.c` came out of cpp's `norm.c`.

`make sizecheck` builds a pass natively and reports text/data/bss against the
budget.

## Language Restrictions

The compiler compiles itself, so tree sources may not use what the compiler
does not implement: no structure assignment or return, no auto aggregate
initializers, no `const` or `signed`. Symbol names are 14 characters or fewer.
See [RESTRICTIONS.md](RESTRICTIONS.md).

## File Organization

**cpp** (in `cpp/`)
- `cpp.c` — entry point, command line, normalizer wiring
- `lex.c` — lexer with embedded directive handling
- `macro.c` — macro definition, lookup, expansion
- `io.c` — unified character stream over files, includes, and macro expansions
- `emit.c` — binary token output, string-literal joining
- `norm.c` — the normalizer: recursive-descent walker
- `tdsrc.c` — source layer: enum lowering, typedef dissolution
- `knr.c` — K&R → ANSI function normalization
- `cfold.c` — size registries and the constant folder
- `filtutil.c` — token buffers and classifiers
- `kw.c`, `mkkw.c` — compressed keyword tables (`kwtab.c` is generated)
- `lexdata.c` — the `token_props[]` classification table
- `util.c` — errors, string interning, the `.n` sidecar writer
- `xdump.c` — `.x` → text renderer (separate binary)

**pass1 / c0** (in `pass1/`)
- `pass1.c` — driver and the span loop
- `lexread.c` — lexeme stream reader
- `parse.c`, `pblock.c`, `swcnt.c` — statements, blocks, switch/count ledgers
- `expr.c`, `eutil.c`, `pfx.c`, `post.c`, `fold.c` — expressions and folding
- `decl.c`, `declare.c`, `init.c`, `istream.c` — declarations and initializers
- `type.c`, `tparse.c`, `name.c` — types, scopes, symbol table
- `outast.c`, `outh.c`, `outfn.c` — AST emission
- `regalloc.c` — register allocation and frame layout
- `error.c`, `util.c`

**pass2 / c1** (in `pass2/`)
- `pass2.c` — entry point
- `astio.c` — AST reading, the `.n` sidecar, output primitives
- `parseast.c` — statement dispatch, prolog/epilog, switch dispatch
- `expr.c` — tree builders, Sethi-Ullman labeling, target assignment
- `rewrite.c` — the rule matcher and template interpolator
- `lower.c` — what matched rules call: compounds, longs, calls, conditions
- `rules.c` — the rule table and shared templates

**peep** (in `peep/`) — `peep.c`, `rules.c`, `regs.c`, `pool.c`

**Shared** — `lib/libutil.c` (built into `libccc.a`), `format.h`,
`cpp/lexeme.h` (token codes, shared by all three passes)

**Auto-generated** — `cpp/kwtab.c`; `pass1/enumlist.h`, `pass1/error.h`;
`pass2/rulepat[]` (by `mkrulepat.py`); `debug.h` and `debugtags.c` in each
directory (by `makedebug.sh`)

## Directory Structure

```
ccc/                      # the repository root
├── ccc/                  # the compiler
│   ├── cpp/              # preprocessor and normalizer
│   ├── pass1/            # c0 - parser and analyzer
│   ├── pass2/            # c1 - code generator
│   ├── peep/             # peephole optimizer
│   ├── lib/              # libccc: utilities shared by the passes
│   └── astpp.c           # AST pretty printer
├── tools/                # asz, ld, nm, wslib, size, and the ccc driver
├── libsrc/               # runtime library source
│   ├── include/          # system headers for the target
│   ├── libc/             # C library
│   ├── libcpm/           # CP/M support
│   └── libu/             # Unix syscall wrappers
├── tests/                # test suites - see tests/README.md
├── attic/                # obsolete code, kept for reference
└── root/                 # installed toolchain (after make install)
    ├── bin/              # cpp, xdump, c0, c1, peep, astpp, ccc, asz, ld, ...
    ├── lib/              # crt0.o, crtcpm.o, and per-compiler areas ccc/ and zc3/
    ├── usr/include/      # installed headers
    └── sim               # the Z80 simulator
```

`root/lib` is split per compiler — `root/lib/ccc/` and `root/lib/zc3/` — because
the two use incompatible calling conventions for byte arguments and for longs.
Nothing links both.

## Building

```bash
make            # build all four passes and astpp
make install    # install into root/
make clean      # remove objects
make clobber    # and the binaries and build directories
```

Per-pass targets, run from `cpp/`, `pass1/`, `pass2/`, or `peep/`:

| Target | Effect |
|--------|--------|
| `make stage1` | Run the whole chain over each of that pass's own sources |
| `make sizecheck` | Compile natively (`ZCC=ccc` or `ZCC=zc3`) and report the footprint |
| `make mx-ccc` | The self-build: ccc compiles the pass, linked for the simulator |
| `make com` | A CP/M `.com` image, for the native footprint |
| `make regression` | Byte-exact baseline harness |
| `make test` / `make langtest` | cpp only: conformance and filter suites |
| `make valgrind` | cpp only: leak and invalid-access check over its own sources |

**Do not run the passes directly from the command line.** Use the `ccc` driver
or a Makefile target — the passes take positional file arguments in a fixed
order and will not do anything useful without all of them.

## Command Line Reference

### ccc — compiler driver

```
ccc [options] files...
```

Files: `.c` (compile), `.s` (assemble), `.o` `.a` (link)

| Option | Description |
|--------|-------------|
| `-o <file>` | Output file (default: `a.out`) |
| `-c` | Compile and assemble only, keep `.o` |
| `-s` | Compile only, keep `.s` (no assembly) |
| `-k` | Keep all intermediates (`.x`, `.n`, `.1`, `.2`, `.s`, `.o`) |
| `-O` | Run the peephole optimizer over the assembly |
| `-S` | Strip symbols from the output |
| `-9` | Use 9-char symbols in the output |
| `-E` | Preprocess only |
| `-H` | Feed pass1 the human-readable `.i` instead of the `.x` |
| `-I<dir>` | Include directory |
| `-i<dir>` | System include directory (default `/usr/include`) |
| `-D<var>[=val]` | Define macro |
| `-l<lib>` / `-L<dir>` | Link with `lib<lib>.a` / add a library directory |
| `-C <flags>` | Pass `-v <flags>` to cpp |
| `-1 <flags>` | Pass `-v <flags>` to pass1 |
| `-2 <flags>` | Pass `-v <flags>` to pass2 |
| `-x` | Print commands as they execute |
| `-n` | Dry run: print commands without executing |

### The passes

These take positional arguments, not `-o`:

```
cpp  [options] <source.c>      # writes <base>.x and <base>.n
c0   <base>.x <base>.1 <base>.2
c1   <base>.1 <base>.2 <base>.s
peep [-v] <in>.s <out>.s
```

`cpp` accepts `-o <base>`, `-I`, `-i`, `-D`, `-p` (also write `<base>.i`),
`-E` (dump to stdout), `-N` (suppress line markers), and `-v <mask>` in DEBUG
builds. `c0` and `c1` accept `-v <mask>` in DEBUG builds and nothing else.

## Usage

```bash
ccc prog.c                  # compile and link to a.out
ccc -O -c prog.c            # optimized object only
ccc -k -s prog.c            # assembly, keeping every intermediate
```

Running under the simulator:

```bash
cd tests
../root/bin/ccc -o prog prog.c
../root/sim prog < /dev/null      # sim uses stdin as its console
```

## Inspecting the Intermediates

```bash
ccc -k -s prog.c            # leaves prog.x prog.n prog.1 prog.2 prog.s

xdump prog.x                # the lexeme stream, as text
cpp -p -o prog prog.c       # same thing, into prog.i

od -An -tx1 prog.1          # the AST, against AST_FORMAT.md
grep '^;' prog.s            # what pass2 made of it (DEBUG builds annotate)
```

`astpp prog.1` is the intended way to read an AST, but it is currently behind
the format — see [ASTPP.md](ASTPP.md) for what it gets wrong and what to use
instead.

## Documents

| Document | Covers |
|----------|--------|
| [RESTRICTIONS.md](RESTRICTIONS.md) | The C subset tree sources may use |
| [AST_FORMAT.md](AST_FORMAT.md) | The `.1` binary AST, byte by byte |
| [ASTPP.md](ASTPP.md) | The AST pretty printer |
| [AUDIT.md](AUDIT.md) | Audit notes |
| [LONGREGS.md](LONGREGS.md) | Longs in HL':HL — the design and the measurements |
| [SHADOW.md](SHADOW.md) | Shadow-register variables — what was measured, and why not |
| [cpp/CPP.md](cpp/CPP.md) | The preprocessor |
| [cpp/NORM.md](cpp/NORM.md) | The normalizer |
| [cpp/INPUT.md](cpp/INPUT.md) | The language cpp accepts |
| [cpp/OUTPUT.md](cpp/OUTPUT.md) | The `.x` stream and the `.n` sidecar |
| [pass1/PASS1.md](pass1/PASS1.md) | Pass 1 overall |
| [pass1/PHASE1.md](pass1/PHASE1.md) | Discovery |
| [pass1/PHASE2.md](pass1/PHASE2.md) | Emission |
| [pass2/README.md](pass2/README.md) | Pass 2 overall |
| [pass2/REWRITE.md](pass2/REWRITE.md) | The rewrite engine and pattern language |
| [pass2/CONDITIONS.md](pass2/CONDITIONS.md) | Conditions, labels, switch dispatch |
| [pass2/STACK.md](pass2/STACK.md) | Frame layout and calling convention |
| [pass2/HELPERS.md](pass2/HELPERS.md) | Runtime helpers pass2 calls |
| [../libsrc/libc/QLONG.md](../libsrc/libc/QLONG.md) | The 32-bit register convention |
| [../tests/README.md](../tests/README.md) | The test suites |
