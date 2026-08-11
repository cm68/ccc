# ccc Compiler Tests

There are five test suites in this tree, and they answer different questions.
Only the first is what this directory's `.c` files are for; the others are the
ones that have caught real bugs.

| Suite | Where | Question |
|-------|-------|----------|
| Parser smoke tests | `tests/*.c` | Does the front end survive this construct? |
| Runtime correctness | `tests/run/` | Does the compiled program compute the right answer? |
| Production coverage | `tests/gen/` | Is there a rule for every shape, and a shape for every rule? |
| cpp regression | `tests/baseline/`, `regress.sh` | Did cpp's output change, byte for byte? |
| cpp conformance | `ccc/cpp/` (`make test`, `make langtest`) | Is the input legal and the output to spec? |

Top-level entry points, from the repository root:

```bash
make test        # tools tests, libcpm check, and the parser smoke tests
make prodtest    # the production-coverage suite (tests/gen)
make regression  # the cpp byte-exact baseline harness
make valgrind    # uninitialised-read sweep over the passes (needs stage1)
```

---

## Runtime correctness — `tests/run/`

**The suite that runs what it compiles.** Everything else here checks that code
compiles and that the assembly looks right; these check that it computes the
right answer, which is the only thing that catches a wrong branch or an
unsigned comparison standing in for a signed one — both of which generate
perfectly clean code.

90 hand-written `rt_*.c` cases, each built and run two ways over the same
source:

- **native** — the host compiler, run directly. The reference.
- **ccc** — this compiler, run under the simulator.

A disagreement between the two is a bug in ccc.

```bash
make -C tests/run              # both paths
make -C tests/run native
make -C tests/run ccc
make -C tests/run one MODE=ccc T=rt_cmp.c      # one file, one path
```

`one` takes any filename, not just an `rt_` one, so a throwaway probe dropped
next to these gets the same build and run without being picked up by the suite.
That is the fastest way to narrow a failure down.

There was a third path once — Hi-Tech's `zc3` under the simulator, to tell a ccc
bug apart from a test making an assumption about widths it should not. It
stopped being worth its upkeep: zc3 failed this suite on its own unsigned-char
arithmetic.

### Assembly snapshots

```bash
make -C tests/run asmsnap DIR=before
# ...change something...
make -C tests/run asmsnap DIR=after && diff -r before after
```

Byte-identical output is a stronger statement than a passing test, because it
covers the paths the tests do not reach. Use it when a change is meant to leave
code generation alone.

---

## Production coverage — `tests/gen/`

Every shape the language can put in front of the compiler, and every rule the
compiler carries, each checking the other. `make prodtest` from the root, or
`make -C tests/gen`. Six legs:

| Leg | What it proves |
|-----|----------------|
| `corpus` | Regenerates the `gp_*.c` programs into `../run`. Deterministic — same seed, same bytes, so a failing check number means the same thing everywhere. |
| `complete` | Compiles the corpus with `-O` and fails on any `XXXXXX` marker — the comment pass2 leaves where **no rule named a shape** and code was silently not emitted. The "no missing productions" direction. |
| `correct` | Runs the corpus native (the oracle) and under ccc, and compares. |
| `cover` | Which rewrite rules never fire, over the corpus **plus the whole tree**. The blessed list is `unfired.ok` and the regression is that it must not *grow*: a new rule ships with something that fires it, or its name lands in the report. |
| `sim` | The completeness leg again, with the compiler itself running under the simulator — `c0.mx`/`c1.mx` compile the corpus and every output must be byte-identical to the host compiler's. |
| `footprint` | The tipping-point guard: the three simulated passes compile the compiler's **own** sources, and every run's heap gap is read from the simulator. Fails on out-of-memory, on any gap under `FLOOR` (default 256), or on divergence from the host. |

`sim` is the slow leg; skip it with `make -C tests/gen complete correct cover`
when iterating. `make -C tests/gen bless` re-blesses the never-fired list.

The footprint leg is the one that matters most now: it is what turns "this bug
fix grew a pass past what Micronix can hold" into a build failure with the
margin table in hand, rather than a discovery in the field.

---

## cpp regression — `regress.sh` and `tests/baseline/`

Runs cpp over a comprehensive corpus and compares each file's output against a
checked-in baseline tree. A regression is any change in any of three files.

```
tests/baseline/<reldir>/<base>.x     cpp output (binary lexeme stream);
                                     missing means cpp failed
tests/baseline/<reldir>/<base>.rc    exit code (always present)
tests/baseline/<reldir>/<base>.err   stderr (only if non-empty after
                                     noise filtering)
```

```bash
./regress.sh                  # compare current output to the baseline
./regress.sh --bless          # regenerate the baseline from the current cpp
./regress.sh --keep           # leave temp outputs in place even on pass
./regress.sh --filter ccc/    # only files matching a shell pattern
./regress.sh --cpp PATH       # use a specific cpp binary
./regress.sh --list           # just list the corpus
```

Each compiler directory has a `make regression` that scopes this to its own
sources.

---

## Parser smoke tests — `tests/*.c`

The oldest suite: single-file constructs fed through the front end. A test
passes if the compiler completes without crashing and exits 0 — **a test may
produce parse errors and still pass**, which is why this suite proves much less
than the three above. It is a crash net, not a correctness check.

```bash
make -C tests test              # the curated list in tests/GNUmakefile
make -C tests tests             # every .c file in the directory
make -C tests test-fail         # the cases expected to fail (error detection)
make -C tests test-all          # both
./runtest.sh decl.c             # a single test
./runtest.sh -v 0x3f decl.c     # with a debug verbosity bitmask
```

Category targets: `test-expr`, `test-decl`, `test-cpp`, `test-kr`, `test-func`,
`test-stmt`, `test-sizeof`, `test-typedef`, `test-cast`, `test-string`,
`test-incr-decr`, `test-ptr-compat`, `test-lvalue`, `test-struct`. Each runs the
corresponding `*_TESTS` list from `tests/GNUmakefile`.

The categories cover: preprocessor directives and macros; declarations, structs,
bitfields, and `sizeof`; expression parsing and constant folding; K&R and ANSI
function definitions; statements and control flow; scopes; string literals;
casts; increment/decrement; pointer compatibility; and lvalue validation.

To add a test: write the file with a header comment saying what it tests, add
it to the right `*_TESTS` list in `tests/GNUmakefile`, and run `make -C tests test`.

---

## Memory hygiene

```bash
make valgrind                  # from the root; needs stage1 first
```

This runs `tests/vgsweep.sh`. Leaks are **not** what it is for — these programs
read a file, write a file, and exit. A field read before it is written is the
thing that matters, because it changes what the compiler emits and does it
differently depending on what was compiled before.

cpp has its own leak-checking target, `make -C ccc/cpp valgrind`, which does
fail on definite and indirect leaks.

---

## Supporting scripts

| Script | Purpose |
|--------|---------|
| `runtest.sh` | Runs one or more parser smoke tests |
| `regress.sh` | The cpp byte-exact baseline harness |
| `vgsweep.sh` | The uninitialised-read sweep behind `make valgrind` |
| `rulecover.py` | Reports which rewrite rules fired; drives `tests/gen`'s `cover` leg |
| `footprint.py` | Per-source footprint measurement |
| `simcheck.c` / `simcheck` | Simulator harness helper |
| `diffcpp.sh`, `diffpass1.sh` | Differential helpers for a single pass |
| `gen/genprod.py` | Generates the `gp_*.c` production corpus |
| `gen/simcheck.sh`, `gen/footprint.sh` | The `sim` and `footprint` legs |
| `run/runtests.sh` | Builds and runs the runtime corpus, native or ccc |

**The simulator reads stdin as its console** and rewinds inherited descriptors,
so anything driving it needs `< /dev/null` — a `while read` loop around it will
otherwise spin forever.
