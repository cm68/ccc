## Quick context for AI coding agents

This repository (`ccc`) is a small, hand-typed C compiler project under reconstruction. Many parts are incomplete; the work is purposeful and incremental. Use the notes below to get productive quickly.

### Big picture
- Two-pass native compiler: `cc1` (parser + embedded CPP) produces a parse tree; `cc2` (generator/assembler) is mostly not implemented yet.
- Key components: `cc1.c` (driver), `lex.c` (lexer + CPP), `parse.c` (recursive-descent parser), `expr.c` (expression parsing and folding), `type.c` (type system), `declare.c` (declarations), `macro.c`/`io.c` (preprocessor helpers), `error.c` (reporting).

### Build / test / debug (concrete commands)
- Build default (compiler frontend): `make` or `make cc1`.
- Run lexer/preprocessor tests: `make test` or `make tests` (calls `./runtest.sh` over `tests/*.c`).
- Run a single test: `./runtest.sh tests/decl.c` (add `-v N` for verbosity, e.g. `-v 3`).
- Regenerate auto-generated files: `make regen` (runs `genop_pri`, `maketokens`, `makedebug.sh`, `makeerror.awk` as needed).
- Clean: `make clean`; remove binaries: `make clobber`.
- Debug runtime output: `./cc1 -v 0x3f -E tests/decl.c` (verbosity is a hex bitmask defined by `VERBOSE()` tags).

### Auto-generated artifacts — do NOT edit
- `enumlist.h`, `tokenlist.c`, `error.h`, `debug.h`, `debugtags.c`, `op_pri.h` are generated. Regenerate via `make regen` or the specific generator targets: `genop_pri`, `maketokens`, `./makedebug.sh`, `awk -f makeerror.awk < errorcodes`.

### Project-specific conventions & patterns
- Token encoding: many tokens are encoded as single-byte printable chars; see `token.h` and `tokenlist.c` (generated) for mappings.
- Type unification: the type system interns types — identical types share the same `struct type *`. Inspect `type.c` to follow creation/search logic.
- Scope/name table: global `names[]` with `level` fields; `push_scope()` / `pop_scope()` manage lexical scopes.
- Expression parsing: precedence is generated into `op_pri.h` (run `./genop_pri` to regenerate). `parse_expr` uses precedence climbing and performs constant folding.

### Where to look for common tasks
- Add a new token: edit `token.h`, update `enumlist.h`/`tokenlist.c` via `make regen` (or run `maketokens`).
- Adjust debug tags: add `VERBOSE(name)` in sources and run `./makedebug.sh` to regenerate `debug.h`/`debugtags.c`.
- Change operator precedence: edit `genop_pri.c` or its input and run `make genop_pri` / `make regen` to refresh `op_pri.h`.

### Known repository state to expect
- The codebase is explicitly incomplete — many features (typedef, full codegen, some statement parsing) are not implemented. Tests in `tests/` mainly exercise lexer/preprocessor and small parser fragments.
- Some code paths are disabled via preprocessor guards (for example, `notdef`); don't assume all files compile into a complete compiler.

### Quick pointers for edits and PRs
- Prefer changing high-level source files (e.g., `parse.c`, `expr.c`, `type.c`) and keep generated files out of commits unless the change requires regenerating them (then run the generator and include updated generated files).
- Run `make` locally, then `make test` / `./runtest.sh` for the parts you touched. Include `make regen` in your change if you changed token lists, debug tags, operator priorities, or errorcodes.

If anything here is unclear or you want more detail on a specific subsystem (type system, expression folding, or preprocessor), tell me which area and I'll expand the instructions with examples and file references.
