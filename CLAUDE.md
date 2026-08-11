# CLAUDE.md

## Project Overview

**ccc** - C compiler targeting Z80.

## Goals

- Target Z80 processor
- Must fit in <64KB (code + data) when compiled natively
- Make-based build system with orchestrated subdirectories
- 4-character tabs (noexpandtab)

## Naming Constraints

- All symbol names (functions, globals, statics) must be 14 characters or less
- The object file format limits symbols to 15 characters, and C's leading underscore uses one

## Code Restrictions (see src/RESTRICTIONS.md)

The compiler must self-host on Z80. Do NOT use:
- Structure assignment or structure return
- Auto aggregate initializers (`struct x = {...}` or `int a[] = {...}`)
- `const` or `signed` qualifiers

## Build Instructions

There are three builds:

- **host** — the compiler built by gcc, running on the build machine, emitting
  Z80. Installed into `desthost/`. This is `make all`.
- **cross** — that compiler compiling itself into Z80 binaries, into
  `destmicronix/` and `destcpm/`. This is `make micronix` / `make cpm`.
- **native** — the same compiler built on Micronix by the tools already there.

The cross and native builds must produce identical output; `make selfhost` is
the gate that asserts it.

Everything driven from the build host — host and cross both — lives in
`GNUmakefile` at every level, with the shared definitions in `GNUmakefile.inc`
at the top of the tree. GNU make looks for `GNUmakefile` before `Makefile`, so
`Makefile` is left for the native Micronix make. Every one of them has `all`,
`install`, `clean` and `clobber`.

Installing is two steps and only the second wants a password. `make install`
is the tree walk — every directory copies what it built into `$(HOSTDIR)`,
which is `desthost/` — and is a no-op after a build. `make sysinstall` copies
that directory onto `$(PREFIX)`, which defaults to `/usr/local`; DESTDIR
stages it elsewhere and `SUDO=` runs it without privilege. Nothing is rebuilt
for the new location.

The three staging trees are named for the machine they are for: `desthost/`
holds host binaries, `destmicronix/` and `destcpm/` hold Z80 ones, each laid
out as that machine's system root (`bin`, `lib`, `usr/include`).

**IMPORTANT:** Do NOT run compiler passes (c0, c1, cpp, etc.) directly from the command line. Compiler passes are ONLY to be run:
- From the GNUmakefile using target rules (e.g., `make stage1`)
- Using the `desthost/bin/ccc` compiler driver

The compiler driver (`ccc`) has options to:
- Stop before assembly, before link, after cpp, etc.
- Pass debug flags to pass1 and pass2
