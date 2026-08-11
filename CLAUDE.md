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
  Z80. Installed into `unix/`. This is `make all`.
- **cross** — that compiler compiling itself into Z80 binaries, into
  `micronix/` and `cpm/`. This is `make micronix` / `make cpm`.
- **native** — the same compiler built on Micronix by the tools already there.

The cross and native builds must produce identical output; `make selfhost` is
the gate that asserts it.

Everything driven from the build host — host and cross both — lives in
`GNUmakefile` at every level, with the shared definitions in `GNUmakefile.inc`
at the top of the tree. GNU make looks for `GNUmakefile` before `Makefile`, so
`Makefile` is left for the native Micronix make. Every one of them has `all`,
`install`, `clean` and `clobber`.

`make install` puts the driver and the object tools in `$(PREFIX)/bin` and
everything the driver runs in `$(PREFIX)/lib`; PREFIX defaults to
`/usr/local` and DESTDIR stages it elsewhere.

**IMPORTANT:** Do NOT run compiler passes (c0, c1, cpp, etc.) directly from the command line. Compiler passes are ONLY to be run:
- From the GNUmakefile using target rules (e.g., `make stage1`)
- Using the `unix/bin/ccc` compiler driver

The compiler driver (`ccc`) has options to:
- Stop before assembly, before link, after cpp, etc.
- Pass debug flags to pass1 and pass2
