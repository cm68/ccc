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

## Code Restrictions (see ccc/RESTRICTIONS.md)

The compiler must self-host on Z80. Do NOT use:
- Structure assignment or structure return
- Auto aggregate initializers (`struct x = {...}` or `int a[] = {...}`)
- `const` or `signed` qualifiers

## Build Instructions

**IMPORTANT:** Do NOT run compiler passes (c0, c1, cpp, etc.) directly from the command line. Compiler passes are ONLY to be run:
- From the Makefile using target rules (e.g., `make stage1`)
- Using the `root/bin/ccc` compiler driver

The compiler driver (`ccc`) has options to:
- Stop before assembly, before link, after cpp, etc.
- Pass debug flags to pass1 and pass2
