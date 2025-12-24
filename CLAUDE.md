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
