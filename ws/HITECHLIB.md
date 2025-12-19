# HiTech Z80 Library File Format

This document describes the library file format (.LIB) used by the HiTech Z80 toolchain.

## Overview

A HiTech library file combines multiple object files into a single archive with an indexed symbol directory for efficient linking. The format separates the symbol directory from the actual module data, allowing the linker to quickly scan symbols without reading full object files.

## File Structure

```
+---------------------------+
| Library Header (4 bytes)  |
+---------------------------+
| Symbol Directory          |
|   Module 1 header + syms  |
|   Module 2 header + syms  |
|   ...                     |
+---------------------------+
| Module Data Section       |
|   Module 1 object data    |
|   Module 2 object data    |
|   ...                     |
+---------------------------+
```

## Library Header

| Offset | Size | Description |
|--------|------|-------------|
| 0-1 | 2 | size_symbols - Total size of symbol directory (little-endian) |
| 2-3 | 2 | num_modules - Number of modules in library (little-endian) |

The module data section begins at offset `4 + size_symbols`.

## Symbol Directory

The symbol directory contains one entry per module, consisting of a module header followed by symbol entries.

### Module Header (12 bytes + name)

| Offset | Size | Description |
|--------|------|-------------|
| 0-1 | 2 | symSize - Size of symbol info for this module (bytes) |
| 2-3 | 2 | symCnt - Number of symbols in this module |
| 4-7 | 4 | moduleSize - Size of module object data (little-endian) |
| 8-11 | 4 | unused - Reserved (typically zero) |
| 12+ | varies | moduleName - Null-terminated module name (e.g., "printf.obj") |

### Symbol Entries

Following the module name are `symCnt` symbol entries:

| Offset | Size | Description |
|--------|------|-------------|
| 0 | 1 | flags - Symbol type |
| 1+ | varies | name - Null-terminated symbol name |

### Symbol Flags

| Value | Type | Description |
|-------|------|-------------|
| 0 | D | Defined - Public symbol exported by module |
| 2 | C | Common - Common block symbol |
| 6 | U | Undefined - External reference needed by module |

## Module Data Section

The module data section contains the raw object file data for each module, concatenated in the same order as the symbol directory entries. Each module's data is a complete HiTech object file (see HITECHOBJ.md).

## Example

For a library with 2 modules:

```
Offset  Content                     Description
------  --------------------------  --------------------------------
0x0000  xx xx                       size_symbols (e.g., 0x0064 = 100)
0x0002  02 00                       num_modules = 2

        === Symbol Directory ===
0x0004  20 00                       Module 1: symSize = 32
0x0006  03 00                       symCnt = 3
0x0008  xx xx xx xx                 moduleSize = size of obj data
0x000C  00 00 00 00                 unused
0x0010  "foo.obj\0"                 moduleName
0x0018  00 "_foo\0"                 Symbol: D _foo (defined)
0x001E  06 "_bar\0"                 Symbol: U _bar (undefined)
0x0024  06 "_baz\0"                 Symbol: U _baz (undefined)
        ...
        (Module 2 header + symbols)
        ...

        === Module Data (at offset 4 + size_symbols) ===
0x0068  0a 00 07 00 01 02 03...     Module 1 object file (IDENT record...)
        ...
        (Module 2 object file)
```

## Usage

The library format enables efficient linking:

1. **Symbol Lookup**: The linker reads only the symbol directory to find which modules define needed symbols.

2. **Selective Loading**: Only modules that satisfy unresolved references are loaded from the module data section.

3. **Fast Scanning**: The fixed-size module header allows quick traversal of the symbol directory.

## Tools

- **libr** - Original HiTech library manager (libr.c)
- **wsnm** - Can dump HiTech library contents with `-v` flag
- **wslib** - Can list (`-t`) and extract (`-x`) from HiTech libraries

## Notes

- All multi-byte integers are little-endian
- All strings are null-terminated
- Module names typically include the .obj extension
- The symSize field includes all symbol entries (flags + names + null terminators)
- Module object data is standard HiTech object format (see HITECHOBJ.md)
