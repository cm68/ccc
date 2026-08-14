# Whitesmith's Object Tools

Tools for assembling, linking, and managing Z80 relocatable object files, plus
the `ccc` compiler driver.

`make` here builds `asz ld wsnm wslib wssize ccc`.

## Tools

### asz - Z80 Assembler

Assembles Z80 source to relocatable object files.

```
asz [-vmn98l] [-o outfile] [infile]
```

| Option | Description |
|--------|-------------|
| `-l` | Write a listing (`<source>.lst`): address, bytes, source, symbols |
| `-9`, `-m` | 9-character symbol names (default 15) — the same flag |
| `-8` | 8080 mode (disable jp->jr relaxation) |
| `-n` | No timeout on stdin |
| `-o file` | Output file (default: a.out or foo.s -> foo.o) |
| `-v` | Verbose (DEBUG builds only) |

See [ASZ.md](ASZ.md) for assembler syntax and instruction set.

### ld - Linker

Links object files and libraries into executables. Accepts both `.o` object
files and `.a` library archives.

```
ld [-vV9rs] [-o outfile] [-L<dir>] [-l<lib>] [-Ttext=addr] [-Tdata=addr] [-Tbss=addr] file...
```

| Option | Description |
|--------|-------------|
| `-v` | Verbose |
| `-V` | List object files linked |
| `-r` | Emit relocatable output (for subsequent links) |
| `-s` | Strip symbol table |
| `-9` | 9-character symbols in output (default 15) |
| `-L<dir>` | Add `<dir>` to the library search path |
| `-l<lib>` | Link with `lib<lib>.a` |
| `-Ttext=addr` | Set text segment base |
| `-Tdata=addr` | Set data segment base |
| `-Tbss=addr` | Set bss segment base |

### wsnm - Symbol Table / Disassembler

Displays object file contents. Accepts both `.o` object files and `.a`
library archives.

```
wsnm [-bdgrv] file [...]
```

| Option | Description |
|--------|-------------|
| (none) | Show symbol table only |
| `-b` | Hex dump text/data segments |
| `-d` | Disassemble text segment |
| `-g` | Generate assemblable .s files |
| `-r` | Show relocations |
| `-v` | Show header |

### wslib - Library Manager

Creates and manages static libraries (archives).

```
wslib [-crvHxat] archive [file...]
```

| Option | Description |
|--------|-------------|
| `-c` | Create archive (with `-r`: create if it does not exist) |
| `-r` | Replace/add files in archive |
| `-t` | List archive contents |
| `-x` | Extract files (all if none specified) |
| `-a` | Append files to archive |
| `-v` | Verbose (list files as processed) |
| `-H` | Create a HiTech format library (default: Whitesmith) |

See [HITECHLIB.md](HITECHLIB.md) for the HiTech archive format and
[HITECHOBJ.md](HITECHOBJ.md) for the HiTech object format that `ld` and
`wsnm` also read.

### wssize - Size Utility

Displays segment sizes for object files.

```
wssize file...
```

Prints text, data, bss, and total per file. The compiler Makefiles' `sizecheck`
targets pipe their objects through it.

### ccc - Compiler Driver

Also built here. Runs cpp, c0, c1, optionally `peep`, then `asz` and `ld`.
See [../ccc/README.md](../ccc/README.md) for its options and the pipeline.

## File Formats

See [WS.md](WS.md) for the Whitesmith's object file and library formats, and
[HITECHOBJ.md](HITECHOBJ.md) / [HITECHLIB.md](HITECHLIB.md) for the HiTech
formats the tools also read (zc3, the reference compiler, emits them).

## Origin

The assembler is extensively modified from TRASM by Gavin Tersteeg:
https://github.com/tergav17/TRASM

Licensed under GPL3.0.
