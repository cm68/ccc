# Whitesmith's Object Tools

Tools for assembling, linking, and managing Z80 relocatable object files.

## Tools

### asz - Z80 Assembler

Assembles Z80 source to relocatable object files.

```
asz [-v] [-8] [-9] [-n] [-o outfile] [infile]
```

| Option | Description |
|--------|-------------|
| `-v` | Verbose (show relaxation stats) |
| `-8` | 8080 mode (disable jp->jr relaxation) |
| `-9` | 9-character symbol names (default 15) |
| `-n` | No timeout on stdin |
| `-o file` | Output file (default: a.out or foo.s -> foo.o) |

See [ASZ.md](ASZ.md) for assembler syntax and instruction set.

### wsld - Linker

Links object files and libraries into executables. Accepts both `.o` object
files and `.a` library archives.

```
wsld [-vVrs9] [-o outfile] [-Ttext=addr] [-Tdata=addr] [-Tbss=addr] file...
```

| Option | Description |
|--------|-------------|
| `-v` | Verbose |
| `-V` | List object files linked |
| `-r` | Emit relocatable output |
| `-s` | Strip symbol table |
| `-9` | 9-character symbols in output |
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
wslib [-crtxav] archive [file...]
```

| Option | Description |
|--------|-------------|
| `-c` | Create archive |
| `-r` | Replace/add files |
| `-t` | List archive contents |
| `-x` | Extract files (all if none specified) |
| `-a` | Append files |
| `-v` | Verbose |

### wssize - Size Utility

Displays segment sizes for object files.

```
wssize file...
```

## File Formats

See [WS.md](WS.md) for object file and library format documentation.

## Origin

The assembler is extensively modified from TRASM by Gavin Tersteeg:
https://github.com/tergav17/TRASM

Licensed under GPL3.0.
