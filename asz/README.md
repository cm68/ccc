# asz - Z80 Assembler

A Z80 assembler producing Whitesmith's relocatable object files.

## Usage

```
asz [-v] [-9] [-n] [-o outfile] [infile]
```

### Options

- `-v` - Verbose mode. Prints relaxation statistics to stderr.
- `-9` - Use 9-character symbol names (default: 15 characters).
- `-n` - Suppress timeout on interactive input.
- `-o outfile` - Specify output file (default: `a.out`).

If no input file is specified, reads from stdin.

## Features

### Jump Relaxation

The assembler automatically converts `jp` instructions to shorter `jr`
instructions when the target is within range (-128 to +127 bytes). This
optimization applies to:

- `jp label` → `jr label`
- `jp nz,label` → `jr nz,label`
- `jp z,label` → `jr z,label`
- `jp nc,label` → `jr nc,label`
- `jp c,label` → `jr c,label`

Conditions `po`, `pe`, `p`, and `m` cannot be relaxed (Z80 has no `jr`
variants for these).

With `-v`, reports bytes saved to stderr.

### Symbol Names

By default, symbols are 15 characters. Use `-9` for compatibility with
systems expecting 9-character names. The object file header indicates
which format is used.

## Directives

- `.text` - Switch to text (code) segment
- `.data` - Switch to data segment
- `.globl sym` - Declare symbol as global
- `.byte val,...` - Emit byte values
- `.word val,...` - Emit word values
- `.blkb n` - Reserve n bytes
- `.blkw n` - Reserve n words
- `.ascii "str"` - Emit ASCII string
- `.asciz "str"` - Emit null-terminated ASCII string

## Output Format

Produces Whitesmith's relocatable object file format with:
- Header with segment sizes and configuration
- Text and data segments
- Symbol table
- Relocation entries

## Origin

Extensively modified from TRASM:
https://github.com/tergav17/TRASM

The author, Gavin Tersteeg, has placed TRASM under GPL3.0, so that's
what this code is licensed as as well.
