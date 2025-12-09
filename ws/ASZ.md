# ASZ - Z80 Assembler

ASZ is a two-pass Z80 assembler that produces Whitesmith's object file format.
It is designed as a compiler backend and works with external preprocessors.

## Usage

```
asz [-vmn98] [-o objectfile] [sourcefile]
```

### Options

- `-v` - Increase verbosity (can be repeated)
- `-9` - 9 character symbol names (default is 15)
- `-8` - 8080 mode (disable jp->jr relaxation)
- `-n` - No timeout (default 5 second timeout)
- `-o file` - Specify output object file

If no source file is specified, reads from stdin and outputs to `a.out`.
Otherwise, `foo.s` produces `foo.o`.

## Syntax

Lines have the format:
```
[label:] [instruction|directive] [operands] [; comment]
```

Labels end with `:`. Using `::` (double colon) exports the symbol globally.

Comments start with `;` and extend to end of line.

## Segments

Three segments are available:

| Directive | Segment |
|-----------|---------|
| `.text`   | Code segment |
| `.data`   | Initialized data |
| `.bss`    | Uninitialized data |

## Symbol Directives

| Directive | Description |
|-----------|-------------|
| `.global name` | Export symbol (also `.globl`) |
| `.extern name` | Import external symbol |

Multiple symbols can be listed, comma-separated:
```
.global _foo, _bar
.extern _printf, _exit
```

## Data Directives

| Directive | Description |
|-----------|-------------|
| `.db val,...` | Define bytes (also `.defb`) |
| `.dw val,...` | Define words (also `.defw`) |
| `.ds count` | Define space (reserve bytes) |

Examples:
```
.db 0x12, 0x34, 'A', "hello", 0
.dw label, label+2, 0x1234
.ds 100
```

## Numeric Constants

Several formats are supported:

| Format | Example | Description |
|--------|---------|-------------|
| Decimal | `123` | Plain decimal |
| Hex (C) | `0x1A`, `0X1a` | C-style hex |
| Hex (Intel) | `1Ah`, `0ABCDh` | Intel-style (suffix h) |
| Octal (C) | `0777` | Leading zero |
| Octal (suffix) | `377o`, `377O` | Suffix o |
| Binary (C) | `0b1010` | C-style binary |
| Binary (suffix) | `1010b`, `1010B` | Suffix b |

**Note:** Hex numbers starting with A-F must be prefixed with `0` (e.g., `0ABCDh`).

## Character Constants

| Format | Example | Description |
|--------|---------|-------------|
| Character | `'A'` | ASCII value |
| Escaped | `'\n'` | Escape sequence |

Supported escapes: `\b` (backspace), `\e` (ESC), `\n` (newline), `\r` (CR),
`\t` (tab), `\v` (VT), `\nnn` (octal), `\\` (backslash).

## String Constants

Strings are enclosed in double quotes and support the same escapes:
```
.db "Hello, World!\n", 0
```

## Expressions

Simple expressions with one operation are supported:

| Expression | Description |
|------------|-------------|
| `symbol` | Symbol value |
| `number` | Numeric constant |
| `$` | Current address |
| `symbol+number` | Symbol plus offset |
| `symbol-number` | Symbol minus offset |

Full arithmetic expressions are not supported.

## Z80 Instructions

All standard Z80 instructions are supported. Mnemonics are case-insensitive.

### Basic Instructions

```
nop, rlca, rrca, rla, rra, daa, cpl, scf, ccf, halt, exx, di, ei
```

### Extended Basic (ED prefix)

```
neg, retn, reti, rrd, rld
ldi, cpi, ini, outi, ldd, cpd, ind, outd
ldir, cpir, inir, otir, lddr, cpdr, indr, otdr
```

### Arithmetic

```
add a,r          ; r = b,c,d,e,h,l,(hl),a,ixh,ixl,(ix+d),iyh,iyl,(iy+d),n
adc a,r
sub r
sbc a,r
and r
xor r
or r
cp r
add hl,rr        ; rr = bc,de,hl,sp
adc hl,rr
sbc hl,rr
add ix,rr        ; rr = bc,de,ix,sp
add iy,rr        ; rr = bc,de,iy,sp
```

### Increment/Decrement

```
inc r            ; r = b,c,d,e,h,l,(hl),a,ixh,ixl,(ix+d),iyh,iyl,(iy+d)
dec r
inc rr           ; rr = bc,de,hl,sp,ix,iy
dec rr
```

### Bit/Shift Operations

```
rlc r, rrc r, rl r, rr r, sla r, sra r, sll r, srl r
bit n,r          ; n = 0-7
res n,r
set n,r
```

### Load Instructions

```
ld r,r'          ; 8-bit register to register
ld r,n           ; 8-bit immediate
ld rr,nn         ; 16-bit immediate
ld rr,(nn)       ; 16-bit from memory
ld (nn),rr       ; 16-bit to memory
ld a,(bc)
ld a,(de)
ld a,(nn)
ld (bc),a
ld (de),a
ld (nn),a
ld a,i
ld a,r
ld i,a
ld r,a
ld sp,hl
ld sp,ix
ld sp,iy
```

### Stack Operations

```
push rr          ; rr = bc,de,hl,af,ix,iy
pop rr
```

### Jump Instructions

```
jp nn
jp cc,nn         ; cc = nz,z,nc,c,po,pe,p,m
jp (hl)
jp (ix)
jp (iy)
jr offset        ; relative jump (-128 to +127)
jr cc,offset     ; cc = nz,z,nc,c only
djnz offset
```

**Jump Relaxation:** By default, `jp` instructions with resolvable targets
in the text segment are automatically converted to `jr` when the target
is within range. Use `-8` to disable this optimization.

### Call/Return

```
call nn
call cc,nn
ret
ret cc
rst n            ; n = 0,8,16,24,32,40,48,56 (or 00h,08h,...,38h)
```

### I/O

```
in a,(n)
in r,(c)
out (n),a
out (c),r
```

### Exchange

```
ex af,af'
ex de,hl
ex (sp),hl
ex (sp),ix
ex (sp),iy
```

### Interrupt Mode

```
im 0
im 1
im 2
```

## Register Names

| 8-bit | 16-bit | Index | Indirect |
|-------|--------|-------|----------|
| a,b,c,d,e,h,l | af,bc,de,hl,sp | ix,iy,ixh,ixl,iyh,iyl | (hl),(bc),(de),(sp),(ix+d),(iy+d),(c) |

### Undocumented Index Half Registers

The undocumented instructions that access the high and low bytes of IX and IY
are supported. These use the `ixh`, `ixl`, `iyh`, `iyl` register names:

```
ld a,ixh         ; load from IX high byte
ld ixl,b         ; load to IX low byte
add a,iyh        ; arithmetic with IY high byte
inc ixl          ; increment IX low byte
```

These instructions work with most 8-bit operations (ld, add, adc, sub, sbc,
and, xor, or, cp, inc, dec) but cannot be mixed across index registers
(e.g., `ld ixh,iyl` is invalid).

Condition codes: `nz`, `z`, `nc`, `c` (or `cr`), `po`, `pe`, `p`, `m`

Special registers: `i`, `r`

## Object File Format

ASZ produces Whitesmith's object format with a 16-byte header:

| Offset | Size | Description |
|--------|------|-------------|
| 0 | 1 | Magic (0x99) |
| 1 | 1 | Config byte |
| 2 | 2 | Symbol table size |
| 4 | 2 | Text segment size |
| 6 | 2 | Data segment size |
| 8 | 2 | BSS segment size |
| 10 | 2 | Heap size |
| 12 | 2 | Text offset |
| 14 | 2 | Data offset |

After header: text data, data segment, symbol table, text relocations,
data relocations.

## Examples

### Hello World

```
        .extern _write, _exit

        .text
_start:
        ld hl,len
        push hl
        ld hl,msg
        push hl
        ld hl,1         ; stdout
        push hl
        call _write
        pop hl
        pop hl
        pop hl

        ld hl,0
        push hl
        call _exit

        .data
msg:    .db "Hello, World!\n"
len = $ - msg
```

### System Call Wrapper

```
        .extern _errno
        .global _close

        .text
_close:
        pop de          ; de = ret addr
        pop hl          ; hl = fd
        push hl         ; restore stack
        push de
        rst 08h
        .db 006h
        ld hl,0
        ret nc
        ld (_errno),hl
        ld hl,0ffffh
        ret
```
