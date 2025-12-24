# ASZ - Z80 Assembler Reference

## Syntax

```
[label:] [instruction|directive] [operands] [; comment]
```

- Labels end with `:`. Using `::` exports the symbol globally.
- Comments start with `;` and extend to end of line.

## Pseudo-Ops (Directives)

### Segment Directives

| Directive | Description |
|-----------|-------------|
| `.text` | Switch to code segment |
| `.data` | Switch to initialized data segment |
| `.bss` | Switch to uninitialized data segment |

### Symbol Directives

| Directive | Alias | Description |
|-----------|-------|-------------|
| `.global name` | `.globl` | Export symbol |
| `.extern name` | | Import external symbol |

Multiple symbols can be listed, comma-separated:
```
.global _foo, _bar
.extern _printf, _exit
```

### Data Directives

| Directive | Alias | Description |
|-----------|-------|-------------|
| `.db val,...` | `.defb` | Define bytes |
| `.dw val,...` | `.defw` | Define words (16-bit) |
| `.ds count` | | Define space (reserve bytes) |

Examples:
```
.db 0x12, 0x34, 'A', "hello", 0
.dw label, label+2, 0x1234
.ds 100
```

### Symbol Definition

```
name = expression
```

Defines `name` as the value of `expression`.

## Numeric Constants

| Format | Example | Description |
|--------|---------|-------------|
| Decimal | `123` | Plain decimal |
| Hex (C) | `0x1A` | C-style hex |
| Hex (Intel) | `0ABCDh` | Suffix h (prefix 0 if starts A-F) |
| Octal (C) | `0777` | Leading zero |
| Octal (suffix) | `377o` | Suffix o |
| Binary (C) | `0b1010` | C-style binary |
| Binary (suffix) | `1010b` | Suffix b |

## Character and String Constants

| Format | Example | Description |
|--------|---------|-------------|
| Character | `'A'` | ASCII value |
| Escaped | `'\n'` | Escape sequence |
| String | `"Hello"` | In .db directive |

Supported escapes: `\b` `\e` `\n` `\r` `\t` `\v` `\nnn` (octal) `\\`

## Expressions

Simple expressions with one operation:
- `symbol` - Symbol value
- `number` - Numeric constant
- `$` - Current address
- `symbol+number` - Symbol plus offset
- `symbol-number` - Symbol minus offset

### Byte Extraction: hi() and lo()

The `hi()` and `lo()` operators extract the high or low byte of a 16-bit address:

```
ld a,lo(symbol)      ; load low byte of symbol's address
ld b,hi(symbol)      ; load high byte of symbol's address
```

These generate relocatable byte values that are resolved at link time. Useful for
loading addresses into register pairs without runtime shifts:

```
; Load address of 'buffer' into HL
ld l,lo(buffer)
ld h,hi(buffer)

; Equivalent to (but resolved at link time):
; ld hl,buffer
; ld a,l
; ld h,h  ; (well, you get the idea)
```

Works with any 8-bit immediate context:
```
ld a,lo(addr)        ; ld r,n instructions
ld b,hi(addr)
.db lo(table)        ; data bytes
.db hi(table)
add a,lo(offset)     ; ALU immediate operations
cp hi(limit)
```

## Z80 Instruction Set

### Basic Instructions (No Operands)

```
nop, rlca, rrca, rla, rra, daa, cpl, scf, ccf, halt, exx, di, ei
```

### Extended Instructions (ED Prefix)

```
neg, retn, reti, rrd, rld
ldi, cpi, ini, outi, ldd, cpd, ind, outd
ldir, cpir, inir, otir, lddr, cpdr, indr, otdr
```

### Register Operands

| Type | Registers |
|------|-----------|
| 8-bit | a, b, c, d, e, h, l |
| 16-bit | af, bc, de, hl, sp |
| Index | ix, iy, ixh, ixl, iyh, iyl |
| Indirect | (hl), (bc), (de), (sp), (ix+d), (iy+d), (c) |
| Conditions | nz, z, nc, c, po, pe, p, m |
| Special | i, r |

### Load Instructions

```
ld r,r'          ; 8-bit register to register
ld r,n           ; 8-bit immediate
ld rr,nn         ; 16-bit immediate
ld rr,(nn)       ; 16-bit from memory
ld (nn),rr       ; 16-bit to memory
ld a,(bc)        ; ld a,(de)  ld a,(nn)
ld (bc),a        ; ld (de),a  ld (nn),a
ld a,i           ; ld a,r  ld i,a  ld r,a
ld sp,hl         ; ld sp,ix  ld sp,iy
```

### Arithmetic

```
add a,r          ; adc a,r  sub r  sbc a,r
and r            ; xor r  or r  cp r
add hl,rr        ; adc hl,rr  sbc hl,rr
add ix,rr        ; add iy,rr
inc r            ; dec r
inc rr           ; dec rr
```

### Bit/Shift Operations

```
rlc r   rrc r   rl r   rr r
sla r   sra r   sll r  srl r
bit n,r          ; n = 0-7
res n,r
set n,r
```

### Stack Operations

```
push rr          ; rr = bc, de, hl, af, ix, iy
pop rr
ex af,af'
ex de,hl
ex (sp),hl       ; ex (sp),ix  ex (sp),iy
```

### Jump Instructions

```
jp nn            ; absolute jump
jp cc,nn         ; conditional (cc = nz,z,nc,c,po,pe,p,m)
jp (hl)          ; jp (ix)  jp (iy)
jr offset        ; relative (-128 to +127)
jr cc,offset     ; cc = nz,z,nc,c only
djnz offset      ; decrement B, jump if not zero
```

### Call/Return

```
call nn
call cc,nn
ret
ret cc
rst n            ; n = 0,8,16,24,32,40,48,56
```

### I/O

```
in a,(n)
in r,(c)
out (n),a
out (c),r
```

### Interrupt Mode

```
im 0
im 1
im 2
```

## Jump Relaxation

By default, `jp` instructions to targets in the text segment are automatically
converted to shorter `jr` instructions when within range (-128 to +127 bytes):

- `jp label` -> `jr label`
- `jp nz,label` -> `jr nz,label`
- `jp z,label` -> `jr z,label`
- `jp nc,label` -> `jr nc,label`
- `jp c,label` -> `jr c,label`

Conditions `po`, `pe`, `p`, `m` cannot be relaxed (no `jr` variants).

Use `-8` option to disable relaxation (8080 compatibility mode).

## Undocumented Instructions

Index half-register access is supported:
```
ld a,ixh         ; load from IX high byte
ld ixl,b         ; load to IX low byte
add a,iyh        ; arithmetic with IY high byte
inc ixl          ; increment IX low byte
```

These work with most 8-bit operations but cannot mix index registers
(e.g., `ld ixh,iyl` is invalid).
