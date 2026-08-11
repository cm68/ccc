# ASZ - Z80 Assembler Reference

## Syntax

```
[label:] [instruction|directive] [operands] [; comment]
```

- Labels end with `:`. Using `::` exports the symbol globally.
- Comments start with `;` and extend to end of line.
- A name starts with a letter, `_` or `@`, and continues with those,
  digits or `.`.

`@` is there for the compiler.  C keeps labels in a namespace of their
own, so `out:` is a good label and says nothing about any `out`
elsewhere - but this assembler has no such namespace, and a line
beginning `out` is the instruction.  Every mnemonic was exposed that
way, so pass2 writes each label the programmer wrote with an `@` in
front of it.  It cannot use `_`: that already marks every C global, so
a label `out` and a function `out` would meet at `_out`.

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
| `.dl val,...` | `.defl` | Define longs (32-bit) |
| `.ds count` | | Define space (reserve bytes) |

Examples:
```
.db 0x12, 0x34, 'A', "hello", 0
.dw label, label+2, 0x1234
.dl 0x12345678
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

## Local Labels

Local labels provide a way to define temporary labels that can be reused within
a file. They're useful for short jumps where inventing unique names is tedious.

### Syntax

- `N:` - Define local label N (where N is any number)
- `Nf` - Reference next (forward) definition of N
- `Nb` - Reference previous (backward) definition of N

```
    jr nz,1f        ; jump forward to next 1:
    xor a
1:                  ; first definition of local 1
    dec b
    jr nz,1b        ; jump backward to the 1: above
    jr 2f           ; jump forward to next 2:
1:                  ; second definition of local 1 (shadows first)
    inc c
2:
    ret
```

### Implementation: Synthetic Name Architecture

Local labels are converted to synthetic global symbols during assembly. This
allows unlimited reuse without fixed array limits.

**State per label number (stored in hash table):**
- `pending` - Synthetic symbol awaiting next `N:` definition (for `Nf` refs)
- `last` - Symbol from most recent `N:` definition (for `Nb` refs)
- `local_seq` - Global counter for unique synthetic names

**Algorithm:**

When `Nf` is encountered:
1. If `pending[N]` is null, create new synthetic symbol `__L<N>_<seq>`, four digits of sequence
2. Return `pending[N]` (may be shared by multiple forward refs)

When `N:` is encountered:
1. If `pending[N]` exists, define it with current address, clear it
2. Create symbol for this definition, set `last[N]` to it

When `Nb` is encountered:
1. Return `last[N]` (error if null)

**Example transformation:**

```
Source:                     Synthetic:
------                      ----------
    jr 1f                   jr __L1_0001     ; create pending[1]=__L1_0001
    jr 1f                   jr __L1_0001     ; reuse pending[1]
1:                          __L1_0001:       ; define pending[1], clear it
    jr 1b                   jr __L1_0001     ; use last[1]
    jr 1f                   jr __L1_0002     ; create pending[1]=__L1_0002
1:                          __L1_0002:       ; define pending[1], clear it
```

**Two-pass consistency:**

State is reset at the start of each pass. Since source order is deterministic,
the same sequence of `Nf` and `N:` generates identical synthetic names in both
passes:

- Pass 0: Creates and defines synthetic symbols in symbol table
- Pass 1: Regenerates same names, looks up pre-defined addresses

Synthetic symbols are internal (not exported) and don't appear in the object
file symbol table.

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
