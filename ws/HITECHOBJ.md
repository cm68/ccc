# HiTech Z80 Object File Format

This document describes the object file format produced by the ZAS (HiTech Z80 Assembler).

## Overview

The object file consists of a sequence of variable-length records. Each record has a 3-byte header followed by record-specific data.

### Record Header Structure

| Offset | Size | Description |
|--------|------|-------------|
| 0-1 | 2 | Record length (little-endian), excluding the 3-byte header |
| 2 | 1 | Record type |
| 3+ | varies | Record data |

### Record Types

| Type | Name | Description |
|------|------|-------------|
| 1 | TEXT | Code/data bytes |
| 2 | PSECT | Program section definition |
| 3 | RELOC | Relocation information |
| 4 | SYMBOL | Symbol table entries |
| 5 | START | Execution start address |
| 6 | END | End of object file |
| 7 | IDENT | File identification header |

---

## Record Type 7: IDENT (File Header)

The IDENT record must appear first in the object file. It identifies the file format and target architecture.

**Fixed size: 13 bytes total**

| Offset | Size | Value | Description |
|--------|------|-------|-------------|
| 0-1 | 2 | 0x0A00 | Length = 10 (little-endian) |
| 2 | 1 | 0x07 | Record type = IDENT |
| 3-6 | 4 | 0x00010203 | 32-bit byte order marker (little-endian) |
| 7-8 | 2 | 0x0100 | 16-bit byte order marker (little-endian) |
| 9-12 | 4 | "Z80\0" | Machine type (null-terminated) |

The byte order markers allow linkers to detect endianness mismatches.

---

## Record Type 1: TEXT (Code/Data)

TEXT records contain assembled code or data bytes for a program section.

| Offset | Size | Description |
|--------|------|-------------|
| 0-1 | 2 | Record length (little-endian) |
| 2 | 1 | 0x01 = TEXT |
| 3-6 | 4 | Starting offset within psect (uint32, little-endian) |
| 7+ | varies | Psect name (null-terminated) |
| varies | 0-509 | Code/data bytes |

- Maximum payload: 509 bytes of code/data per record
- Multiple TEXT records may exist for the same psect at different offsets
- The offset specifies where this data should be placed within the psect

---

## Record Type 2: PSECT (Program Section)

PSECT records define program sections and their attributes.

| Offset | Size | Description |
|--------|------|-------------|
| 0-1 | 2 | Record length (little-endian) |
| 2 | 1 | 0x02 = PSECT |
| 3-4 | 2 | Flags (uint16, little-endian) |
| 5+ | varies | Psect name (null-terminated) |

### PSECT Flags

| Flag | Value | Description |
|------|-------|-------------|
| F_GLOBAL | 0x010 | Global (exported) |
| F_PURE | 0x020 | Pure/read-only code |
| F_OVRLD | 0x040 | Overlay |
| F_ABS | 0x080 | Absolute address |
| F_PSECT | 0x100 | Psect marker |
| F_BPAGE | 0x200 | Big page |
| F_LOCAL | 0x800 | Local scope |

Common combinations:
- `0xD0` (208): Absolute psect with global and pure flags

---

## Record Type 3: RELOC (Relocation)

RELOC records specify locations in TEXT records that need to be adjusted by the linker.

| Offset | Size | Description |
|--------|------|-------------|
| 0-1 | 2 | Record length (little-endian) |
| 2 | 1 | 0x03 = RELOC |
| 3+ | varies | One or more relocation entries |

### Relocation Entry Format

| Offset | Size | Description |
|--------|------|-------------|
| 0-1 | 2 | Offset within TEXT data (uint16, little-endian) |
| 2 | 1 | Relocation type |
| 3+ | varies | Symbol or psect name (null-terminated) |

### Relocation Types

The relocation type byte encodes both the reference type and size:

| Value | Meaning |
|-------|---------|
| 0x11 | 1-byte psect-relative relocation |
| 0x12 | 2-byte psect-relative relocation |
| 0x21 | 1-byte external symbol relocation |
| 0x22 | 2-byte external symbol relocation |

High nibble:
- 0x10 = RPSECT (reference to program section)
- 0x20 = RNAME (reference to external symbol)

Low nibble:
- 0x01 = 1-byte relocation
- 0x02 = 2-byte (word) relocation

---

## Record Type 4: SYMBOL (Symbol Table)

SYMBOL records contain symbol definitions.

| Offset | Size | Description |
|--------|------|-------------|
| 0-1 | 2 | Record length (little-endian) |
| 2 | 1 | 0x04 = SYMBOL |
| 3+ | varies | One or more symbol entries |

### Symbol Entry Format

| Offset | Size | Description |
|--------|------|-------------|
| 0-3 | 4 | Symbol value (uint32, little-endian) |
| 4-5 | 2 | Flags (uint16, little-endian) |
| 6+ | varies | Psect name (null-terminated, or 0x00 if absolute) |
| varies | varies | Symbol name (null-terminated) |

### Symbol Flags

Flags are masked to 0x1F for output:

| Flag | Value | Description |
|------|-------|-------------|
| F_GLOBAL | 0x010 | Global (exported) symbol |
| F_PURE | 0x020 | Pure attribute |
| F_OVRLD | 0x040 | Overlay attribute |
| F_ABS | 0x080 | Absolute symbol |

Special encoding:
- Value 0x16 (22): RRNAME - Relocated external reference

---

## Record Type 5: START (Entry Point)

The START record specifies the program's execution entry point. This record is optional.

| Offset | Size | Description |
|--------|------|-------------|
| 0-1 | 2 | Record length (little-endian) |
| 2 | 1 | 0x05 = START |
| 3-6 | 4 | Start address (uint32, little-endian) |
| 7+ | varies | Psect name containing start address (null-terminated) |

---

## Record Type 6: END (End of File)

The END record marks the end of the object file. It must be the last record.

| Offset | Size | Value | Description |
|--------|------|-------|-------------|
| 0-1 | 2 | 0x0200 | Length = 2 (little-endian) |
| 2 | 1 | 0x06 | Record type = END |
| 3-4 | 2 | 0x0000 | End marker (always zero) |

---

## Record Ordering

Records appear in the following order:

1. IDENT (type 7) - exactly one, first in file
2. TEXT (type 1) - zero or more, code/data for each psect
3. RELOC (type 3) - zero or more, relocation entries
4. PSECT (type 2) - one per program section
5. SYMBOL (type 4) - symbol table entries
6. START (type 5) - optional entry point
7. END (type 6) - exactly one, last in file

---

## Example Hex Dump

```
0000: 0a 00 07 00 01 02 03 00 01 5a 38 30 00  ; IDENT record
                                              ; len=10, type=7, "Z80"

000d: 0b 00 01 00 00 00 00 74 65 78 74 00 c3  ; TEXT record
                                              ; len=11, type=1, offset=0
                                              ; psect="text", data=0xC3

0018: 07 00 03 01 00 12 74 65 78 74 00        ; RELOC record
                                              ; len=7, type=3
                                              ; offset=1, type=0x12 (2-byte psect)
                                              ; name="text"

0023: 06 00 02 d0 00 74 65 78 74 00           ; PSECT record
                                              ; len=6, type=2, flags=0xD0
                                              ; name="text"

002d: 02 00 06 00 00                          ; END record
                                              ; len=2, type=6, marker=0
```

---

## Notes

- All multi-byte integers are little-endian
- All strings are null-terminated
- TEXT records are limited to 509 bytes of payload (512 byte buffer minus header overhead)
- The `-x` assembler option excludes local symbols from the object file
- Macro argument symbols (F_ARGS = 0x1000) are never written to object files
- Files may be padded with 0x1A bytes (CP/M EOF marker) to reach a 128-byte sector boundary
