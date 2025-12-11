# Whitesmith's Object File Format

## Object File Structure

```
+------------------+
| Header (16 bytes)|
+------------------+
| Text segment     |
+------------------+
| Data segment     |
+------------------+
| Symbol table     |
+------------------+
| Text relocations |
+------------------+
| Data relocations |
+------------------+
```

## Header (16 bytes)

| Offset | Size | Field | Description |
|--------|------|-------|-------------|
| 0 | 1 | magic | Magic number (0x99) |
| 1 | 1 | config | Configuration byte |
| 2 | 2 | symtab | Symbol table size (bytes) |
| 4 | 2 | text | Text segment size |
| 6 | 2 | data | Data segment size |
| 8 | 2 | bss | BSS segment size |
| 10 | 2 | heap | Heap size (text + data in memory) |
| 12 | 2 | textoff | Text start offset (usually 0) |
| 14 | 2 | dataoff | Data start offset (usually text_size) |

All multi-byte values are little-endian.

### Configuration Byte

| Bits | Mask | Description |
|------|------|-------------|
| 0-2 | 0x07 | Symbol length: (config & 7) * 2 + 1 |
| 3 | 0x08 | 32-bit integers |
| 4 | 0x10 | Little-endian |
| 5-6 | 0x60 | Alignment |
| 7 | 0x80 | No relocations |

Common configurations:
- `0x14` - 9-character symbols, little-endian
- `0x17` - 15-character symbols, little-endian

## Segments

| ID | Name | Description |
|----|------|-------------|
| 0 | UNDEF | Undefined (extern reference) |
| 1 | TEXT | Code segment |
| 2 | DATA | Initialized data |
| 3 | BSS | Uninitialized data |
| 4 | ABS | Absolute (not relocatable) |
| 5 | EXT | External reference |

## Symbol Table Entry

Each entry is `symlen + 3` bytes:

| Offset | Size | Field | Description |
|--------|------|-------|-------------|
| 0 | 2 | value | Segment-relative offset |
| 2 | 1 | type | Segment (bits 0-2) + global flag (bit 3) |
| 3 | N | name | Symbol name (N = symlen from config) |

Type byte encoding:
- Bits 0-2: Segment (4=abs, 5=text, 6=data, 7=bss)
- Bit 3: Global flag (0x08)

## Relocation Records

Each relocation section is terminated by a 0x00 byte.

### Bump Encoding (Distance to Next Relocation)

| Byte Value | Distance |
|------------|----------|
| 0x01-0x1f | 1-31 bytes |
| 0x20-0x3f | Extended: ((b-32)<<8) + next_byte + 32 |

Maximum single bump: 8223 bytes.

### Control Bytes (Relocation Type)

| Byte | Type |
|------|------|
| 0x40 | Absolute (SEG_ABS) |
| 0x44 | Text segment (SEG_TEXT) |
| 0x48 | Data segment (SEG_DATA) |
| 0x4c | BSS segment (SEG_BSS) |
| 0x50-0xfb | Symbol index = (b - 0x50) >> 2 |
| 0xfc | Extended symbol (see below) |

### Extended Symbol Index

When control byte is 0xfc:
| Next Byte | Symbol Index |
|-----------|--------------|
| 0x00-0x7f | index = b + 43 |
| 0x80-0xff | index = ((b-0x80)<<8) + next_byte + 171 |

## Archive (Library) Format

```
+------------------+
| Magic (2 bytes)  |
+------------------+
| Entry 1          |
+------------------+
| Entry 2          |
+------------------+
| ...              |
+------------------+
| End marker       |
+------------------+
```

### Archive Header

| Offset | Size | Description |
|--------|------|-------------|
| 0 | 2 | Magic (0xFF75, little-endian) |

### Archive Entry

| Offset | Size | Description |
|--------|------|-------------|
| 0 | 14 | Member name (null-padded) |
| 14 | 2 | Member length (bytes) |
| 16 | N | Object file data |

### End Marker

Entry with null (all zeros) name.
