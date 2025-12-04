/*
 * wsobj.h - Whitesmith's object file format definitions
 *
 * vim: tabstop=4 shiftwidth=4 noexpandtab:
 */
#ifndef WSOBJ_H
#define WSOBJ_H

/*
 * object file magic number (first byte)
 */
#define MAGIC       0x99

/*
 * archive magic number (first 2 bytes, little-endian)
 * 0177565 octal
 */
#define AR_MAGIC    0xFF75

/*
 * configuration byte flags (second byte of object file)
 */
#define CONF_SYMLEN 0x07    /* mask: symbol length = (conf & 7) * 2 + 1 */
#define CONF_INT32  0x08    /* 32-bit integers */
#define CONF_LITTLE 0x10    /* little-endian */
#define CONF_ALIGN  0x60    /* alignment mask */
#define CONF_NORELO 0x80    /* no relocations */

/*
 * common configurations
 */
#define CONF_9      (CONF_LITTLE | (4 & CONF_SYMLEN))   /* 9 char syms */
#define CONF_15     (CONF_LITTLE | (7 & CONF_SYMLEN))   /* 15 char syms */

/*
 * maximum symbol length
 */
#define SYMLEN      15

/*
 * segment types - used in symbol table and relocations
 *
 * In object file symbol table entries (type byte):
 *   bits 0-2: segment (4=abs, 5=text, 6=data, 7=bss)
 *   bit 3: global flag (0x08)
 *
 * Internal segment numbers (used in assembler):
 */
#define SEG_UNDEF   0       /* undefined (extern reference) */
#define SEG_TEXT    1       /* text segment */
#define SEG_DATA    2       /* data segment */
#define SEG_BSS     3       /* bss segment */
#define SEG_ABS     4       /* absolute (not relocatable) */
#define SEG_EXT     5       /* external reference */

/*
 * Object file header layout (after magic and config bytes):
 *
 *   Offset  Size  Description
 *   0       1     Magic (0x99)
 *   1       1     Config byte
 *   2       2     Symbol table size (bytes)
 *   4       2     Text segment size
 *   6       2     Data segment size
 *   8       2     BSS segment size
 *   10      2     Heap size (text + data in memory)
 *   12      2     Text start offset (usually 0)
 *   14      2     Data start offset (usually text_size)
 *
 * After header:
 *   - Symbol table entries
 *   - Text segment data
 *   - Text relocation records
 *   - Data segment data
 *   - Data relocation records
 *
 * Symbol table entry:
 *   - N bytes: symbol name (N = symlen from config)
 *   - 1 byte: type (segment in bits 0-2, global flag in bit 3)
 *   - 2 bytes: value (segment-relative offset)
 *
 * Relocation record:
 *   - 2 bytes: address within segment to patch
 *   - 2 bytes: symbol index (or segment marker)
 *
 * Archive format:
 *   - 2 bytes: magic (0xFF75)
 *   - Entries: 14-byte name + 2-byte length + object data
 *   - End marker: entry with null name
 */

#endif /* WSOBJ_H */
