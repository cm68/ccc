/*
 * hitechobj.h - HiTech Z80 object file format definitions
 *
 * See HITECHOBJ.md for full format documentation.
 *
 * vim: tabstop=4 shiftwidth=4 noexpandtab:
 */
#ifndef HITECHOBJ_H
#define HITECHOBJ_H

/*
 * Record types
 */
#define HT_TEXT     1       /* code/data bytes */
#define HT_PSECT    2       /* program section definition */
#define HT_RELOC    3       /* relocation information */
#define HT_SYMBOL   4       /* symbol table entries */
#define HT_START    5       /* execution start address */
#define HT_END      6       /* end of object file */
#define HT_IDENT    7       /* file identification header */

/*
 * PSECT flags
 */
#define HT_F_GLOBAL 0x010   /* global (exported) */
#define HT_F_PURE   0x020   /* pure/read-only */
#define HT_F_OVRLD  0x040   /* overlay */
#define HT_F_ABS    0x080   /* absolute */
#define HT_F_PSECT  0x100   /* psect marker */
#define HT_F_BPAGE  0x200   /* big page */
#define HT_F_LOCAL  0x800   /* local scope */

/*
 * Relocation types (high nibble)
 */
#define HT_RPSECT   0x10    /* psect-relative relocation */
#define HT_RNAME    0x20    /* external symbol relocation */

/*
 * Relocation size (low nibble)
 */
#define HT_RSIZE_MASK 0x0f  /* size mask */
#define HT_RBYTE    0x01    /* 1-byte relocation */
#define HT_RWORD    0x02    /* 2-byte relocation */

/*
 * IDENT record structure (fixed 13 bytes total)
 *
 * Offset  Size  Description
 * 0-1     2     Length = 10 (little-endian)
 * 2       1     Type = 7 (HT_IDENT)
 * 3-6     4     32-bit byte order marker (0x00010203)
 * 7-8     2     16-bit byte order marker (0x0100)
 * 9-12    4     Machine type "Z80\0"
 */
#define HT_IDENT_LEN    10
#define HT_ID_MAGIC     0x00010203
#define HT_ID_MAG16     0x0100

/*
 * Record header structure
 *
 * All records have a 3-byte header:
 *   Offset 0-1: Length (little-endian), excluding header
 *   Offset 2:   Record type
 */
#define HT_HDR_SIZE     3

/*
 * Check if file is HiTech object format
 * First record must be IDENT with length=10, type=7
 */
#define HT_IS_HITECH(buf) \
    ((buf)[0] == 0x0a && (buf)[1] == 0x00 && (buf)[2] == HT_IDENT)

/*
 * HiTech Library Format (.LIB)
 *
 * Header (4 bytes):
 *   Offset 0-1: size_symbols (total size of symbol directory)
 *   Offset 2-3: num_modules (number of modules)
 *
 * Symbol Directory (repeated for each module):
 *   Offset 0-1:  symSize (size of symbol info for this module)
 *   Offset 2-3:  symCnt (number of symbols in this module)
 *   Offset 4-7:  moduleSize (size of module object data)
 *   Offset 8-11: unused (reserved)
 *   Offset 12+:  moduleName (null-terminated string)
 *   Following:   symbol entries (symCnt entries)
 *     Each symbol: 1-byte flags + null-terminated name
 *
 * Module Data Section:
 *   Sequential object file data (moduleSize bytes per module)
 *
 * Symbol flags:
 *   0 = defined (public)
 *   2 = common
 *   6 = undefined (extern)
 */
#define HT_LIBHDRSZ     4
#define HT_MODHDRSZ     12

/* Symbol types in library */
#define HT_SYM_DEF      0   /* D - defined/public */
#define HT_SYM_COMMON   2   /* C - common */
#define HT_SYM_UNDEF    6   /* U - undefined/extern */

#endif /* HITECHOBJ_H */
