/*
 * wsnm - Whitesmith's object file dump utility
 *
 * Displays symbol table, relocations, and hex dump of segments
 * disassembles, too.
 */
#if defined(linux) || defined(__linux__)
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#else
#include <stdio.h>
#endif

#include "wsobj.h"
#include "hitechobj.h"

int fd;
unsigned char *filebuf;
long filesize;
int bflag;      /* -b: hex dump segments */
int dflag;      /* -d: disassemble */
int gflag;      /* -g: generate .s files */
int rflag;      /* -r: show relocations */
int vflag;      /* -v: show header */
FILE *gfile;    /* output file for -g */
char gname[256]; /* output filename for -g */

/* use wsSegNames from wsobj.c */

/*
 * Decode Whitesmith symbol type to text
 * type byte: bits 0-2 = segment (4=abs,5=text,6=data,7=bss), bit 3 = global
 */
void
ws_decode_sym(type, seg, scope, stype)
unsigned char type;
char *seg;
char *scope;
char *stype;
{
    int segval = type & 0x07;  /* bits 0-2 for segment encoding */

    /* global flag is bit 3 */
    if (type & 0x08)
        strcpy(scope, "global");
    else
        strcpy(scope, "local");

    /* segment from bits 0-2 */
    switch (segval) {
    case 4: strcpy(seg, "abs"); strcpy(stype, "def"); break;
    case 5: strcpy(seg, "text"); strcpy(stype, "def"); break;
    case 6: strcpy(seg, "data"); strcpy(stype, "def"); break;
    case 7: strcpy(seg, "bss"); strcpy(stype, "def"); break;
    default: strcpy(seg, "-"); strcpy(stype, "undef"); break;  /* extern/undef */
    }
}

/*
 * Decode HiTech symbol flags to text
 * flags: bit 4 = global, bits 0-3 = type (0=def, 2=common, 6=undef)
 */
void
ht_decode_sym(flags, psect, scope, stype, seg)
unsigned short flags;
char *psect;
char *scope;
char *stype;
char *seg;
{
    /* global flag is bit 4 (0x10) */
    if (flags & 0x10)
        strcpy(scope, "global");
    else
        strcpy(scope, "local");

    /* type from low nibble */
    switch (flags & 0x0f) {
    case 0: strcpy(stype, "def"); break;    /* defined */
    case 2: strcpy(stype, "common"); break; /* common block */
    case 6: strcpy(stype, "undef"); break;  /* undefined/external */
    default: sprintf(stype, "?%d", flags & 0x0f); break;
    }

    /* segment from psect name or flags */
    if (psect[0]) {
        strcpy(seg, psect);
    } else if (flags & 0x80) {
        strcpy(seg, "abs");
    } else {
        strcpy(seg, "-");
    }
}

/*
 * Micronix syscall table - argbytes is bytes after rst 08h (including syscall number)
 * Total syscall block size is 1 + argbytes
 */
struct syscall {
    unsigned char argbytes;
    char *name;
} syscalls[] = {
    {3, "indir"},   /* 0 - indirect syscall */
    {1, "exit"},    /* 1 */
    {1, "fork"},    /* 2 */
    {5, "read"},    /* 3 */
    {5, "write"},   /* 4 */
    {5, "open"},    /* 5 */
    {1, "close"},   /* 6 */
    {1, "wait"},    /* 7 */
    {5, "creat"},   /* 8 */
    {5, "link"},    /* 9 */
    {3, "unlink"},  /* 10 */
    {5, "exec"},    /* 11 */
    {3, "chdir"},   /* 12 */
    {1, "time"},    /* 13 */
    {7, "mknod"},   /* 14 */
    {5, "chmod"},   /* 15 */
    {5, "chown"},   /* 16 */
    {3, "sbrk"},    /* 17 */
    {5, "stat"},    /* 18 */
    {5, "seek"},    /* 19 */
    {1, "getpid"},  /* 20 */
    {7, "mount"},   /* 21 */
    {3, "umount"},  /* 22 */
    {1, "setuid"},  /* 23 */
    {1, "getuid"},  /* 24 */
    {1, "stime"},   /* 25 */
    {7, "ptrace"},  /* 26 */
    {1, "alarm"},   /* 27 */
    {3, "fstat"},   /* 28 */
    {1, "pause"},   /* 29 */
    {1, "bad"},     /* 30 */
    {3, "stty"},    /* 31 */
    {3, "gtty"},    /* 32 */
    {5, "access"},  /* 33 */
    {1, "nice"},    /* 34 */
    {1, "sleep"},   /* 35 */
    {1, "sync"},    /* 36 */
    {3, "kill"},    /* 37 */
    {1, "csw"},     /* 38 */
    {1, "ssw"},     /* 39 */
    {1, "bad"},     /* 40 */
    {1, "dup"},     /* 41 */
    {1, "pipe"},    /* 42 */
    {3, "times"},   /* 43 */
    {9, "profil"},  /* 44 */
    {1, "bad"},     /* 45 */
    {1, "bad"},     /* 46 */
    {1, "bad"},     /* 47 */
    {5, "signal"},  /* 48 */
    {3, "lock"},    /* 49 */
    {1, "unlock"},  /* 50 */
    {0, 0}
};
#define NSYS (sizeof(syscalls)/sizeof(syscalls[0]) - 1)

/* Z80 register names */
char *r8[] = { "b", "c", "d", "e", "h", "l", "(hl)", "a" };
char *r16[] = { "bc", "de", "hl", "sp" };
char *r16a[] = { "bc", "de", "hl", "af" };  /* for push/pop */
char *cc[] = { "nz", "z", "nc", "c", "po", "pe", "p", "m" };
char *alu[] = { "add a,", "adc a,", "sub ", "sbc a,", "and ", "xor ", "or ", "cp " };

/* symbol table for disassembly */
struct sym {
    unsigned short value;
    unsigned char type;
    char name[16];
} *symtab;
int nsyms;
int symlen_g;

/* unified relocation table for disassembler */
struct ureloc {
    unsigned long offset;
    unsigned char size;     /* 1=byte, 2=word */
    unsigned char hilo;     /* 0=word, 1=lo, 2=hi (for byte relocs) */
    unsigned char segment;  /* USEG_TEXT or USEG_DATA */
    char target[80];        /* resolved target name with offset */
} *ureltab;
int nurels;
int disasm_pc;      /* current PC during disassembly, for reloc lookup */

/* legacy Whitesmith relocation table */
struct reloc {
    unsigned short offset;
    int symidx;     /* -4=abs, -1=text, -2=data, -3=bss, >=0=symbol index */
    unsigned char hilo;  /* 0=word, 1=lo, 2=hi */
} *reltab;
int nrels;
unsigned short text_off_g, data_off_g, bss_off_g;  /* segment offsets for reloc fixup */

/* HiTech relocation entry for collection */
struct ht_reloc {
    unsigned long offset;
    unsigned char type;
    char target[64];
};

/* HiTech symbol entry for collection */
struct ht_sym {
    unsigned long value;
    unsigned short flags;
    char psect[32];
    char name[64];
};

/*
 * Unified object data structures
 * Both Whitesmiths and HiTech readers populate these for common output
 */

/* segment types */
#define USEG_UNDEF  0
#define USEG_ABS    1
#define USEG_TEXT   2
#define USEG_DATA   3
#define USEG_BSS    4

/* symbol scope */
#define USCOPE_LOCAL  0
#define USCOPE_GLOBAL 1

/* unified symbol entry */
struct usym {
    char name[64];
    unsigned long value;
    int segment;      /* USEG_* */
    int scope;        /* USCOPE_* */
};

/* unified object data */
struct uobj {
    unsigned char *text;
    unsigned long textsize;
    unsigned char *data;
    unsigned long datasize;
    unsigned long bsssize;
    struct usym *syms;
    int nsyms;
    struct ureloc *relocs;
    int nrelocs;
};

/* global unified object for current file */
struct uobj uobj;

/* synthetic local labels - offsets within segments that are referenced */
unsigned short *datarefs = NULL;
int ndatarefs = 0;
unsigned short *textrefs = NULL;
int ntextrefs = 0;
unsigned short *bssrefs = NULL;
int nbssrefs = 0;
unsigned short *relrefs = NULL;  /* relative jump targets */
int nrelrefs = 0;

/* forward declarations */
int find_reloc();
char *reloc_name();
char *reloc_name_byte();

/*
 * find data reference index, returns -1 if not found
 */
int
find_data_ref(offset)
int offset;
{
    int i;
    for (i = 0; i < ndatarefs; i++) {
        if (datarefs[i] == offset)
            return i;
    }
    return -1;
}

/*
 * add a data reference offset, returns the index
 */
int
add_data_ref(offset)
int offset;
{
    int i;
    /* check if already in list */
    for (i = 0; i < ndatarefs; i++) {
        if (datarefs[i] == offset)
            return i;
    }
    /* add to list */
    datarefs = (unsigned short *)realloc(datarefs,
               (ndatarefs + 1) * sizeof(unsigned short));
    datarefs[ndatarefs] = offset;
    return ndatarefs++;
}

/*
 * find text reference index, returns -1 if not found
 */
int
find_text_ref(offset)
int offset;
{
    int i;
    for (i = 0; i < ntextrefs; i++) {
        if (textrefs[i] == offset)
            return i;
    }
    return -1;
}

/*
 * add a text reference offset, returns the index
 */
int
add_text_ref(offset)
int offset;
{
    int i;
    /* check if already in list */
    for (i = 0; i < ntextrefs; i++) {
        if (textrefs[i] == offset)
            return i;
    }
    /* add to list */
    textrefs = (unsigned short *)realloc(textrefs,
               (ntextrefs + 1) * sizeof(unsigned short));
    textrefs[ntextrefs] = offset;
    return ntextrefs++;
}

/*
 * find bss reference index, returns -1 if not found
 */
int
find_bss_ref(offset)
int offset;
{
    int i;
    for (i = 0; i < nbssrefs; i++) {
        if (bssrefs[i] == offset)
            return i;
    }
    return -1;
}

/*
 * add a bss reference offset, returns the index
 */
int
add_bss_ref(offset)
int offset;
{
    int i;
    /* check if already in list */
    for (i = 0; i < nbssrefs; i++) {
        if (bssrefs[i] == offset)
            return i;
    }
    /* add to list */
    bssrefs = (unsigned short *)realloc(bssrefs,
               (nbssrefs + 1) * sizeof(unsigned short));
    bssrefs[nbssrefs] = offset;
    return nbssrefs++;
}

/*
 * find relative jump reference index, returns -1 if not found
 */
int
find_rel_ref(offset)
int offset;
{
    int i;
    for (i = 0; i < nrelrefs; i++) {
        if (relrefs[i] == offset)
            return i;
    }
    return -1;
}

/*
 * add a relative jump reference offset, returns the index
 */
int
add_rel_ref(offset)
int offset;
{
    int i;
    /* check if already in list */
    for (i = 0; i < nrelrefs; i++) {
        if (relrefs[i] == offset)
            return i;
    }
    /* add to list */
    relrefs = (unsigned short *)realloc(relrefs,
               (nrelrefs + 1) * sizeof(unsigned short));
    relrefs[nrelrefs] = offset;
    return nrelrefs++;
}

/*
 * free synthetic references
 */
void
free_synth_refs()
{
    if (datarefs) {
        free(datarefs);
        datarefs = NULL;
    }
    ndatarefs = 0;
    if (textrefs) {
        free(textrefs);
        textrefs = NULL;
    }
    ntextrefs = 0;
    if (bssrefs) {
        free(bssrefs);
        bssrefs = NULL;
    }
    nbssrefs = 0;
    if (relrefs) {
        free(relrefs);
        relrefs = NULL;
    }
    nrelrefs = 0;
}

/*
 * initialize unified object
 */
void
uobj_init()
{
    memset(&uobj, 0, sizeof(uobj));
}

/*
 * free unified object
 */
void
uobj_free()
{
    if (uobj.text) free(uobj.text);
    if (uobj.data) free(uobj.data);
    if (uobj.syms) free(uobj.syms);
    if (uobj.relocs) free(uobj.relocs);
    memset(&uobj, 0, sizeof(uobj));
}

/*
 * lookup symbol by value and segment in unified object
 */
char *
usym_lookup(val, seg)
unsigned long val;
int seg;
{
    int i;
    for (i = 0; i < uobj.nsyms; i++) {
        if (uobj.syms[i].segment == seg && uobj.syms[i].value == val)
            return uobj.syms[i].name;
    }
    return NULL;
}

/*
 * find relocation in unified table
 */
int
find_ureloc(offset)
unsigned long offset;
{
    int i;
    for (i = 0; i < nurels; i++) {
        if (ureltab[i].offset == offset)
            return i;
    }
    return -1;
}

/*
 * find relocation in uobj relocation table for given segment
 */
int
find_obj_reloc(offset, segment)
unsigned long offset;
int segment;
{
    int i;
    for (i = 0; i < uobj.nrelocs; i++) {
        if (uobj.relocs[i].offset == offset && uobj.relocs[i].segment == segment)
            return i;
    }
    return -1;
}

/*
 * format address with symbol if available
 * uses relocation table when available for accurate symbol resolution
 * falls back to symbol table lookup for non-relocatable files
 */
void
fmt_addr(buf, val)
char *buf;
unsigned short val;
{
    int ri;

    /* check unified relocation table first */
    if (disasm_pc >= 0 && ureltab) {
        ri = find_ureloc(disasm_pc);
        if (ri >= 0 && ureltab[ri].size == 2) {
            strcpy(buf, ureltab[ri].target);
            return;
        }
    }

    /* check legacy Whitesmith relocation table */
    if (disasm_pc >= 0 && reltab) {
        ri = find_reloc(disasm_pc);
        if (ri >= 0) {
            reloc_name(reltab[ri].symidx, val, buf);
            return;
        }
    }

    /* no relocation - use literal value */
    /* hex numbers starting with a-f need leading 0 for assembler */
    if ((val >> 12) >= 10)
        sprintf(buf, "0%04xh", val);
    else
        sprintf(buf, "%04xh", val);
}

/*
 * format a byte operand, checking for hi/lo relocations
 */
void
fmt_byte(buf, val)
char *buf;
unsigned char val;
{
    int ri;
    char symbuf[64];

    /* check unified relocation table first */
    if (disasm_pc >= 0 && ureltab) {
        ri = find_ureloc(disasm_pc);
        if (ri >= 0 && ureltab[ri].size == 1) {
            if (ureltab[ri].hilo == 1)
                sprintf(buf, "low(%s)", ureltab[ri].target);
            else if (ureltab[ri].hilo == 2)
                sprintf(buf, "high(%s)", ureltab[ri].target);
            else
                strcpy(buf, ureltab[ri].target);
            return;
        }
    }

    /* check legacy Whitesmith relocation table */
    if (disasm_pc >= 0 && reltab) {
        ri = find_reloc(disasm_pc);
        if (ri >= 0 && reltab[ri].hilo != 0) {
            reloc_name_byte(reltab[ri].symidx, val, reltab[ri].hilo, symbuf);
            sprintf(buf, "%s(%s)", reltab[ri].hilo == 1 ? "lo" : "hi", symbuf);
            return;
        }
    }

    /* no byte relocation - use literal value */
    if (val >= 0xa0)
        sprintf(buf, "0%02xh", val);
    else
        sprintf(buf, "%02xh", val);
}

/*
 * format relative jump target address
 * when gflag is set, creates synthetic R0, R1... labels
 */
void
fmt_rel_addr(buf, target)
char *buf;
unsigned short target;
{
    if (gflag) {
        sprintf(buf, "R%d", add_rel_ref(target));
    } else {
        if ((target >> 12) >= 10)
            sprintf(buf, "0%04xh", target);
        else
            sprintf(buf, "%04xh", target);
    }
}

/*
 * disassemble one instruction, return length
 * for -g mode, operand_pc is the PC for relocation lookup
 */
int
disasm(addr, pc, buf)
long addr;
int pc;
char *buf;
{
    unsigned char op, op2, d, n;
    unsigned short nn;
    int len = 1;
    char abuf[32];
    char rel;
    int r, b;
    static char *rot[] = { "rlc", "rrc", "rl", "rr", "sla", "sra", "sll", "srl" };

    op = filebuf[addr];

    /* CB prefix - bit operations */
    if (op == 0xcb) {
        op2 = filebuf[addr + 1];
        len = 2;
        r = op2 & 7;
        b = (op2 >> 3) & 7;
        if (op2 < 0x40) {
            sprintf(buf, "%s %s", rot[b], r8[r]);
        } else if (op2 < 0x80) {
            sprintf(buf, "bit %d,%s", b, r8[r]);
        } else if (op2 < 0xc0) {
            sprintf(buf, "res %d,%s", b, r8[r]);
        } else {
            sprintf(buf, "set %d,%s", b, r8[r]);
        }
        return len;
    }

    /* ED prefix - extended */
    if (op == 0xed) {
        op2 = filebuf[addr + 1];
        len = 2;
        switch (op2) {
        case 0x40: sprintf(buf, "in b,(c)"); break;
        case 0x41: sprintf(buf, "out (c),b"); break;
        case 0x42: sprintf(buf, "sbc hl,bc"); break;
        case 0x43: nn = filebuf[addr+2] | (filebuf[addr+3]<<8); len=4;
                   disasm_pc = pc + 2;
                   fmt_addr(abuf, nn); sprintf(buf, "ld (%s),bc", abuf); break;
        case 0x44: sprintf(buf, "neg"); break;
        case 0x45: sprintf(buf, "retn"); break;
        case 0x46: sprintf(buf, "im 0"); break;
        case 0x47: sprintf(buf, "ld i,a"); break;
        case 0x48: sprintf(buf, "in c,(c)"); break;
        case 0x49: sprintf(buf, "out (c),c"); break;
        case 0x4a: sprintf(buf, "adc hl,bc"); break;
        case 0x4b: nn = filebuf[addr+2] | (filebuf[addr+3]<<8); len=4;
                   disasm_pc = pc + 2;
                   fmt_addr(abuf, nn); sprintf(buf, "ld bc,(%s)", abuf); break;
        case 0x4d: sprintf(buf, "reti"); break;
        case 0x4f: sprintf(buf, "ld r,a"); break;
        case 0x50: sprintf(buf, "in d,(c)"); break;
        case 0x51: sprintf(buf, "out (c),d"); break;
        case 0x52: sprintf(buf, "sbc hl,de"); break;
        case 0x53: nn = filebuf[addr+2] | (filebuf[addr+3]<<8); len=4;
                   disasm_pc = pc + 2;
                   fmt_addr(abuf, nn); sprintf(buf, "ld (%s),de", abuf); break;
        case 0x56: sprintf(buf, "im 1"); break;
        case 0x57: sprintf(buf, "ld a,i"); break;
        case 0x58: sprintf(buf, "in e,(c)"); break;
        case 0x59: sprintf(buf, "out (c),e"); break;
        case 0x5a: sprintf(buf, "adc hl,de"); break;
        case 0x5b: nn = filebuf[addr+2] | (filebuf[addr+3]<<8); len=4;
                   disasm_pc = pc + 2;
                   fmt_addr(abuf, nn); sprintf(buf, "ld de,(%s)", abuf); break;
        case 0x5e: sprintf(buf, "im 2"); break;
        case 0x5f: sprintf(buf, "ld a,r"); break;
        case 0x60: sprintf(buf, "in h,(c)"); break;
        case 0x61: sprintf(buf, "out (c),h"); break;
        case 0x62: sprintf(buf, "sbc hl,hl"); break;
        case 0x67: sprintf(buf, "rrd"); break;
        case 0x68: sprintf(buf, "in l,(c)"); break;
        case 0x69: sprintf(buf, "out (c),l"); break;
        case 0x6a: sprintf(buf, "adc hl,hl"); break;
        case 0x6f: sprintf(buf, "rld"); break;
        case 0x72: sprintf(buf, "sbc hl,sp"); break;
        case 0x73: nn = filebuf[addr+2] | (filebuf[addr+3]<<8); len=4;
                   disasm_pc = pc + 2;
                   fmt_addr(abuf, nn); sprintf(buf, "ld (%s),sp", abuf); break;
        case 0x78: sprintf(buf, "in a,(c)"); break;
        case 0x79: sprintf(buf, "out (c),a"); break;
        case 0x7a: sprintf(buf, "adc hl,sp"); break;
        case 0x7b: nn = filebuf[addr+2] | (filebuf[addr+3]<<8); len=4;
                   disasm_pc = pc + 2;
                   fmt_addr(abuf, nn); sprintf(buf, "ld sp,(%s)", abuf); break;
        case 0xa0: sprintf(buf, "ldi"); break;
        case 0xa1: sprintf(buf, "cpi"); break;
        case 0xa2: sprintf(buf, "ini"); break;
        case 0xa3: sprintf(buf, "outi"); break;
        case 0xa8: sprintf(buf, "ldd"); break;
        case 0xa9: sprintf(buf, "cpd"); break;
        case 0xaa: sprintf(buf, "ind"); break;
        case 0xab: sprintf(buf, "outd"); break;
        case 0xb0: sprintf(buf, "ldir"); break;
        case 0xb1: sprintf(buf, "cpir"); break;
        case 0xb2: sprintf(buf, "inir"); break;
        case 0xb3: sprintf(buf, "otir"); break;
        case 0xb8: sprintf(buf, "lddr"); break;
        case 0xb9: sprintf(buf, "cpdr"); break;
        case 0xba: sprintf(buf, "indr"); break;
        case 0xbb: sprintf(buf, "otdr"); break;
        default: sprintf(buf, "db 0edh,0%02xh", op2); break;
        }
        return len;
    }

    /* DD/FD prefix - IX/IY */
    if (op == 0xdd || op == 0xfd) {
        char *ir = (op == 0xdd) ? "ix" : "iy";
        op2 = filebuf[addr + 1];
        len = 2;

        if (op2 == 0x21) {
            nn = filebuf[addr+2] | (filebuf[addr+3]<<8); len=4;
            disasm_pc = pc + 2;
            fmt_addr(abuf, nn); sprintf(buf, "ld %s,%s", ir, abuf);
        } else if (op2 == 0x22) {
            nn = filebuf[addr+2] | (filebuf[addr+3]<<8); len=4;
            disasm_pc = pc + 2;
            fmt_addr(abuf, nn); sprintf(buf, "ld (%s),%s", abuf, ir);
        } else if (op2 == 0x23) {
            sprintf(buf, "inc %s", ir);
        } else if (op2 == 0x2a) {
            nn = filebuf[addr+2] | (filebuf[addr+3]<<8); len=4;
            disasm_pc = pc + 2;
            fmt_addr(abuf, nn); sprintf(buf, "ld %s,(%s)", ir, abuf);
        } else if (op2 == 0x2b) {
            sprintf(buf, "dec %s", ir);
        } else if (op2 == 0x34) {
            d = filebuf[addr+2]; len=3;
            sprintf(buf, "inc (%s%+d)", ir, (char)d);
        } else if (op2 == 0x35) {
            d = filebuf[addr+2]; len=3;
            sprintf(buf, "dec (%s%+d)", ir, (char)d);
        } else if (op2 == 0x36) {
            d = filebuf[addr+2]; n = filebuf[addr+3]; len=4;
            sprintf(buf, "ld (%s%+d),0%02xh", ir, (char)d, n);
        } else if (op2 == 0xe1) {
            sprintf(buf, "pop %s", ir);
        } else if (op2 == 0xe3) {
            sprintf(buf, "ex (sp),%s", ir);
        } else if (op2 == 0xe5) {
            sprintf(buf, "push %s", ir);
        } else if (op2 == 0xe9) {
            sprintf(buf, "jp (%s)", ir);
        } else if (op2 == 0xf9) {
            sprintf(buf, "ld sp,%s", ir);
        } else if ((op2 & 0xc0) == 0x40 && (op2 & 7) == 6) {
            /* ld r,(ix+d) */
            d = filebuf[addr+2]; len=3;
            sprintf(buf, "ld %s,(%s%+d)", r8[(op2>>3)&7], ir, (char)d);
        } else if ((op2 & 0xc0) == 0x40 && ((op2>>3) & 7) == 6) {
            /* ld (ix+d),r */
            d = filebuf[addr+2]; len=3;
            sprintf(buf, "ld (%s%+d),%s", ir, (char)d, r8[op2&7]);
        } else if ((op2 & 0xc0) == 0x80 && (op2 & 7) == 6) {
            /* alu (ix+d) */
            d = filebuf[addr+2]; len=3;
            sprintf(buf, "%s(%s%+d)", alu[(op2>>3)&7], ir, (char)d);
        } else if (op2 == 0xcb) {
            /* DD/FD CB d op */
            int b;
            static char *rot[] = { "rlc", "rrc", "rl", "rr", "sla", "sra", "sll", "srl" };
            d = filebuf[addr+2];
            op2 = filebuf[addr+3];
            len = 4;
            b = (op2 >> 3) & 7;
            if (op2 < 0x40) {
                sprintf(buf, "%s (%s%+d)", rot[b], ir, (char)d);
            } else if (op2 < 0x80) {
                sprintf(buf, "bit %d,(%s%+d)", b, ir, (char)d);
            } else if (op2 < 0xc0) {
                sprintf(buf, "res %d,(%s%+d)", b, ir, (char)d);
            } else {
                sprintf(buf, "set %d,(%s%+d)", b, ir, (char)d);
            }
        } else {
            sprintf(buf, "db 0%02xh,0%02xh", op, op2);
        }
        return len;
    }

    /* main opcodes */
    switch (op) {
    case 0x00: sprintf(buf, "nop"); break;
    case 0x01: nn = filebuf[addr+1] | (filebuf[addr+2]<<8); len=3;
               disasm_pc = pc + 1;
               fmt_addr(abuf, nn); sprintf(buf, "ld bc,%s", abuf); break;
    case 0x02: sprintf(buf, "ld (bc),a"); break;
    case 0x03: sprintf(buf, "inc bc"); break;
    case 0x04: sprintf(buf, "inc b"); break;
    case 0x05: sprintf(buf, "dec b"); break;
    case 0x06: disasm_pc = pc + 1;
               fmt_byte(abuf, filebuf[addr+1]); sprintf(buf, "ld b,%s", abuf); len=2; break;
    case 0x07: sprintf(buf, "rlca"); break;
    case 0x08: sprintf(buf, "ex af,af'"); break;
    case 0x09: sprintf(buf, "add hl,bc"); break;
    case 0x0a: sprintf(buf, "ld a,(bc)"); break;
    case 0x0b: sprintf(buf, "dec bc"); break;
    case 0x0c: sprintf(buf, "inc c"); break;
    case 0x0d: sprintf(buf, "dec c"); break;
    case 0x0e: disasm_pc = pc + 1;
               fmt_byte(abuf, filebuf[addr+1]); sprintf(buf, "ld c,%s", abuf); len=2; break;
    case 0x0f: sprintf(buf, "rrca"); break;

    case 0x10: rel = filebuf[addr+1]; len=2;
               nn = pc + 2 + rel;
               fmt_rel_addr(abuf, nn); sprintf(buf, "djnz %s", abuf); break;
    case 0x11: nn = filebuf[addr+1] | (filebuf[addr+2]<<8); len=3;
               disasm_pc = pc + 1;
               fmt_addr(abuf, nn); sprintf(buf, "ld de,%s", abuf); break;
    case 0x12: sprintf(buf, "ld (de),a"); break;
    case 0x13: sprintf(buf, "inc de"); break;
    case 0x14: sprintf(buf, "inc d"); break;
    case 0x15: sprintf(buf, "dec d"); break;
    case 0x16: disasm_pc = pc + 1;
               fmt_byte(abuf, filebuf[addr+1]); sprintf(buf, "ld d,%s", abuf); len=2; break;
    case 0x17: sprintf(buf, "rla"); break;
    case 0x18: rel = filebuf[addr+1]; len=2;
               nn = pc + 2 + rel;
               fmt_rel_addr(abuf, nn); sprintf(buf, "jr %s", abuf); break;
    case 0x19: sprintf(buf, "add hl,de"); break;
    case 0x1a: sprintf(buf, "ld a,(de)"); break;
    case 0x1b: sprintf(buf, "dec de"); break;
    case 0x1c: sprintf(buf, "inc e"); break;
    case 0x1d: sprintf(buf, "dec e"); break;
    case 0x1e: disasm_pc = pc + 1;
               fmt_byte(abuf, filebuf[addr+1]); sprintf(buf, "ld e,%s", abuf); len=2; break;
    case 0x1f: sprintf(buf, "rra"); break;

    case 0x20: rel = filebuf[addr+1]; len=2;
               nn = pc + 2 + rel;
               fmt_rel_addr(abuf, nn); sprintf(buf, "jr nz,%s", abuf); break;
    case 0x21: nn = filebuf[addr+1] | (filebuf[addr+2]<<8); len=3;
               disasm_pc = pc + 1;
               fmt_addr(abuf, nn); sprintf(buf, "ld hl,%s", abuf); break;
    case 0x22: nn = filebuf[addr+1] | (filebuf[addr+2]<<8); len=3;
               disasm_pc = pc + 1;
               fmt_addr(abuf, nn); sprintf(buf, "ld (%s),hl", abuf); break;
    case 0x23: sprintf(buf, "inc hl"); break;
    case 0x24: sprintf(buf, "inc h"); break;
    case 0x25: sprintf(buf, "dec h"); break;
    case 0x26: disasm_pc = pc + 1;
               fmt_byte(abuf, filebuf[addr+1]); sprintf(buf, "ld h,%s", abuf); len=2; break;
    case 0x27: sprintf(buf, "daa"); break;
    case 0x28: rel = filebuf[addr+1]; len=2;
               nn = pc + 2 + rel;
               fmt_rel_addr(abuf, nn); sprintf(buf, "jr z,%s", abuf); break;
    case 0x29: sprintf(buf, "add hl,hl"); break;
    case 0x2a: nn = filebuf[addr+1] | (filebuf[addr+2]<<8); len=3;
               disasm_pc = pc + 1;
               fmt_addr(abuf, nn); sprintf(buf, "ld hl,(%s)", abuf); break;
    case 0x2b: sprintf(buf, "dec hl"); break;
    case 0x2c: sprintf(buf, "inc l"); break;
    case 0x2d: sprintf(buf, "dec l"); break;
    case 0x2e: disasm_pc = pc + 1;
               fmt_byte(abuf, filebuf[addr+1]); sprintf(buf, "ld l,%s", abuf); len=2; break;
    case 0x2f: sprintf(buf, "cpl"); break;

    case 0x30: rel = filebuf[addr+1]; len=2;
               nn = pc + 2 + rel;
               fmt_rel_addr(abuf, nn); sprintf(buf, "jr nc,%s", abuf); break;
    case 0x31: nn = filebuf[addr+1] | (filebuf[addr+2]<<8); len=3;
               disasm_pc = pc + 1;
               fmt_addr(abuf, nn); sprintf(buf, "ld sp,%s", abuf); break;
    case 0x32: nn = filebuf[addr+1] | (filebuf[addr+2]<<8); len=3;
               disasm_pc = pc + 1;
               fmt_addr(abuf, nn); sprintf(buf, "ld (%s),a", abuf); break;
    case 0x33: sprintf(buf, "inc sp"); break;
    case 0x34: sprintf(buf, "inc (hl)"); break;
    case 0x35: sprintf(buf, "dec (hl)"); break;
    case 0x36: disasm_pc = pc + 1;
               fmt_byte(abuf, filebuf[addr+1]); sprintf(buf, "ld (hl),%s", abuf); len=2; break;
    case 0x37: sprintf(buf, "scf"); break;
    case 0x38: rel = filebuf[addr+1]; len=2;
               nn = pc + 2 + rel;
               fmt_rel_addr(abuf, nn); sprintf(buf, "jr c,%s", abuf); break;
    case 0x39: sprintf(buf, "add hl,sp"); break;
    case 0x3a: nn = filebuf[addr+1] | (filebuf[addr+2]<<8); len=3;
               disasm_pc = pc + 1;
               fmt_addr(abuf, nn); sprintf(buf, "ld a,(%s)", abuf); break;
    case 0x3b: sprintf(buf, "dec sp"); break;
    case 0x3c: sprintf(buf, "inc a"); break;
    case 0x3d: sprintf(buf, "dec a"); break;
    case 0x3e: disasm_pc = pc + 1;
               fmt_byte(abuf, filebuf[addr+1]); sprintf(buf, "ld a,%s", abuf); len=2; break;
    case 0x3f: sprintf(buf, "ccf"); break;

    case 0x76: sprintf(buf, "halt"); break;

    case 0xc0: sprintf(buf, "ret nz"); break;
    case 0xc1: sprintf(buf, "pop bc"); break;
    case 0xc2: nn = filebuf[addr+1] | (filebuf[addr+2]<<8); len=3;
               disasm_pc = pc + 1;
               fmt_addr(abuf, nn); sprintf(buf, "jp nz,%s", abuf); break;
    case 0xc3: nn = filebuf[addr+1] | (filebuf[addr+2]<<8); len=3;
               disasm_pc = pc + 1;
               fmt_addr(abuf, nn); sprintf(buf, "jp %s", abuf); break;
    case 0xc4: nn = filebuf[addr+1] | (filebuf[addr+2]<<8); len=3;
               disasm_pc = pc + 1;
               fmt_addr(abuf, nn); sprintf(buf, "call nz,%s", abuf); break;
    case 0xc5: sprintf(buf, "push bc"); break;
    case 0xc6: disasm_pc = pc + 1;
               fmt_byte(abuf, filebuf[addr+1]); sprintf(buf, "add a,%s", abuf); len=2; break;
    case 0xc7: sprintf(buf, "rst 00h"); break;
    case 0xc8: sprintf(buf, "ret z"); break;
    case 0xc9: sprintf(buf, "ret"); break;
    case 0xca: nn = filebuf[addr+1] | (filebuf[addr+2]<<8); len=3;
               disasm_pc = pc + 1;
               fmt_addr(abuf, nn); sprintf(buf, "jp z,%s", abuf); break;
    case 0xcc: nn = filebuf[addr+1] | (filebuf[addr+2]<<8); len=3;
               disasm_pc = pc + 1;
               fmt_addr(abuf, nn); sprintf(buf, "call z,%s", abuf); break;
    case 0xcd: nn = filebuf[addr+1] | (filebuf[addr+2]<<8); len=3;
               disasm_pc = pc + 1;
               fmt_addr(abuf, nn); sprintf(buf, "call %s", abuf); break;
    case 0xce: disasm_pc = pc + 1;
               fmt_byte(abuf, filebuf[addr+1]); sprintf(buf, "adc a,%s", abuf); len=2; break;
    case 0xcf: sprintf(buf, "rst 08h"); break;

    case 0xd0: sprintf(buf, "ret nc"); break;
    case 0xd1: sprintf(buf, "pop de"); break;
    case 0xd2: nn = filebuf[addr+1] | (filebuf[addr+2]<<8); len=3;
               disasm_pc = pc + 1;
               fmt_addr(abuf, nn); sprintf(buf, "jp nc,%s", abuf); break;
    case 0xd3: sprintf(buf, "out (0%02xh),a", filebuf[addr+1]); len=2; break;
    case 0xd4: nn = filebuf[addr+1] | (filebuf[addr+2]<<8); len=3;
               disasm_pc = pc + 1;
               fmt_addr(abuf, nn); sprintf(buf, "call nc,%s", abuf); break;
    case 0xd5: sprintf(buf, "push de"); break;
    case 0xd6: disasm_pc = pc + 1;
               fmt_byte(abuf, filebuf[addr+1]); sprintf(buf, "sub %s", abuf); len=2; break;
    case 0xd7: sprintf(buf, "rst 10h"); break;
    case 0xd8: sprintf(buf, "ret c"); break;
    case 0xd9: sprintf(buf, "exx"); break;
    case 0xda: nn = filebuf[addr+1] | (filebuf[addr+2]<<8); len=3;
               disasm_pc = pc + 1;
               fmt_addr(abuf, nn); sprintf(buf, "jp c,%s", abuf); break;
    case 0xdb: sprintf(buf, "in a,(0%02xh)", filebuf[addr+1]); len=2; break;
    case 0xdc: nn = filebuf[addr+1] | (filebuf[addr+2]<<8); len=3;
               disasm_pc = pc + 1;
               fmt_addr(abuf, nn); sprintf(buf, "call c,%s", abuf); break;
    case 0xde: disasm_pc = pc + 1;
               fmt_byte(abuf, filebuf[addr+1]); sprintf(buf, "sbc a,%s", abuf); len=2; break;
    case 0xdf: sprintf(buf, "rst 18h"); break;

    case 0xe0: sprintf(buf, "ret po"); break;
    case 0xe1: sprintf(buf, "pop hl"); break;
    case 0xe2: nn = filebuf[addr+1] | (filebuf[addr+2]<<8); len=3;
               disasm_pc = pc + 1;
               fmt_addr(abuf, nn); sprintf(buf, "jp po,%s", abuf); break;
    case 0xe3: sprintf(buf, "ex (sp),hl"); break;
    case 0xe4: nn = filebuf[addr+1] | (filebuf[addr+2]<<8); len=3;
               disasm_pc = pc + 1;
               fmt_addr(abuf, nn); sprintf(buf, "call po,%s", abuf); break;
    case 0xe5: sprintf(buf, "push hl"); break;
    case 0xe6: disasm_pc = pc + 1;
               fmt_byte(abuf, filebuf[addr+1]); sprintf(buf, "and %s", abuf); len=2; break;
    case 0xe7: sprintf(buf, "rst 20h"); break;
    case 0xe8: sprintf(buf, "ret pe"); break;
    case 0xe9: sprintf(buf, "jp (hl)"); break;
    case 0xea: nn = filebuf[addr+1] | (filebuf[addr+2]<<8); len=3;
               disasm_pc = pc + 1;
               fmt_addr(abuf, nn); sprintf(buf, "jp pe,%s", abuf); break;
    case 0xeb: sprintf(buf, "ex de,hl"); break;
    case 0xec: nn = filebuf[addr+1] | (filebuf[addr+2]<<8); len=3;
               disasm_pc = pc + 1;
               fmt_addr(abuf, nn); sprintf(buf, "call pe,%s", abuf); break;
    case 0xee: disasm_pc = pc + 1;
               fmt_byte(abuf, filebuf[addr+1]); sprintf(buf, "xor %s", abuf); len=2; break;
    case 0xef: sprintf(buf, "rst 28h"); break;

    case 0xf0: sprintf(buf, "ret p"); break;
    case 0xf1: sprintf(buf, "pop af"); break;
    case 0xf2: nn = filebuf[addr+1] | (filebuf[addr+2]<<8); len=3;
               disasm_pc = pc + 1;
               fmt_addr(abuf, nn); sprintf(buf, "jp p,%s", abuf); break;
    case 0xf3: sprintf(buf, "di"); break;
    case 0xf4: nn = filebuf[addr+1] | (filebuf[addr+2]<<8); len=3;
               disasm_pc = pc + 1;
               fmt_addr(abuf, nn); sprintf(buf, "call p,%s", abuf); break;
    case 0xf5: sprintf(buf, "push af"); break;
    case 0xf6: disasm_pc = pc + 1;
               fmt_byte(abuf, filebuf[addr+1]); sprintf(buf, "or %s", abuf); len=2; break;
    case 0xf7: sprintf(buf, "rst 30h"); break;
    case 0xf8: sprintf(buf, "ret m"); break;
    case 0xf9: sprintf(buf, "ld sp,hl"); break;
    case 0xfa: nn = filebuf[addr+1] | (filebuf[addr+2]<<8); len=3;
               disasm_pc = pc + 1;
               fmt_addr(abuf, nn); sprintf(buf, "jp m,%s", abuf); break;
    case 0xfb: sprintf(buf, "ei"); break;
    case 0xfc: nn = filebuf[addr+1] | (filebuf[addr+2]<<8); len=3;
               disasm_pc = pc + 1;
               fmt_addr(abuf, nn); sprintf(buf, "call m,%s", abuf); break;
    case 0xfe: disasm_pc = pc + 1;
               fmt_byte(abuf, filebuf[addr+1]); sprintf(buf, "cp %s", abuf); len=2; break;
    case 0xff: sprintf(buf, "rst 38h"); break;

    default:
        /* ld r,r' and alu r patterns */
        if (op >= 0x40 && op < 0x80) {
            sprintf(buf, "ld %s,%s", r8[(op>>3)&7], r8[op&7]);
        } else if (op >= 0x80 && op < 0xc0) {
            sprintf(buf, "%s%s", alu[(op>>3)&7], r8[op&7]);
        } else {
            sprintf(buf, "db 0%02xh", op);
        }
        break;
    }

    return len;
}

void
error(msg)
char *msg;
{
    fprintf(stderr, "wsnm: %s\n", msg);
    exit(1);
}

void
error2(msg, arg)
char *msg;
char *arg;
{
    fprintf(stderr, "wsnm: %s: %s\n", msg, arg);
    exit(1);
}

unsigned short
get_word(off)
long off;
{
    return filebuf[off] | (filebuf[off + 1] << 8);
}

unsigned char
get_byte(off)
long off;
{
    return filebuf[off];
}

/*
 * decode segment from type byte
 */
int
decode_seg(type)
unsigned char type;
{
    switch (type & 0x07) {
    case 4: return SEG_ABS;
    case 5: return SEG_TEXT;
    case 6: return SEG_DATA;
    case 7: return SEG_BSS;
    default: return SEG_EXT;  /* extern/undef */
    }
}

/*
 * hex dump a region
 * addr_base is the displayed address offset
 */
void
hexdump(name, start, size, addr_base)
char *name;
long start;
int size;
int addr_base;
{
    int i, j;
    unsigned char c;

    if (size == 0) {
        printf("\n%s: (empty)\n", name);
        return;
    }

    printf("\n%s: %d bytes\n", name, size);

    for (i = 0; i < size; i += 16) {
        printf("  %04x: ", addr_base + i);

        /* hex bytes */
        for (j = 0; j < 16; j++) {
            if (i + j < size)
                printf("%02x ", filebuf[start + i + j]);
            else
                printf("   ");
            if (j == 7)
                printf(" ");
        }

        printf(" |");

        /* ascii */
        for (j = 0; j < 16 && i + j < size; j++) {
            c = filebuf[start + i + j];
            if (c >= 0x20 && c < 0x7f)
                printf("%c", c);
            else
                printf(".");
        }

        printf("|\n");
    }
}

/*
 * dump symbol table
 */
void
dump_symbols(symtab_off, symtab_size, symlen)
long symtab_off;
int symtab_size;
int symlen;
{
    int num_syms, i;
    unsigned short val;
    unsigned char type;
    char name[16];
    char scope[16], stype[16];
    long off;

    num_syms = symtab_size / (symlen + 3);

    printf("\nSymbol table: %d symbols\n", num_syms);
    printf("  Value   Segment  Scope   Type   Name\n");
    printf("  ------  -------  ------  -----  ----\n");

    off = symtab_off;
    for (i = 0; i < num_syms; i++) {
        char seg[16];

        val = get_word(off);
        type = get_byte(off + 2);
        memcpy(name, &filebuf[off + 3], symlen);
        name[symlen] = '\0';

        ws_decode_sym(type, seg, scope, stype);

        printf("  0x%04x  %-7s  %-6s  %-5s  %s\n",
               val, seg, scope, stype, name);

        off += symlen + 3;
    }
}

/*
 * read next relocation record from stream
 * off: file offset pointer (updated)
 * pos: position pointer (updated)
 * symidx: output symbol index (-4=abs, -1=text, -2=data, -3=bss, >=0=symbol)
 * hilo: output hilo (0=word, 1=lo, 2=hi)
 * returns: 1 if relocation found, 0 if end of stream
 */
int
read_reloc(off, pos, symidx, hilo)
long *off;
int *pos;
int *symidx;
int *hilo;
{
    unsigned char b;
    int bump, idx;

    while (1) {
        b = get_byte((*off)++);
        if (b == 0) return 0;  /* end of relocs */

        if (b < REL_BUMP_EXT) {
            *pos += b;
        } else if (b < REL_ABS) {
            bump = ((b - REL_BUMP_EXT) << 8) + get_byte((*off)++) + REL_BUMP_EXT;
            *pos += bump;
        } else {
            /* control byte - decode relocation */
            *hilo = b & 3;

            if ((b & ~3) == REL_ABS) {
                *symidx = -4;
            } else if ((b & ~3) == REL_TEXT) {
                *symidx = -1;
            } else if ((b & ~3) == REL_DATA) {
                *symidx = -2;
            } else if ((b & ~3) == REL_BSS) {
                *symidx = -3;
            } else if (b >= REL_SYM_BASE && b < REL_SYM_EXT) {
                *symidx = (b - REL_SYM_BASE) >> 2;
            } else if ((b & ~3) == REL_SYM_EXT) {
                b = get_byte((*off)++);
                if (b < REL_EXT_LONG) {
                    idx = b + REL_EXT_THR1 - REL_SYM_OFS;
                } else {
                    idx = ((b - REL_EXT_LONG) << 8) + get_byte((*off)++) + REL_EXT_THR2 - REL_SYM_OFS;
                }
                *symidx = idx;
            } else {
                *symidx = -100;  /* unknown */
            }
            return 1;
        }
    }
}

/*
 * dump relocation table
 */
void
dump_relocs(name, reloc_off, symtab_off, symlen, num_syms)
char *name;
long reloc_off;
long symtab_off;
int symlen;
int num_syms;
{
    long off = reloc_off;
    int pos = 0;
    int symidx, hilo;
    int count = 0;
    char *segtab[] = { "text", "data", "bss", "abs" };

    printf("\n%s relocations:\n", name);
    printf("  Offset  Size  Segment  Target\n");
    printf("  ------  ----  -------  ------\n");

    while (read_reloc(&off, &pos, &symidx, &hilo)) {
        char *seg;
        char *size;
        char target[32];
        char symname[20];

        size = hilo == 0 ? "word" : (hilo == 1 ? "lo" : "hi");

        if (symidx >= 0) {
            /* look up symbol name from symbol table */
            if (symidx < num_syms) {
                long soff = symtab_off + symidx * (symlen + 3);
                int i;
                for (i = 0; i < symlen && i < 19; i++) {
                    symname[i] = filebuf[soff + 3 + i];
                    if (symname[i] == '\0') break;
                }
                symname[i] = '\0';
                strcpy(target, symname);
            } else {
                sprintf(target, "sym[%d]", symidx);
            }
            seg = "-";
        } else if (symidx >= -4) {
            seg = segtab[-symidx - 1];
            target[0] = '\0';
        } else {
            seg = "?";
            sprintf(target, "(%d)", symidx);
        }

        if (target[0])
            printf("  0x%04x  %-4s  %-7s  %s\n", pos, size, seg, target);
        else
            printf("  0x%04x  %-4s  %s\n", pos, size, seg);
        pos += hilo ? 1 : 2;
        count++;
    }

    if (count == 0) {
        printf("  (none)\n");
    }
}

/*
 * parse relocation stream into reltab array
 * returns offset after relocations
 */
long
parse_relocs(reloc_off, limit)
long reloc_off;
long limit;
{
    long off = reloc_off;
    int pos = 0;
    int symidx, hilo;

    nrels = 0;
    reltab = 0;

    /* first pass: count relocations */
    while (read_reloc(&off, &pos, &symidx, &hilo))
        nrels++;

    if (nrels == 0)
        return off;

    /* allocate */
    reltab = (struct reloc *)malloc(nrels * sizeof(struct reloc));

    /* second pass: fill in relocations */
    off = reloc_off;
    pos = 0;
    nrels = 0;

    while (read_reloc(&off, &pos, &symidx, &hilo)) {
        reltab[nrels].offset = pos;
        reltab[nrels].symidx = symidx;
        reltab[nrels].hilo = hilo;
        pos += hilo ? 1 : 2;
        nrels++;
    }

    return off;
}

/*
 * check if there's a relocation at given offset
 * returns relocation index or -1
 */
int
find_reloc(offset)
int offset;
{
    int i;
    for (i = 0; i < nrels; i++) {
        if (reltab[i].offset == offset)
            return i;
    }
    return -1;
}

/*
 * get symbol name for relocation target
 * for segment-relative relocs, look up symbol at target offset
 * addend is the raw value from the instruction (absolute offset)
 */
char *
reloc_name(symidx, addend, buf)
int symidx;
int addend;
char *buf;
{
    int i, seg;
    int rel_off;  /* offset relative to segment start */

    /* for segment-relative, find symbol at or before target offset */
    if (symidx >= -3 && symidx <= -1) {
        /* -1=text(5), -2=data(6), -3=bss(7) */
        seg = 5 - symidx - 1;  /* convert to type seg value */

        /* adjust addend to be relative to segment start */
        if (symidx == -1)
            rel_off = addend - text_off_g;
        else if (symidx == -2)
            rel_off = addend - data_off_g;
        else
            rel_off = addend - bss_off_g;

        /* check for exact symbol match at this offset */
        for (i = 0; i < nsyms; i++) {
            if ((symtab[i].type & 0x07) == seg && symtab[i].value == rel_off) {
                sprintf(buf, "%s", symtab[i].name);
                return buf;
            }
        }

        /* no exact match - use synthetic local label */
        if (symidx == -1) {
            sprintf(buf, "T%d", add_text_ref(rel_off));
        } else if (symidx == -2) {
            sprintf(buf, "D%d", add_data_ref(rel_off));
        } else {
            sprintf(buf, "B%d", add_bss_ref(rel_off));
        }
    } else if (symidx >= 0 && symidx < nsyms) {
        if (addend)
            sprintf(buf, "%s+%d", symtab[symidx].name, addend);
        else
            sprintf(buf, "%s", symtab[symidx].name);
    } else {
        sprintf(buf, "?sym%d", symidx);
    }
    return buf;
}

/*
 * get symbol name for byte relocation target
 * symidx: symbol index or segment (-1=text, -2=data, -3=bss)
 * val: the byte value at the relocation site
 * hilo: 1=lo, 2=hi
 */
char *
reloc_name_byte(symidx, val, hilo, buf)
int symidx;
unsigned char val;
int hilo;
char *buf;
{
    /* symbol reference - just use symbol name */
    if (symidx >= 0 && symidx < nsyms) {
        sprintf(buf, "%s", symtab[symidx].name);
        return buf;
    }

    /* segment-relative: use segment+offset form (can't recover local symbol name) */
    if (symidx >= -3 && symidx <= -1) {
        if (symidx == -1)
            sprintf(buf, "_.text+0%02xh", val);
        else if (symidx == -2)
            sprintf(buf, "_.data+0%02xh", val);
        else
            sprintf(buf, "_.bss+0%02xh", val);
        return buf;
    }

    sprintf(buf, "?sym%d", symidx);
    return buf;
}

/*
 * Generate unified .s file from uobj
 * This is the common output function for both Whitesmiths and HiTech formats
 * When dflag is set, output goes to stdout with hex dump per line
 * When gflag is set, output goes to a .s file
 */
void
gen_uobj_sfile(name)
char *name;
{
    int pc, i, len, ri, ref_idx;
    char nbuf[128];
    char *p, *sym;
    unsigned char *save_filebuf;

    if (dflag) {
        /* disassembly mode: output to stdout */
        gfile = stdout;
    } else {
        /* generate mode: output to file */
        strncpy(gname, name, sizeof(gname) - 3);
        gname[sizeof(gname) - 3] = '\0';
        p = strrchr(gname, '.');
        if (p && (strcmp(p, ".obj") == 0 || strcmp(p, ".o") == 0)) {
            strcpy(p, ".s");
        } else {
            strcat(gname, ".s");
        }

        gfile = fopen(gname, "w");
        if (!gfile) {
            fprintf(stderr, "wsnm: cannot create %s\n", gname);
            return;
        }

        printf("  -> %s\n", gname);
    }

    /* copy uobj.relocs to ureltab for disassembler fmt_addr to use */
    ureltab = uobj.relocs;
    nurels = uobj.nrelocs;

    /* header */
    fprintf(gfile, "; Generated from %s by wsnm -g\n", name);

    /* emit externs */
    for (i = 0; i < uobj.nsyms; i++) {
        if (uobj.syms[i].segment == USEG_UNDEF) {
            fprintf(gfile, "\t.extern %s\n", uobj.syms[i].name);
        }
    }

    /* emit globals */
    for (i = 0; i < uobj.nsyms; i++) {
        if (uobj.syms[i].scope == USCOPE_GLOBAL &&
            uobj.syms[i].segment != USEG_UNDEF) {
            fprintf(gfile, "\t.global %s\n", uobj.syms[i].name);
        }
    }

    /* text section */
    if (uobj.textsize > 0) {
        fprintf(gfile, "\n\t.text\n");

        save_filebuf = filebuf;
        filebuf = uobj.text;

        /* pre-pass: disassemble to find all relative jump targets */
        pc = 0;
        while (pc < (int)uobj.textsize) {
            disasm_pc = -1;
            len = disasm(pc, pc, nbuf);
            pc += len;
        }

        /* main pass: output with labels */
        pc = 0;
        while (pc < (int)uobj.textsize) {
            /* check for symbol at this address */
            sym = usym_lookup(pc, USEG_TEXT);
            if (sym) {
                fprintf(gfile, "%s:\n", sym);
            }
            /* check for synthetic text label */
            ref_idx = find_text_ref(pc);
            if (ref_idx >= 0) {
                fprintf(gfile, "T%d:\n", ref_idx);
            }
            /* check for relative jump target label */
            ref_idx = find_rel_ref(pc);
            if (ref_idx >= 0) {
                fprintf(gfile, "R%d:\n", ref_idx);
            }

            /* check for relocation at this address */
            ri = find_obj_reloc(pc, USEG_TEXT);
            if (ri >= 0) {
                if (uobj.relocs[ri].size == 2) {
                    if (dflag) {
                        fprintf(gfile, "  %04x  %02x %02x        ", pc,
                                uobj.text[pc], uobj.text[pc+1]);
                    } else {
                        fprintf(gfile, "\t");
                    }
                    fprintf(gfile, ".dw %s\n", uobj.relocs[ri].target);
                    pc += 2;
                } else {
                    if (dflag) {
                        fprintf(gfile, "  %04x  %02x           ", pc, uobj.text[pc]);
                    } else {
                        fprintf(gfile, "\t");
                    }
                    if (uobj.relocs[ri].hilo == 1)
                        fprintf(gfile, ".db low(%s)\n", uobj.relocs[ri].target);
                    else if (uobj.relocs[ri].hilo == 2)
                        fprintf(gfile, ".db high(%s)\n", uobj.relocs[ri].target);
                    else
                        fprintf(gfile, ".db %s\n", uobj.relocs[ri].target);
                    pc += 1;
                }
            } else {
                /* disassemble instruction */
                disasm_pc = -1;
                len = disasm(pc, pc, nbuf);
                if (dflag) {
                    fprintf(gfile, "  %04x  ", pc);
                    for (i = 0; i < 4; i++) {
                        if (i < len)
                            fprintf(gfile, "%02x ", uobj.text[pc + i]);
                        else
                            fprintf(gfile, "   ");
                    }
                    fprintf(gfile, " %s\n", nbuf);
                } else {
                    fprintf(gfile, "\t%s\n", nbuf);
                }
                pc += len;
            }
        }

        filebuf = save_filebuf;
    }

    /* data section */
    if (uobj.datasize > 0) {
        int linelen, in_string, line_start;
        unsigned char c;

        fprintf(gfile, "\n\t.data\n");

        pc = 0;
        while (pc < (int)uobj.datasize) {
            /* check for symbol at this address */
            sym = usym_lookup(pc, USEG_DATA);
            if (sym) {
                fprintf(gfile, "%s:\n", sym);
            }
            /* check for synthetic data label */
            ref_idx = find_data_ref(pc);
            if (ref_idx >= 0) {
                fprintf(gfile, "D%d:\n", ref_idx);
            }

            /* check for relocation at this address */
            ri = find_obj_reloc(pc, USEG_DATA);
            if (ri >= 0) {
                if (uobj.relocs[ri].size == 2) {
                    if (dflag) {
                        fprintf(gfile, "  %04x  %02x %02x        ", pc,
                                uobj.data[pc], uobj.data[pc+1]);
                    } else {
                        fprintf(gfile, "\t");
                    }
                    fprintf(gfile, ".dw %s\n", uobj.relocs[ri].target);
                    pc += 2;
                } else {
                    if (dflag) {
                        fprintf(gfile, "  %04x  %02x           ", pc, uobj.data[pc]);
                    } else {
                        fprintf(gfile, "\t");
                    }
                    if (uobj.relocs[ri].hilo == 1)
                        fprintf(gfile, ".db low(%s)\n", uobj.relocs[ri].target);
                    else if (uobj.relocs[ri].hilo == 2)
                        fprintf(gfile, ".db high(%s)\n", uobj.relocs[ri].target);
                    else
                        fprintf(gfile, ".db %s\n", uobj.relocs[ri].target);
                    pc += 1;
                }
                continue;
            }

            if (dflag) {
                /* dflag mode: emit one byte per line with hex dump */
                c = uobj.data[pc];
                fprintf(gfile, "  %04x  %02x           .db 0%02xh\n", pc, c, c);
                pc++;
            } else {
                /* no relocation - emit raw bytes with string detection */
                fprintf(gfile, "\t.db\t");
                linelen = 8;
                in_string = 0;
                line_start = pc;

                while (pc < (int)uobj.datasize && linelen < 60) {
                    /* check for symbol, data ref, or relocation - must break line */
                    if (pc > line_start && (usym_lookup(pc, USEG_DATA) ||
                                   find_data_ref(pc) >= 0 ||
                                   find_obj_reloc(pc, USEG_DATA) >= 0)) {
                        break;
                    }

                    c = uobj.data[pc];

                    /* printable ASCII (excluding quotes and backslash) */
                    if (c >= 0x20 && c <= 0x7e && c != '"' && c != '\\') {
                        if (!in_string) {
                            if (linelen > 8) {
                                fprintf(gfile, ",");
                                linelen++;
                            }
                            fprintf(gfile, "\"");
                            linelen++;
                            in_string = 1;
                        }
                        fprintf(gfile, "%c", c);
                        linelen++;
                    } else {
                        if (in_string) {
                            fprintf(gfile, "\"");
                            linelen++;
                            in_string = 0;
                        }
                        if (linelen > 8) {
                            fprintf(gfile, ",");
                            linelen++;
                        }
                        fprintf(gfile, "0%02xh", c);
                        linelen += 4;
                    }
                    pc++;
                }

                if (in_string) {
                    fprintf(gfile, "\"");
                }
                fprintf(gfile, "\n");
            }
        }
    }

    /* bss section */
    if (uobj.bsssize > 0) {
        fprintf(gfile, "\n\t.bss\n");
        fprintf(gfile, "_.bss:\n");

        pc = 0;
        while (pc < (int)uobj.bsssize) {
            /* check for symbol at this address */
            sym = usym_lookup(pc, USEG_BSS);
            if (sym) {
                fprintf(gfile, "%s:\n", sym);
            }
            /* check for synthetic bss label */
            ref_idx = find_bss_ref(pc);
            if (ref_idx >= 0) {
                fprintf(gfile, "B%d:\n", ref_idx);
            }

            /* find next symbol/label or end */
            for (i = pc + 1; i <= (int)uobj.bsssize; i++) {
                if (i == (int)uobj.bsssize ||
                    usym_lookup(i, USEG_BSS) ||
                    find_bss_ref(i) >= 0) {
                    break;
                }
            }

            fprintf(gfile, "\t.ds %d\n", i - pc);
            pc = i;
        }
    }

    /* only close if writing to a file, not stdout */
    if (!dflag) {
        fclose(gfile);
    }

    /* clear ureltab reference */
    ureltab = NULL;
    nurels = 0;
}

/*
 * Load Whitesmiths object data into unified object structure
 */
void
ws_load_uobj(base, objsize)
long base;
long objsize;
{
    unsigned char config;
    int symlen;
    unsigned short symtab_size, text_size, data_size, bss_size;
    long symtab_off, textRelocOff, dataRelocOff;
    int num_syms, i, seg;
    long limit = base + objsize;
    int addend;
    int text_relocs, data_relocs, total_relocs;
    char nbuf[80];

    uobj_init();

    config = get_byte(base + 1);
    symlen = (config & CONF_SYMLEN) * 2 + 1;
    symtab_size = get_word(base + 2);
    text_size = get_word(base + 4);
    data_size = get_word(base + 6);
    bss_size = get_word(base + 8);
    text_off_g = get_word(base + 12);
    data_off_g = get_word(base + 14);
    bss_off_g = data_off_g + data_size;
    num_syms = symtab_size / (symlen + 3);

    /* copy text segment */
    if (text_size > 0) {
        uobj.text = (unsigned char *)malloc(text_size);
        memcpy(uobj.text, &filebuf[base + 16], text_size);
        uobj.textsize = text_size;
    }

    /* copy data segment */
    if (data_size > 0) {
        uobj.data = (unsigned char *)malloc(data_size);
        memcpy(uobj.data, &filebuf[base + 16 + text_size], data_size);
        uobj.datasize = data_size;
    }

    uobj.bsssize = bss_size;

    /* load symbol table into legacy symtab for reloc_name to use */
    symtab_off = base + 16 + text_size + data_size;
    nsyms = num_syms;
    symlen_g = symlen;
    if (nsyms > 0) {
        long soff = symtab_off;
        symtab = (struct sym *)malloc(nsyms * sizeof(struct sym));
        for (i = 0; i < nsyms; i++) {
            symtab[i].value = get_word(soff);
            symtab[i].type = get_byte(soff + 2);
            memcpy(symtab[i].name, &filebuf[soff + 3], symlen);
            symtab[i].name[symlen] = '\0';
            soff += symlen + 3;
        }
    }

    /* convert symbols to unified format */
    if (nsyms > 0) {
        uobj.syms = (struct usym *)malloc(nsyms * sizeof(struct usym));
        for (i = 0; i < nsyms; i++) {
            strncpy(uobj.syms[i].name, symtab[i].name, sizeof(uobj.syms[i].name) - 1);
            uobj.syms[i].name[sizeof(uobj.syms[i].name) - 1] = '\0';
            uobj.syms[i].value = symtab[i].value;

            /* convert segment: WS uses 4=abs, 5=text, 6=data, 7=bss, <4=undef */
            seg = symtab[i].type & 0x07;
            if (seg < 4) {
                uobj.syms[i].segment = USEG_UNDEF;
            } else if (seg == 4) {
                uobj.syms[i].segment = USEG_ABS;
            } else if (seg == 5) {
                uobj.syms[i].segment = USEG_TEXT;
            } else if (seg == 6) {
                uobj.syms[i].segment = USEG_DATA;
            } else {
                uobj.syms[i].segment = USEG_BSS;
            }

            /* global flag is bit 3 */
            uobj.syms[i].scope = (symtab[i].type & 0x08) ? USCOPE_GLOBAL : USCOPE_LOCAL;
        }
        uobj.nsyms = nsyms;
    }

    /* count and load relocations */
    if (!(config & CONF_NORELO)) {
        /* parse text relocations */
        textRelocOff = symtab_off + symtab_size;
        dataRelocOff = parse_relocs(textRelocOff, limit);
        text_relocs = nrels;

        /* save text relocs temporarily */
        struct reloc *text_reltab = reltab;
        int text_nrels = nrels;
        reltab = NULL;
        nrels = 0;

        /* parse data relocations */
        parse_relocs(dataRelocOff, limit);
        data_relocs = nrels;

        /* allocate unified relocs */
        total_relocs = text_relocs + data_relocs;
        if (total_relocs > 0) {
            uobj.relocs = (struct ureloc *)malloc(total_relocs * sizeof(struct ureloc));

            /* convert text relocations */
            for (i = 0; i < text_nrels; i++) {
                uobj.relocs[i].offset = text_reltab[i].offset;
                uobj.relocs[i].hilo = text_reltab[i].hilo;
                uobj.relocs[i].size = text_reltab[i].hilo ? 1 : 2;
                uobj.relocs[i].segment = USEG_TEXT;

                /* resolve target name */
                if (text_reltab[i].hilo) {
                    reloc_name_byte(text_reltab[i].symidx,
                                   uobj.text[text_reltab[i].offset],
                                   text_reltab[i].hilo, nbuf);
                } else {
                    addend = uobj.text[text_reltab[i].offset] |
                            (uobj.text[text_reltab[i].offset + 1] << 8);
                    reloc_name(text_reltab[i].symidx, addend, nbuf);
                }
                strncpy(uobj.relocs[i].target, nbuf, sizeof(uobj.relocs[i].target) - 1);
                uobj.relocs[i].target[sizeof(uobj.relocs[i].target) - 1] = '\0';
            }

            /* convert data relocations */
            for (i = 0; i < data_relocs; i++) {
                int idx = text_nrels + i;
                uobj.relocs[idx].offset = reltab[i].offset;
                uobj.relocs[idx].hilo = reltab[i].hilo;
                uobj.relocs[idx].size = reltab[i].hilo ? 1 : 2;
                uobj.relocs[idx].segment = USEG_DATA;

                /* resolve target name */
                if (reltab[i].hilo) {
                    reloc_name_byte(reltab[i].symidx,
                                   uobj.data[reltab[i].offset],
                                   reltab[i].hilo, nbuf);
                } else {
                    addend = uobj.data[reltab[i].offset] |
                            (uobj.data[reltab[i].offset + 1] << 8);
                    reloc_name(reltab[i].symidx, addend, nbuf);
                }
                strncpy(uobj.relocs[idx].target, nbuf, sizeof(uobj.relocs[idx].target) - 1);
                uobj.relocs[idx].target[sizeof(uobj.relocs[idx].target) - 1] = '\0';
            }

            uobj.nrelocs = total_relocs;
        }

        /* free temporary reloc tables */
        if (text_reltab) free(text_reltab);
        if (reltab) free(reltab);
        reltab = NULL;
        nrels = 0;
    }

    /* free legacy symtab - we've converted to uobj.syms */
    if (symtab) {
        free(symtab);
        symtab = NULL;
    }
    nsyms = 0;
}

/*
 * generate .s file from Whitesmiths object - uses unified path
 */
void
gen_sfile(name, base, objsize)
char *name;
long base;
long objsize;
{
    /* load into unified structure */
    ws_load_uobj(base, objsize);

    /* generate output using unified function */
    gen_uobj_sfile(name);

    /* cleanup */
    uobj_free();
    free_synth_refs();
}

/*
 * process object at given offset in filebuf
 * objsize is the size limit for this object (for bounds checking)
 */
void
processObj(name, base, objsize)
char *name;
long base;
long objsize;
{
    unsigned char magic, config;
    int symlen;
    unsigned short symtab_size, text_size, data_size, bss_size;
    unsigned short heap_size, text_off, data_off;
    long symtab_off, textRelocOff, dataRelocOff;
    int num_syms;
    long off;
    long limit;
    int i;

    limit = base + objsize;

    /* parse header */
    magic = get_byte(base);
    if (magic != MAGIC) {
        fprintf(stderr, "wsnm: %s: bad magic 0x%02x\n", name, magic);
        return;
    }

    /* -g or -d mode: generate .s file (or stdout with hex dump) and return */
    if (gflag || dflag) {
        gen_sfile(name, base, objsize);
        return;
    }

    config = get_byte(base + 1);
    symlen = (config & CONF_SYMLEN) * 2 + 1;
    symtab_size = get_word(base + 2);
    text_size = get_word(base + 4);
    data_size = get_word(base + 6);
    bss_size = get_word(base + 8);
    heap_size = get_word(base + 10);
    text_off = get_word(base + 12);
    data_off = get_word(base + 14);

    num_syms = symtab_size / (symlen + 3);

    printf("=== %s ===\n", name);
    if (vflag) {
        printf("\nHeader:\n");
        printf("  Magic:       0x%02x\n", magic);
        printf("  Config:      0x%02x", config);
        if (config & CONF_LITTLE) printf(" little-endian");
        if (config & CONF_INT32) printf(" 32-bit-int");
        if (config & CONF_NORELO) printf(" no-reloc");
        printf("\n");
        printf("  Symlen:      %d chars\n", symlen);
        printf("  Symtab:      %d bytes (%d symbols)\n", symtab_size, num_syms);
        printf("  Text:        %d bytes\n", text_size);
        printf("  Data:        %d bytes\n", data_size);
        printf("  BSS:         %d bytes\n", bss_size);
        printf("  Heap:        %d bytes\n", heap_size);
        printf("  Text offset: 0x%04x\n", text_off);
        printf("  Data offset: 0x%04x\n", data_off);
    }

    /* load symbol table for disassembly */
    symtab_off = base + 16 + text_size + data_size;
    nsyms = num_syms;
    symlen_g = symlen;
    if (nsyms > 0) {
        long soff = symtab_off;
        symtab = (struct sym *)malloc(nsyms * sizeof(struct sym));
        for (i = 0; i < nsyms; i++) {
            symtab[i].value = get_word(soff);
            symtab[i].type = get_byte(soff + 2);
            memcpy(symtab[i].name, &filebuf[soff + 3], symlen);
            symtab[i].name[symlen] = '\0';
            soff += symlen + 3;
        }
    }

    textRelocOff = symtab_off + symtab_size;

    /* hex dump segments if -b */
    if (bflag) {
        hexdump("Text segment", base + 16, text_size, text_off);
        if (data_size > 0)
            hexdump("Data segment", base + 16 + text_size, data_size, data_off);
    }

    /* dump symbol table */
    if (symtab_size > 0) {
        dump_symbols(symtab_off, symtab_size, symlen);
    } else {
        printf("\nSymbol table: (none)\n");
    }

    /* dump relocations */
    if (rflag) {
        if (!(config & CONF_NORELO)) {
            dump_relocs("Text", textRelocOff, symtab_off, symlen, num_syms);

            /* find data reloc offset by scanning past text relocs */
            off = textRelocOff;
            while (off < limit) {
                unsigned char b = get_byte(off++);
                if (b == 0) break;
                if (b >= REL_BUMP_EXT && b < REL_ABS) off++;
                else if (b == REL_SYM_EXT) {
                    b = get_byte(off++);
                    if (b >= REL_EXT_LONG) off++;
                }
            }
            dataRelocOff = off;

            dump_relocs("Data", dataRelocOff, symtab_off, symlen, num_syms);
        } else {
            printf("\nRelocations: (stripped)\n");
        }
    }

    printf("\n");
    if (symtab)
        free(symtab);
    symtab = 0;
    nsyms = 0;
}

/*
 * generate relocation target name for HiTech
 */
void
ht_reloc_name(relocs, ri, syms, nsyms, addend, buf)
struct ht_reloc *relocs;
int ri;
struct ht_sym *syms;
int nsyms;
int addend;
char *buf;
{
    int i;
    char *target = relocs[ri].target;

    if ((relocs[ri].type & 0xf0) == HT_RPSECT) {
        /* psect-relative: check for exact symbol match */
        for (i = 0; i < nsyms; i++) {
            if (strcmp(syms[i].psect, target) == 0 &&
                (int)syms[i].value == addend) {
                sprintf(buf, "%s", syms[i].name);
                return;
            }
        }
        /* no exact match - use synthetic local label */
        if (strcmp(target, "data") == 0) {
            sprintf(buf, "D%d", add_data_ref(addend));
        } else if (strcmp(target, "text") == 0) {
            sprintf(buf, "T%d", add_text_ref(addend));
        } else if (strcmp(target, "bss") == 0) {
            sprintf(buf, "B%d", add_bss_ref(addend));
        } else {
            sprintf(buf, "_.%s+%d", target, addend);
        }
    } else {
        /* external symbol */
        if (addend == 0) {
            sprintf(buf, "%s", target);
        } else {
            sprintf(buf, "%s+%d", target, addend);
        }
    }
}

/*
 * Load HiTech object data into unified object structure
 */
void
ht_load_uobj(textbuf, textsize, databuf, datasize, bsssize, relocs, nrelocs, syms, nsyms)
unsigned char *textbuf;
unsigned long textsize;
unsigned char *databuf;
unsigned long datasize;
unsigned long bsssize;
struct ht_reloc *relocs;
int nrelocs;
struct ht_sym *syms;
int nsyms;
{
    int i, addend;

    uobj_init();

    /* copy text segment */
    if (textsize > 0) {
        uobj.text = (unsigned char *)malloc(textsize);
        memcpy(uobj.text, textbuf, textsize);
        uobj.textsize = textsize;
    }

    /* copy data segment */
    if (datasize > 0) {
        uobj.data = (unsigned char *)malloc(datasize);
        memcpy(uobj.data, databuf, datasize);
        uobj.datasize = datasize;
    }

    uobj.bsssize = bsssize;

    /* convert symbols */
    if (nsyms > 0) {
        uobj.syms = (struct usym *)malloc(nsyms * sizeof(struct usym));
        for (i = 0; i < nsyms; i++) {
            strncpy(uobj.syms[i].name, syms[i].name, sizeof(uobj.syms[i].name) - 1);
            uobj.syms[i].name[sizeof(uobj.syms[i].name) - 1] = '\0';
            uobj.syms[i].value = syms[i].value;

            /* convert segment */
            if ((syms[i].flags & 0x0f) == 6) {
                uobj.syms[i].segment = USEG_UNDEF;
            } else if (strcmp(syms[i].psect, "text") == 0) {
                uobj.syms[i].segment = USEG_TEXT;
            } else if (strcmp(syms[i].psect, "data") == 0) {
                uobj.syms[i].segment = USEG_DATA;
            } else if (strcmp(syms[i].psect, "bss") == 0) {
                uobj.syms[i].segment = USEG_BSS;
            } else {
                uobj.syms[i].segment = USEG_ABS;
            }

            /* convert scope */
            uobj.syms[i].scope = (syms[i].flags & 0x10) ? USCOPE_GLOBAL : USCOPE_LOCAL;
        }
        uobj.nsyms = nsyms;
    }

    /* convert relocations */
    if (nrelocs > 0) {
        uobj.relocs = (struct ureloc *)malloc(nrelocs * sizeof(struct ureloc));
        for (i = 0; i < nrelocs; i++) {
            uobj.relocs[i].offset = relocs[i].offset;
            uobj.relocs[i].size = relocs[i].type & HT_RSIZE_MASK;
            uobj.relocs[i].hilo = 0;  /* HiTech doesn't have hi/lo byte relocs */
            uobj.relocs[i].segment = USEG_TEXT;  /* HiTech relocs are in text */

            /* get addend from data */
            addend = textbuf[relocs[i].offset];
            if (uobj.relocs[i].size == 2 && relocs[i].offset + 1 < textsize)
                addend |= textbuf[relocs[i].offset + 1] << 8;

            /* resolve target name */
            ht_reloc_name(relocs, i, syms, nsyms, addend, uobj.relocs[i].target);
        }
        uobj.nrelocs = nrelocs;
    }
}

/*
 * generate .s file from HiTech object - uses unified path
 */
void
gen_ht_sfile(name, textbuf, textsize, databuf, datasize, bsssize, relocs, nrelocs, syms, nsyms)
char *name;
unsigned char *textbuf;
unsigned long textsize;
unsigned char *databuf;
unsigned long datasize;
unsigned long bsssize;
struct ht_reloc *relocs;
int nrelocs;
struct ht_sym *syms;
int nsyms;
{
    /* load into unified structure */
    ht_load_uobj(textbuf, textsize, databuf, datasize, bsssize, relocs, nrelocs, syms, nsyms);

    /* generate output using unified function */
    gen_uobj_sfile(name);

    /* cleanup */
    uobj_free();
    free_synth_refs();
}

/*
 * process HiTech object file
 */
void
processHitech(name)
char *name;
{
    long off = 0;
    int reclen, rectype;
    char machine[8];
    int i;

    /* collected data */
    struct ht_reloc *relocs = NULL;
    int nrelocs = 0;
    int reloc_alloc = 0;
    struct ht_sym *htsyms = NULL;
    int nhtsyms = 0;
    int htsym_alloc = 0;
    long symbol_off = 0;
    int symbol_len = 0;
    unsigned long cur_text_off = 0;  /* offset of current TEXT chunk */
    int in_text_psect = 0;           /* current TEXT record is "text" psect */
    unsigned char *textbuf = NULL;   /* combined text data */
    unsigned long textsize = 0;      /* total text size */
    unsigned long textalloc = 0;     /* allocated size */
    unsigned char *databuf = NULL;   /* combined data segment */
    unsigned long datasize = 0;      /* total data size */
    unsigned long dataalloc = 0;     /* allocated size */
    unsigned long bsssize = 0;       /* total bss size */

    if (!gflag)
        printf("=== %s (HiTech) ===\n", name);

    /* first pass: collect relocations and find symbol record */
    off = 0;
    while (off < filesize - 3) {
        reclen = filebuf[off] | (filebuf[off + 1] << 8);
        rectype = filebuf[off + 2];
        off += 3;

        if (off + reclen > filesize)
            break;

        if (rectype == HT_TEXT && reclen >= 5) {
            /* collect TEXT data - only from "text" psect */
            char psect[64];
            int plen, dlen;
            unsigned long endoff;

            cur_text_off = filebuf[off] | (filebuf[off+1] << 8) |
                          ((unsigned long)filebuf[off+2] << 16) |
                          ((unsigned long)filebuf[off+3] << 24);

            /* get psect name */
            for (plen = 0; plen < 63 && off + 4 + plen < filesize; plen++) {
                psect[plen] = filebuf[off + 4 + plen];
                if (psect[plen] == '\0') break;
            }
            psect[plen] = '\0';

            dlen = reclen - 4 - plen - 1;
            if (dlen < 0) dlen = 0;

            /* collect text and data psects */
            in_text_psect = (strcmp(psect, "text") == 0);
            if (in_text_psect) {
                endoff = cur_text_off + dlen;
                if (endoff > textsize) textsize = endoff;

                /* grow buffer if needed */
                if (endoff > textalloc) {
                    textalloc = endoff + 1024;
                    textbuf = (unsigned char *)realloc(textbuf, textalloc);
                    if (!textbuf) { fprintf(stderr, "out of memory\n"); return; }
                }

                /* copy text data */
                if (dlen > 0) {
                    memcpy(textbuf + cur_text_off, filebuf + off + 4 + plen + 1, dlen);
                }
            } else if (strcmp(psect, "data") == 0) {
                endoff = cur_text_off + dlen;
                if (endoff > datasize) datasize = endoff;

                /* grow buffer if needed */
                if (endoff > dataalloc) {
                    dataalloc = endoff + 1024;
                    databuf = (unsigned char *)realloc(databuf, dataalloc);
                    if (!databuf) { fprintf(stderr, "out of memory\n"); return; }
                }

                /* copy data */
                if (dlen > 0) {
                    memcpy(databuf + cur_text_off, filebuf + off + 4 + plen + 1, dlen);
                }
            } else if (strcmp(psect, "bss") == 0) {
                /* BSS has no data, just track size */
                endoff = cur_text_off + dlen;
                if (endoff > bsssize) bsssize = endoff;
            }
        } else if (rectype == HT_RELOC && (rflag || gflag || dflag) && in_text_psect) {
            long roff = off;
            long rend = off + reclen;

            while (roff < rend) {
                unsigned short reloff;
                unsigned char reltype;
                char target[64];
                int tlen;

                if (roff + 3 > rend) break;

                reloff = filebuf[roff] | (filebuf[roff+1] << 8);
                reltype = filebuf[roff + 2];
                roff += 3;

                for (tlen = 0; tlen < 63 && roff + tlen < rend; tlen++) {
                    target[tlen] = filebuf[roff + tlen];
                    if (target[tlen] == '\0') break;
                }
                target[tlen] = '\0';
                roff += tlen + 1;

                /* grow array if needed */
                if (nrelocs >= reloc_alloc) {
                    reloc_alloc = reloc_alloc ? reloc_alloc * 2 : 64;
                    relocs = (struct ht_reloc *)realloc(relocs,
                             reloc_alloc * sizeof(struct ht_reloc));
                }
                relocs[nrelocs].offset = reloff + cur_text_off;
                relocs[nrelocs].type = reltype;
                strcpy(relocs[nrelocs].target, target);
                nrelocs++;
            }
        } else if (rectype == HT_SYMBOL) {
            /* collect symbols */
            long soff = off;
            long send = off + reclen;

            symbol_off = off;
            symbol_len = reclen;

            while (soff < send) {
                unsigned long val;
                unsigned short flags;
                char psect[64], symname[64];
                int plen, nlen;

                if (soff + 7 > send) break;

                val = filebuf[soff] | (filebuf[soff+1] << 8) |
                      ((unsigned long)filebuf[soff+2] << 16) |
                      ((unsigned long)filebuf[soff+3] << 24);
                flags = filebuf[soff+4] | (filebuf[soff+5] << 8);
                soff += 6;

                for (plen = 0; plen < 63 && soff + plen < send; plen++) {
                    psect[plen] = filebuf[soff + plen];
                    if (psect[plen] == '\0') break;
                }
                psect[plen] = '\0';
                soff += plen + 1;

                for (nlen = 0; nlen < 63 && soff + nlen < send; nlen++) {
                    symname[nlen] = filebuf[soff + nlen];
                    if (symname[nlen] == '\0') break;
                }
                symname[nlen] = '\0';
                soff += nlen + 1;

                /* grow array if needed */
                if (nhtsyms >= htsym_alloc) {
                    htsym_alloc = htsym_alloc ? htsym_alloc * 2 : 32;
                    htsyms = (struct ht_sym *)realloc(htsyms,
                             htsym_alloc * sizeof(struct ht_sym));
                }
                htsyms[nhtsyms].value = val;
                htsyms[nhtsyms].flags = flags;
                strncpy(htsyms[nhtsyms].psect, psect, 31);
                htsyms[nhtsyms].psect[31] = '\0';
                strncpy(htsyms[nhtsyms].name, symname, 63);
                htsyms[nhtsyms].name[63] = '\0';
                nhtsyms++;
            }
        }

        off += reclen;
    }

    /* -g or -d mode: generate .s file (or stdout with hex dump) and return */
    if (gflag || dflag) {
        gen_ht_sfile(name, textbuf, textsize, databuf, datasize, bsssize, relocs, nrelocs, htsyms, nhtsyms);
        if (relocs) free(relocs);
        if (textbuf) free(textbuf);
        if (databuf) free(databuf);
        if (htsyms) free(htsyms);
        return;
    }

    /* display combined text section */
    if (textsize > 0) {
        if (vflag) {
            printf("\nTEXT: size=%lu\n", textsize);
        }
        if (bflag) {
            /* use textbuf directly for hexdump */
            unsigned char *save_filebuf = filebuf;
            filebuf = textbuf;
            hexdump("text", 0, (int)textsize, 0);
            filebuf = save_filebuf;
        }
    }

    /* second pass: display psects and other info */
    off = 0;
    while (off < filesize - 3) {
        reclen = filebuf[off] | (filebuf[off + 1] << 8);
        rectype = filebuf[off + 2];
        off += 3;

        if (off + reclen > filesize)
            break;

        switch (rectype) {
        case HT_IDENT:
            if (reclen >= 10) {
                for (i = 0; i < 4 && i < reclen - 6; i++)
                    machine[i] = filebuf[off + 6 + i];
                machine[i] = '\0';
                if (vflag) {
                    printf("\nIDENT: machine=%s\n", machine);
                }
            }
            break;

        case HT_PSECT:
            if (vflag && reclen >= 3) {
                unsigned short flags;
                char psect[64];
                int plen;

                flags = filebuf[off] | (filebuf[off+1] << 8);

                for (plen = 0; plen < 63 && off + 2 + plen < filesize; plen++) {
                    psect[plen] = filebuf[off + 2 + plen];
                    if (psect[plen] == '\0') break;
                }
                psect[plen] = '\0';

                printf("\nPSECT: name=%s flags=0x%04x", psect[0] ? psect : "(empty)", flags);
                if (flags & HT_F_GLOBAL) printf(" GLOBAL");
                if (flags & HT_F_PURE) printf(" PURE");
                if (flags & HT_F_OVRLD) printf(" OVRLD");
                if (flags & HT_F_ABS) printf(" ABS");
                printf("\n");
            }
            break;

        case HT_START:
            if (vflag && reclen >= 4) {
                unsigned long start;
                char psect[64];
                int plen;

                start = filebuf[off] | (filebuf[off+1] << 8) |
                        ((unsigned long)filebuf[off+2] << 16) |
                        ((unsigned long)filebuf[off+3] << 24);

                for (plen = 0; plen < 63 && off + 4 + plen < filesize; plen++) {
                    psect[plen] = filebuf[off + 4 + plen];
                    if (psect[plen] == '\0') break;
                }
                psect[plen] = '\0';

                printf("\nSTART: addr=0x%08lx psect=%s\n", start, psect);
            }
            break;

        case HT_END:
            if (vflag) {
                printf("\nEND\n");
            }
            break;

        default:
            break;
        }

        off += reclen;
    }

    /* print collected relocations */
    if (rflag && nrelocs > 0) {
        printf("\nRelocations:\n");
        printf("  Offset  Size  Segment  Target\n");
        printf("  ------  ----  -------  ------\n");

        for (i = 0; i < nrelocs; i++) {
            char *seg;
            char *size;

            size = (relocs[i].type & HT_RSIZE_MASK) == 1 ? "byte" : "word";

            if ((relocs[i].type & 0xf0) == HT_RPSECT) {
                seg = relocs[i].target;
                printf("  0x%04lx  %-4s  %s\n", relocs[i].offset, size, seg);
            } else {
                seg = "-";
                printf("  0x%04lx  %-4s  %-7s  %s\n", relocs[i].offset, size, seg, relocs[i].target);
            }
        }
    }

    /* print symbol table */
    if (symbol_len > 0) {
        long soff = symbol_off;
        long send = symbol_off + symbol_len;
        int nsym = 0;

        /* count symbols */
        while (soff < send) {
            int nlen;
            if (soff + 7 > send) break;
            soff += 6;
            while (soff < send && filebuf[soff]) soff++;
            soff++;
            for (nlen = 0; soff + nlen < send && filebuf[soff + nlen]; nlen++);
            soff += nlen + 1;
            nsym++;
        }

        printf("\nSymbol table: %d symbols\n", nsym);
        printf("  Value     Segment  Scope   Type    Name\n");
        printf("  --------  -------  ------  ------  ----\n");

        soff = symbol_off;
        while (soff < send) {
            unsigned long val;
            unsigned short flags;
            char psect[64], symname[64];
            char scope[16], stype[16], seg[16];
            int plen, nlen;

            if (soff + 7 > send) break;

            val = filebuf[soff] | (filebuf[soff+1] << 8) |
                  ((unsigned long)filebuf[soff+2] << 16) |
                  ((unsigned long)filebuf[soff+3] << 24);
            flags = filebuf[soff+4] | (filebuf[soff+5] << 8);
            soff += 6;

            for (plen = 0; plen < 63 && soff + plen < send; plen++) {
                psect[plen] = filebuf[soff + plen];
                if (psect[plen] == '\0') break;
            }
            psect[plen] = '\0';
            soff += plen + 1;

            for (nlen = 0; nlen < 63 && soff + nlen < send; nlen++) {
                symname[nlen] = filebuf[soff + nlen];
                if (symname[nlen] == '\0') break;
            }
            symname[nlen] = '\0';
            soff += nlen + 1;

            ht_decode_sym(flags, psect, scope, stype, seg);

            printf("  0x%06lx  %-7s  %-6s  %-6s  %s\n",
                   val, seg, scope, stype, symname);
        }
    }

    if (relocs)
        free(relocs);
    if (textbuf)
        free(textbuf);
    if (databuf)
        free(databuf);
    if (htsyms)
        free(htsyms);

    printf("\n");
}

/*
 * process HiTech library file (.LIB)
 */
void
processHitechLib(name)
char *name;
{
    long off;
    unsigned short size_symbols, num_modules;
    unsigned short symSize, symCnt;
    unsigned long moduleSize;
    long modDataOff;
    char moduleName[256];
    char symName[256];
    int i, j, len;
    unsigned char symFlags;
    char *symTypes = "D?C???U";

    printf("=== %s (HiTech Library) ===\n", name);

    /* read header */
    size_symbols = filebuf[0] | (filebuf[1] << 8);
    num_modules = filebuf[2] | (filebuf[3] << 8);

    if (vflag) {
        printf("\nHeader:\n");
        printf("  Symbol directory size: %d bytes\n", size_symbols);
        printf("  Number of modules: %d\n", num_modules);
    }

    /* module data starts after header + symbol directory */
    modDataOff = 4 + size_symbols;

    /* process symbol directory */
    off = 4;
    for (i = 0; i < num_modules && off < filesize; i++) {
        if (off + 12 > filesize) break;

        symSize = filebuf[off] | (filebuf[off+1] << 8);
        symCnt = filebuf[off+2] | (filebuf[off+3] << 8);
        moduleSize = filebuf[off+4] | (filebuf[off+5] << 8) |
                     ((unsigned long)filebuf[off+6] << 16) |
                     ((unsigned long)filebuf[off+7] << 24);
        off += 12;

        /* read module name */
        for (len = 0; len < 255 && off + len < filesize; len++) {
            moduleName[len] = filebuf[off + len];
            if (moduleName[len] == '\0') break;
        }
        moduleName[len] = '\0';
        off += len + 1;

        printf("\n%-15s  size=%ld  symbols=%d\n", moduleName, moduleSize, symCnt);

        /* read symbols */
        if (vflag || rflag) {
            for (j = 0; j < symCnt && off < filesize; j++) {
                symFlags = filebuf[off++];

                /* read symbol name */
                for (len = 0; len < 255 && off + len < filesize; len++) {
                    symName[len] = filebuf[off + len];
                    if (symName[len] == '\0') break;
                }
                symName[len] = '\0';
                off += len + 1;

                printf("  %c %s\n",
                       symFlags < 7 ? symTypes[symFlags] : '?',
                       symName);
            }
        } else {
            /* skip symbols */
            for (j = 0; j < symCnt && off < filesize; j++) {
                off++;  /* skip flags */
                while (off < filesize && filebuf[off]) off++;
                off++;  /* skip null terminator */
            }
        }

        /* optionally process module data */
        if (bflag && modDataOff + moduleSize <= filesize) {
            hexdump(moduleName, modDataOff, (int)moduleSize, 0);
        }

        modDataOff += moduleSize;
    }

    printf("\n");
}

/*
 * process archive file
 * format: 2-byte magic (0xFF75), then entries of:
 *   14-byte name, 2-byte length, file contents
 * terminated by entry with null name
 */
void
processAr(filename)
char *filename;
{
    long off;
    char name[15];
    unsigned short len;

    printf("=== Archive: %s ===\n\n", filename);

    off = 2;  /* skip magic */
    while (off < filesize) {
        /* read 14-byte name */
        if (off + 16 > filesize) break;
        memcpy(name, &filebuf[off], 14);
        name[14] = '\0';

        /* null name marks end */
        if (name[0] == '\0') break;

        len = get_word(off + 14);
        off += 16;

        /* process the object */
        if (off + len <= filesize) {
            processObj(name, off, len);
        }

        off += len;
    }
}

void
process_file(filename)
char *filename;
{
    unsigned short magic16;

    fd = open(filename, O_RDONLY);
    if (fd < 0)
        error2("cannot open", filename);

    /* get file size */
    filesize = lseek(fd, 0, SEEK_END);
    lseek(fd, 0, SEEK_SET);

    /* read entire file */
    filebuf = (unsigned char *)malloc(filesize);
    if (!filebuf)
        error("out of memory");
    if (read(fd, filebuf, filesize) != filesize)
        error("read error");
    close(fd);

    /* check for archive, HiTech, or Whitesmith object */
    magic16 = get_word(0);
    if (magic16 == AR_MAGIC) {
        processAr(filename);
    } else if (filesize >= 13 && HT_IS_HITECH(filebuf)) {
        processHitech(filename);
    } else if (get_byte(0) == MAGIC) {
        processObj(filename, 0, filesize);
    } else {
        /* check for HiTech library format */
        /* library header: 2-byte sym_size, 2-byte num_modules */
        /* module data starts at offset 4 + sym_size */
        unsigned short sym_size = get_word(0);
        unsigned short num_mods = get_word(2);
        long mod_data_off = 4 + sym_size;
        if (num_mods > 0 && num_mods < 1000 &&
            mod_data_off > 4 && mod_data_off < filesize &&
            filesize >= mod_data_off + 13 &&
            HT_IS_HITECH(filebuf + mod_data_off)) {
            processHitechLib(filename);
        } else {
            error2("bad magic", filename);
        }
    }

    free(filebuf);
}

void
usage()
{
    fprintf(stderr, "usage: wsnm [-bdgrv] file.o [...]\n");
    fprintf(stderr, "  -b    hex dump text/data segments\n");
    fprintf(stderr, "  -d    disassemble text segment\n");
    fprintf(stderr, "  -g    generate assemblable .s files\n");
    fprintf(stderr, "  -r    show relocations\n");
    fprintf(stderr, "  -v    show header\n");
    fprintf(stderr, "With no options, only symbol table is shown.\n");
    exit(1);
}

int
main(argc, argv)
int argc;
char **argv;
{
    int i;
    int nfiles = 0;

    for (i = 1; i < argc; i++) {
        if (argv[i][0] == '-') {
            switch (argv[i][1]) {
            case 'b':
                bflag++;
                break;
            case 'd':
                dflag++;
                break;
            case 'g':
                gflag++;
                break;
            case 'r':
                rflag++;
                break;
            case 'v':
                vflag++;
                break;
            default:
                usage();
            }
        } else {
            process_file(argv[i]);
            nfiles++;
        }
    }

    if (nfiles == 0)
        usage();

    return 0;
}

/*
 * vim: tabstop=4 shiftwidth=4 noexpandtab:
 */
