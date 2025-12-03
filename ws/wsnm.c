/*
 * wsnm - Whitesmith's object file dump utility
 *
 * Displays symbol table, relocations, and hex dump of segments
 *
 * vim: tabstop=4 shiftwidth=4 noexpandtab:
 */
#ifdef linux
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#else
#include <stdio.h>
#define void int
#endif

#define MAGIC       0x99
#define AR_MAGIC    0xFF75  /* 0177565 octal */

#define CONF_SYMLEN 0x07
#define CONF_INT32  0x08
#define CONF_LITTLE 0x10
#define CONF_ALIGN  0x60
#define CONF_NORELO 0x80

int fd;
unsigned char *filebuf;
long filesize;
int dflag;      /* -d: disassemble */

char *segnames[] = { "abs", "text", "data", "bss", "ext" };

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

/*
 * lookup symbol by value and segment
 */
char *
sym_lookup(val, seg)
unsigned short val;
int seg;
{
    int i, sseg;
    for (i = 0; i < nsyms; i++) {
        sseg = (symtab[i].type & 0x07);
        /* text=5, data=6, bss=7 */
        if (symtab[i].value == val) {
            if (seg == 1 && sseg == 5) return symtab[i].name;  /* text */
            if (seg == 2 && sseg == 6) return symtab[i].name;  /* data */
            if (seg == 3 && sseg == 7) return symtab[i].name;  /* bss */
        }
    }
    return 0;
}

/*
 * format address with symbol if available
 */
void
fmt_addr(buf, val)
char *buf;
unsigned short val;
{
    char *sym = sym_lookup(val, 1);  /* try text first */
    if (!sym) sym = sym_lookup(val, 2);  /* then data */
    if (!sym) sym = sym_lookup(val, 3);  /* then bss */
    if (sym)
        sprintf(buf, "%s", sym);
    else
        sprintf(buf, "%04xh", val);
}

/*
 * disassemble one instruction, return length
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
    signed char rel;

    op = filebuf[addr];

    /* CB prefix - bit operations */
    if (op == 0xcb) {
        int r, b;
        static char *rot[] = { "rlc", "rrc", "rl", "rr", "sla", "sra", "sll", "srl" };
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
                   fmt_addr(abuf, nn); sprintf(buf, "ld (%s),bc", abuf); break;
        case 0x44: sprintf(buf, "neg"); break;
        case 0x45: sprintf(buf, "retn"); break;
        case 0x46: sprintf(buf, "im 0"); break;
        case 0x47: sprintf(buf, "ld i,a"); break;
        case 0x48: sprintf(buf, "in c,(c)"); break;
        case 0x49: sprintf(buf, "out (c),c"); break;
        case 0x4a: sprintf(buf, "adc hl,bc"); break;
        case 0x4b: nn = filebuf[addr+2] | (filebuf[addr+3]<<8); len=4;
                   fmt_addr(abuf, nn); sprintf(buf, "ld bc,(%s)", abuf); break;
        case 0x4d: sprintf(buf, "reti"); break;
        case 0x4f: sprintf(buf, "ld r,a"); break;
        case 0x50: sprintf(buf, "in d,(c)"); break;
        case 0x51: sprintf(buf, "out (c),d"); break;
        case 0x52: sprintf(buf, "sbc hl,de"); break;
        case 0x53: nn = filebuf[addr+2] | (filebuf[addr+3]<<8); len=4;
                   fmt_addr(abuf, nn); sprintf(buf, "ld (%s),de", abuf); break;
        case 0x56: sprintf(buf, "im 1"); break;
        case 0x57: sprintf(buf, "ld a,i"); break;
        case 0x58: sprintf(buf, "in e,(c)"); break;
        case 0x59: sprintf(buf, "out (c),e"); break;
        case 0x5a: sprintf(buf, "adc hl,de"); break;
        case 0x5b: nn = filebuf[addr+2] | (filebuf[addr+3]<<8); len=4;
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
                   fmt_addr(abuf, nn); sprintf(buf, "ld (%s),sp", abuf); break;
        case 0x78: sprintf(buf, "in a,(c)"); break;
        case 0x79: sprintf(buf, "out (c),a"); break;
        case 0x7a: sprintf(buf, "adc hl,sp"); break;
        case 0x7b: nn = filebuf[addr+2] | (filebuf[addr+3]<<8); len=4;
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
        default: sprintf(buf, "db 0edh,%02xh", op2); break;
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
            fmt_addr(abuf, nn); sprintf(buf, "ld %s,%s", ir, abuf);
        } else if (op2 == 0x22) {
            nn = filebuf[addr+2] | (filebuf[addr+3]<<8); len=4;
            fmt_addr(abuf, nn); sprintf(buf, "ld (%s),%s", abuf, ir);
        } else if (op2 == 0x23) {
            sprintf(buf, "inc %s", ir);
        } else if (op2 == 0x2a) {
            nn = filebuf[addr+2] | (filebuf[addr+3]<<8); len=4;
            fmt_addr(abuf, nn); sprintf(buf, "ld %s,(%s)", ir, abuf);
        } else if (op2 == 0x2b) {
            sprintf(buf, "dec %s", ir);
        } else if (op2 == 0x34) {
            d = filebuf[addr+2]; len=3;
            sprintf(buf, "inc (%s%+d)", ir, (signed char)d);
        } else if (op2 == 0x35) {
            d = filebuf[addr+2]; len=3;
            sprintf(buf, "dec (%s%+d)", ir, (signed char)d);
        } else if (op2 == 0x36) {
            d = filebuf[addr+2]; n = filebuf[addr+3]; len=4;
            sprintf(buf, "ld (%s%+d),%02xh", ir, (signed char)d, n);
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
            sprintf(buf, "ld %s,(%s%+d)", r8[(op2>>3)&7], ir, (signed char)d);
        } else if ((op2 & 0xc0) == 0x40 && ((op2>>3) & 7) == 6) {
            /* ld (ix+d),r */
            d = filebuf[addr+2]; len=3;
            sprintf(buf, "ld (%s%+d),%s", ir, (signed char)d, r8[op2&7]);
        } else if ((op2 & 0xc0) == 0x80 && (op2 & 7) == 6) {
            /* alu (ix+d) */
            d = filebuf[addr+2]; len=3;
            sprintf(buf, "%s(%s%+d)", alu[(op2>>3)&7], ir, (signed char)d);
        } else if (op2 == 0xcb) {
            /* DD/FD CB d op */
            int b;
            static char *rot[] = { "rlc", "rrc", "rl", "rr", "sla", "sra", "sll", "srl" };
            d = filebuf[addr+2];
            op2 = filebuf[addr+3];
            len = 4;
            b = (op2 >> 3) & 7;
            if (op2 < 0x40) {
                sprintf(buf, "%s (%s%+d)", rot[b], ir, (signed char)d);
            } else if (op2 < 0x80) {
                sprintf(buf, "bit %d,(%s%+d)", b, ir, (signed char)d);
            } else if (op2 < 0xc0) {
                sprintf(buf, "res %d,(%s%+d)", b, ir, (signed char)d);
            } else {
                sprintf(buf, "set %d,(%s%+d)", b, ir, (signed char)d);
            }
        } else {
            sprintf(buf, "db %02xh,%02xh", op, op2);
        }
        return len;
    }

    /* main opcodes */
    switch (op) {
    case 0x00: sprintf(buf, "nop"); break;
    case 0x01: nn = filebuf[addr+1] | (filebuf[addr+2]<<8); len=3;
               fmt_addr(abuf, nn); sprintf(buf, "ld bc,%s", abuf); break;
    case 0x02: sprintf(buf, "ld (bc),a"); break;
    case 0x03: sprintf(buf, "inc bc"); break;
    case 0x04: sprintf(buf, "inc b"); break;
    case 0x05: sprintf(buf, "dec b"); break;
    case 0x06: sprintf(buf, "ld b,%02xh", filebuf[addr+1]); len=2; break;
    case 0x07: sprintf(buf, "rlca"); break;
    case 0x08: sprintf(buf, "ex af,af'"); break;
    case 0x09: sprintf(buf, "add hl,bc"); break;
    case 0x0a: sprintf(buf, "ld a,(bc)"); break;
    case 0x0b: sprintf(buf, "dec bc"); break;
    case 0x0c: sprintf(buf, "inc c"); break;
    case 0x0d: sprintf(buf, "dec c"); break;
    case 0x0e: sprintf(buf, "ld c,%02xh", filebuf[addr+1]); len=2; break;
    case 0x0f: sprintf(buf, "rrca"); break;

    case 0x10: rel = filebuf[addr+1]; len=2;
               nn = pc + 2 + rel;
               fmt_addr(abuf, nn); sprintf(buf, "djnz %s", abuf); break;
    case 0x11: nn = filebuf[addr+1] | (filebuf[addr+2]<<8); len=3;
               fmt_addr(abuf, nn); sprintf(buf, "ld de,%s", abuf); break;
    case 0x12: sprintf(buf, "ld (de),a"); break;
    case 0x13: sprintf(buf, "inc de"); break;
    case 0x14: sprintf(buf, "inc d"); break;
    case 0x15: sprintf(buf, "dec d"); break;
    case 0x16: sprintf(buf, "ld d,%02xh", filebuf[addr+1]); len=2; break;
    case 0x17: sprintf(buf, "rla"); break;
    case 0x18: rel = filebuf[addr+1]; len=2;
               nn = pc + 2 + rel;
               fmt_addr(abuf, nn); sprintf(buf, "jr %s", abuf); break;
    case 0x19: sprintf(buf, "add hl,de"); break;
    case 0x1a: sprintf(buf, "ld a,(de)"); break;
    case 0x1b: sprintf(buf, "dec de"); break;
    case 0x1c: sprintf(buf, "inc e"); break;
    case 0x1d: sprintf(buf, "dec e"); break;
    case 0x1e: sprintf(buf, "ld e,%02xh", filebuf[addr+1]); len=2; break;
    case 0x1f: sprintf(buf, "rra"); break;

    case 0x20: rel = filebuf[addr+1]; len=2;
               nn = pc + 2 + rel;
               fmt_addr(abuf, nn); sprintf(buf, "jr nz,%s", abuf); break;
    case 0x21: nn = filebuf[addr+1] | (filebuf[addr+2]<<8); len=3;
               fmt_addr(abuf, nn); sprintf(buf, "ld hl,%s", abuf); break;
    case 0x22: nn = filebuf[addr+1] | (filebuf[addr+2]<<8); len=3;
               fmt_addr(abuf, nn); sprintf(buf, "ld (%s),hl", abuf); break;
    case 0x23: sprintf(buf, "inc hl"); break;
    case 0x24: sprintf(buf, "inc h"); break;
    case 0x25: sprintf(buf, "dec h"); break;
    case 0x26: sprintf(buf, "ld h,%02xh", filebuf[addr+1]); len=2; break;
    case 0x27: sprintf(buf, "daa"); break;
    case 0x28: rel = filebuf[addr+1]; len=2;
               nn = pc + 2 + rel;
               fmt_addr(abuf, nn); sprintf(buf, "jr z,%s", abuf); break;
    case 0x29: sprintf(buf, "add hl,hl"); break;
    case 0x2a: nn = filebuf[addr+1] | (filebuf[addr+2]<<8); len=3;
               fmt_addr(abuf, nn); sprintf(buf, "ld hl,(%s)", abuf); break;
    case 0x2b: sprintf(buf, "dec hl"); break;
    case 0x2c: sprintf(buf, "inc l"); break;
    case 0x2d: sprintf(buf, "dec l"); break;
    case 0x2e: sprintf(buf, "ld l,%02xh", filebuf[addr+1]); len=2; break;
    case 0x2f: sprintf(buf, "cpl"); break;

    case 0x30: rel = filebuf[addr+1]; len=2;
               nn = pc + 2 + rel;
               fmt_addr(abuf, nn); sprintf(buf, "jr nc,%s", abuf); break;
    case 0x31: nn = filebuf[addr+1] | (filebuf[addr+2]<<8); len=3;
               fmt_addr(abuf, nn); sprintf(buf, "ld sp,%s", abuf); break;
    case 0x32: nn = filebuf[addr+1] | (filebuf[addr+2]<<8); len=3;
               fmt_addr(abuf, nn); sprintf(buf, "ld (%s),a", abuf); break;
    case 0x33: sprintf(buf, "inc sp"); break;
    case 0x34: sprintf(buf, "inc (hl)"); break;
    case 0x35: sprintf(buf, "dec (hl)"); break;
    case 0x36: sprintf(buf, "ld (hl),%02xh", filebuf[addr+1]); len=2; break;
    case 0x37: sprintf(buf, "scf"); break;
    case 0x38: rel = filebuf[addr+1]; len=2;
               nn = pc + 2 + rel;
               fmt_addr(abuf, nn); sprintf(buf, "jr c,%s", abuf); break;
    case 0x39: sprintf(buf, "add hl,sp"); break;
    case 0x3a: nn = filebuf[addr+1] | (filebuf[addr+2]<<8); len=3;
               fmt_addr(abuf, nn); sprintf(buf, "ld a,(%s)", abuf); break;
    case 0x3b: sprintf(buf, "dec sp"); break;
    case 0x3c: sprintf(buf, "inc a"); break;
    case 0x3d: sprintf(buf, "dec a"); break;
    case 0x3e: sprintf(buf, "ld a,%02xh", filebuf[addr+1]); len=2; break;
    case 0x3f: sprintf(buf, "ccf"); break;

    case 0x76: sprintf(buf, "halt"); break;

    case 0xc0: sprintf(buf, "ret nz"); break;
    case 0xc1: sprintf(buf, "pop bc"); break;
    case 0xc2: nn = filebuf[addr+1] | (filebuf[addr+2]<<8); len=3;
               fmt_addr(abuf, nn); sprintf(buf, "jp nz,%s", abuf); break;
    case 0xc3: nn = filebuf[addr+1] | (filebuf[addr+2]<<8); len=3;
               fmt_addr(abuf, nn); sprintf(buf, "jp %s", abuf); break;
    case 0xc4: nn = filebuf[addr+1] | (filebuf[addr+2]<<8); len=3;
               fmt_addr(abuf, nn); sprintf(buf, "call nz,%s", abuf); break;
    case 0xc5: sprintf(buf, "push bc"); break;
    case 0xc6: sprintf(buf, "add a,%02xh", filebuf[addr+1]); len=2; break;
    case 0xc7: sprintf(buf, "rst 00h"); break;
    case 0xc8: sprintf(buf, "ret z"); break;
    case 0xc9: sprintf(buf, "ret"); break;
    case 0xca: nn = filebuf[addr+1] | (filebuf[addr+2]<<8); len=3;
               fmt_addr(abuf, nn); sprintf(buf, "jp z,%s", abuf); break;
    case 0xcc: nn = filebuf[addr+1] | (filebuf[addr+2]<<8); len=3;
               fmt_addr(abuf, nn); sprintf(buf, "call z,%s", abuf); break;
    case 0xcd: nn = filebuf[addr+1] | (filebuf[addr+2]<<8); len=3;
               fmt_addr(abuf, nn); sprintf(buf, "call %s", abuf); break;
    case 0xce: sprintf(buf, "adc a,%02xh", filebuf[addr+1]); len=2; break;
    case 0xcf: sprintf(buf, "rst 08h"); break;

    case 0xd0: sprintf(buf, "ret nc"); break;
    case 0xd1: sprintf(buf, "pop de"); break;
    case 0xd2: nn = filebuf[addr+1] | (filebuf[addr+2]<<8); len=3;
               fmt_addr(abuf, nn); sprintf(buf, "jp nc,%s", abuf); break;
    case 0xd3: sprintf(buf, "out (%02xh),a", filebuf[addr+1]); len=2; break;
    case 0xd4: nn = filebuf[addr+1] | (filebuf[addr+2]<<8); len=3;
               fmt_addr(abuf, nn); sprintf(buf, "call nc,%s", abuf); break;
    case 0xd5: sprintf(buf, "push de"); break;
    case 0xd6: sprintf(buf, "sub %02xh", filebuf[addr+1]); len=2; break;
    case 0xd7: sprintf(buf, "rst 10h"); break;
    case 0xd8: sprintf(buf, "ret c"); break;
    case 0xd9: sprintf(buf, "exx"); break;
    case 0xda: nn = filebuf[addr+1] | (filebuf[addr+2]<<8); len=3;
               fmt_addr(abuf, nn); sprintf(buf, "jp c,%s", abuf); break;
    case 0xdb: sprintf(buf, "in a,(%02xh)", filebuf[addr+1]); len=2; break;
    case 0xdc: nn = filebuf[addr+1] | (filebuf[addr+2]<<8); len=3;
               fmt_addr(abuf, nn); sprintf(buf, "call c,%s", abuf); break;
    case 0xde: sprintf(buf, "sbc a,%02xh", filebuf[addr+1]); len=2; break;
    case 0xdf: sprintf(buf, "rst 18h"); break;

    case 0xe0: sprintf(buf, "ret po"); break;
    case 0xe1: sprintf(buf, "pop hl"); break;
    case 0xe2: nn = filebuf[addr+1] | (filebuf[addr+2]<<8); len=3;
               fmt_addr(abuf, nn); sprintf(buf, "jp po,%s", abuf); break;
    case 0xe3: sprintf(buf, "ex (sp),hl"); break;
    case 0xe4: nn = filebuf[addr+1] | (filebuf[addr+2]<<8); len=3;
               fmt_addr(abuf, nn); sprintf(buf, "call po,%s", abuf); break;
    case 0xe5: sprintf(buf, "push hl"); break;
    case 0xe6: sprintf(buf, "and %02xh", filebuf[addr+1]); len=2; break;
    case 0xe7: sprintf(buf, "rst 20h"); break;
    case 0xe8: sprintf(buf, "ret pe"); break;
    case 0xe9: sprintf(buf, "jp (hl)"); break;
    case 0xea: nn = filebuf[addr+1] | (filebuf[addr+2]<<8); len=3;
               fmt_addr(abuf, nn); sprintf(buf, "jp pe,%s", abuf); break;
    case 0xeb: sprintf(buf, "ex de,hl"); break;
    case 0xec: nn = filebuf[addr+1] | (filebuf[addr+2]<<8); len=3;
               fmt_addr(abuf, nn); sprintf(buf, "call pe,%s", abuf); break;
    case 0xee: sprintf(buf, "xor %02xh", filebuf[addr+1]); len=2; break;
    case 0xef: sprintf(buf, "rst 28h"); break;

    case 0xf0: sprintf(buf, "ret p"); break;
    case 0xf1: sprintf(buf, "pop af"); break;
    case 0xf2: nn = filebuf[addr+1] | (filebuf[addr+2]<<8); len=3;
               fmt_addr(abuf, nn); sprintf(buf, "jp p,%s", abuf); break;
    case 0xf3: sprintf(buf, "di"); break;
    case 0xf4: nn = filebuf[addr+1] | (filebuf[addr+2]<<8); len=3;
               fmt_addr(abuf, nn); sprintf(buf, "call p,%s", abuf); break;
    case 0xf5: sprintf(buf, "push af"); break;
    case 0xf6: sprintf(buf, "or %02xh", filebuf[addr+1]); len=2; break;
    case 0xf7: sprintf(buf, "rst 30h"); break;
    case 0xf8: sprintf(buf, "ret m"); break;
    case 0xf9: sprintf(buf, "ld sp,hl"); break;
    case 0xfa: nn = filebuf[addr+1] | (filebuf[addr+2]<<8); len=3;
               fmt_addr(abuf, nn); sprintf(buf, "jp m,%s", abuf); break;
    case 0xfb: sprintf(buf, "ei"); break;
    case 0xfc: nn = filebuf[addr+1] | (filebuf[addr+2]<<8); len=3;
               fmt_addr(abuf, nn); sprintf(buf, "call m,%s", abuf); break;
    case 0xfe: sprintf(buf, "cp %02xh", filebuf[addr+1]); len=2; break;
    case 0xff: sprintf(buf, "rst 38h"); break;

    default:
        /* ld r,r' and alu r patterns */
        if (op >= 0x40 && op < 0x80) {
            sprintf(buf, "ld %s,%s", r8[(op>>3)&7], r8[op&7]);
        } else if (op >= 0x80 && op < 0xc0) {
            sprintf(buf, "%s%s", alu[(op>>3)&7], r8[op&7]);
        } else {
            sprintf(buf, "db %02xh", op);
        }
        break;
    }

    return len;
}

/*
 * disassemble text segment
 */
void
disassemble(start, size)
long start;
int size;
{
    int pc = 0;
    char buf[64];
    char *sym;
    int len, i;

    printf("\nDisassembly: %d bytes\n", size);

    while (pc < size) {
        /* check for symbol at this address */
        sym = sym_lookup(pc, 1);
        if (sym) {
            printf("%s:\n", sym);
        }

        len = disasm(start + pc, pc, buf);

        /* print address and bytes */
        printf("  %04x  ", pc);
        for (i = 0; i < 4; i++) {
            if (i < len)
                printf("%02x ", filebuf[start + pc + i]);
            else
                printf("   ");
        }
        printf(" %s\n", buf);

        pc += len;
    }
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
    case 4: return 0;   /* abs */
    case 5: return 1;   /* text */
    case 6: return 2;   /* data */
    case 7: return 3;   /* bss */
    default: return 4;  /* extern/undef */
    }
}

/*
 * hex dump a region
 */
void
hexdump(name, start, size)
char *name;
long start;
int size;
{
    int i, j;
    unsigned char c;

    if (size == 0) {
        printf("\n%s: (empty)\n", name);
        return;
    }

    printf("\n%s: %d bytes\n", name, size);

    for (i = 0; i < size; i += 16) {
        printf("  %04x: ", i);

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
    int seg;
    long off;

    num_syms = symtab_size / (symlen + 3);

    printf("\nSymbol table: %d symbols (%d-char names)\n", num_syms, symlen);
    printf("  Index  Value  Type Seg    Global  Name\n");
    printf("  -----  -----  ---- -----  ------  ----\n");

    off = symtab_off;
    for (i = 0; i < num_syms; i++) {
        val = get_word(off);
        type = get_byte(off + 2);
        memcpy(name, &filebuf[off + 3], symlen);
        name[symlen] = '\0';

        seg = decode_seg(type);

        printf("  [%3d]  %04x   %02x   %-5s  %s      %s\n",
               i, val, type, segnames[seg],
               (type & 0x08) ? "yes" : "no ",
               name);

        off += symlen + 3;
    }
}

/*
 * dump relocation table
 */
void
dump_relocs(name, reloc_off, symlen, num_syms)
char *name;
long reloc_off;
int symlen;
int num_syms;
{
    long off = reloc_off;
    int pos = 0;
    unsigned char b;
    int bump, idx;
    int count = 0;

    printf("\n%s relocations:\n", name);
    printf("  Offset  Type        Target\n");
    printf("  ------  ----        ------\n");

    while (off < filesize) {
        b = get_byte(off++);
        if (b == 0) break;  /* end of relocs */

        /* decode bump or control */
        if (b < 32) {
            pos += b;
        } else if (b < 64) {
            bump = ((b - 32) << 8) + get_byte(off++) + 32;
            pos += bump;
        } else {
            /* control byte - relocation type */
            char *target;
            char symbuf[32];

            if (b == 0x40) {
                target = "absolute";
            } else if (b == 0x44) {
                target = "text segment";
            } else if (b == 0x48) {
                target = "data segment";
            } else if (b == 0x4c) {
                target = "bss segment";
            } else if (b >= 0x50 && b < 0xfc) {
                idx = (b - 0x50) >> 2;
                sprintf(symbuf, "symbol[%d]", idx);
                target = symbuf;
            } else if (b == 0xfc) {
                b = get_byte(off++);
                if (b < 0x80) {
                    idx = b + 47 - 4;
                } else {
                    idx = ((b - 0x80) << 8) + get_byte(off++) + 175 - 4;
                }
                sprintf(symbuf, "symbol[%d] (ext)", idx);
                target = symbuf;
            } else {
                sprintf(symbuf, "unknown (0x%02x)", b);
                target = symbuf;
            }

            printf("  %04x    word        %s\n", pos, target);
            pos += 2;
            count++;
        }
    }

    if (count == 0) {
        printf("  (none)\n");
    }

    /* return next offset */
}

/*
 * process object at given offset in filebuf
 * objsize is the size limit for this object (for bounds checking)
 */
void
process_object(name, base, objsize)
char *name;
long base;
long objsize;
{
    unsigned char magic, config;
    int symlen;
    unsigned short symtab_size, text_size, data_size, bss_size;
    unsigned short heap_size, text_off, data_off;
    long symtab_off, text_reloc_off, data_reloc_off;
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

    /* hex dump or disassemble segments */
    if (dflag && text_size > 0) {
        disassemble(base + 16, text_size);
    } else {
        hexdump("Text segment", base + 16, text_size);
    }
    hexdump("Data segment", base + 16 + text_size, data_size);

    /* dump symbol table */
    if (symtab_size > 0) {
        dump_symbols(symtab_off, symtab_size, symlen);
    } else {
        printf("\nSymbol table: (none)\n");
    }

    /* dump relocations */
    if (!(config & CONF_NORELO)) {
        text_reloc_off = symtab_off + symtab_size;

        dump_relocs("Text", text_reloc_off, symlen, num_syms);

        /* find data reloc offset by scanning past text relocs */
        off = text_reloc_off;
        while (off < limit) {
            unsigned char b = get_byte(off++);
            if (b == 0) break;
            if (b >= 32 && b < 64) off++;
            else if (b == 0xfc) {
                b = get_byte(off++);
                if (b >= 0x80) off++;
            }
        }
        data_reloc_off = off;

        dump_relocs("Data", data_reloc_off, symlen, num_syms);
    } else {
        printf("\nRelocations: (stripped)\n");
    }

    printf("\n");
    if (symtab)
        free(symtab);
    symtab = 0;
    nsyms = 0;
}

/*
 * process archive file
 * format: 2-byte magic (0xFF75), then entries of:
 *   14-byte name, 2-byte length, file contents
 * terminated by entry with null name
 */
void
process_archive(filename)
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
            process_object(name, off, len);
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

    /* check for archive or object */
    magic16 = get_word(0);
    if (magic16 == AR_MAGIC) {
        process_archive(filename);
    } else if (get_byte(0) == MAGIC) {
        process_object(filename, 0, filesize);
    } else {
        error2("bad magic", filename);
    }

    free(filebuf);
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
            case 'd':
                dflag++;
                break;
            default:
                fprintf(stderr, "usage: wsnm [-d] file.o [...]\n");
                fprintf(stderr, "  -d    disassemble text segment\n");
                exit(1);
            }
        } else {
            process_file(argv[i]);
            nfiles++;
        }
    }

    if (nfiles == 0) {
        fprintf(stderr, "usage: wsnm [-d] file.o [...]\n");
        fprintf(stderr, "  -d    disassemble text segment\n");
        exit(1);
    }

    return 0;
}

/*
 * vim: tabstop=4 shiftwidth=4 noexpandtab:
 */
