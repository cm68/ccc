/*
 * z80 assembler guts
 *
 * substantially rewritten to remove stuff not needed for a compiler backend
 * or an assembler that is used in conjunction with a preprocessor
 * things removed:  the type machinery, and the odd defl, def syntax
 * the most significant thing removed is any notion of arithmetic expressions
 *
 * another messy feature removed is the local label stuff.
 * 
 * /usr/src/cmd/asz/asm.c 
 *
 * Changed: <2025-11-20 08:09:18 curt>
 *
 * vim: tabstop=4 shiftwidth=4 noexpandtab:
 */
#ifdef linux
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#define INIT
#else
#include <stdio.h>
#define INIT = 0
#endif

#include "asm.h"
#include "wsobj.h"

/*
 * verbosity levels:
 * 1 = file 
 * 2 = pass progress
 * 3 = instructions
 * 4 = allocations/symbols/relocs
 * 5 - tokens
 */

extern int infd;
extern int inbuffd;
extern int lineNum;
extern char *infile;
extern char verbose;
extern char m_flag;

void asm_reset();
void assemble();
unsigned char peekchar();
unsigned char nextchar();
void outbyte();
void outtmp();
unsigned char operand();

/*
 * all token numbers are biased by 0x80, and are unsigned
 * this means that 7 bit ascii characters are literally
 * matched
 */
#define T_BIAS  0x80

#define T_B     (T_BIAS + 0)
#define T_C     (T_BIAS + 1)
#define T_D     (T_BIAS + 2)
#define T_E     (T_BIAS + 3)
#define T_H     (T_BIAS + 4)
#define T_L     (T_BIAS + 5)
#define T_HL_I  (T_BIAS + 6)
#define T_A     (T_BIAS + 7)

#define T_BC    (T_BIAS + 8)
#define T_DE    (T_BIAS + 9)
#define T_HL    (T_BIAS + 10)
#define T_SP    (T_BIAS + 11)
#define T_AF    (T_BIAS + 12)
#define T_IX    (T_BIAS + 13)
#define T_IY    (T_BIAS + 14)

#define T_NZ    (T_BIAS + 15)
#define T_Z     (T_BIAS + 16)
#define T_NC    (T_BIAS + 17)
#define T_CR    (T_BIAS + 18)
#define T_PO    (T_BIAS + 19)
#define T_PE    (T_BIAS + 20)
#define T_P     (T_BIAS + 21)
#define T_M     (T_BIAS + 22)

#define T_IXH   (T_BIAS + 23)
#define T_IXL   (T_BIAS + 24)
#define T_IX_D  (T_BIAS + 25)
#define T_IYH   (T_BIAS + 26)
#define T_IYL   (T_BIAS + 27)
#define T_IY_D  (T_BIAS + 28)

#define T_PLAIN (T_BIAS + 29)   /* an immediate value */
#define T_INDIR (T_BIAS + 30)   /* in indirect immediate */

#define T_SP_I  (T_BIAS + 31)
#define T_BC_I  (T_BIAS + 32)
#define T_DE_I  (T_BIAS + 33)
#define T_IX_I  (T_BIAS + 34)
#define T_IY_I  (T_BIAS + 35)

#define T_C_I   (T_BIAS + 36)
#define T_I     (T_BIAS + 37)
#define T_R     (T_BIAS + 38)

#define T_NAME  (T_BIAS + 39)
#define T_NUM   (T_BIAS + 40)
#define T_STR   (T_BIAS + 41)
#define T_EOF   (T_BIAS + 42)

char *tokname[] = {
    /*  0 */ "B", "C", "D", "E", "H", "L", "(HL)", "A",
    /*  8 */ "BC", "DE", "HL", "SP", "AF", "IX", "IY",
    /* 15 */ "NZ", "Z", "NC", "CR", "PO", "PE", "P", "M",
    /* 23 */ "IXH", "IXL", "(IX+d)", "IYH", "IYL", "(IY+d)",
    /* 29 */ "SYMREF", "INDIR", 
    /* 31 */ "(SP)", "(BC)", "(DE)", "(IX)", "(IY)", "(C)", "I", "R",
    /* 39 */ "NAME", "NUM", "STR", "EOF"
};

#define IEND        0   /* end marker */
#define IBASIC      1   /* 1 byte instruction, no args */
#define IBASIC_EXT  2   /* 2 byte instruction, no arg */
#define IARITH      3   /* arithmetic operation group */
#define IINCR       4   /* increment / decrement group */
#define IBITSH      5   /* bit / shift instruction */
#define ISTACK      6   /* stack pop / push */
#define IRET        7   /* return program flow */
#define IJMP        8   /* jump program flow */
#define IJRL        9   /* jump relative program flow */
#define ICALL       10  /* call program flow */
#define IRST        11  /* rst program flow */
#define IIN         12  /* i/o in instruction */
#define IOUT        13  /* i/o out instruction */
#define IEXCH       14  /* exchange instruction */
#define IINTMODE    15  /* interrupt mode instruction */
#define ILOAD       16  /* load instruction */

#define UNARY 0
#define CARRY 1
#define ADD 2

/*
 * operand table
 */
struct oprnd {
	unsigned char token;
	char *mnem;
};

struct oprnd op_table[] = {
	{ T_B, "b" },
	{ T_C, "c" },
	{ T_D, "d" },
	{ T_E, "e" },
	{ T_H, "h" },
	{ T_L, "l" },
	{ T_A, "a" },
	{ T_BC, "bc" },
	{ T_DE, "de" },
	{ T_HL, "hl" },
	{ T_SP, "sp" },
	{ T_AF, "af" },
	{ T_NZ, "nz" },
	{ T_Z, "z" },
	{ T_NC, "nc" },
	{ T_CR, "cr" },
	{ T_PO, "po" },
	{ T_PE, "pe" },
	{ T_P, "p" },
	{ T_M, "m" },
	{ T_IX, "ix" },
	{ T_IY, "iy" },
	{ T_IXH, "ixh" },
	{ T_IXL, "ixl" },
	{ T_IYH, "iyh" },
	{ T_IYL, "iyl" },
	{ T_I, "i" },
	{ T_R, "r" },
	{ 255, "" }
};


/*
 * instruction table
 */
struct instruct {
	char type;
	char *mnem;
	unsigned char opcode;
	unsigned char arg;
};

struct instruct isr_table[] = {
	/* basic instructions */
	{ IBASIC, "nop", 0x00, 0 },
	{ IBASIC, "rlca", 0x07, 0 },
	{ IBASIC, "rrca", 0x0F, 0 },
	{ IBASIC, "rla", 0x17, 0 },
	{ IBASIC, "rra", 0x1F, 0 },
	{ IBASIC, "daa", 0x27, 0 },
	{ IBASIC, "cpl", 0x2F, 0 },
	{ IBASIC, "scf", 0x37, 0 },
	{ IBASIC, "ccf", 0x3F, 0 },
	{ IBASIC, "halt", 0x76, 0 },
	{ IBASIC, "exx", 0xD9, 0 },
	{ IBASIC, "di", 0xF3, 0 },
	{ IBASIC, "ei", 0xFB, 0 },
	
	/* extended basic instructions */
	{ IBASIC_EXT, "neg", 0x44, 0xED },
	{ IBASIC_EXT, "retn", 0x44, 0xED },
	{ IBASIC_EXT, "reti", 0x4D, 0xED },
	{ IBASIC_EXT, "rrd", 0x67, 0xED },
	{ IBASIC_EXT, "rld", 0x6F, 0xED },
	{ IBASIC_EXT, "ldi", 0xA0, 0xED },
	{ IBASIC_EXT, "cpi", 0xA1, 0xED },
	{ IBASIC_EXT, "ini", 0xA2, 0xED },
	{ IBASIC_EXT, "outi", 0xA3, 0xED },
	{ IBASIC_EXT, "ldd", 0xA8, 0xED },
	{ IBASIC_EXT, "cpd", 0xA9, 0xED },
	{ IBASIC_EXT, "ind", 0xAA, 0xED },
	{ IBASIC_EXT, "outd", 0xAB, 0xED },
	{ IBASIC_EXT, "ldir", 0xB0, 0xED },
	{ IBASIC_EXT, "cpir", 0xB1, 0xED },
	{ IBASIC_EXT, "inir", 0xB2, 0xED },
	{ IBASIC_EXT, "otir", 0xB3, 0xED },
	{ IBASIC_EXT, "lddr", 0xB8, 0xED },
	{ IBASIC_EXT, "cpdr", 0xB9, 0xED },
	{ IBASIC_EXT, "indr", 0xBA, 0xED },
	{ IBASIC_EXT, "otdr", 0xBB, 0xED },
	
	/* arithmetic */
	{ IARITH, "add", 0x80, ADD },
	{ IARITH, "adc", 0x88, CARRY },
	{ IARITH, "sub", 0x90, UNARY },
	{ IARITH, "sbc", 0x98, CARRY },
	{ IARITH, "and", 0xA0, UNARY },
	{ IARITH, "xor", 0xA8, UNARY },
	{ IARITH, "or", 0xB0, UNARY },
	{ IARITH, "cp", 0xB8, UNARY },
	
	/* inc / dec */
	{ IINCR, "inc", 0x04, 0x03 },
	{ IINCR, "dec", 0x05, 0x0B },
	
	/* bit / shift */
	{ IBITSH, "rlc", 0x00, 0 },
	{ IBITSH, "rrc", 0x08, 0 },
	{ IBITSH, "rl", 0x10, 0 },
	{ IBITSH, "rr", 0x18, 0 },
	{ IBITSH, "sla", 0x20, 0 },
	{ IBITSH, "sra", 0x28, 0 },
	{ IBITSH, "sll", 0x30, 0 },
	{ IBITSH, "srl", 0x38, 0 },
	{ IBITSH, "bit", 0x40, 1 },
	{ IBITSH, "res", 0x80, 1 },
	{ IBITSH, "set", 0xC0, 1 },
	
	/* stack ops */
	{ ISTACK, "pop", 0xC1, 0 },
	{ ISTACK, "push", 0xC5, 0 },
	
	/* return */
	{ IRET, "ret", 0xC0, 0xC9 },
	
	/* jump */
	{ IJMP, "jp", 0xC2, 0xE9 },
	
	/* jump relative */
	{ IJRL, "jr", 0x18, 1 },
	{ IJRL, "djnz", 0x10, 0},
	
	/* call */
	{ ICALL, "call", 0xC4, 0xCD },
	
	/* rst */
	{ IRST, "rst", 0xC7, 0 },
	
	/* in */
	{ IIN, "in", 0xDB, 0x40 },
	
	/* out */
	{ IOUT, "out", 0xD3, 0x41 },
	
	/* exchange */
	{ IEXCH, "ex", 0xE3, 0x08 },
	
	/* interrupt mode */
	{ IINTMODE, "im", 0x46, 0x5E },
	
	/* load instructions */
	{ ILOAD, "ld", 0x00, 0x00 },
	
	{ IEND, "", 0x00, 0x00}
};

#define TOKLEN 19

/* use wsSegNames from wsobj.c */
#define segname wsSegNames

/*
 * symbols come in a couple of flavors that are driven by
 * the assembler semantics:
 *
 * global symbols are exported to the object file, but can
 * have relocations referring to them.
 *
 * extern symbols are also found in the object file, and
 * are very likely to have relocations referring to them
 *
 * static symbols are not exported to the object file, but
 * are also likely to have relocations applied to them. these
 * relocations in the object file are implemented at segment
 * offsets.  they also are likely to start out unresolved
 * until they find definitions
 *
 * symbols are created when encountered, and usually it's a
 * forward reference without any information other than the
 * name.
 *
 * symbols that are intended to be in the object file get 
 * assigned an index in pass 1 of 0, otherwise 0xffff.
 *
 */
struct symbol {
    unsigned char seg;              /* SEG_* */
    unsigned short index;           /* object file ordinal */
    unsigned short value;           /* segment relative */
    char name[SYMLEN+1];			/* zero padded */
    struct symbol *next;
};

/*
 * relocs are chained off of headers and need to stay
 * ordered.
 */
#define RELOC_WORD  0       /* full 16-bit relocation */
#define RELOC_LO    1       /* low byte only */
#define RELOC_HI    2       /* high byte only */

struct reloc {
    unsigned short addr;    /* where the fixup goes */
    struct symbol *sym;     /* what it contains */
    unsigned char hilo;     /* RELOC_WORD/LO/HI */
    struct reloc *next;
};

struct rhead {
    char *segment;
    struct reloc *head;
    struct reloc *tail;
};

/*
 * expressions can take values of this:
 * if both sym and num are present, this is a biased symbol
 * something like .dw  foo+34
 */
struct expval {
    struct symbol *sym;
    union {
        unsigned long l;
        unsigned int w;
        unsigned char b;
    } num;
    unsigned char hilo;     /* RELOC_WORD/LO/HI */
};

unsigned char *lineptr = (unsigned char *)"";
unsigned char linebuf[256];
#define FILEBUFSIZE 512
unsigned char filebuf[FILEBUFSIZE+1];

/*
 * token buffer 
 */
char token_buf[TOKLEN] INIT;
char sym_name[TOKLEN] INIT;
unsigned long token_val;
unsigned char cur_token;

/*
 * current assembly address 
 */
unsigned short cur_address INIT;

/*
 * segment tops 
 */
unsigned short text_top INIT;
unsigned short data_top INIT;
unsigned short bss_top INIT;

/*
 * sizes for header
 */
unsigned short text_size INIT;
unsigned short mem_size INIT;
unsigned short data_size INIT;
unsigned short bss_size INIT;

char pass INIT;

char segment INIT;

struct rhead textr = { "text" };
struct rhead datar = { "data" };

/*
 * jump records for jp->jr relaxation
 * only jp instructions with resolvable targets in text segment
 */
struct jump {
    unsigned short addr;        /* address of jp instruction */
    struct symbol *sym;         /* target symbol (NULL for absolute) */
    unsigned short offset;      /* target offset */
    unsigned char cond;         /* condition (T_NZ..T_CR) or 0 for unconditional */
    unsigned char is_jr;        /* 1 if converted to jr */
    struct jump *next;
};

struct jump *jumps INIT;

struct symbol *symbols INIT;
struct symbol *symbols_tail INIT;  /* for append order */

/*
 * if looking at whitespace, skip it
 */
unsigned char
skipwhite()
{
	unsigned char c;

	while(1) {
		c = peekchar();
		if ((c == '\t') || (c == ' ')) {
			c = nextchar();
		} else {
			break;
		}
	}
	return c;
}

/*
 * convert token to register number
 * strips T_BIAS and maps IX/IY registers to H-L range
 */
unsigned char
tok2reg(tok)
unsigned char tok;
{
	tok &= ~T_BIAS;  /* Strip 0x80 bit */

	/* Map IX registers (IXH, IXL, (IX+d)) down to H-L range */
	if (tok >= 23 && tok <= 25) {
		tok -= 19;  /* 23->4, 24->5, 25->6 */
	}
	/* Map IY registers (IYH, IYL, (IY+d)) down to H-L range */
	else if (tok >= 26 && tok <= 28) {
		tok -= 22;  /* 26->4, 27->5, 28->6 */
	}

	return tok;
}

/*
 * checks if a string is equal
 * string a is read as lower case
 */
char
match(a, b)
char *a;
char *b;
{
	char lower;

	while (*b) {
		lower = *a;
		if (lower >= 'A' && lower <= 'Z')
			lower += 'a' - 'A';

		if (*a != *b)
			return 0;

		a++;
		b++;
	}

	return *a == *b;
}

/*
 * prints out an error message and exits
 *
 * msg = error message
 */
void
gripe(msg)
char *msg;
{
	printf("%s:%d %s\n%s", 
        infile, lineNum, msg, linebuf);
	exit(1);
}

void
gripe2(msg, arg)
char *msg;
char *arg;
{
	printf("%s:%d %s%s\n%s", 
        infile, lineNum, msg, arg, linebuf);
	exit(1);
}

void
save_symn()
{
	int i;
    char c;

	for (i = 0; i < SYMLEN; i++) {
        c = token_buf[i];
		sym_name[i] = c;
        if (!c) break;
    }
    sym_name[i] = '\0';
}

/*
 * is this an alphabetic or underscore
 */
char
alpha(in)
char in;
{
	return (in >= 'A' && in <= 'Z') || (in >= 'a' && in <= 'z')
		|| in == '_';
}

/*
 * is this a valid inner symbol character (includes '.')
 */
char
symchar(in)
char in;
{
	return alpha(in) || (in >= '0' && in <= '9') || in == '.';
}

/*
 * converts an escaped char into its value
 *
 * \[bernetv] = c escape for control chars
 * \000 (octal) = 
 * \<anything else> = same
 */
char
escape()
{
    char c;
    int i = 0;

    c = nextchar();
	switch (c) {
	case 'b':
		return '\b';
	case 'e':
		return 0x1B;
	case 'r':
		return '\r';
	case 'n':
		return '\n';
	case 't':
		return '\t';
	case 'v':
		return 0x0B;
    case '0': case '1': case '2': case '3':
    case '4': case '5': case '6': case '7':
        i = c - '0';
        while (1) {
            c = peekchar();
            if (c > '7' || c < '0') break;
            c = nextchar();
            i = (i << 3) + c - '0';
        }
        return i;    
	default:
		return c;
	}
}

/*
 * parse number - handles quite a few formats:
 * decimal: 20, 78
 * hex:  0x0, 0X00, 000H, 00h
 * octal: 06, 003, 05o, 06O
 * binary: 0b0001010 000100B 01010b
 */
unsigned long
parsenum(s)
char *s;
{
    int i = strlen(s);
    unsigned long val = 0;
    int base = 10;
    char c;

    /* Check 0x/0b prefix FIRST (before trailing radix check) */
    if (*s == '0' && s[1]) {
        c = s[1] | 0x20;
        if (c == 'x') {
            base = 16;
            s += 2;
        } else if (c == 'b') {
            base = 2;
            s += 2;
        } else {
            /* Leading 0 but not 0x/0b - check for trailing radix or octal */
            c = s[i-1] | 0x20;
            if (c == 'h') {
                base = 16;
                s[i-1] = '\0';
            } else if (c == 'o') {
                base = 8;
                s[i-1] = '\0';
            } else if (c == 'b') {
                base = 2;
                s[i-1] = '\0';
            } else {
                base = 8;
                s++;
            }
        }
    } else {
        /* No leading 0 - check for trailing radix marker */
        c = s[i-1] | 0x20;
        if (c == 'h') {
            base = 16;
            s[i-1] = '\0';
        } else if (c == 'o') {
            base = 8;
            s[i-1] = '\0';
        } else if (c == 'b') {
            base = 2;
            s[i-1] = '\0';
        }
    }

    while (*s) {
        int d;
        val *= base;
        c = *s | 0x20;
        d = c - '0';
        if ((base == 16) && (d > 9))
            d -= ('a' - '0') - 10;
        if ((d >= base) || (d < 0)) {
            gripe("numeric digit out of range");
        }
        val += d;
        s++;
    }
    return val;
}

unsigned char *limit = 0;
unsigned char *inptr = 0;

int
fillbuf()
{
    int i;

    i = read(infd, filebuf, FILEBUFSIZE);
    if (i < 0) {
        gripe("io error on read"); 
    } else if (i == 0) {
        return 0;
    } else {
        inptr = filebuf;
        limit = &filebuf[i];
    }
    if (pass == 0 && infd == 0) {
        write(inbuffd, filebuf, i);
    }
    return i;
}

/*
 * read an entire line into a null-terminated C string
 * the line will end with a newline.
 * comments and trailing whitespace are stripped.
 */
void
get_line()
{
    int i;
    unsigned char c;
    unsigned char *p;

    lineptr = linebuf;
	for (i = 0; i < sizeof(linebuf); i++) {
        if (inptr >= limit) {
            if (fillbuf() == 0) {
                *lineptr++ = T_EOF;
                break;
            }
        }
        c = *inptr++;
		*lineptr++ = c;
		if (c == '\n') {
			break;
		}
	}
	*lineptr = '\0';
    lineNum++;

    /* strip comments: find ; outside of strings and truncate */
    {
        int in_string = 0;
        for (p = linebuf; *p; p++) {
            if (*p == '"' && (p == linebuf || p[-1] != '\\')) {
                in_string = !in_string;
            } else if (*p == ';' && !in_string) {
                *p++ = '\n';
                *p = '\0';
                break;
            }
        }
    }

    /* strip trailing whitespace before newline */
    i = strlen((char *)linebuf);
    while (i >= 2 && (linebuf[i - 2] == ' ' || linebuf[i - 2] == '\t' ||
                      linebuf[i - 2] == '\r')) {
        linebuf[i - 2] = '\n';
        linebuf[i - 1] = '\0';
        i--;
    }

	lineptr = linebuf;
}

/*
 * get the next character that we would read, but don't advance
 */
unsigned char
peekchar()
{
    unsigned char c;

    c = *lineptr;
    if (verbose > 5)
        printf("peekchar: %d \'%c\'\n", c, (c > ' ') ? c : ' ');
    return (c);
}

char *tokenname(t)
unsigned char t;
{
    static char tbuf[30];
    if (t < ' ') {
        switch (t) {
        case '\t':
            sprintf(tbuf, "\\t");
            break;
        case '\n':
            sprintf(tbuf, "\\n");
            break;
        default:
            sprintf(tbuf, "\\%o", t); 
            break;
        }
    } else if (t < T_BIAS) {
        sprintf(tbuf, "%c", t);
    } else {
        return tokname[t - T_BIAS];
    }
    return tbuf;
}

/*
 * returns the next character in the source, or -1 if complete
 */
unsigned char
nextchar()
{
    unsigned char c;

    if (!*lineptr)
        get_line();
    
    c = *lineptr;
    if (c != T_EOF) {
        lineptr++;
    }

    return c;
}

/*
 * consumes to end of line
 */
void
consume()
{
    *lineptr = '\0';
}

/*
 * the lexer. 
 *
 * really quite sloppy.  the notion of what a token is
 * is quite imprecise.  really, what this is a input
 * scanner that returns special character codes for
 * recognized strings of related characters.
 *
 * [a-zA-Z_0-9]+ -> T_NAME, token_buf filled
 * [digits]+ '\escape' 'c' -> T_NUM, token_val filled
 * "string" -> T_STR, token_buf filled
 *
 * anything else passes as the character
 * finally, return 0 if end of line
 * and -1 for end of file
 * NB: ambiguity: how is ABBAH parsed?  we call it a NAME.
 * to make it a number, prefix it with 0. 0ABBAH.
 */
void
get_token()
{
    int i = 0;
    unsigned char c;

    /* skip over whitespace (comments stripped in get_line) */
    while (1) {
		/* ensure buffer has content */
		if (!*lineptr) {
			get_line();
		}

		c = skipwhite();

        if (c == T_EOF) {
            cur_token = T_EOF;
            return;
        }
        break;
    }

    /* if it looks like a symbol, fill it */
    if (alpha(c)) {
        token_buf[i++] = nextchar();
        while (1) {
            c = peekchar();
            if (symchar(c)) {
                token_buf[i++] = nextchar();
            } else {
                break;
            }
        }
        token_buf[i++] = '\0';
        if (i > (m_flag ? 10 : 16)) {  /* >9 or >15 chars plus null terminator */
            printf("%s:%d warning: symbol '%s' longer than %d characters\n",
                   infile, lineNum, token_buf, m_flag ? 9 : 15);
        }
        c = T_NAME;
    }

    /* numbers can have radix info, so look for a delimiter */
    else if ((c >= '0') && (c <= '9')) {
        token_buf[i++] = nextchar();
        while (1) {
            c = peekchar();
            if ((c == ')') || (c == ',') || (c == ' ') || 
                (c == '\t') || (c == '\n') || (c == T_EOF) ||
                (c == '+') || (c == '-')) {
                break;
            }
            token_buf[i++] = nextchar();
        }
        token_buf[i++] = '\0';
        token_val = parsenum(token_buf);
        c = T_NUM;
    }

    /*
     * literal character in quotes is a number
     * but standalone ' is returned as itself (for af')
     */
    else if (c == '\'') {
        nextchar();  /* consume the ' */
        i = peekchar();
        if (i == '\n' || i == ' ' || i == '\t' || i == -1) {
            /* standalone ' - return as itself */
            /* c is already '\'' */
        } else {
            /* character literal 'X' */
            token_val = nextchar();
            if (token_val == '\\') {
                token_val = escape();
            }
            if (nextchar() != '\'') {
                gripe("unterminated char literal");
            }
            c = T_NUM;
        }
    }

    /*
     * literal string detected - just parse into token_buf
     */
    else if (c == '\"') {
        while (1) {
            c = nextchar();
            if (c == '\n') {
                gripe("unterminated string");
            }
            if (c == '\"') {
                break;
            }
            if (c == '\\') {
                c = escape();
            }
            token_buf[i++] = c;
        }
        token_buf[i++] = '\0';
        c = T_STR;
    }

    /*
     * Single-character token (operators, punctuation, etc.)
     * Need to consume the character that skipwhite() peeked at
     */
    else {
        c = nextchar();
    }

    cur_token = c;

    if (verbose > 5) {
        printf("get_token: %d %s", c, tokenname(c));
        if (c == T_NAME) {
            printf(":%s", token_buf);
        } else if (c == T_NUM) {
            printf(":0x%lx", token_val);
        }
        printf("\n");
    }
	return;
}

/*
 * require a specific token
 */
void
need(c)
unsigned char c;
{
	get_token();

	if (cur_token != c) {
        char s[20];
        sprintf(s, " %d", c);
		gripe2("expected character", s);
	}
}

/*
 * fetches the symbol
 * returns pointer to found symbol, or null
 */
struct symbol *
sym_fetch(name)
char *name;
{
	struct symbol *sym;
	int i;
	char equal;

	for (sym = symbols; sym; sym = sym->next) {

		equal = 1;
		for (i = 0; i < SYMLEN; i++) {
			if (sym->name[i] != name[i])
				equal = 0;
			if (!sym->name[i])
				break;
		}
		if (equal)
			return sym;
	}
	return NULL;
}

/*
 * defines or redefines a symbol
 */
struct symbol *
sym_update(name, seg, value, visible)
char *name;
short seg;
unsigned short value;
int visible;
{
	struct symbol *sym;
	int i;

	sym = sym_fetch(name);

	if (!sym) {
		sym = (struct symbol *) malloc(sizeof(struct symbol));
		sym->next = 0;
		/* append to preserve first-reference order */
		if (symbols_tail)
			symbols_tail->next = sym;
		else
			symbols = sym;
		symbols_tail = sym;
		sym->seg = SEG_UNDEF;
		sym->index = 0xffff;
		for (i = 0; i < SYMLEN && name[i]; i++)
			sym->name[i] = name[i];
		sym->name[i] = 0;
	}

	if ((sym->seg != SEG_UNDEF) && (seg == SEG_UNDEF)) {
		/* Symbol already defined, just marking visible - preserve value */
		if (visible) sym->index = 0;
		return sym;
	}

	/*
	 * update the symbol
	 */
    if ((sym->seg != SEG_UNDEF) &&
        (sym->seg != seg)) {
		printf("pass: %d from: %s to: %s\n", pass, segname[sym->seg], segname[seg]);
        gripe2("segment for symbol changed", name);
    }
	sym->seg = seg;
	sym->value = value;
    if (visible) sym->index = 0;
	return sym;
}

void
freerelocs(rh)
struct rhead *rh;
{
    struct reloc *r, *n;

    for (r = rh->head; r;) {
        n = r->next;
        free(r);
        r = n;
    }

    rh->tail = 0;
    rh->head = 0;
}

void
freejumps()
{
    struct jump *j, *n;

    for (j = jumps; j;) {
        n = j->next;
        free(j);
        j = n;
    }
    jumps = 0;
}

/*
 * resets all allocation stuff
 * this is what we run between assemblies.
 * it should clean out everything.
 */
void
asm_reset()
{
    struct symbol *s, *n;

    for (s = symbols; s;) {
        n = s->next;
        free(s);
        s = n;
    }
    freerelocs(&textr);
    freerelocs(&datar);
    freejumps();
}

/*
 * adds an reference into a relocation table
 * we only do this in the second pass, since that's when
 * all symbols and segment addresses are resolved
 */
void
add_reloc(tab, addr, sym, hilo)
struct rhead *tab;
unsigned short addr;
struct symbol *sym;
unsigned char hilo;
{
	struct reloc *r;

	if (!pass)
		return;

    if (verbose > 2)
        printf("add_reloc: %s %x %s %s\n",
            tab->segment, addr, sym ? sym->name : "nosym",
            hilo == RELOC_HI ? "hi" : hilo == RELOC_LO ? "lo" : "word");

    if (sym->seg == SEG_ABS)
        return;

    if (sym->seg == SEG_UNDEF)
        return;

    r = (struct reloc *) malloc(sizeof(struct reloc));

	r->addr = addr;
    r->sym = sym;
    r->hilo = hilo;
	r->next = 0;

	if (!tab->head) {
		tab->tail = tab->head = r;
	} else {
		tab->tail->next = r;
	}
	tab->tail = r;
}

/*
 * record a jp instruction for potential conversion to jr
 * only in pass 0, only for text segment
 */
void
add_jump(addr, sym, offset, cond)
unsigned short addr;
struct symbol *sym;
unsigned short offset;
unsigned char cond;
{
    struct jump *j;

    if (pass != 0)
        return;
    if (segment != SEG_TEXT)
        return;

    j = (struct jump *)malloc(sizeof(struct jump));
    j->addr = addr;
    j->sym = sym;
    j->offset = offset;
    j->cond = cond;
    j->is_jr = 0;
    j->next = jumps;
    jumps = j;
}

/*
 * find jump record for address
 */
struct jump *
find_jump(addr)
unsigned short addr;
{
    struct jump *j;

    for (j = jumps; j; j = j->next) {
        if (j->addr == addr)
            return j;
    }
    return 0;
}

/*
 * relax jp instructions to jr where possible
 * iterate until no more changes
 * jr only supports conditions NZ, Z, NC, C (not PO, PE, P, M)
 */
void
relax_jmp()
{
    struct jump *j, *k;
    struct symbol *s;
    int changed;
    int target, dist;
    int saved = 0;
    unsigned short conv_addr;

    /* -8 flag disables relaxation (8080 mode) */
    if (no_relax)
        return;

    if (verbose > 1)
        printf("relaxing jumps\n");

    do {
        changed = 0;

        for (j = jumps; j; j = j->next) {
            if (j->is_jr)
                continue;

            /* jr only supports NZ, Z, NC, C (conditions 0-3) */
            /* cond==0 means unconditional, T_NZ..T_CR are conditions 0-3 */
            if (j->cond != 0 && (j->cond < T_NZ || j->cond > T_CR))
                continue;

            /* calculate target address */
            if (j->sym) {
                /* symbol must be defined and in text segment */
                if (j->sym->seg == SEG_UNDEF || j->sym->seg == SEG_EXT)
                    continue;
                if (j->sym->seg != SEG_TEXT)
                    continue;
                target = j->sym->value + j->offset;
            } else {
                target = j->offset;
            }

            /* jr offset is from PC after the 2-byte jr instruction */
            /* jp is 3 bytes, so the address field is at addr+1 */
            /* if we convert to jr, offset is from addr+2 */
            dist = target - (j->addr + 2);

            if (dist >= -128 && dist <= 127) {
                j->is_jr = 1;
                changed = 1;
                saved++;
                conv_addr = j->addr;

                if (verbose > 2)
                    printf("  convert jp at %04x to jr (target %04x, dist %d)\n",
                           j->addr, target, dist);

                /* adjust all symbols after this jp */
                for (s = symbols; s; s = s->next) {
                    if (s->seg == SEG_TEXT && s->value > conv_addr)
                        s->value--;
                }

                /* adjust all jump addresses and targets after this jp */
                for (k = jumps; k; k = k->next) {
                    if (k->addr > conv_addr)
                        k->addr--;
                    /* if target is a symbol, it's already adjusted */
                    /* if target is absolute and after this jp, adjust it */
                    if (!k->sym && k->offset > conv_addr)
                        k->offset--;
                }

                /* adjust segment size */
                text_top--;
            }
        }
    } while (changed);

    if (verbose && saved)
        fprintf(stderr, "relaxation: %d bytes saved\n", saved);
}

/*
 * outputs a relocation table to whitesmith's object
 *
 * tab = relocation table
 */
extern int tmpfd;

void
reloc_out(r, base)
struct reloc *r;
unsigned short base;
{
	int last = base;
	int bump;
	int seg;
	int size;

	while (r) {
		seg = r->sym->seg;
		size = (r->hilo == RELOC_WORD) ? 2 : 1;
		if (verbose > 3) {
			printf("reloc: base: %x addr: %x seg: %s(%d) %s %s\n",
				   base, r->addr, segname[seg], seg, r->sym->name,
				   r->hilo == RELOC_HI ? "hi" : r->hilo == RELOC_LO ? "lo" : "");
		}

		bump = r->addr - last;
		if (verbose > 4) {
			printf("bump: %d\n", bump);
		}
		wsEncBump(tmpfd, bump);

		if (seg == SEG_UNDEF) {
			printf("reloc for undef\n");
		} else if (seg >= SEG_TEXT && seg <= SEG_ABS &&
				   r->sym->index == 0xffff) {
			/* local symbol - segment-relative relocation */
			wsEncReloc(tmpfd, seg, 0, r->hilo);
		} else {
			/* global/extern symbol reference */
			wsEncReloc(tmpfd, -1, r->sym->index, r->hilo);
		}
		last += bump + size;
		r = r->next;
	}
	wsEndReloc(tmpfd);
}

/*
 * emits a byte into assembly output
 * no bytes emitted on first pass, only update addresses
 *
 * b = byte to emit
 */
void
emitbyte(b)
unsigned char b;
{
	if (pass == 1) {
		switch (segment) {
		case SEG_TEXT:
			outbyte((char) b);
			break;
		case SEG_DATA:
			outtmp((char) b);
			break;
		case SEG_BSS:
			if (b)
				gripe("data in bss");
			break;
		default:
			break;
		}
	}

	cur_address++;
}

/*
 * emits a little endian word to the binary
 *
 * w = word to emit
 */
void
emitword(w)
unsigned short w;
{
	emitbyte(w & 0xFF);
	emitbyte(w >> 8);
}

/*
 * emits a little endian long (4 bytes) to the binary
 */
void
emitlong(l)
unsigned long l;
{
	emitbyte(l & 0xFF);
	emitbyte((l >> 8) & 0xFF);
	emitbyte((l >> 16) & 0xFF);
	emitbyte((l >> 24) & 0xFF);
}

void
outword(word)
unsigned short word;
{
	outbyte(word & 0xFF);
	outbyte(word >> 8);
}

/*
 * fills a region with either zeros or undefined allocated space
 *
 * size = number of bytes to fill
 */
void
fill(size)
unsigned short size;
{
	if (verbose > 3)
		printf("fill segment: %d for %d\n", segment, size);
	while (size--)
		emitbyte(0);
}

/*
 * emits up to two bytes, and handles relocation tracking
 *
 * size = number of bytes to emit
 * vp = value to push out
 */
void
emit_exp(size, vp)
unsigned short size;
struct expval *vp;
{
	unsigned short rel;
    unsigned char seg;

    if (vp->sym) {
        seg = vp->sym->seg;
    } else {
        seg = SEG_ABS;
    }
	if (seg == SEG_UNDEF) {
		/* if we are on the second pass, error out */
		if (pass == 1)
			gripe2("undefined symbol ", vp->sym->name);
	}

	if (vp->hilo != RELOC_WORD) {
		/*
		 * hi() or lo() byte extraction from symbol
		 */
		unsigned short val = vp->num.w + (vp->sym ? vp->sym->value : 0);
		if (vp->hilo == RELOC_HI)
			val >>= 8;
		emitbyte(val & 0xff);
		if (vp->sym && pass) {
			switch (segment) {
			case SEG_TEXT:
				add_reloc(&textr, cur_address - 1, vp->sym, vp->hilo);
				break;
			case SEG_DATA:
				add_reloc(&datar, cur_address - 1, vp->sym, vp->hilo);
				break;
			default:
				gripe("invalid segment");
			}
		}
	} else if (size == 1) {
		/*
		 * here we output only a byte
		 */
		if ((seg >= SEG_EXT) && (pass == 1))
			gripe("cannot extern byte");

		if (seg == SEG_TEXT) {
			rel = (vp->sym->value - cur_address) - 1;
			if ((rel < 0x80) || (rel > 0xFF7F))
				emitbyte(rel);
			else
				gripe("relative out of bounds");
		} else {
			emitbyte(vp->num.b);
		}

	} else {

		if (vp->sym && pass) {
			switch (segment) {
			case SEG_TEXT:
				add_reloc(&textr, cur_address, vp->sym, RELOC_WORD);
				break;
			case SEG_DATA:
				add_reloc(&datar, cur_address, vp->sym, RELOC_WORD);
				break;
			default:
				gripe("invalid segment");
			}
		}
		/* emit symbol value + offset for internal symbols */
		/* external symbols have value 0, linker fills in */
		emitword(vp->num.w + (vp->sym ? vp->sym->value : 0));
	}
}

/*
 * helper function to emit an immediate and do type checking
 * only absolute resolutions will be allowed, unless hi/lo
 */
void
emit_imm(vp)
struct expval *vp;
{
	if (vp->hilo != RELOC_WORD) {
		/* hi/lo byte extraction - use emit_exp to handle relocation */
		emit_exp(1, vp);
		return;
	}
	if (vp->sym && vp->sym->seg != SEG_ABS && (pass == 1)) {
		printf("sym: %s seg: %s\n", vp->sym->name, segname[vp->sym->seg]);
		gripe("must be absolute");
	}

	emitbyte(vp->num.b);
}

/*
 */
void
db()
{
    unsigned char c;
    struct expval value;

	while (1) {
		c = peekchar();
        if (c == '\n')
            break;
        if (c == T_EOF)
            break;

		c = skipwhite();

		if (c == '"') {
            /* eat the double quote */
            nextchar();

            while (1) {
                c = nextchar();

                if (c == '\n') {
                    gripe("unterminated string constant");
                    break;
                }

                if (c == '\"') {
                    break;
                }

                if (c == '\\') {
                    c = escape();
                }
                emitbyte(c);
            }
		} else {
            c = operand(&value);
            if (c != T_PLAIN) {
                gripe("unexpected value");
            }
			emit_exp(1, &value);
		}
		if (peekchar() != ',')
			break;
		else
			need(',');
	}
}

void
dw()
{
    struct expval value;

	while (peekchar() != '\n' && peekchar() != -1) {
        if (operand(&value) != T_PLAIN) {
            gripe("unexpected value");
        }
		emit_exp(2, &value);
		if (peekchar() != ',')
			break;
		else
			need(',');
	}
}

void
dl()
{
    struct expval value;

	while (peekchar() != '\n' && peekchar() != -1) {
        if (operand(&value) != T_PLAIN) {
            gripe("unexpected value");
        }
		/* No relocation support for longs - just emit the value */
		if (value.sym && pass == 1) {
			gripe("cannot use symbol in .dl");
		}
		emitlong(value.num.l);
		if (peekchar() != ',')
			break;
		else
			need(',');
	}
}

void
ds()
{
    unsigned char c;
    struct expval value;

    c = operand(&value);
    if (c != T_PLAIN && (value.sym != 0)) {
        gripe("ds requires absolute argument");
    }
    fill(value.num.w);
}

/*
 * parses an operand, 
 * returns token describing the argument,
 * populate vp if it's passed in.
 * if the operand is an (ix+d), then the expval is the displacement
 */
unsigned char
operand(vp)
struct expval *vp;
{
	int i;
	char c;
	unsigned char ret;
    int indir = 0;

    vp->num.l = 0;
    vp->sym = 0;
    vp->hilo = RELOC_WORD;

	/*
	 * check if there is anything next
	 */
    c = peekchar();
	if (c == '\n' || c == -1)
		return 255;

	/*
	 * read the token
	 */
	get_token();

	/*
	 * hi() or lo() byte extraction?
	 */
	if (cur_token == T_NAME && (match(token_buf, "hi") || match(token_buf, "lo"))) {
		vp->hilo = (token_buf[0] == 'h') ? RELOC_HI : RELOC_LO;
		need('(');
		get_token();
	}

	/* after skipping whitespace/comments, may be at end of line */
	if (cur_token == '\n')
		return 255;

	/*
	 * maybe a register symbol? sometimes 'c' means carry
	 */
	if (cur_token == T_NAME) {
		for (i = 0; op_table[i].token != 255; i++) {
			if (match(token_buf, op_table[i].mnem)) {
				return op_table[i].token;
			}
		}
	}

	/*
	 * maybe in parenthesis?
	 */
	if (cur_token == '(') {
		get_token();
		if (cur_token == T_NUM) {
			/* numeric indirect like (1234h) */
			indir++;
		} else if (cur_token == T_NAME) {
            if (match(token_buf, "hl")) {
                need(')');
                return T_HL_I;
            } else if (match(token_buf, "c")) {
                need(')');
                return T_C_I;
            } else if (match(token_buf, "sp")) {
                need(')');
                return T_SP_I;
            } else if (match(token_buf, "bc")) {
                need(')');
                return T_BC_I;
            } else if (match(token_buf, "de")) {
                need(')');
                return T_DE_I;
            } else if (match(token_buf, "ix") || match(token_buf, "iy")) {
				/*
				 * (ix+d) (ix-d) (iy+d) (iy-d) 
				 * populate displacement and eat ')'
				 */
				ret = token_buf[1] == 'x' ? T_IX_D : T_IY_D;
				c = skipwhite();
				if ((c == '+') || (c == '-')) {
					get_token();
                	c = cur_token;
                	get_token();
                	if (cur_token != T_NUM) {
                    	gripe("index displacement missing");
                	}
                	if (c == '-') {
                    	vp->num.w = -token_val;
                	} else {
                    	vp->num.w = token_val;
                	}
				} else {
					ret = (ret - T_IX_D) + T_IX_I;
				}
				need(')');
            	return ret;
			} else {
				indir++;
				/* fall through */
			}
		}
	}

    if (cur_token == T_NAME) {
        vp->sym = sym_fetch(token_buf);
        if (!vp->sym) {
            if (pass == 1) {
                gripe2("undefined symbol ", vp->sym->name);
            } else {
                vp->sym = sym_update(token_buf, SEG_UNDEF, 0, 0);
            }
	    }
    } else if (cur_token == T_NUM) {
		vp->num.w = token_val;
    } else if (cur_token == '$') {
		vp->num.w = cur_address;
    } else if (cur_token == '-') {
		get_token();
		if (cur_token == T_NUM) {
			vp->num.w = -token_val;
		} else if (cur_token == '$') {
			vp->num.w = -cur_address;
		} else {
			gripe("expected number or $ after -");
		}
    } else {
        gripe("need an operand");
    }

	c = skipwhite();

	if (c == '+') {
		nextchar();
		get_token();
		if (cur_token == T_NUM) {
			vp->num.w += token_val;
		} else {
			gripe("expected number after +");
		}
	} else if (c == '-') {
		nextchar();
		get_token();
		if (cur_token == T_NUM) {
			vp->num.w -= token_val;
		} else {
			gripe("expected number after -");
		}
	}

    if (indir) {
	    need(')');
        return T_INDIR;
	}
	if (vp->hilo != RELOC_WORD)
		need(')');
	return T_PLAIN;
}

/*
 * load indirect
 */
int
do_stax(vp)
struct expval *vp;
{
	unsigned char arg;
	struct expval value;

	need(',');
	arg = operand(&value);

	switch (arg) {
	case T_HL:					/* ld (nn), hl */
		emitbyte(0x22);
		break;

	case T_A:					/* ld (nn), a */
		emitbyte(0x32);
		break;

	case T_IX:					/* ld (nn), ix */
		emitbyte(0xDD);
		emitbyte(0x22);
		break;

	case T_IY:					/* ld (nn), iy */
		emitbyte(0xFD);
		emitbyte(0x22);
		break;

	case T_BC:					/* ld (nn), bc */
	case T_DE:					/* ld (nn), de */
	case T_SP:					/* ld (nn), sp */
		emitbyte(0xED);
		emitbyte(0x43 + ((arg - T_BC) << 4));
		break;

	default:
		return 1;
	}
	emit_exp(2, vp);
	return 0;
}

/*
 * 16 bit load
 */
int
do_16i(reg)
unsigned char reg;
{
	unsigned char arg;
	struct expval value;

	/*
	 * correct for ix,iy into hl 
	 */
	if (reg == T_IX) {
		emitbyte(0xDD);
		reg = T_HL;
	} else if (reg == T_IY) {
		emitbyte(0xFD);
		reg = T_HL;
	}

	/*
	 * grab a direct or deferred word 
	 */
	need(',');
	arg = operand(&value);

	if (arg == T_PLAIN) {
		/*
		 * ld bc|de|hl|sp, nn 
		 */
		emitbyte(0x01 + ((reg - T_BC) << 4));
		emit_exp(2, &value);
	} else if (arg == T_INDIR) {
		if (reg == T_HL) {
			emitbyte(0x2A);
		} else {
			/*
			 * ld bc|de|sp, (nn) 
			 */
			emitbyte(0xED);
			emitbyte(0x4B + ((reg - T_BC) << 4));
		}
		emit_exp(2, &value);
	} else if (reg == T_SP) {
		/*
		 * ld sp,hl|ix|iy specials 
		 */
		switch (arg) {
		case T_HL:
			emitbyte(0xF9);
			break;
		case T_IX:
			emitbyte(0xDD);
			emitbyte(0xF9);
			break;
		case T_IY:
			emitbyte(0xFD);
			emitbyte(0xF9);
			break;
		default:
			return 1;
		}
	} else
		return 1;
	return 0;
}

/*
 * if there is a passed in expval, it's a displacement for the first arg
 * cases:
 * ld a|b|c|d|e|h|l|(hl)|(ix+d)|(iy+d), a|b|c|d|e|h|l|(hl)|(ix+d)|(iy+d)
 * ld a,(bc)|(de)|(nnnn)|i|r
 */
int
do_ldr8(arg, disp)
unsigned char arg;
struct expval *disp;
{
	unsigned char reg;
    struct expval value;
	struct expval *disp_ptr;
	unsigned char arg_reg, reg_reg;
    value.sym = 0;
    disp->sym = 0;

	disp_ptr = 0;

	if (arg == T_IX_D || arg == T_IY_D) {
        disp_ptr = disp;
	}
	need(',');

	reg = operand(&value);

	if (arg >= T_IXH && arg <= T_IY_D) {
		if (arg <= T_IX_D) {
			emitbyte(0xDD);
            /* lose on ld ix*, iy* or ld ix[hl], (ix+d) */
            if (reg >= T_IYH && reg <= T_IY_D)
                return 1;
            if (arg != T_IX_D && reg == T_IX_D)
                return 1;
		} else {
			emitbyte(0xFD);
            /* lose on ld iy*, ix* or ld iy[hl], (iy+d) */
            if (reg >= T_IXH && reg <= T_IX_D)
                return 1;
            if (arg != T_IY_D && reg == T_IY_D)
                return 1;
		}
	} else if (reg >= T_IXH && reg <= T_IY_D) {
		if (arg == T_HL_I)
			return 1;

		if (reg <= T_IX_D) {
			emitbyte(0xDD);
		} else {
			emitbyte(0xFD);
		}
		if (reg == T_IX_D || reg == T_IY_D) {
            disp_ptr = &value;
		} else if (tok2reg(arg) == 4 || tok2reg(arg) == 5)
            /* lose on ld [hl], ix[hl] */
			return 1;
	}

	/*
	 * no (hl),(hl)
	 */
	if (arg == T_HL_I && reg == T_HL_I)
		return 1;

	/* Convert tokens to register numbers for opcode calculation */
	arg_reg = tok2reg(arg);
	reg_reg = tok2reg(reg);

	if (arg_reg <= 7 && reg_reg <= 7) {
		/* reg8->reg8 */
		emitbyte(0x40 + (arg_reg << 3) + reg_reg);
		if (disp_ptr)
			emit_imm(disp_ptr);
	} else if (arg_reg <= 7 && (reg == T_PLAIN)) {
		/* ld reg8, n */
		emitbyte(0x06 + (arg_reg << 3));
		if (disp_ptr)
			emit_imm(disp_ptr);
		emit_imm(&value);
	} else if (arg == T_A) {
		/*
		 * special a loads 
		 */
		switch (reg) {
		case T_BC_I:
			emitbyte(0x0A);
			break;

		case T_DE_I:
			emitbyte(0x1A);
			break;

		case T_INDIR:
			emitbyte(0x3A);
			emit_exp(2, &value);
			break;

		case T_I:
			emitbyte(0xED);
			emitbyte(0x57);
			break;

		case T_R:
			emitbyte(0xED);
			emitbyte(0x5F);
			break;

		default:
			return 1;
		}
	} else
		return 1;
	return 0;
}

static char
do_basic(isr)
struct instruct *isr;
{
	emitbyte(isr->opcode);
	return 0;
}

static char
do_basic_ext(isr)
struct instruct *isr;
{
	emitbyte(isr->arg);
	emitbyte(isr->opcode);
	return 0;
}

static char
do_arith(isr)
struct instruct *isr;
{
	unsigned char prim = 0, arg, reg;
	struct expval value;

	arg = operand(&value);

	if (isr->arg == CARRY) {
		if (arg == T_HL) {
			prim = 1;
		} else if (arg != T_A)
			return 1;

		need(',');
		arg = operand(&value);
	} else if (isr->arg == ADD) {
		if (arg == T_HL) {
			prim = 2;
		} else if (arg == T_IX || arg == T_IY) {
			prim = 3;
			reg = arg;
		} else if (arg != T_A)
			return 1;

		need(',');
		arg = operand(&value);

		if (prim == 3 && arg == T_HL)
			return 1;

		if (prim == 3 && arg == reg)
			arg = T_HL;
	}

	if (prim == 0) {
		if (arg <= T_A) {
			emitbyte(isr->opcode + (arg - T_B));
		} else if (arg >= T_IXH && arg <= T_IX_D) {
			emitbyte(0xDD);
			emitbyte(isr->opcode + (arg - T_IXH) + 4);
			if (arg == T_IX_D)
				emitbyte(value.num.b);
		} else if (arg >= T_IYH && arg <= T_IY_D) {
			emitbyte(0xFD);
			emitbyte(isr->opcode + (arg - T_IYH) + 4);
			if (arg == T_IY_D)
				emitbyte(value.num.b);
		} else if (arg == T_PLAIN) {
			emitbyte(isr->opcode + 0x46);
			emitbyte(value.num.b);
		} else
			return 1;
	} else if (prim == 1) {
		if (arg >= T_BC && arg <= T_SP) {
			emitbyte(0xED);
			emitbyte((0x42 + (isr->opcode == 0x88 ? 8 : 0)) +
					 ((arg - 8) << 4));
		} else
			return 1;
	} else if (prim == 2) {
		if (arg >= T_BC && arg <= T_SP) {
			emitbyte(0x09 + ((arg - 8) << 4));
		} else
			return 1;
	} else if (prim == 3) {
		if (arg == T_HL)
			arg = reg;
		if (arg == reg)
			arg = T_HL;

		if (reg == T_IX)
			emitbyte(0xDD);
		else
			emitbyte(0xFD);

		if (arg >= T_BC && arg <= T_SP) {
			emitbyte(0x09 + ((arg - 8) << 4));
		} else
			return 1;
	}
	return 0;
}

static char
do_incr(isr)
struct instruct *isr;
{
	unsigned char arg;
	struct expval value;

	arg = operand(&value);

	if (arg <= T_A) {
		emitbyte(isr->opcode + ((arg) << 3));
	} else if (arg <= T_SP) {
		emitbyte(isr->arg + ((arg - T_BC) << 4));
	} else if (arg == T_IX) {
		emitbyte(0xDD);
		emitbyte(isr->arg + 0x20);
	} else if (arg == T_IY) {
		emitbyte(0xFD);
		emitbyte(isr->arg + 0x20);
	} else if (arg >= T_IXH && arg <= T_IX_D) {
		emitbyte(0xDD);
		emitbyte(isr->opcode + ((arg - T_IXH + 4) << 3));
		if (arg == T_IX_D)
			emitbyte(value.num.b);
	} else if (arg >= T_IYH && arg <= T_IY_D) {
		emitbyte(0xFD);
		emitbyte(isr->opcode + ((arg - T_IYH + 4) << 3));
		if (arg == T_IY_D)
			emitbyte(value.num.b);
	} else
		return 1;
	return 0;
}

static char
do_bitsh(isr)
struct instruct *isr;
{
	unsigned char arg, reg;
	struct expval value;

	arg = operand(&value);

	reg = 0;
	if (isr->arg) {
		if (arg != T_PLAIN || value.sym)
			return 1;

		if (value.num.b > 7)
			return 1;

		reg = value.num.b;

		need(',');
		arg = operand(&value);
	}

	if (arg == T_IX_D || arg == T_IY_D) {

		if (arg == T_IX_D)
			emitbyte(0xDD);
		else
			emitbyte(0xFD);

		emitbyte(0xCB);

		emitbyte(&value);

		arg = T_HL_I;
		if (peekchar() == ',') {
			need(',');
			arg = operand(&value);

			if (arg == 6)
				arg = 8;
		}
	} else
		emitbyte(0xCB);

	/* Convert register token to register code (0-7) */
	if (arg >= T_B && arg <= T_A)
		arg -= T_B;
	else if (arg == T_HL_I)
		arg = 6;
	else if (arg > 7)
		return 1;

	emitbyte(isr->opcode + arg + (reg << 3));
	return 0;
}

static char
do_stack(isr)
struct instruct *isr;
{
	unsigned char arg;
	struct expval value;

	arg = operand(&value);
	if (arg == T_AF)
		arg = T_SP;

	if (arg >= T_BC && arg <= T_SP) {
		emitbyte(isr->opcode + ((arg - T_BC) << 4));
	} else if (arg == T_IX) {
		emitbyte(0xDD);
		emitbyte(isr->opcode + 0x20);
	} else if (arg == T_IY) {
		emitbyte(0xFD);
		emitbyte(isr->opcode + 0x20);
	} else
		return 1;
	return 0;
}

static char
do_ret(isr)
struct instruct *isr;
{
	unsigned char arg;
	struct expval value;

	arg = operand(&value);

	if (arg == T_C) arg = T_CR;  /* 'c' means carry, not register C */
	if (arg >= T_NZ && arg <= T_M) {
		emitbyte(isr->opcode + ((arg - T_NZ) << 3));
	} else if (arg == 255) {
		emitbyte(isr->arg);
	} else
		return 1;
	return 0;
}

static char
do_jmp(isr)
struct instruct *isr;
{
	unsigned char arg, cond;
	struct expval value;
	struct jump *j;
	unsigned short addr;
	int target, dist;

	arg = operand(&value);

	if (arg == T_C) arg = T_CR;
	if (arg >= T_NZ && arg <= T_M) {
		cond = arg;
		need(',');
		arg = operand(&value);

		/* record jump for relaxation */
		addr = cur_address;
		add_jump(addr, value.sym, value.num.w, cond);

		/* check if relaxed to jr */
		j = find_jump(addr);
		if (j && j->is_jr) {
			/* emit jr cc, offset */
			/* jr nz=20, z=28, nc=30, c=38 */
			emitbyte(0x20 + ((cond - T_NZ) << 3));
			/* calculate relative offset */
			if (value.sym)
				target = value.sym->value + value.num.w;
			else
				target = value.num.w;
			dist = target - (cur_address + 1);
			emitbyte(dist & 0xff);
		} else {
			emitbyte(isr->opcode + ((cond - T_NZ) << 3));
			emit_exp(2, &value);
		}
	} else if (arg == T_NUM || arg == T_PLAIN) {
		/* unconditional jp */
		addr = cur_address;
		add_jump(addr, value.sym, value.num.w, 0);

		j = find_jump(addr);
		if (j && j->is_jr) {
			/* emit jr offset */
			emitbyte(0x18);
			if (value.sym)
				target = value.sym->value + value.num.w;
			else
				target = value.num.w;
			dist = target - (cur_address + 1);
			emitbyte(dist & 0xff);
		} else {
			emitbyte(isr->opcode + 1);
			emit_exp(2, &value);
		}
	} else if (arg == T_HL_I) {
		emitbyte(isr->arg);
	} else if (arg == T_IX_I) {
		emitbyte(0xDD);
		emitbyte(isr->arg);
	} else if (arg == T_IY_I) {
		emitbyte(0xFD);
		emitbyte(isr->arg);
	} else
		return 1;
	return 0;
}

static char
do_jrl(isr)
struct instruct *isr;
{
	unsigned char arg, reg;
	struct expval value;
	int target, dist;

	arg = operand(&value);

	reg = 0;
	if (isr->arg) {
		if (arg == T_C) arg = T_CR;  /* 'c' means carry, not register C */
		if (arg >= T_NZ && arg <= T_CR) {
			/* conditional jr: base opcode 0x20 + (cond * 8) */
			reg = 0x08 + ((arg - T_NZ) << 3);
			need(',');
			arg = operand(&value);
		} else if (arg != T_NUM && arg != T_PLAIN)
			return 1;
	}

	if (arg != T_PLAIN)
		return 1;

	emitbyte(isr->opcode + reg);
	/* compute PC-relative offset: target - (PC after 2-byte jr) */
	if (value.sym)
		target = value.sym->value + value.num.w;
	else
		target = value.num.w;
	dist = target - (cur_address + 1);
	if (pass == 1 && (dist < -128 || dist > 127))
		gripe("relative jump out of range");
	emitbyte(dist & 0xff);
	return 0;
}

static char
do_call(isr)
struct instruct *isr;
{
	unsigned char arg;
	struct expval value;

	arg = operand(&value);

	if (arg == T_C) arg = T_CR;  /* 'c' means carry, not register C */
	if (arg == 1) arg = T_CR;
	if (arg >= T_NZ && arg <= T_M) {
		emitbyte(isr->opcode + ((arg - T_NZ) << 3));
		need(',');
		operand(&value);  /* get the address */
		emit_exp(2, &value);
	} else if (arg == T_PLAIN) {
		emitbyte(isr->arg);
		emit_exp(2, &value);
	} else
		return 1;
	return 0;
}

static char
do_rst(isr)
struct instruct *isr;
{
	unsigned char arg;
	struct expval value;

	arg = operand(&value);

	if (arg != T_PLAIN || value.num.b & 0x7 || value.num.b > 0x38)
		return 1;

	emitbyte(isr->opcode + value.num.b);
	return 0;
}

static char
do_in(isr)
struct instruct *isr;
{
	unsigned char arg, reg;
	struct expval value;

	arg = operand(&value);

	if (arg == T_C_I) {
		emitbyte(0xED);
		emitbyte(0x70);
		return 0;
	}

	if (arg == T_HL_I || arg > T_A)
		return 1;

	reg = arg;
	need(',');
	arg = operand(&value);

	if (reg == T_A && arg == T_INDIR) {
		emitbyte(isr->opcode);
		emitbyte(&value);
	} else if (arg == T_C_I) {
		emitbyte(0xED);
		emitbyte(0x40 + (reg << 3));
	} else
		return 1;
	return 0;
}

static char
do_out(isr)
struct instruct *isr;
{
	unsigned char arg, reg;
	struct expval value;

	arg = operand(&value);

	if (arg == T_INDIR) {
		reg = value.num.b;
		need(',');
		arg = operand(&value);

		if (arg != T_A)
			return 1;

		emitbyte(isr->opcode);
		emitbyte(reg);
	} else if (arg == T_C_I) {
		need(',');
		arg = operand(&value);

		if (arg == T_HL_I)
			return 1;
		if (arg == T_PLAIN && !value.num.w)
			arg = T_HL_I;

		if (arg > T_A)
			return 1;

		emitbyte(0xED);
		emitbyte(0x41 + (arg << 3));
	} else
		return 1;
	return 0;
}

static char
do_exch(isr)
struct instruct *isr;
{
	unsigned char arg, reg;
	struct expval value;

	reg = operand(&value);
	need(',');
	arg = operand(&value);

	if (reg == T_AF) {
		if (arg == T_AF) {
			need('\'');
			emitbyte(isr->arg);
		} else
			return 1;
	}
	else if (reg == T_DE) {
		if (arg == T_HL) {
			emitbyte(isr->opcode + 0x08);
		} else
			return 1;
	}
	else if (reg == T_SP_I) {
		switch (arg) {
		case T_HL:
			break;
		case T_IX:
			emitbyte(0xDD);
			break;
		case T_IY:
			emitbyte(0xFD);
			break;
		default:
			return 1;
		}
		emitbyte(isr->opcode);
	}
	return 0;
}

static char
do_intmode(isr)
struct instruct *isr;
{
	unsigned char arg;
	struct expval value;

	arg = operand(&value);

	if (arg != T_PLAIN)
		return 1;

	emitbyte(0xED);
	switch (value.num.w) {
	case 0:
	case 1:
		emitbyte(isr->opcode + (value.num.b << 4));
		break;

	case 2:
		emitbyte(isr->arg);
		break;

	default:
		return 1;
	}
	return 0;
}

static char
do_load(isr)
struct instruct *isr;
{
	unsigned char arg, reg;
	struct expval value;

	arg = operand(&value);

	if (arg == T_INDIR) {
		return do_stax(&value);
	}

	if (arg <= T_A || (arg >= T_IXH && arg <= T_IY_D)) {
		return do_ldr8(arg, &value);
	}

	if ((arg >= T_BC && arg <= T_SP) || (arg == T_IX || arg == T_IY)) {
		return do_16i(arg);
	}

	if (arg >= T_BC_I && arg <= T_R) {
		need(',');
		reg = operand(&value);
		if (reg != T_A)
			return 1;

		switch (arg) {
		case T_BC_I:
			emitbyte(0x02);
			break;

		case T_DE_I:
			emitbyte(0x12);
			break;

		case T_I:
			emitbyte(0xED);
			emitbyte(0x47);
			break;

		case T_R:
			emitbyte(0xED);
			emitbyte(0x4F);
			break;
		}
	} else
		return 1;
	return 0;
}

static char (*isr_handlers[])() = {
	0,
	do_basic,
	do_basic_ext,
	do_arith,
	do_incr,
	do_bitsh,
	do_stack,
	do_ret,
	do_jmp,
	do_jrl,
	do_call,
	do_rst,
	do_in,
	do_out,
	do_exch,
	do_intmode,
	do_load
};

/*
 * attempts to assemble an instruction assuming a symbol has just been tokenized
 *
 * in = pointer to string
 * returns 0 if an instruction is not matched, 1 if it is
 */
char
asm_instr(in)
char *in;
{
	int i;
	struct instruct *isr;

	for (i = 0; isr_table[i].type != IEND; i++) {
		if (match(in, isr_table[i].mnem)) {
			isr = &isr_table[i];
			if (isr_handlers[isr->type](isr))
				gripe("invalid operand");
			return 1;
		}
	}
	return 0;
}

/*
 * changes segments for first pass segment top tracking
 * save our place
 * next = next segment
 */
void
change_seg(next)
char next;
{
	switch (segment) {
	case SEG_TEXT:
		text_top = cur_address;
		break;
	case SEG_DATA:
		data_top = cur_address;
		break;
	case SEG_BSS:
		bss_top = cur_address;
		break;
	default:
		break;
	}

	switch (next) {
	case SEG_TEXT:
		cur_address = text_top;
		break;
	case SEG_DATA:
		cur_address = data_top;
		break;
	case SEG_BSS:
		cur_address = bss_top;
		break;
	default:
		break;
	}
	segment = next;
}

/*
 * perform assembly functions
 * two passes over the source code:
 * pass 0: locate symbols in relative segments, calculate sizes
 * pass 1: emit code and data with final addresses
 * then output symbol table and relocations
 */
void
assemble()
{
    unsigned short type;
	unsigned short result;
	struct symbol *sym;
    unsigned short next;

	asm_reset();

	pass = 0;

	segment = SEG_TEXT;
	text_top = data_top = bss_top = 0;
	cur_address = 0;

	/*
	 * run passes 
	 */
	while (1) {

		change_seg(SEG_TEXT);
		cur_address = 0;
		text_top = 0;

		if (verbose) {
			printf("start of pass %d\n", pass);
			printf
				("text_top: %d data_top: %d bss_top: %d mem_size: %d\n",
				 text_top, data_top, bss_top, mem_size);
		}

		while (1) {
            get_token();

            if (cur_token == T_EOF) {
                break;
            }            

			if (verbose > 4)
				printf("line %d: %s", lineNum, linebuf);

			/*
			 * command read 
			 */
			if (cur_token == '.') {
				get_token();

				if (cur_token != T_NAME)
					gripe2("expected directive", token_buf);

				next = 0;
				if (match(token_buf, "text")) {
					next = 1;
				} else if (match(token_buf, "data")) {
					next = 2;
				} else if (match(token_buf, "bss")) {
					next = 3;
				}

				/*
				 * change segment 
				 */
				if (next != 0) {
					change_seg(next);
					consume();
					continue;
				}

				if (match(token_buf, "globl") ||
					match(token_buf, "global")) {
					while (1) {
						get_token();
						if (cur_token != T_NAME)
							gripe("expected symbol");
						if (pass == 0) {
							sym = sym_update(token_buf, SEG_UNDEF, 0, 1);
						}
						/* see if there is another */
						if (peekchar() == ',')
							need(',');
						else
							break;
					}
					consume();
					continue;
				}

				if (match(token_buf, "extern")) {
					while (1) {
						get_token();
						if (cur_token != T_NAME)
							gripe("expected symbol");
						if (pass == 0) {
							sym = sym_update(token_buf, SEG_EXT, 0, 1);
						}
						/* see if there is another */
						if (peekchar() == ',')
							need(',');
						else
							break;
					}
					consume();
					continue;
				}

				/*
				 * .ds <byte count> 
				 */
				if (match(token_buf, "ds")) {
					ds();
					consume();
					continue;
				}

				/*
				 * .defb <byte>|<string>[,...] 
				 */
				if (match(token_buf, "defb") ||
					match(token_buf, "db")) {
					db();
					consume();
					continue;
				}

				/*
				 * .defw <word>[,...]
				 */
				if (match(token_buf, "defw") ||
					match(token_buf, "dw")) {
					dw();
					consume();
					continue;
				}

				/*
				 * .defl <long>[,...]
				 */
				if (match(token_buf, "defl") ||
					match(token_buf, "dl")) {
					dl();
					consume();
					continue;
				}

				printf("%s\n", token_buf);
				gripe("unkown directive");
				continue;
			}

			/*
			 * symbol read 
			 */
			else if (cur_token == T_NAME) {
				/*
				 * try to get the type of the symbol 
				 */
				if (asm_instr(token_buf)) {
					/*
					 * it's an instruction 
					 */
					consume();
				} else if (peekchar() == '=') {
					/*
					 * it's a symbol definition 
					 */
					save_symn();
					get_token();

					type = operand(&result);

					sym_update(sym_name, type, result, 0);
					consume();
				} else if (peekchar() == ':') {
					/*
					 * set the new symbol (if it is the first pass)
					 * label:: (double colon) exports the symbol
					 */
					int visible = 0;
					nextchar();  /* consume first : */
					if (peekchar() == ':') {
						nextchar();  /* consume second : */
						visible = 1;
					}
					if (pass == 0) {
						sym_update(token_buf, segment, cur_address, visible);
					}
				} else {
					gripe("unexpected symbol");
				}
			} else if (cur_token != '\n') {
				gripe("unexpected token");
			}
		}

		change_seg(SEG_TEXT);
        
		if (verbose) {
			printf("end of pass %d\n", pass);
			printf
				("text_top: %d data_top: %d bss_top: %d mem_size: %d\n\n",
				 text_top, data_top, bss_top, mem_size);
		}

		pass++;

		/*
		 * pass 1, so we know our text + data segment sizes
		 */
		if (pass == 1) {

			change_seg(SEG_TEXT);

			/* relax jp->jr before finalizing sizes */
			relax_jmp();

			mem_size = text_top + data_top + bss_top;
			text_size = text_top;
			data_size = data_top;
			bss_size = bss_top;

            next = 0;

            /* we've seen everything, so we can assign indexes */
	        for (sym = symbols; sym; sym = sym->next) {

                if (sym->seg == SEG_UNDEF) {
                    /* Treat undefined symbols as extern */
                    sym->seg = SEG_EXT;
                    sym->index = next++;
                } else if (sym->index == 0) {
                    sym->index = next++;
                }
		        if (sym->seg == SEG_DATA) {
                    sym->value += text_size;
                }
                if (sym->seg == SEG_BSS) {
                    sym->value += text_size + data_size;
                }
            }

			outbyte(MAGIC);		/* magic */
			outbyte(m_flag ? CONF_9 : CONF_15);		/* config byte */
			outword(next * ((m_flag ? 9 : 15) + 3)); /* symbol table size */
			outword(text_size);	/* text */
			outword(data_size);	/* data */
			outword(bss_size);	/* bss */
			outword(0);			/* stack+heap */
			outword(0);			/* textoff */
			outword(text_size);	/* dataoff */

            if (verbose)
                printf("magic %x text:%d data:%d bss:%d heap:%d "
                       "symbols:%d textoff:%x dataoff:%x\n",
                       m_flag ? 0x9914 : 0x9917, text_size, data_size, bss_size, 0,
                       next * ((m_flag ? 9 : 15) + 3), 0, text_size);

			/*
			 * reset segment addresses to their final addresses
			 */
			text_top = 0;
			data_top = text_size;
			bss_top = data_top + data_size;
			cur_address = 0;

            lineNum = 0;

            *lineptr = '\0';
            if (infd == 0) {
                infd = inbuffd;
            }	
			lseek(infd, 0, SEEK_SET);

			continue;
		}

		if (pass == 2)
			break;
	}

	/*
	 * output symbols and relocation tables
	 */
	for (sym = symbols; sym; sym = sym->next) {
		switch (sym->seg) {
		case SEG_UNDEF:
			type = 0x08;
			break;
		case SEG_TEXT:
			type = 0x05 | 0x08;
			break;
		case SEG_DATA:
			type = 0x06 | 0x08;
			break;
		case SEG_BSS:
			type = 0x07 | 0x08;
			break;
		case SEG_ABS:
			type = 0x04 | 0x08;
			break;
		case SEG_EXT:
			type = 0x08;
			break;
		default:
			break;
		}
		if (verbose > 3) {
			printf("sym: %9s index: %5d seg: %s(%d) type: %x\n",
				sym->name, sym->index, segname[sym->seg], sym->seg, type);
		}
		if (sym->index == 0xffff)
			continue;
		outtmp(sym->value & 0xff);
		outtmp(sym->value >> 8);
		outtmp(type);
		for (next = 0; next < (m_flag ? 9 : 15); next++) {
			outtmp(sym->name[next]);
		}
	}

	reloc_out(textr.head, 0);
	reloc_out(datar.head, text_top);
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
