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
 * Changed: <2025-12-22 05:20:21 curt>
 *
 * vim: tabstop=4 shiftwidth=4 noexpandtab:
 */
#ifdef linux
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#else
#include <stdio.h>
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

extern char asm_instr();

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

extern unsigned char *lineptr;
extern unsigned char linebuf[];

/*
 * token buffer 
 */
char token_buf[TOKLEN];
char sym_name[TOKLEN];
unsigned long token_val;
unsigned char cur_token;

/*
 * current assembly address 
 */
unsigned short cur_address;

/*
 * segment tops 
 */
unsigned short text_top;
unsigned short data_top;
unsigned short bss_top;

/*
 * sizes for header
 */
unsigned short text_size;
unsigned short mem_size;
unsigned short data_size;
unsigned short bss_size;

char pass;

char segment;

struct rhead textr = { "text" };
struct rhead datar = { "data" };

/*
 * jump records for jp->jr relaxation
 * only jp instructions with resolvable targets in text segment
 */
struct jump *jumps;

struct symbol *symbols;
struct symbol *symbols_tail;  /* for append order */

extern unsigned char skipwhite();
extern char alpha();
extern char symchar();
extern char escape();

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

extern char match();
extern unsigned long parsenum();
extern void gripe();
extern void gripe2();

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

extern void get_line();
extern unsigned char peekchar();
extern unsigned char nextchar();
extern void consume();

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

	while ((peekchar() != '\n') && (peekchar() != T_EOF)) {
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

	while (peekchar() != '\n' && peekchar() != T_EOF) {
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
	if ((c == '\n') || (c == -1))
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
                    /* sym->value += text_size + data_size; */
                    sym->value += text_size;
                    sym->value += data_size;
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
