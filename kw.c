/*
 * look up a keyword in a table
 *
 * the architecture of this is to have a brutally fast, tight lexer
 * that does minimal searching and uses little memory. 
 * building the lexer table is manual and critical, so adding keywords
 * needs to be carefully done.
 *
 * we have 2 different tables to use, one for C, one for cpp.
 *
 * each table is an array of characters, with the following grammar:
 * byte:
 * 0xff <token number>		if the input string is at null,
 *				return token, else 0;
 * 0xfe <token number>		if the input string is at null,
 *				return token, else advance to next
 *				pattern
 * <character>			if string matches character, advance
 *				both. else return 0
 * <character|0x80> <skip>	if string matches low 7 bits, advance
 *				string by 1 and pattern by 2. if not,
 *				advance pattern by skip
 */
#include "cc1.h"

#define	HI	0x80

/*
 * the C pre-processor
 */
unsigned char cppkw[] = {
    'd'|HI, 7, 'e', 'f', 'i', 'n', 'e', 0xff, DEFINE,
    'i'|HI, 25, 'f'|HI, 15, 0xfe, IF,
            'd'|HI, 4, 'e', 'f', 0xff, IFDEF,
            'n'|HI, 5, 'd', 'e', 'f', 0xff, IFNDEF,
        'n', 'c', 'l', 'u', 'd', 'e', 0xff, INCLUDE,
    'e'|HI, 17, 'l'|HI, 9, 's'|HI, 3, 'e', 0xff, ELSE,
            'i', 'f', 0xff, ELIF,
        'n', 'd', 'i', 'f', 0xff, ENDIF,
    'u'|HI, 6, 'n', 'd', 'e', 'f', 0xff, UNDEF,
    0xff, 0
};

/*
 * the C language
 * XXX - strictly speaking, the lexer table should be ordered by use
 * frequency, so you'll hit quickly. perhaps they should be ordered
 * i, c, s, u, b, v, w, f
 */

unsigned char ckw[] = {
	'a'|HI, 10, 's'|HI, 3, 'm', 0xff, ASM,
		'u', 't', 'o', 0xff, AUTO,
	'b'|HI, 6, 'r', 'e', 'a', 'k', 0xff, BREAK,
	'c'|HI, 26, 'a'|HI, 4, 's', 'e', 0xff, CASE,
		'h'|HI, 4, 'a', 'r', 0xff, CHAR,
		'o', 'n', 's'|HI, 3, 't', 0xff, CONST,
			't', 'i', 'n', 'u', 'e', 0xff, CONTINUE,
	'd'|HI, 19, 'e'|HI, 7, 'f', 'a', 'u', 'l', 't', 0xff, DEFAULT,
		'o'|HI, 2, 0xfe, DO,
			'u', 'b', 'l', 'e', 0xff, DOUBLE,
	'e'|HI, 19, 'l'|HI, 4, 's', 'e', 0xff, ELSE,
		'n'|HI, 4, 'u', 'm', 0xff, ENUM,
		'x', 't', 'e', 'r', 'n', 0xff, EXTERN,
	'f'|HI, 11, 'l'|HI, 5, 'o', 'a', 't', 0xff, FLOAT,
			'o', 'r', 0xff, FOR,
	'g'|HI, 5, 'o', 't', 'o', 0xff, GOTO,
	'i'|HI, 8, 'f'|HI, 2, 0xff, IF,
		'n', 't', 0xff, INT,
	'l'|HI, 5, 'o', 'n', 'g', 0xff, LONG,
	'r'|HI, 16, 'e', 't'|HI, 5, 'u', 'r', 'n', 0xff, RETURN,
			'g', 'i', 's', 't', 'e', 'r', 0xff, REGISTER,
	's'|HI, 37, 'i'|HI, 6, 'z', 'e', 'o', 'f', 0xff, SIZEOF,
		'h'|HI, 5, 'o', 'r', 't', 0xff, SHORT,
		't'|HI, 13, 'a'|HI, 5, 't', 'i', 'c', 0xff, STATIC,
			'r', 'u', 'c', 't', 0xff, STRUCT,
		'w', 'i', 't', 'c', 'h', 0xff, SWITCH,
	't'|HI, 8, 'y', 'p', 'e', 'd', 'e', 'f', 0xff, TYPEDEF,
	'u'|HI, 15, 'n', 'i'|HI, 4, 'o', 'n', 0xff, UNION,
			's', 'i', 'g', 'n', 'e', 'd', 0xff, UNSIGNED,
	'v'|HI, 14, 'o', 'i'|HI, 3, 'd', 0xff, VOID,
			'l', 'a', 't', 'i', 'l', 'e', 0xff, VOLATILE,
	'w', 'h', 'i', 'l', 'e', 0xff, WHILE,
	0xff, 0
};

#ifdef ASMKWLOOK
char
kwlook(char *str, char *table)
ASMFUNC
{
    static char *s;
    static char *t;

    s = str;        // de
    t = table;      // hl

    ASMSTART
        ld hl,(t) ; ld de,(s) ; jr done_p;
    lp:
    top:
        ld a,(hl);
        cp a,#ff; jr nz,not_term;
        inc hl; ld a,(de); or a,a; ld a,0; jr nz,fin;
    ret_tok:
        ld a,(hl); jr fin;
    not_term:
        cp a,#fe; jr nz,cond;
        inc hl; ld a,(de); or a,a; jr z,ret_tok;
        inc hl; jr lp;
    cond:
        bit 7,a; jr z,liter;
        res 7,a; ex de,hl; cp a,(hl); ex de,hl; jr nz,skip
        inc hl; jr next;
    skip:
        inc hl; ld a,(hl);
        add a,#2; add a,l; ld l,a;
        ld a,#0; adc a,h; ld h,a;
        jr lp;
    liter:
        cp a,(hl); ld a,0; jr nz,fin;
    next:
        inc hl; inc de;
    done_p:
        ld a,(de); or a,a; jr nz, top;
    fin:
        ld l,a; ld h,0;
    ASMEND
    return 0;  // Never reached - assembly does its own return
}
#else
char
kwlook(unsigned char *str, unsigned char *table)
{
	unsigned char c;

//    printf("kwlook(%s)\n", str);
	while (*str) {
		c = *table;
//        printf("  *str='%c' table[0]='%c' (0x%02x)\n", *str, c & 0x7f, c);
		if (c == 0xff) {
            return 0;
		}
        if (c == 0xfe) {
            table += 2;
            continue;
        }
        if (c & 0x80) {
            if (*str == (c & 0x7f)) {
                str++;
                table += 2;
            } else {
                table += table[1] + 2;
            }
            continue;
        }
        if (c != *str) {
            return 0;
        }
        str++;
        table++;
	}
    if (*table == 0xff || *table == 0xfe) {
        return table[1];
    }
    return 0;
}
#endif

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
