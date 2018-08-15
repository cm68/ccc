/*
 * look up a keyword in a table
 * we have 2 different tables to use, one for C and one for asm.
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
#include "ccc.h"

#define	HI	0x80

char ckw[] = {
	'a'|HI, 10, 's'|HI, 3, 'm', 0xff, ASM,
		'u', 't', 'o', 0xff, AUTO,
	'b'|HI, 15, 'o'|HI, 7, 'o','l','e','a','n', 0xff, BOOLEAN,
		'r', 'e', 'a', 'k', 0xff, BREAK,
	'c'|HI, 26, 'a'|HI, 4, 's', 'e', 0xff, CASE,
		'h'|HI, 4, 'a', 'r', 0xff, CHAR,
		'o', 'n', 's'|HI, 3, 's', 't', 0xff, CONST,
			't', 'i', 'n', 'u', 'e', 0xff, CONTINUE,
	'd'|HI, 18, 'e', 'f', 'a', 'u', 'l', 't', 0xff, DEFAULT,
		'o', 0xfe, DO,
		'u', 'b', 'l', 'e', 0xff, DOUBLE,
	'e'|HI, 19, 'l'|HI, 7, 's', 'e', 0xff, ELSE,
			'n'|HI, 'u', 'm', 0xff, ENUM,
		'x', 't', 'e', 'r', 'n', 0xff, EXTERN,
	'f'|HI, 11, 'l'|HI, 5, 'o', 'a', 't', 0xff, FLOAT,
			'o', 'r', 0xff, FOR,
	'g'|HI, 5, 'o', 't', 'o', 0xff, GOTO,
	'i'|HI, 8, 'f'|80, 1, 0xff, IF,
		'n', 't', 0xff, INT,
	'l'|HI, 5, 'o', 'n', 'g', 0xff, LONG,
	'r'|HI, 17, 'e'|HI, 8, 'g', 'i', 's', 't', 'e', 'r', 0xff, REGISTER,
		't', 'u', 'r', 'n', 0xff, RETURN,
	's'|HI, 29, 'i'|HI, 6, 'z', 'e', 'o', 'f', 0xff, SIZEOF,
		'h'|HI, 5, 'o', 'r', 't', 0xff, SHORT,
		't', 'a'|HI, 5, 't', 'i', 'c', 0xff, STATIC,
			'r', 'u', 'c', 't', 0xff, STRUCT,
	't'|HI, 8, 'y', 'p', 'e', 'd', 'e', 'f', 0xff, TYPEDEF,
	'u'|HI, 15, 'n', 'i'|HI, 4, 'o', 'n', 0xff, UNION,
			's', 'i', 'g', 'n', 'e', 'd', 0xff, UNSIGNED,
	'v'|HI, 'o', 'i'|HI, 3, 'd', 0xff, VOID,
			'l', 'a', 't', 'i', 'l', 'e', 0xff, VOLATILE,
	'w', 'h', 'i', 'l', 'e', 0xff, WHILE,
	0xff, 0
};

#ifdef ASMKWLOOK
char
kwlook(char *str, char *table)
{
    static char *s;
    static char *t;

    s = str;        // de
    t = table;      // hl

    asm {
        ld hl,(t) ; ld de,(s) ; jr done_p;
    top:
        ld a,(hl); 
        cp a,0xff; jr nz,not_term;
        inc hl; ld a,(de); or a,a; ld a,0; jr nz,fin;
    ret_tok:
        ld a,(hl); jr fin;
    not_term:
        cp a,0xfe; jr nz,cond;
        inc hl; ld a,(de); or a,a; jr z,ret_tok;
        inc hl; jr lp;
    cond:
        bit 7,a; jr z,liter;
        res 7,a; ex de,hl; cp a,(hl); ex de,hl; jr nz,skip
        inc hl; jr next;
    skip:
        inc hl; ld a,(hl);
        add a,2; add a,l; ld l,a;
        ld a,0; adc a,h; ld h,a;
        jr lp;
    liter:
        cp a,(hl); ld a,0; jr nz,fin;
    next:
        inc hl; inc de;
    done_p:
        ld a,(de); or a,a; jr nz, top;
    fin:
        ld l,a; ld h,0;
    }
}
#else
char
kwlook(char *str, char *table)
{
	unsigned char c;

	while (*str) {
		c = *table;
		if (c == 0xff) {
			if (!*str) return table[1];
            return 0;
		}
        if (c == 0xfe) {
            if (!*str) return table[1];
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
    return 0;
}
#endif

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
