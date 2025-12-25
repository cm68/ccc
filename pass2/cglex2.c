#include "cgen.h"

/*
 * File - lex2.c
 */

/*
 * lookupToken - Find token code by name (binary search)
 *
 * Searches tnames[] table using binary search algorithm.
 * Returns token code (0-71) or -1 if not found.
 */
char lookupToken(register char *target) {
    uint8_t hi, lo, mid;
    char cmp;

    /* clang-format off */
    static char *tnames[] = {
        "",    "!",  "!=",  "#",  "$",  "$U", "%",  "&",  "&&", "&U",
        "(",   ")",  "*",   "*U", "+",  "++", "+U", ",",  "-",  "--",
        "->",  "-U", ".",   "..", "/",  ":",  ":U", ":s", ";",  ";;",
        "<",   "<<", "<=",  "=",  "=%", "=&", "=*", "=+", "=-", "=/",
        "=<<", "==", "=>>", "=^", "=|", ">",  ">=", ">>", "?",  "@",
        "[\\", "[a", "[c",  "[e", "[i", "[s", "[u", "[v", "^",  "{",
        "|",   "||", "}",   "~", "RECIP", "TYPE",
        "ID", "CONST", "FCONST", "REG", "INAREG", "BITFIELD"
    };
    /* clang-format on */

    static char lastTok = 65;

    lo                  = 0;
    hi                  = lastTok;
    do {
        mid = (hi + lo) / 2;
        if ((cmp = strcmp(target, tnames[mid])) == 0)
            return mid;
        if (cmp < 0)
            hi = mid - 1;
        else
            lo = mid + 1;
    } while (hi >= lo);
    return (-1); /* Search terminates as unsuccessful */
}

/*
 * gethashptr OK++ PMO		Used in: lookupSymbol, declareSymbol
 *
 * Convert name to a hash table pointer
 *
 * Returns pointer to pointer to structure associated with
 * pointer to token.  for example
 *
 * Input	Hash key  Destination
 * string  	dec hex
 *   "f"  	 1	- float
 *   "i"  	 4	- int
 *   "l"  	 7	- long
 *   "s"  	14 0eh	- short
 *   "v"  	17 11h	- void
 *   "x"  	19 13h	- x ?
 *   "uc" 	30 1eh	- uint8_t
 *   "ui" 	36 24h	- uint16_t
 *   "ul" 	39 27h	- uint32_t
 *   "us" 	46 2eh	- uint16_t short
 *   "ux" 	51 33h	- uint16_t ?
 *   "b"  	98 62h	- b ?
 *   "c"  	99 63h	- char
 */

#define HASHTABSIZE 101

member_t **gethashptr(register char *str) {
    uint16_t key;

    /* calculate hash */
    for (key = 0; *str; str++)
        key = key * 2 + *(uint8_t *)str;

    /* Hash table index is determined by    */
    /* hash function using division method  */
    return &hashtab[key % HASHTABSIZE];
}

/*
 * lookupSymbol OK++ PMO
 *
 * Looks up a symbol by name in the hash table.
 * If found, returns the existing entry.
 * If not found, creates a new entry at current scope.
 *
 * Unlike declareSymbol, this does NOT check scope depth -
 * it only creates a new entry if the name doesn't exist anywhere.
 *
 * Used for: type names, identifier lookup, base type resolution.
 *
 * Returns: symbol table entry (member_t*), never NULL
 */
member_t *lookupSymbol(char *str) {
    member_t **hashPtr;
    register member_t *symbol;

    hashPtr = gethashptr(str);               /* Get hash bucket */
    for (symbol = *hashPtr; symbol; symbol = symbol->next)
        if (strcmp(symbol->name, str) == 0)
            return symbol;                   /* Found - return existing */

    /* Not found - create new entry */
    symbol         = allocMem(sizeof(member_t));
    symbol->next = *hashPtr;
    symbol->name = allocMem(strlen(str) + 1);
    strcpy(symbol->name, str);
    symbol->depth = (uint8_t)nstdpth;
    *hashPtr        = symbol;

    return symbol;
}

/*
 * getToken - Read next token from input stream
 *
 * Reads whitespace-delimited tokens from input. Handles:
 *   - Line number directives: "123 (sets lineno, expects filename)
 *   - Filename tracking: stores in progname[]
 *   - Comment pass-through: ;; prefix echoes rest of line
 *
 * Returns: pointer to token buffer, or (char*)~0 on EOF
 */
char *getToken() {
    int ch, expectName;
    register char *ptr;
    static char buffer[MAXBUF];

    expectName = false;
    for (;;) {
        ptr = buffer;

        while ((ch = fgetchar()) != EOF && isspace(ch))
            ;
        if (ch == EOF)
            return (char *)~0;
        *ptr++ = ch;
        do {
            *ptr++ = ch = fgetchar();
        } while (ch != EOF && !isspace(ch));
        *--ptr = 0;
        if (buffer[0] == '"') {
            lineno     = atoi(buffer + 1);
            expectName = ch != '\n';
        } else if (expectName) {
            strncpy(progname, buffer, sizeof(progname) - 1);
            expectName = false;
        } else if (buffer[0] == ';' && buffer[1] == ';') {
            do {
                fputchar(ch = fgetchar());
            } while (ch != '\n');
        } else
            return buffer;
    }
}

/*
 * leaveBlock - Clean up symbols when leaving a scope block
 *
 * Removes and frees symbols that have gone out of scope.
 */
void leaveBlock() {
    member_t **pHashEntry;
    int hashCount;
    member_t *newlist;
    member_t *next;
    member_t **memberList;
    int memberCount;
    register member_t *symbol;

    hashCount         = HASHTABSIZE;
    pHashEntry = hashtab;
    do {
        newlist = 0;
        /* note with the fixed handling of free, this could be converted to a for loop */
        symbol = *pHashEntry;
        while (symbol && symbol->depth == nstdpth) {
            if (symbol->sclass == VAR) {
                if (symbol->tflag == 2)
                    availRegs |= regBitMask[symbol->u.i];
                if (symbol->tflag == 4 || symbol->tflag == 3)
                    allocVar(symbol, 0); /* Add parameter 0! */
                if (0 < nstdpth && symbol->tflag == 3) {
                    next       = symbol->next;
                    symbol->next = newlist;
                    newlist    = symbol;
                    symbol->depth--;
                    symbol = next;
                    continue;
                }
            }

            free(symbol->name);

            if (symbol->sclass == UNION || symbol->sclass == STRUCT) {
                memberCount = symbol->u.mlist->cnt;
                memberList = symbol->u.mlist->vals;
                while (memberCount-- != 0)
                    free(*memberList++);
                free(symbol->u.mlist);
            }
            /*  code modified to get symbol->next before freeing symbol !!!*/
            next = symbol->next;
            free(symbol);
            /* symbol = symbol->next original code relied on b_next not being at start of block !!!*/
            symbol = next;
        }
        if (newlist) {                 /* if l3b list exists add to end */
            *(pHashEntry++) = newlist; /* set hash table to the l3b list */
            while (newlist->next)
                newlist = newlist->next; /* m14: */
            newlist->next = symbol;
        } else
            *(pHashEntry++) = symbol; /* else initialise hash table entry with new item  */
    } while (--hashCount != 0);

    nstdpth--;
    if (lvlidx >= 0) {
        if (funcScopeDepth[lvlidx] == nstdpth) {
            symbol = funcSymbol[lvlidx];
            if (symbol->type->sclass == STRUCT && (symbol->refl & B_REFL_STRUCT) == 0)
                prStrcRetCpy(symbol->offset, symbol->size);
            prFrameTail(symbol->offset, funcLocalSize[lvlidx]);
            lvlidx--;
            availRegs = REGS_ALL;
        }
    }
}

/*
 * parseTypeSpec OK++ PMO
 *
 * Parses a type descriptor string and extracts pointer/function modifiers.
 * Input strings start with '`' followed by modifiers and base type name:
 *   `int      - base type "int", no modifiers
 *   `*int     - pointer to int
 *   `**char   - pointer to pointer to char
 *   `(*int    - function returning pointer to int
 *
 * Modifier encoding (2 bits each):
 *   '*' = pointer,  encoded as 1
 *   '(' = function, encoded as 2
 *
 * Parameters:
 *   ch  - type descriptor string (starting with '`')
 *   par - output: reversed modifier bits for processing
 *
 * Returns: base type's symbol table entry (member_t*)
 */
member_t *parseTypeSpec(register char *ptr, uint16_t *modifiers) {
    uint16_t bits;

    bits = 0;
    ptr++;
    do {
        bits <<= 2;
        if (*ptr == '(') {
            bits |= 2;
            ptr++;
        } else if (*ptr == '*') {
            bits |= 1;
            ptr++;
        }
    } while (!isalnum(*ptr) && *ptr != '_');
    /* invert them */
    *modifiers = 0;
    while (bits != 0) {
        *modifiers <<= 2;
        *modifiers |= (bits & 3);
        bits >>= 2;
    }
    return lookupSymbol(ptr);
}

/*
 * badIntCode - Fatal error for invalid intermediate code
 */
void badIntCode() {

    fatalErr("Bad int. code");
}

/*
 * parseStmt - Parse and compile intermediate code statements
 */
void parseStmt() {
    register char *ch;
    int tok;

    availRegs = REGS_ALL;
    while ((ch = getToken()) != (char *)~0) { /* get_token    */
        tok = lookupToken(ch);
        switch (tok) { /* search_token */
        case LBRACE:   /* "{" opening block */
            nstdpth++; /* Increase the current nesting depth */
            break;
        case RBRACE: /* "}" end of block 	*/
            leaveBlock();
            break;
        case EXPR: /* "[e" expression	*/
            prPsect(P_TEXT);
            emitExpr(parseExpr());
            expect(']');
            break;
        case VAR: /* "[v" variable	*/
            parseVariable();
            break;
        case STRUCT: /* "[s" struct	*/
        case UNION:  /* "[u" union	*/
            parseMembers(tok);
            break;
        case CASE: /* "[\\" */
            parseSwitch();
            break;
        case ENUM: /* "[c" enum		*/
            parseEnum();
            break;
        case INIT: /* "[i" initialization	*/
            parseInit();
            break;
        case UNKNOWN: /* "[a" 		*/
            parseData();
            break;
        case BXOR: /* "^" */
        case BOR:  /* "|" */
        case LOR:  /* "||" */
        default:
            badIntCode();
        }
    }
    leaveBlock();
}

/*
 * expect - Expect and consume a specific character
 */
void expect(char par) {
    char ch;

    do {
        ch = fgetchar();
    } while (isspace(ch)); /* Skip white-space characters */

    if (ch != par)
        badIntCode();
}

/*
 * parseData - Parse and emit data block "[a" directive
 */
void parseData() {
    char *token;
    char buf[1024];
    register char *ptr;

    prPsect(P_DATA);
    genExprCode(optimizeExpr(mkNode(COLON_U, mkNode(COLON_S, parseExpr(), 0), 0)));
    ptr = buf;
    for (;;) {
        if (*(token = getToken()) == ']') /* parse non-terminating tokens */
            break;
        *ptr = atoi(token); /* Convert string to int value and place it in buffer */
        ptr++;
    }

    prDefb(buf, (int)(ptr - buf)); /* Emit data "defb byte1, ..." ("ptr-buf" bytes from "buf")*/
}
/* end of lex2.c */

/* vim: tabstop=4 shiftwidth=4 noexpandtab: */
