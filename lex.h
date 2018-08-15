/*
 * lexemes are placed into the token character when the lexer runs
 * these are all ascii characters for the operators and other significant
 * single characters, and they are low integers for everything else
 */

enum tok {
    SYMBOL = 1, INTEGER, STRING, CHARACTER,

    EQEQ, BANGEQ,

    INC, DEC,
    LEFT, RIGHT,

    ADDEQ, SUBEQ, MULEQ, DIVEQ, MODEQ, 
    OREQ, ANDEQ, XOREQ,
    LEFTEQ, RIGHTEQ, 

    IF, WHILE, DO, SWITCH, CASE, DEFAULT, BREAK, CONTINUE

};

#define MAXLIT  128

extern char token;
extern char literal[MAXLIT];
extern int value;
extern int litlen;
extern char *inptr;

extern int lineno;
extern char *fname;

extern void advance();

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
