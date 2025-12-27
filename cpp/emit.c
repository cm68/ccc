/*
 * emit.c - Lexeme stream and preprocessed output emission
 *
 * Binary .x format - all data as raw bytes (no hex encoding)
 */
#include "cpp.h"
#include <unistd.h>

/* Global file descriptors */
int lexFd = -1;
int ppFd = -1;

/* Line tracking for LINENO emission */
static int lastLine = 0;      /* for .x file */
static int lastLinePP = 0;    /* for .i file */
static char *lastName = NULL;

/* Forward declarations */
void emitLine(int line, char *file);
void emitLinePP(int line, char *file);

/*
 * Initialize line tracking and emit initial line directive for source file
 */
void
emitFileStart(char *file)
{
    char buf[300];
    if (!noLineMarkers) {
        emitLine(1, file);
        /* Initial directive without leading newline */
        sprintf(buf, "# %d \"%s\"\n", 1, file);
        emitPPStr(buf);
        lastLine = 1;
        lastLinePP = 1;
        lastName = filename;
    }
}

/*
 * Emit a simple token to .x file (1 byte)
 */
void
emitToken(unsigned char tok)
{
    write(lexFd, &tok, 1);
}

/*
 * Emit keyword: KEYW(19) + kwval byte
 */
void
emitKeyword(unsigned char kwval)
{
    unsigned char buf[2];
    buf[0] = KEYW;
    buf[1] = kwval;
    write(lexFd, buf, 2);
}

/*
 * Emit symbol: SYM(20) + len byte + name bytes
 */
void
emitSym(char *name)
{
    unsigned char hdr[2];
    int len = strlen(name);
    if (len > 255) len = 255;
    hdr[0] = SYM;
    hdr[1] = len;
    write(lexFd, hdr, 2);
    write(lexFd, name, len);
}

/*
 * Emit number: NUMBER(21) + 4-byte little-endian value
 */
void
emitNumber(long val)
{
    unsigned char buf[5];
    buf[0] = NUMBER;
    buf[1] = val & 0xff;
    buf[2] = (val >> 8) & 0xff;
    buf[3] = (val >> 16) & 0xff;
    buf[4] = (val >> 24) & 0xff;
    write(lexFd, buf, 5);
}

/*
 * Emit float: FNUMBER(23) + 4-byte IEEE754 bits (little-endian)
 */
void
emitFNumber(float val)
{
    unsigned char buf[5];
    union { float f; unsigned long l; } u;
    u.f = val;
    buf[0] = FNUMBER;
    buf[1] = u.l & 0xff;
    buf[2] = (u.l >> 8) & 0xff;
    buf[3] = (u.l >> 16) & 0xff;
    buf[4] = (u.l >> 24) & 0xff;
    write(lexFd, buf, 5);
}

/*
 * Emit string with 2-byte length: token + 2-byte len + string bytes
 * Used for both STRING and ASMSTR tokens
 */
static void
emitStr2(unsigned char tok, char *str, int len)
{
    unsigned char hdr[3];
    if (len > 65535) len = 65535;
    hdr[0] = tok;
    hdr[1] = len & 0xff;
    hdr[2] = (len >> 8) & 0xff;
    write(lexFd, hdr, 3);
    write(lexFd, str, len);
}

/*
 * Emit string: STRING(22) + 2-byte len + string bytes
 */
void
emitString(char *str, int len)
{
    emitStr2(STRING, str, len);
}

/*
 * Emit asm string: ASMSTR(118) + 2-byte len + string bytes
 */
void
emitAsmString(char *str, int len)
{
    emitStr2(ASMSTR, str, len);
}

/*
 * Emit label: LABEL(112) + len byte + name bytes
 */
void
emitLabel(char *name)
{
    unsigned char hdr[2];
    int len = strlen(name);
    if (len > 255) len = 255;
    hdr[0] = LABEL;
    hdr[1] = len;
    write(lexFd, hdr, 2);
    write(lexFd, name, len);
}

/*
 * Emit newline marker to .x: single NEWLINE byte (means line++)
 */
void
emitNewline(void)
{
    unsigned char c = NEWLINE;
    write(lexFd, &c, 1);
}

/*
 * Emit line number with file to .x: LINENO(116) + 2-byte line + len byte + filename
 */
void
emitLine(int line, char *file)
{
    unsigned char hdr[4];
    int len = strlen(file);

    if (len > 255) len = 255;
    hdr[0] = LINENO;
    hdr[1] = line & 0xff;
    hdr[2] = (line >> 8) & 0xff;
    hdr[3] = len;
    write(lexFd, hdr, 4);
    write(lexFd, file, len);
}

/*
 * Emit # line directive to .i file only
 */
void
emitLinePP(int line, char *file)
{
    char buf[300];
    sprintf(buf, "\n# %d \"%s\"\n", line, file);
    emitPPStr(buf);
}

/*
 * Emit asm block: KW_ASM as keyword + STRING with asm text
 */
void
emitAsm(char *text, int len)
{
    emitKeyword(KW_ASM);
    emitString(text, len);
}

/*
 * Emit to .i file (preprocessed output)
 */
void
emitPP(char *text, int len)
{
    if (ppFd >= 0)
        write(ppFd, text, len);
}

/*
 * Emit string to .i file with escape sequences restored
 */
void
emitPPString(char *text, int len)
{
    int i;
    char c, esc[3];

    if (ppFd < 0) return;

    esc[0] = '\\';
    esc[2] = 0;
    for (i = 0; i < len; i++) {
        c = text[i];
        switch (c) {
        case '\n': esc[1] = 'n'; write(ppFd, esc, 2); break;
        case '\t': esc[1] = 't'; write(ppFd, esc, 2); break;
        case '\r': esc[1] = 'r'; write(ppFd, esc, 2); break;
        case '\\': esc[1] = '\\'; write(ppFd, esc, 2); break;
        case '"':  esc[1] = '"'; write(ppFd, esc, 2); break;
        default:   write(ppFd, &c, 1); break;
        }
    }
}

void
emitPPStr(char *text)
{
    if (ppFd >= 0)
        write(ppFd, text, strlen(text));
}

/*
 * Keyword value to string for .i output
 */
static char *
kw2str(unsigned char kw)
{
    switch (kw) {
    case KW_INT: return "int";
    case KW_CHAR: return "char";
    case KW_LONG: return "long";
    case KW_FLOAT: return "float";
    case KW_DOUBLE: return "double";
    case KW_VOID: return "void";
    case KW_UNSIGNED: return "unsigned";
    case KW_STATIC: return "static";
    case KW_EXTERN: return "extern";
    case KW_AUTO: return "auto";
    case KW_REGISTER: return "register";
    case KW_TYPEDEF: return "typedef";
    case KW_STRUCT: return "struct";
    case KW_UNION: return "union";
    case KW_ENUM: return "enum";
    case KW_IF: return "if";
    case KW_ELSE: return "else";
    case KW_WHILE: return "while";
    case KW_DO: return "do";
    case KW_FOR: return "for";
    case KW_SWITCH: return "switch";
    case KW_CASE: return "case";
    case KW_DEFAULT: return "default";
    case KW_BREAK: return "break";
    case KW_CONTINUE: return "continue";
    case KW_RETURN: return "return";
    case KW_GOTO: return "goto";
    case KW_ASM: return "asm";
    default: return "?kw?";
    }
}

/*
 * Operator token to string for .i output
 */
static char *
op2str(token_t t)
{
    switch (t) {
    case SIZEOF: return "sizeof";
    case EQ: return "==";
    case NEQ: return "!=";
    case LE: return "<=";
    case GE: return ">=";
    case LAND: return "&&";
    case LOR: return "||";
    case LSHIFT: return "<<";
    case RSHIFT: return ">>";
    case INCR: return "++";
    case DECR: return "--";
    case ARROW: return "->";
    case PLUSEQ: return "+=";
    case SUBEQ: return "-=";
    case MULTEQ: return "*=";
    case DIVEQ: return "/=";
    case MODEQ: return "%=";
    case ANDEQ: return "&=";
    case OREQ: return "|=";
    case XOREQ: return "^=";
    case LSHIFTEQ: return "<<=";
    case RSHIFTEQ: return ">>=";
    default: return NULL;
    }
}

/*
 * Emit current token to .x stream and .i file
 * Called after each token is lexed
 */
void
emitCurToken(void)
{
    char buf[32];
    char *op;

    /* Emit line info to .x when line or file changes (unless -N) */
    if (!noLineMarkers) {
        if (lastName != cur.filename) {
            /* File changed - emit full LINENO with filename */
            emitLine(cur.lineno, cur.filename ? cur.filename : "");
            emitLinePP(cur.lineno, cur.filename ? cur.filename : "");
            lastLine = cur.lineno;
            lastLinePP = cur.lineno;
            lastName = cur.filename;
        } else if (cur.lineno == lastLine + 1) {
            /* Line incremented by 1 - emit single NEWLINE byte */
            emitNewline();
            lastLine = cur.lineno;
        } else if (cur.lineno != lastLine) {
            /* Line jumped - emit full LINENO */
            emitLine(cur.lineno, cur.filename ? cur.filename : "");
            lastLine = cur.lineno;
        }
        /* Sync .i file line number with newlines */
        while (lastLinePP < cur.lineno) {
            emitPPStr("\n");
            lastLinePP++;
        }
    }

    /* Emit to lexeme stream */
    switch (cur.type) {
    case KEYW:
        /* sizeof is an operator in cc1, not a keyword */
        if (cur.v.numeric == KW_SIZEOF)
            emitToken(SIZEOF);
        else if (cur.v.numeric == KW_CONST || cur.v.numeric == KW_VOLATILE)
            return;  /* Skip const/volatile - not supported */
        else
            emitKeyword(cur.v.numeric);
        break;
    case SYM:
        emitSym(cur.v.name);
        break;
    case NUMBER:
        emitNumber(cur.v.numeric);
        break;
    case FNUMBER:
        emitFNumber(cur.v.fval);
        break;
    case STRING:
        /* cur.v.str has 2-byte length prefix */
        {
            int len = (unsigned char)cur.v.str[0] |
                      ((unsigned char)cur.v.str[1] << 8);
            emitString(cur.v.str + 2, len);
        }
        break;
    case ASMSTR:
        /* cur.v.name is a raw null-terminated string */
        emitAsmString(cur.v.name, strlen(cur.v.name));
        break;
    case LABEL:
        emitLabel(cur.v.name);
        break;
    case E_O_F:
        emitToken(E_O_F);
        break;
    default:
        /* Simple token - just emit the byte */
        emitToken(cur.type);
        break;
    }

    /* Emit to preprocessed output */
    if (cur.type == KEYW) {
        if (cur.v.numeric == KW_SIZEOF)
            emitPPStr("sizeof ");
        else {
            emitPPStr(kw2str(cur.v.numeric));
            emitPPStr(" ");
        }
    } else if ((op = op2str(cur.type)) != NULL) {
        emitPPStr(op);
        emitPPStr(" ");
    } else {
        switch (cur.type) {
        case SYM:
            emitPPStr(cur.v.name);
            emitPPStr(" ");
            break;
        case NUMBER:
            sprintf(buf, "%ld ", cur.v.numeric);
            emitPPStr(buf);
            break;
        case FNUMBER:
            sprintf(buf, "%g ", cur.v.fval);
            emitPPStr(buf);
            break;
        case STRING:
            {
                int len = (unsigned char)cur.v.str[0] |
                          ((unsigned char)cur.v.str[1] << 8);
                emitPPStr("\"");
                emitPPString(cur.v.str + 2, len);
                emitPPStr("\" ");
            }
            break;
        case ASMSTR:
            emitPPStr("{ ");
            emitPPString(cur.v.name, strlen(cur.v.name));
            emitPPStr(" } ");
            break;
        case LABEL:
            emitPPStr(cur.v.name);
            emitPPStr(": ");
            break;
        case SEMI:
            emitPPStr("; ");
            break;
        case BEGIN:
            emitPPStr("{ ");
            break;
        case END:
            emitPPStr("} ");
            break;
        case E_O_F:
            break;
        default:
            /* Operator tokens that aren't multi-char */
            buf[0] = 0;
            switch (cur.type) {
            case PLUS: buf[0] = '+'; break;
            case MINUS: buf[0] = '-'; break;
            case STAR: case TIMES: buf[0] = '*'; break;
            case DIV: buf[0] = '/'; break;
            case MOD: buf[0] = '%'; break;
            case AMPER: case AND: buf[0] = '&'; break;
            case OR: buf[0] = '|'; break;
            case XOR: buf[0] = '^'; break;
            case LT: buf[0] = '<'; break;
            case GT: buf[0] = '>'; break;
            case BANG: buf[0] = '!'; break;
            case TWIDDLE: buf[0] = '~'; break;
            case QUES: buf[0] = '?'; break;
            case COLON: buf[0] = ':'; break;
            case DOT: buf[0] = '.'; break;
            case ASSIGN: buf[0] = '='; break;
            case LPAR: buf[0] = '('; break;
            case RPAR: buf[0] = ')'; break;
            case LBRACK: buf[0] = '['; break;
            case RBRACK: buf[0] = ']'; break;
            case COMMA: buf[0] = ','; break;
            }
            if (buf[0]) {
                buf[1] = ' ';
                buf[2] = 0;
                emitPPStr(buf);
            }
            break;
        }
    }
}
