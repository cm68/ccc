/*
 * emit.c - Lexeme stream and preprocessed output emission
 *
 * Outputs tokens to .l file and preprocessed source to .i file
 */
#include "cpp.h"
#include <unistd.h>

/* Global file descriptors */
int lexFd = -1;
int ppFd = -1;

/*
 * Emit a simple token to .l file
 */
void
emitToken(unsigned char tok)
{
    char buf[2];
    buf[0] = tok;
    write(lexFd, buf, 1);
}

/*
 * Emit symbol: '5' + 2-hex length + bytes
 */
void
emitSym(char *name)
{
    char buf[256];
    int len = strlen(name);
    if (len > 255) len = 255;
    sprintf(buf, "5%02x", len);
    write(lexFd, buf, 3);
    write(lexFd, name, len);
}

/*
 * Emit number: '9' + 8-hex value
 */
void
emitNumber(long val)
{
    char buf[16];
    sprintf(buf, "9%08lx", (unsigned long)val);
    write(lexFd, buf, 9);
}

/*
 * Emit float: 'b' + 8-hex IEEE754 bits
 */
void
emitFNumber(float val)
{
    char buf[16];
    union { float f; unsigned long l; } u;
    u.f = val;
    sprintf(buf, "b%08lx", u.l);
    write(lexFd, buf, 9);
}

/*
 * Emit string: '"' + 2-hex length + bytes
 */
void
emitString(char *str, int len)
{
    char buf[4];
    sprintf(buf, "\"%02x", len);
    write(lexFd, buf, 3);
    write(lexFd, str, len);
}

/*
 * Emit label: '3' + 2-hex length + bytes
 */
void
emitLabel(char *name)
{
    char buf[4];
    int len = strlen(name);
    if (len > 255) len = 255;
    sprintf(buf, "3%02x", len);
    write(lexFd, buf, 3);
    write(lexFd, name, len);
}

/*
 * Emit asm block: 'A' + 4-hex length + bytes (16-bit length for large blocks)
 */
void
emitAsm(char *text, int len)
{
    char buf[8];
    if (len > 65535) len = 65535;
    sprintf(buf, "A%04x", len);
    write(lexFd, buf, 5);
    write(lexFd, text, len);
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

void
emitPPStr(char *text)
{
    if (ppFd >= 0)
        write(ppFd, text, strlen(text));
}

/*
 * Token to human-readable string for .i output
 */
static char *
tok2str(token_t t)
{
    switch (t) {
    case INT: return "int";
    case CHAR: return "char";
    case LONG: return "long";
    case SHORT: return "short";
    case FLOAT: return "float";
    case DOUBLE: return "double";
    case VOID: return "void";
    case UNSIGNED: return "unsigned";
    case CONST: return "const";
    case STATIC: return "static";
    case EXTERN: return "extern";
    case AUTO: return "auto";
    case REGISTER: return "register";
    case VOLATILE: return "volatile";
    case TYPEDEF: return "typedef";
    case STRUCT: return "struct";
    case UNION: return "union";
    case ENUM: return "enum";
    case IF: return "if";
    case ELSE: return "else";
    case WHILE: return "while";
    case DO: return "do";
    case FOR: return "for";
    case SWITCH: return "switch";
    case CASE: return "case";
    case DEFAULT: return "default";
    case BREAK: return "break";
    case CONTINUE: return "continue";
    case RETURN: return "return";
    case GOTO: return "goto";
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
 * Emit current token to .l stream and .i file
 * Called after each token is lexed
 */
void
emitCurToken(void)
{
    char buf[32];
    char *kw;

    /* Emit to lexeme stream */
    switch (cur.type) {
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
        /* cur.v.str is a counted string - first byte is length */
        emitString(cur.v.str + 1, (unsigned char)cur.v.str[0]);
        break;
    case LABEL:
        emitLabel(cur.v.name);
        break;
    case ASM:
        /* cur.v.str is null-terminated asm text */
        if (cur.v.str)
            emitAsm(cur.v.str, strlen(cur.v.str));
        else
            emitAsm("", 0);
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
    kw = tok2str(cur.type);
    if (kw) {
        emitPPStr(kw);
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
            emitPPStr("\"");
            emitPP(cur.v.str + 1, (unsigned char)cur.v.str[0]);
            emitPPStr("\" ");
            break;
        case LABEL:
            emitPPStr(cur.v.name);
            emitPPStr(": ");
            break;
        case ASM:
            emitPPStr("asm { ");
            if (cur.v.str)
                emitPPStr(cur.v.str);
            emitPPStr(" }\n");
            break;
        case SEMI:
            emitPPStr(";\n");
            break;
        case BEGIN:
            emitPPStr("{\n");
            break;
        case END:
            emitPPStr("}\n");
            break;
        case E_O_F:
            break;
        default:
            /* Single char token */
            buf[0] = cur.type;
            buf[1] = ' ';
            buf[2] = 0;
            emitPPStr(buf);
            break;
        }
    }
}
