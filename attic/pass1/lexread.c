/*
 * lexread.c - Lexeme stream reader for cc1
 *
 * Reads preprocessed token stream from .x files produced by cpp.
 * This is the only lexer in cc1 - source mode has been removed.
 *
 * Lexeme format:
 *   Simple tokens: single byte (token type)
 *   SYM ('5'):     5 + 2-hex length + name bytes
 *   NUMBER ('9'):  9 + 8-hex value (32-bit)
 *   FNUMBER ('b'): b + 8-hex IEEE754 bits
 *   STRING ('"'):  " + 2-hex length + bytes
 *   LABEL ('3'):   3 + 2-hex length + name bytes
 *   ASM ('A'):     A + 4-hex length + bytes (16-bit for large blocks)
 */
#include "cc1.h"
#include <fcntl.h>
#include <unistd.h>

/* Token lookahead - visible to parser */
struct token cur, next;

/* String buffer for literals */
char strbuf[STRBUFSIZE];

/* Stub globals for error reporting (no source lines in lexeme mode) */
char *filename = "(null)";
int lineno = 0;
char linebuf[1] = "";
char prevline[1] = "";
int linepos = 0;

/* Input buffer */
static int lexFd = -1;
static unsigned char lexBuf[512];
static int lexPos = 0;
static int lexValid = 0;

/*
 * Read a byte from the lexeme stream
 */
static int
readByte(void)
{
    if (lexPos >= lexValid) {
        lexValid = read(lexFd, lexBuf, sizeof(lexBuf));
        lexPos = 0;
        if (lexValid <= 0)
            return -1;
    }
    return lexBuf[lexPos++];
}

/*
 * Read N bytes into buffer
 */
static int
readBytes(char *buf, int n)
{
    int i;
    for (i = 0; i < n; i++) {
        int c = readByte();
        if (c < 0) return -1;
        buf[i] = c;
    }
    return n;
}

/*
 * Convert hex char to value
 */
static int
hexval(int c)
{
    if (c >= '0' && c <= '9') return c - '0';
    if (c >= 'a' && c <= 'f') return c - 'a' + 10;
    if (c >= 'A' && c <= 'F') return c - 'A' + 10;
    return -1;
}

/*
 * Read 2-char hex value (00-ff)
 */
static int
readHex2(void)
{
    int h, l;
    h = readByte();
    l = readByte();
    if (h < 0 || l < 0) return -1;
    return (hexval(h) << 4) | hexval(l);
}

/*
 * Read 4-char hex value (16-bit)
 */
static int
readHex4(void)
{
    int val = 0;
    int i;
    for (i = 0; i < 4; i++) {
        int c = readByte();
        if (c < 0) return -1;
        val = (val << 4) | hexval(c);
    }
    return val;
}

/*
 * Read 8-char hex value (32-bit)
 */
static unsigned long
readHex8(void)
{
    unsigned long val = 0;
    int i;
    for (i = 0; i < 8; i++) {
        int c = readByte();
        if (c < 0) return 0;
        val = (val << 4) | hexval(c);
    }
    return val;
}

/*
 * Free token resources
 */
static void
freeToken(struct token *t)
{
    if (t->type == SYM || t->type == LABEL) {
        if (t->v.name) {
            free(t->v.name);
            t->v.name = NULL;
        }
    } else if (t->type == ASM) {
        if (t->v.str) {
            free(t->v.str);
            t->v.str = NULL;
        }
    }
}

/*
 * Read next token into 'next'
 */
static void
readNextToken(void)
{
    int c;
    int len;
    char *s;
    union { unsigned long l; float f; } u;

    c = readByte();
    if (c < 0) {
        next.type = E_O_F;
        return;
    }

    next.type = c;
    next.v.name = NULL;

    switch (c) {
    case SYM:
        /* 5 + 2-hex length + name bytes */
        len = readHex2();
        if (len < 0 || len >= STRBUFSIZE) {
            next.type = E_O_F;
            return;
        }
        s = malloc(len + 1);
        readBytes(s, len);
        s[len] = '\0';
        next.v.name = s;
        break;

    case NUMBER:
        /* 9 + 8-hex value */
        next.v.numeric = (long)readHex8();
        break;

    case FNUMBER:
        /* b + 8-hex IEEE754 bits */
        u.l = readHex8();
        next.v.fval = u.f;
        break;

    case STRING:
        /* " + 2-hex length + bytes */
        len = readHex2();
        if (len < 0 || len >= STRBUFSIZE - 1) {
            next.type = E_O_F;
            return;
        }
        /* Counted string format: first byte is length */
        strbuf[0] = len;
        readBytes(strbuf + 1, len);
        next.v.str = strbuf;
        break;

    case LABEL:
        /* 3 + 2-hex length + name bytes */
        len = readHex2();
        if (len < 0 || len >= STRBUFSIZE) {
            next.type = E_O_F;
            return;
        }
        s = malloc(len + 1);
        readBytes(s, len);
        s[len] = '\0';
        next.v.name = s;
        break;

    case ASM:
        /* A + 4-hex length + bytes (16-bit length for large blocks) */
        len = readHex4();
        if (len < 0) {
            next.type = E_O_F;
            return;
        }
        s = malloc(len + 1);
        readBytes(s, len);
        s[len] = '\0';
        next.v.str = s;
        break;

    default:
        /* Simple token - type byte is the token */
        break;
    }
}

/*
 * Open lexeme file and prime the token stream
 */
void
lexOpen(char *filename)
{
    lexFd = open(filename, O_RDONLY);
    if (lexFd < 0) {
        fdprintf(2, "cannot open lexeme file: %s\n", filename);
        exit(1);
    }

    /* Prime the token stream - need two reads */
    readNextToken();     /* Fill next */
    /* Shift to cur, fill next */
    cur = next;
    readNextToken();
}

/*
 * Close lexeme file
 */
void
lexClose(void)
{
    if (lexFd >= 0) {
        close(lexFd);
        lexFd = -1;
    }
}

/*
 * Rewind lexeme file to start for phase 2
 */
void
lexRewind(void)
{
    /* Free any allocated token memory */
    freeToken(&cur);
    freeToken(&next);

    /* Seek to start of file */
    lseek(lexFd, 0, SEEK_SET);

    /* Reset buffer state */
    lexPos = 0;
    lexValid = 0;

    /* Re-prime the token stream */
    readNextToken();
    cur = next;
    readNextToken();
}

/*
 * Get next token - shifts next into cur, reads new next
 */
void
gettoken(void)
{
    /* Free old cur if it had allocated memory */
    freeToken(&cur);

    /* Shift next to cur */
    cur = next;

    /* Read new next */
    readNextToken();
}

/*
 * Check if current token matches and consume it if so
 */
char
match(token_t t)
{
    if (cur.type == t) {
        gettoken();
        return 1;
    }
    return 0;
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
