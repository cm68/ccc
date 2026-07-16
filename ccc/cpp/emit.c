/*
 * emit.c - Lexeme stream and preprocessed output emission
 *
 * Binary .x format - all data as raw bytes (no hex encoding)
 */
#include "cpp.h"
#include <unistd.h>

/* Global file descriptor */
char lexFd = -1;

/* Line tracking for LINENO emission */
static int lastLine = 0;
static char *lastName = NULL;

/* Brace balance tracking */
static int braceCount = 0;

/* Forward declarations */
void emitLine(int line, char *file);
void emitStructTok(struct token *t);

/*
 * Initialize line tracking and emit initial line directive for source file
 */
void
emitFileStart(char *file)
{
    if (!noLineMarkers) {
        emitLine(1, file);
        lastLine = 1;
        lastName = filename;
    }
}

/*
 * Emit a simple token to .x file (1 byte)
 */
void
emitToken(unsigned char tok)
{
    outbufWrite(&tok, 1);
}

/*
 * Emit keyword token (single byte, 128-159 range)
 */
void
emitKeyword(unsigned char kwtok)
{
    outbufWrite(&kwtok, 1);
}

/*
 * Emit name token: tok + len byte + name bytes
 */
static void
emitName(unsigned char tok, char *name)
{
    unsigned char hdr[2];
    int len = strlen(name);
    if (len > 255) len = 255;
    hdr[0] = tok;
    hdr[1] = len;
    outbufWrite(hdr, 2);
    outbufWrite(name, len);
}

/*
 * Emit symbol: SYM(20) + len byte + name bytes
 */
void
emitSym(char *name)
{
    emitName(SYM, name);
}

/*
 * Emit 4-byte little-endian value with tag
 */
static void
emit4(unsigned char tag, unsigned long val)
{
    unsigned char buf[5];
    buf[0] = tag;
    buf[1] = val & 0xff;
    buf[2] = (val >> 8) & 0xff;
    buf[3] = (val >> 16) & 0xff;
    buf[4] = (val >> 24) & 0xff;
    outbufWrite(buf, 5);
}

void
emitNumber(long val)
{
    emit4(NUMBER, (unsigned long)val);
}

void
emitFNumber(float val)
{
    union { float f; unsigned long l; } u;
    u.f = val;
    emit4(FNUMBER, u.l);
}

/*
 * Emit string with 2-byte length: token + 2-byte len + string bytes
 * Used for both STRING and ASMSTR tokens
 */
static void
emitStr2(unsigned char tok, char *str, int len)
{
    unsigned char hdr[3];
    /*
     * Clamp to 32767, not 65535: the .x length field is 16 bits, but
     * 65535 is -1 in a 16-bit int, so that comparison fired for EVERY
     * string under the z80 compilers.  32767 is representable
     * everywhere; on the z80 the test is simply never true.
     */
    if (len > 32767) len = 32767;
    hdr[0] = tok;
    hdr[1] = len & 0xff;
    hdr[2] = (len >> 8) & 0xff;
    outbufWrite(hdr, 3);
    outbufWrite(str, len);
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
    emitName(LABEL, name);
}

/*
 * Emit newline marker to .x: single NEWLINE byte (means line++)
 */
void
emitNewline(void)
{
    unsigned char c = NEWLINE;
    outbufWrite(&c, 1);
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
    outbufWrite(hdr, 4);
    outbufWrite(file, len);
}


/*
 * Emit current token to .x stream
 * Just calls emitStructTok with &cur
 */
void
emitCurToken(void)
{
    emitStructTok(&cur);
}

/*
 * Emit a token from struct to .x stream (used by pull-based filter chain)
 */
void
emitStructTok(struct token *t)
{
    /* Emit line info to .x when line or file changes (unless -N) */
    if (!noLineMarkers) {
        if (lastName != t->filename) {
            /* File changed - emit full LINENO with filename */
            emitLine(t->lineno, t->filename ? t->filename : "");
            lastLine = t->lineno;
            lastName = t->filename;
        } else if (t->lineno == lastLine + 1) {
            /* Line incremented by 1 - emit single NEWLINE byte */
            emitNewline();
            lastLine = t->lineno;
        } else if (t->lineno != lastLine) {
            /* Line jumped - emit full LINENO */
            emitLine(t->lineno, t->filename ? t->filename : "");
            lastLine = t->lineno;
        }
    }

    /* Emit to lexeme stream */
    if (t->type >= KW_FIRST && t->type <= KW_LAST) {
        if (t->type == SIZEOF_KW)
            emitToken(SIZEOF);
        else if (t->type == CONST || t->type == VOLATILE)
            ;  /* Skip const/volatile - not supported */
        else
            emitKeyword(t->type);
    } else switch (t->type) {
    case SYM:
#ifdef DEBUG
        if (VERBOSE(V_FILTER))
            fdprintf(2, "emitStructTok SYM name=%s\n", t->v.name ? t->v.name : "(null)");
#endif
        emitSym(t->v.name);
        break;
    case NUMBER:
        emitNumber(t->v.numeric);
        break;
    case FNUMBER:
        emitFNumber(t->v.fval);
        break;
    case STRING:
        {
            int len = (unsigned char)t->v.str[0] |
                      ((unsigned char)t->v.str[1] << 8);
            emitString(t->v.str + 2, len);
        }
        break;
    case ASMSTR:
        emitAsmString(t->v.name, strlen(t->v.name));
        break;
    case LABEL:
        emitLabel(t->v.name);
        break;
    case E_O_F:
        break;
    case BEGIN:
        braceCount++;
        emitToken(t->type);
        break;
    case END:
        braceCount--;
        emitToken(t->type);
        break;
    default:
        emitToken(t->type);
        break;
    }

    /* Free STRING memory after emission - only used once */
    if (t->type == STRING && t->v.str) {
        free(t->v.str);
        t->v.str = NULL;
    }
}

/*
 * Check brace balance at EOF - call before emitting E_O_F
 */
void
emitChkBraces(void)
{
    if (braceCount != 0) {
        char buf[64];
        fmtstr(buf, "cpp: brace mismatch at EOF: %d unmatched {\n",
               braceCount);
        write(2, buf, strlen(buf));
        exit(1);
    }
}
