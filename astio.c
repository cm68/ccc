/*
 * astio.c - Low-level I/O and token parsing for AST parser
 *
 * Handles buffered input, whitespace/comment skipping, and basic token reading.
 * These functions are separated from semantic parsing to keep parseast.c focused
 * on higher-level AST construction.
 */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include "astio.h"
#include "cc2.h"

/* Parser state */
static int inFd;
static char buf[BUFSIZE];
static int bufPos;
static int bufValid;

/* Global parser state (accessible to parser) */
int lineNum = 1;
unsigned char curchar;

/* Static buffers for token reading */
static char symbuf[256];
static char strbuf[1024];
static char typebuf[256];

/*
 * Initialize I/O subsystem with file descriptor
 */
void
initAstio(int fd)
{
    inFd = fd;
    bufPos = 0;
    bufValid = 0;
    lineNum = 1;
    curchar = 0;
}

/*
 * Read next character from input
 * Returns 0 on EOF
 */
unsigned char
nextchar(void)
{
    if (bufPos >= bufValid) {
        bufValid = read(inFd, buf, BUFSIZE);
        if (bufValid <= 0) {
            curchar = 0;
            return 0;
        }
        bufPos = 0;
    }
    curchar = buf[bufPos++];
    if (curchar == '\n') {
        lineNum++;
    }
    return curchar;
}

/*
 * Skip whitespace
 */
static void
skipws(void)
{
    while (curchar == ' ' || curchar == '\t' || curchar == '\n') {
        nextchar();
    }
}

/*
 * Skip comments (lines starting with ;)
 */
static void
skipcomment(void)
{
    if (curchar == ';') {
        while (curchar && curchar != '\n') {
            nextchar();
        }
        if (curchar == '\n') {
            nextchar();
        }
    }
}

/*
 * Skip whitespace and comments
 */
void
skip(void)
{
    while (1) {
        skipws();
        if (curchar == ';') {
            skipcomment();
        } else {
            break;
        }
    }
}

/*
 * Expect and consume a specific character
 */
int
expect(unsigned char c)
{
    skip();
    if (curchar != c) {
        fdprintf(2, "parseast: line %d: expected '%c', got '%c'\n",
                 lineNum, c, curchar);
        return 0;
    }
    nextchar();
    return 1;
}

/*
 * Read a quoted string literal with escape sequences
 * Expects curchar to be on the opening quote
 * Returns pointer to static buffer with unescaped string data
 */
char *
readQuotedStr(void)
{
    unsigned char i = 0;

    skip();

    /* Expect opening quote */
    if (curchar != '"') {
        fdprintf(2, "parseast: line %d: expected '\"' at start of string\n",
            lineNum);
        strbuf[0] = '\0';
        return strbuf;
    }

    nextchar();  /* Skip opening quote */

    /* Read until closing quote */
    while (curchar && curchar != '"') {
        if (curchar == '\\') {
            /* Handle escape sequences */
            nextchar();
            switch (curchar) {
            case 'n':
                if (i < sizeof(strbuf) - 1) strbuf[i++] = '\n';
                break;
            case 't':
                if (i < sizeof(strbuf) - 1) strbuf[i++] = '\t';
                break;
            case 'r':
                if (i < sizeof(strbuf) - 1) strbuf[i++] = '\r';
                break;
            case '\\':
                if (i < sizeof(strbuf) - 1) strbuf[i++] = '\\';
                break;
            case '"':
                if (i < sizeof(strbuf) - 1) strbuf[i++] = '"';
                break;
            case 'x':
                /* Hex escape: \xNN */
                {
                    unsigned char hex1, hex2, val;
                    nextchar();
                    hex1 = curchar;
                    nextchar();
                    hex2 = curchar;
                    val = 0;

                    if (hex1 >= '0' && hex1 <= '9')
                        val = (hex1 - '0') << 4;
                    else if (hex1 >= 'a' && hex1 <= 'f')
                        val = (hex1 - 'a' + 10) << 4;
                    else if (hex1 >= 'A' && hex1 <= 'F')
                        val = (hex1 - 'A' + 10) << 4;

                    if (hex2 >= '0' && hex2 <= '9')
                        val |= (hex2 - '0');
                    else if (hex2 >= 'a' && hex2 <= 'f')
                        val |= (hex2 - 'a' + 10);
                    else if (hex2 >= 'A' && hex2 <= 'F')
                        val |= (hex2 - 'A' + 10);

                    if (i < sizeof(strbuf) - 1) strbuf[i++] = val;
                }
                break;
            default:
                /* Unknown escape, just copy the character */
                if (i < sizeof(strbuf) - 1) strbuf[i++] = curchar;
                break;
            }
            nextchar();
        } else {
            /* Regular character */
            if (i < sizeof(strbuf) - 1) {
                strbuf[i++] = curchar;
            }
            nextchar();
        }
    }

    strbuf[i] = '\0';

    /* Expect closing quote */
    if (curchar == '"') {
        nextchar();
    }

    return strbuf;
}

/*
 * Read a symbol name (starting with $ or alphanumeric)
 * Returns pointer to static buffer
 */
char *
readSymbol(void)
{
    unsigned char i = 0;

    skip();

    /* Symbol can start with $ or letter */
    if (curchar == '$' || (curchar >= 'a' && curchar <= 'z') ||
        (curchar >= 'A' && curchar <= 'Z') || curchar == '_') {
        symbuf[i++] = curchar;
        nextchar();

        /* Continue with alphanumeric or underscore */
        while ((curchar >= 'a' && curchar <= 'z') ||
               (curchar >= 'A' && curchar <= 'Z') ||
               (curchar >= '0' && curchar <= '9') ||
               curchar == '_') {
            if (i < sizeof(symbuf) - 1) {
                symbuf[i++] = curchar;
            }
            nextchar();
        }
    }

    symbuf[i] = '\0';
    return symbuf;
}

/*
 * Read a number (decimal integer or constant)
 */
long
readNumber(void)
{
    long val = 0;
    int sign = 1;

    skip();

    if (curchar == '-') {
        sign = -1;
        nextchar();
    }

    while (curchar >= '0' && curchar <= '9') {
        val = val * 10 + (curchar - '0');
        nextchar();
    }

    return val * sign;
}

/*
 * Read a type name (e.g., _short_, _char_, :ptr, :array:10)
 */
char *
readType(void)
{
    unsigned char i = 0;

    skip();

    /* Type can start with _ or : */
    if (curchar == '_' || curchar == ':') {
        while ((curchar >= 'a' && curchar <= 'z') ||
               (curchar >= 'A' && curchar <= 'Z') ||
               (curchar >= '0' && curchar <= '9') ||
               curchar == '_' || curchar == ':' || curchar == '-') {
            if (i < sizeof(typebuf) - 1) {
                typebuf[i++] = curchar;
            }
            nextchar();
        }
    }

    typebuf[i] = '\0';
    return typebuf;
}

/*
 * Save current parser state
 */
void
saveParseSt(struct parser_state *state)
{
    state->saved_buf = malloc(BUFSIZE);
    memcpy(state->saved_buf, buf, bufValid);
    state->saved_buf_pos = bufPos;
    state->saved_bufvld = bufValid;
    state->savedLineNum = lineNum;
    state->saved_curchar = curchar;
    state->saved_in_fd = inFd;
}

/*
 * Restore parser state (can be called multiple times)
 */
void
restoreParse(struct parser_state *state)
{
    memcpy(buf, state->saved_buf, state->saved_bufvld);
    bufPos = state->saved_buf_pos;
    bufValid = state->saved_bufvld;
    lineNum = state->savedLineNum;
    curchar = state->saved_curchar;
    inFd = state->saved_in_fd;
}

/*
 * Free parser state buffer (call when done with saved state)
 */
void
freeParseSt(struct parser_state *state)
{
    free(state->saved_buf);
    state->saved_buf = 0;
}

/*
 * Set up parser to read from a string buffer
 */
void
setupStrInput(char *str, int len)
{
    /* Copy string to main buffer */
    if (len > BUFSIZE - 1) {
        len = BUFSIZE - 1;
    }
    memcpy(buf, str, len);
    buf[len] = 0;
    bufPos = 0;
    bufValid = len;
    inFd = -1;  /* Mark as string input */

    /* Initialize curchar */
    if (len > 0) {
        curchar = buf[0];
        bufPos = 1;
    } else {
        curchar = 0;
    }
}

/*
 * Check if a line is a label (ends with ':')
 */
int
isLabel(char *line)
{
    int len;

    /* Trim trailing whitespace to find actual end */
    len = strlen(line);
    while (len > 0 && (line[len-1] == ' ' || line[len-1] == '\t')) {
        len--;
    }

    return len > 0 && line[len-1] == ':';
}

/*
 * Trim leading and trailing whitespace from a line
 * Also collapse multiple consecutive spaces into single spaces
 */
char *
trimLine(char *line)
{
    char *end;
    char *src, *dst;
    int lastWasSpace;

    /* Trim leading space */
    while (*line == ' ' || *line == '\t') {
        line++;
    }

    /* Collapse multiple spaces into single spaces */
    src = dst = line;
    lastWasSpace = 0;
    while (*src) {
        if (*src == ' ' || *src == '\t') {
            if (!lastWasSpace) {
                *dst++ = ' ';
                lastWasSpace = 1;
            }
            src++;
        } else {
            *dst++ = *src++;
            lastWasSpace = 0;
        }
    }
    *dst = '\0';

    /* Trim trailing space */
    end = line + strlen(line) - 1;
    while (end >= line && (*end == ' ' || *end == '\t')) {
        *end = '\0';
        end--;
    }

    return line;
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
