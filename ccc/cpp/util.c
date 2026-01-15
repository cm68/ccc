/*
 * util.c - Utility functions for cpp
 */
#include "cpp.h"
#include "libutil.h"
#include <stdarg.h>
#include <unistd.h>

/*
 * Copy token structure
 * Note: duplicates name for SYM tokens to avoid dangling pointers
 */
void
tokcpy(struct token *d, struct token *s)
{
#ifdef DEBUG
    extern short verbose;
    extern int fdprintf(int, char*, ...);
    if ((verbose & 2) && s->type == SYM)
        fdprintf(2, "tokcpy SYM: %s\n", s->v.name ? s->v.name : "(null)");
#endif
    d->type = s->type;
    d->lineno = s->lineno;
    d->filename = s->filename;
    if (s->type == SYM && s->v.name)
        d->v.name = strdup(s->v.name);
    else
        d->v.numeric = s->v.numeric;
}

/*
 * Synthesize a simple token (no value)
 */
void
toksynth(struct token *out, unsigned char type)
{
    out->type = type;
    out->v.numeric = 0;
    out->lineno = lineno;
    out->filename = filename;
}

/*
 * Synthesize a named token (SYM or LABEL)
 */
void
toksynthnam(struct token *out, unsigned char type, char *name)
{
    out->type = type;
    out->v.name = name;
    out->lineno = lineno;
    out->filename = filename;
}

/*
 * Error messages for error codes
 */
static char *errmsgs[] = {
    "unknown error",
    "invalid escape sequence",      /* ER_C_NX */
    "bad character constant",       /* ER_C_BC */
    "bad numeric constant",         /* ER_C_CD */
    "token too long",               /* ER_C_TL */
    "macro name expected",          /* ER_C_MN */
    "#elif without #if",            /* ER_C_CU */
    "missing #endif",               /* ER_C_ME */
    "invalid directive",            /* ER_C_ID */
    "bad digit",                    /* ER_C_BD */
    "unknown token",                /* ER_C_UT */
    "defined requires identifier",  /* ER_C_DP */
    "macro argument count mismatch", /* ER_C_MA */
    "symbol truncated (warning)",   /* ER_W_SYMTRUNC */
};

extern int exitCode;

char printbuf[128];

/*
 * Report an error by code
 */
void
gripe(error_t err)
{
    char *msg = (err < sizeof(errmsgs)/sizeof(errmsgs[0])) ? errmsgs[err] : "unknown error";
    fmtstr(printbuf, "%s:%d: %s\n", filename ? filename : "?", lineno, msg);
    write(2, printbuf, strlen(printbuf));
    if (err < ER_LAST)  /* Not a warning */
        exitCode = 1;
}

/*
 * Return the index in an array of the first occurrence of a char
 * Return 0xff for miss
 */
unsigned char
lookupc(char *s, unsigned char c)
{
    unsigned char i;
    for (i = 0; s[i]; i++) {
        if (c == (unsigned char)s[i]) {
            return i;
        }
    }
    return 0xff;
}

/*
 * Simple fdprintf implementation (debug only)
 */
#ifdef DEBUG
int
fdprintf(int fd, char *fmt, ...)
{
    va_list ap;
    int len;

    va_start(ap, fmt);
    len = vsprintf(printbuf, fmt, ap);
    va_end(ap);

    write(fd, printbuf, len);
    return len;
}

char xxbuf[200];

void
hexdump(char *tag, char *h, int l)
{
    int i;
    char *z = xxbuf;
    unsigned char c;

    strcpy(xxbuf, tag);

    for (i = 0; i < l; i++) {
        c = h[i];
        if ((i % 16) == 0) {
            fdprintf(2, " %s\n%04x  ", xxbuf, i);
            z = xxbuf;
            *z = 0;
        }
        fdprintf(2, "%02x ", c);
        if ((i % 4) == 3) printf(" ");
        if ((c < ' ') || (c > 0x7e)) c = '.';
        *z++ = c;
        *z = 0;
    }
    while ((i++ % 16) != 0) {
        if ((i % 4) == 3) printf(" ");
        fdprintf(2, "   ");
    }
    printf(" %s\n", xxbuf);
}
#endif

/*
 * Parse constant expression for #if/#elif
 * This is a simplified version - just evaluate basic expressions
 */
long
parseConst(token_t stop)
{
    long val = 0;
    long term;
    char op = 0;

    while (cur.type != stop && cur.type != E_O_F) {
        /* Get term value */
        if (cur.type == NUMBER) {
            term = cur.v.numeric;
        } else if (cur.type == SYM) {
            /* Undefined macro evaluates to 0 */
            term = 0;
        } else if (cur.type == LPAR) {
            gettoken();
            term = parseConst(RPAR);
            if (cur.type == RPAR)
                gettoken();
        } else if (cur.type == BANG) {
            gettoken();
            term = !parseConst(stop);
            continue;
        } else if (cur.type == TWIDDLE) {
            gettoken();
            term = ~parseConst(stop);
            continue;
        } else if (cur.type == MINUS) {
            gettoken();
            term = -parseConst(stop);
            continue;
        } else {
            break;
        }

        /* Apply pending operator */
        switch (op) {
        case 0:   val = term; break;
        case '+': val = val + term; break;
        case '-': val = val - term; break;
        case '*': val = val * term; break;
        case '/': val = term ? val / term : 0; break;
        case '%': val = term ? val % term : 0; break;
        case '&': val = val & term; break;
        case '|': val = val | term; break;
        case '^': val = val ^ term; break;
        case '<': val = val < term; break;
        case '>': val = val > term; break;
        case 'Q': val = val == term; break;  /* EQ */
        case 'n': val = val != term; break;  /* NEQ */
        case 'L': val = val <= term; break;  /* LE */
        case 'g': val = val >= term; break;  /* GE */
        case 'j': val = val && term; break;  /* LAND */
        case 'h': val = val || term; break;  /* LOR */
        case 'y': val = val << term; break;  /* LSHIFT */
        case 'w': val = val >> term; break;  /* RSHIFT */
        }

        gettoken();

        /* Get operator */
        if (cur.type == PLUS || cur.type == MINUS ||
            cur.type == STAR || cur.type == DIV || cur.type == MOD ||
            cur.type == AND || cur.type == OR || cur.type == XOR ||
            cur.type == LT || cur.type == GT ||
            cur.type == EQ || cur.type == NEQ ||
            cur.type == LE || cur.type == GE ||
            cur.type == LAND || cur.type == LOR ||
            cur.type == LSHIFT || cur.type == RSHIFT) {
            op = cur.type;
            gettoken();
        } else {
            break;
        }
    }

    return val;
}

/* vim: set tabstop=4 shiftwidth=4 noexpandtab: */
