/*
 * util.c - Utility functions for cpp
 */
#include "cpp.h"
#include "libutil.h"
#include <stdarg.h>
#include <unistd.h>
#include <fcntl.h>

/*
 * permalloc()/permdup() (interned names, macro definitions, typedefs,
 * include paths) now live in libc: see libsrc/libc/permalloc.c.
 */

/*
 * String-intern pool for SYM and LABEL token names.
 *
 * Identifiers flow through 5+ pipeline stages and the same name is
 * referenced many times.  Rather than strdup at every tokcpy (the
 * pre-existing leak) or hand-maintain single-ownership at every drop
 * site, we keep one canonical arena copy per unique string in a
 * small hash pool.  Every tokcpy is now a flat field copy.
 *
 * The pool is bounded by source vocabulary, not stream length, and
 * is intentionally never freed (cpp is a single-shot tool).
 */
#define INTERN_HASH 127
static struct ient {
    char *str;
    unsigned short id;      /* -j: 2-byte identity, minted on first emit */
    struct ient *next;
} *ipool[INTERN_HASH];

/*
 * The id side of the pool, for the -j format: identifiers travel
 * through the passes as 2-byte ids, and the names live only here
 * and in the .n sidecar internWrite() dumps.  Ids are minted at
 * first EMISSION, not first sight, so the sidecar holds only names
 * the stream actually uses.  0 is reserved for "no name".
 */
static unsigned short nextid = 1;

#ifdef DEBUG
void
poolstats(void)
{
    extern struct macro *macros;
    struct macro *m;
    struct ient *e;
    int i, nm=0, nb=0, tb=0, pm=0, pb=0, ni=0, ib=0;
    int etext=0, en=0;
    extern int ndefstat(void);

    for (m = macros; m; m = m->next) {
        nm++;
        nb += strlen(m->name) + 1;
        if (m->mactext) tb += strlen(m->mactext) + 1;
        if (m->parms) { pm++; pb += m->parmcount * 2; }
        if (m->mactext) {
            char *p = m->mactext; int dig = 1;
            for (; *p; p++) if (!(*p>='0'&&*p<='9') && *p!='x' && *p!='-') { dig=0; break; }
            if (dig) { en++; etext += strlen(m->mactext)+1; }
        }
    }
    for (i = 0; i < INTERN_HASH; i++)
        for (e = ipool[i]; e; e = e->next) { ni++; ib += strlen(e->str)+1; }
    fdprintf(2, "POOLSTATS macros=%d names=%dB texts=%dB fnlike=%d parmB=%d numeric=%d(%dB) ndefs=%d intern=%d strB=%d\n",
        nm, nb, tb, pm, pb, en, etext, ndefstat(), ni, ib);
}
#endif

char *
intern(char *s)
{
    unsigned h = 0;
    char *p;
    struct ient *e;

    for (p = s; *p; p++)
        h = h * 31 + (unsigned char)*p;
    h %= INTERN_HASH;
    for (e = ipool[h]; e; e = e->next)
        if (strcmp(e->str, s) == 0)
            return e->str;
    e = (struct ient *)permalloc(sizeof(*e));
    e->str = permdup(s);
    e->next = ipool[h];
    ipool[h] = e;
    return e->str;
}

/*
 * The id for a name, minting one on first call.  The lexer interns
 * every identifier it reads, so the entry is normally already
 * there; a synthetic name that never went through the lexer gets
 * pooled on the way.
 */
unsigned short
idOf(char *s)
{
    unsigned h = 0;
    char *p;
    struct ient *e;

    for (p = s; *p; p++)
        h = h * 31 + (unsigned char)*p;
    h %= INTERN_HASH;
    for (e = ipool[h]; e; e = e->next)
        if (strcmp(e->str, s) == 0)
            break;
    if (!e) {
        intern(s);
        for (e = ipool[h]; e; e = e->next)
            if (strcmp(e->str, s) == 0)
                break;
    }
    if (e->id == 0)
        e->id = nextid++;
    return e->id;
}

/*
 * Write the .n sidecar, the id-to-name table for c1 and the driver.
 *
 *	2 bytes		count N, little-endian
 *	N * 2 bytes	offset of name i+1 from file start
 *	names		NUL-terminated
 *
 * Two seeks fetch any name; nothing is obliged to hold the file.
 * The offset TABLE is in id order - readers index it - but the
 * names behind it sit in whatever order the pool walk visits them,
 * each offset seeked into its slot.  The id-to-entry array this
 * replaces cost more than a kilobyte of doubling permallocs on the
 * machine where cpp itself has to fit; two walks and a seek per
 * name at exit cost nothing that matters.
 */
int
internWrite(char *fname)
{
    int fd, i;
    unsigned int off;
    unsigned char b[2];
    int n = nextid - 1;
    struct ient *e;

    fd = creat(fname, 0644);
    if (fd < 0)
        return -1;
    b[0] = n & 0xff;
    b[1] = (n >> 8) & 0xff;
    write(fd, (char *)b, 2);
    off = 2 + 2 * n;
    for (i = 0; i < INTERN_HASH; i++) {
        for (e = ipool[i]; e; e = e->next) {
            if (!e->id)
                continue;
            lseek(fd, (long)(2 + 2 * (e->id - 1)), 0);
            b[0] = off & 0xff;
            b[1] = (off >> 8) & 0xff;
            write(fd, (char *)b, 2);
            off += strlen(e->str) + 1;
        }
    }
    /* second walk, same order: the names */
    lseek(fd, (long)(2 + 2 * n), 0);
    for (i = 0; i < INTERN_HASH; i++)
        for (e = ipool[i]; e; e = e->next)
            if (e->id)
                write(fd, e->str, strlen(e->str) + 1);
    close(fd);
    return 0;
}

/*
 * Copy token structure.  Names are interned (shared canonical pointers),
 * so this is a flat field-by-field copy - no allocation, no free.
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
    "bad enum",                     /* ER_C_EV */
    "too many parameters",          /* ER_C_PC */
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
    if (err < ER_W_FIRST)  /* Not a warning */
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
        if (cur.type == NUMBER || cur.type == LNUMBER) {
            /* the suffix says nothing here - a preprocessor
             * expression is arithmetic on whatever it is handed */
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
