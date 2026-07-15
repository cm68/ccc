/*
 * util.c - Utility functions for cpp
 */
#include "cpp.h"
#include "libutil.h"
#include <stdarg.h>
#include <unistd.h>

/*
 * Bump arena for permanent allocations (interned names, macro
 * definitions, typedefs, include paths).  These live until exit, so
 * there is no free: we carve them out of big malloc'd chunks with no
 * per-object header.  On the Z80 this saves the allocator overhead of
 * a thousand-plus tiny blocks; a #undef simply abandons its storage.
 */
#define PCHUNK 1024

static char *pnext;			/* cursor into current chunk */
static unsigned int pleft;	/* bytes left in current chunk */
static char *pchain;		/* chunk bases, linked through word 0 */

static void
pgrab(int n)
{
    int chunk;
    char *p;

    n += sizeof(char *);
    chunk = n > PCHUNK ? n : PCHUNK;
    p = malloc(chunk);
    if (p == 0) {
        write(2, "cpp: out of memory\n", 19);
        exit(1);
    }
    /* chain chunk bases so the arena stays walkable (and provably
       reachable under valgrind) */
    *(char **)p = pchain;
    pchain = p;
    pnext = p + sizeof(char *);
    pleft = chunk - sizeof(char *);
}

/* aligned permanent allocation (structs, pointer arrays) */
char *
permalloc(int n)
{
    char *p;
    int pad;

    pad = (int)(sizeof(char *) - 1) & -(int)pnext;
    if (n + pad > pleft) {
        pgrab(n);
        pad = 0;
    }
    p = pnext + pad;
    pnext = p + n;
    pleft -= n + pad;
    return p;
}

/* permanent string copy (unaligned, packed tight) */
char *
permdup(char *s)
{
    int n = strlen(s) + 1;
    char *p;

    if (n > pleft)
        pgrab(n);
    p = pnext;
    pnext += n;
    pleft -= n;
    return strcpy(p, s);
}

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
    struct ient *next;
} *ipool[INTERN_HASH];

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
