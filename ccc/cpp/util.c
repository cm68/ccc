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
    unsigned short id;      /* -j: 2-byte identity, minted on first emit */
    struct ient *next;
    char name[1];           /* the entry is cut to fit */
} *ipool[INTERN_HASH];

/*
 * The name is embedded, so the entry is pointer arithmetic away
 * from any canonical string - which makes minting an id O(1) once
 * intern() has vouched for the pointer.
 */
#define IENTOF(s) ((struct ient *)((s) - \
    ((char *)&((struct ient *)0)->name - (char *)0)))

/*
 * The id side of the pool, for the -j format: identifiers travel
 * through the passes as 2-byte ids, and the names live only here
 * and in the .n sidecar internWrite() dumps.  Ids are minted at
 * first EMISSION, not first sight, so the sidecar holds only names
 * the stream actually uses.  0 is reserved for "no name".
 *
 * The id is the low 14 bits.  The top two are a score of how often
 * the name has been emitted, saturating at "more than once" - and a
 * name the stream mentions exactly once is mentioned only where it is
 * declared, so nothing in the file refers to it and c0 need not
 * remember it at all.  That is most of what c0 holds: on pass1's
 * outast.c, 222 of its 258 file-scope names are never referred to,
 * and at 37 bytes each they are the bulk of the heap it runs out of.
 *
 * The score lives in the id rather than in a field of its own because
 * cpp has the largest peak of the three passes and the least room to
 * spare at it - the entries are already the biggest thing in its
 * arena, and a byte apiece is a few hundred it does not have.  Ids
 * run to a few hundred, so fourteen bits is room to spare, and going
 * past it is a fatal rather than a wrapped id.
 */
#define IDMASK  0x3fff      /* the id proper */
#define IDONCE  0x4000      /* emitted exactly once */
#define IDMANY  0x8000      /* emitted more than once */

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
    extern int ndefstat(int *);

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
        for (e = ipool[i]; e; e = e->next) { ni++; ib += strlen(e->name)+1; }
    {
        extern int incstat(int *);
        extern int tbpeak;
        int ndb, inb, nd, in;
        nd = ndefstat(&ndb);
        in = incstat(&inb);
        fdprintf(2, "POOLSTATS macros=%d names=%dB texts=%dB fnlike=%d parmB=%d numeric=%d(%dB) ndefs=%d(%dB) intern=%d strB=%d\n",
            nm, nb, tb, pm, pb, en, etext, nd, ndb, ni, ib);
        {
            extern int macpeak;
            fdprintf(2, "POOLSTATS2 incs=%d(%dB) tbpeak=%d macpeak=%d\n",
                in, inb, tbpeak, macpeak);
        }
    }
    {
        extern long fbufPk;
        extern long tdkeepB;
        extern int tdkeepN;

        fdprintf(2, "POOLSTATS3 fbufpeak=%ld tdkept=%d(%ldB)\n",
            fbufPk, tdkeepN, tdkeepB);
        { extern void bufdump(void); bufdump(); }
    }
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
        if (strcmp(e->name, s) == 0)
            return e->name;
    e = (struct ient *)permalloc(sizeof(*e) + strlen(s));
    strcpy(e->name, s);
    e->next = ipool[h];
    ipool[h] = e;
    return e->name;
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
    struct ient *e;

    e = IENTOF(intern(s));
    if ((e->id & IDMASK) == 0) {
        if (nextid > IDMASK) {
            write(2, "cpp: too many identifiers\n", 26);
            exit(1);
        }
        e->id = nextid++;
    }
    /* none -> once -> many, and many stays many */
    if (e->id & IDONCE)
        e->id = (e->id & ~IDONCE) | IDMANY;
    else if (!(e->id & IDMANY))
        e->id |= IDONCE;
    return e->id & IDMASK;
}

/*
 * Write the .n sidecar, the id-to-name table for c1 and the driver.
 *
 *	2 bytes		count N, little-endian
 *	N * 2 bytes	offset of name i+1 from file start
 *	(N+7)/8 bytes	score bitmap: bit i-1 set = id i emitted once
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
/*
 * The sidecar fd is private - opened, written, seeked, closed right
 * here - and nothing ever asks for its tracked position, so the
 * Z80 build can use the bare seek syscall and leave lseek's _fdpos
 * machinery (and its 600 bytes) out of the binary entirely.
 */
#ifdef CCC
extern int seekraw();
#define NSEEK(fd, off) seekraw(fd, (int)(off), 0)
#else
#define NSEEK(fd, off) lseek(fd, (long)(off), 0)
#endif

int
internWrite(char *fname)
{
    int fd, i, id;
    unsigned int off;
    unsigned char b[2];
    int n = nextid - 1;
    int nb = (n + 7) / 8;
    char *once;
    struct ient *e;

    fd = creat(fname, 0644);
    if (fd < 0)
        return -1;
    once = permalloc(nb);       /* zeroed */
    b[0] = n & 0xff;
    b[1] = (n >> 8) & 0xff;
    write(fd, (char *)b, 2);
    off = 2 + 2 * n + nb;       /* names sit behind the scores */
    for (i = 0; i < INTERN_HASH; i++) {
        for (e = ipool[i]; e; e = e->next) {
            id = e->id & IDMASK;
            if (!id)
                continue;
            if (e->id & IDONCE)
                once[(id - 1) >> 3] |= 1 << ((id - 1) & 7);
            NSEEK(fd, 2 + 2 * (id - 1));
            b[0] = off & 0xff;
            b[1] = (off >> 8) & 0xff;
            write(fd, (char *)b, 2);
            off += strlen(e->name) + 1;
        }
    }
    /*
     * The scores, then the names behind them.  A reader that wants
     * only spellings is unaffected: it indexes the offset table and
     * seeks to what it finds there, never assuming where the names
     * begin.  Putting the scores at a spot the header alone gives -
     * 2 + 2N - is what lets c0 reach them without seeking to the end
     * of the file, which the Z80 lseek is no good at.
     */
    NSEEK(fd, 2 + 2 * n);
    write(fd, once, nb);
    /* second walk, same order: the names */
    for (i = 0; i < INTERN_HASH; i++)
        for (e = ipool[i]; e; e = e->next)
            if (e->id & IDMASK)
                write(fd, e->name, strlen(e->name) + 1);
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
 * Allocation that cannot come back empty.
 *
 * There were thirteen malloc call sites in this program and not one
 * of them looked at what came back.  Every one dereferenced it
 * immediately, so when the heap ran out the null was used as a
 * buffer - and on this machine the first thing written through a
 * null lands in page zero, where the rst 08 syscall trap lives.
 *
 * Destroying that trap is not a crash.  The next write() simply does
 * not trap: execution runs forward through page zero, which is now
 * zeros, until it reaches 0x0100 - the entry point - which calls
 * main again.  main runs out of memory again, and again, each pass
 * eating another frame, until the stack has walked down through the
 * heap and into the text.  cpp looping and consuming itself, with no
 * diagnostic anywhere and every symptom a long way from the cause.
 *
 * There is nothing useful to do with a failed allocation here, so
 * say so and stop.
 */
void
xnomem(void)
{
    write(2, "cpp: out of memory\n", 19);
    exit(1);
}

char *
xalloc(unsigned int n)
{
    char *p = malloc(n);

    if (!p)
        xnomem();
    return p;
}

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
