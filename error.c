/*
 * errors, messages, and recovery
 */
#include "cc1.h"

#define DEF_ERRMSG
#include "error.h"

int error;

/*
 * print a message for an error code and emit an error location
 * continue
 */
void
gripe(error_t errcode)
{
    int i;
    struct textbuf *t;

    i = errcode;
    if (i > ER_WTF) i = ER_WTF;
    printf("file: %s line: %d error code %d %s\n",
        filename, lineno, errcode, errmsg[i]);

    /* Print include chain traceback */
    if (tbtop) {
        /* Start from current file and walk backwards */
        for (t = tbtop; t; t = t->prev) {
            /* Skip macro buffers, only show files */
            if (t->fd != -1 && t->name) {
                /* Don't print the current file (already shown in main error message) */
                if (t != tbtop || (filename && strcmp(filename, t->name) != 0)) {
                    printf("  included from: %s line: %d\n",
                        t->name, t->lineno);
                }
            }
        }
    }

    error = errcode;
}

/*
 * dump the symbol table
 */
void
dump_symbols()
{
    int i;
    struct name *n;
    struct type *t;

    printf("\n=== SYMBOL TABLE DUMP ===\n");
    printf("lexlevel=%d lastname=%d\n\n", lexlevel, lastname);

    printf("--- NAMES TABLE ---\n");
    if (names && lastname >= 0) {
        for (i = 0; i <= lastname; i++) {
            n = names[i];
            if (n) {
                printf("[%d] %s%s kind=%s level=%d sclass=0x%x",
                    i, n->is_tag ? "tag:" : "",
                    n->name ? n->name : "(null)",
                    (n->kind >= 0 && n->kind <= 10) ? kindname[n->kind] : "???",
                    n->level, n->sclass);
                if (n->type) {
                    printf(" type=");
                    dump_type(n->type, 0);
                }
                printf("\n");
            }
        }
    } else {
        printf("(names table not initialized)\n");
    }

    printf("\n--- TYPES TABLE ---\n");
    if (types) {
        int type_count = 0;
        int max_types = 1000;  /* Prevent infinite loops from cycles */
        for (t = types; t && type_count < max_types; t = t->next) {
            printf("type @%p: ", (void*)t);
            dump_type(t, 0);
            printf("\n");
            type_count++;
        }
        if (t) {
            printf("... (stopped after %d types, possible cycle)\n", max_types);
        }
    } else {
        printf("(types table not initialized)\n");
    }
    printf("=== END SYMBOL TABLE DUMP ===\n\n");
}

/*
 * some errors are too nasty to fix
 */
void
fatal(error_t errcode)
{
    gripe(errcode);
    printf("too severe to recover\n");
    dump_symbols();
    exit(-errcode);
}

/*
 * throw an error message and discard tokens until we see the token we specify
 */
void
recover(error_t errcode, token_t skipto)
{
    gripe(errcode);
    while ((cur.type != skipto) && (cur.type != E_O_F)) {
        gettoken();
    }
}

/*
 * the next token must be 'check'.  if it isn't, gripe about it and skip
 * until we find 'skipto'
 */
void
need(token_t check, token_t skipto, error_t errcode)
{
    if (cur.type == check) {
        gettoken();
        return;
    }
    recover(errcode, skipto);
}

#ifdef notdef
/*
 * a variant sprintf with support for formats: 
 * %[<precision>]<format>
 * where precision is [-][0][0-9]*
 *    if -, right pad
 *    if leading 0, zero pad
 * formats:
 *    b - binary
 *    d - decimal
 *    x - hex
 *    s - string     
 */
void
sprintf(char *d, char *fmt)
{
    char **ap;
    char c;
    unsigned char zpad;
    unsigned char width;
    unsigned char base;
    unsigned char i;
    unsigned char rpad;
    int v;
    char b[8];
    char *s;

    ap = &fmt;
    ap++;

    while ((c = *fmt++)) {
        if (c != '%') {
            *d++ = c;
            continue;
        }
        c = *fmt++;
        zpad = 0;
        width = 0;
        base = 0;
        rpad = 0;
        if (c == '-') {
            rpad++; 
            c = *fmt++;
        }
        if (c == '0') {
            zpad++;
            c = *fmt++;
        }
        while (c >= '0' && c <= '9') {
            width = width * 10 + c - '0';
            c = *fmt++;
        }
        for (i = 0; i < sizeof(b); i++) {
            b[i] = zpad ? '0' : ' ';
        }
        if (c == 'd') base = 10;
        else if (c == 'x') base = 16;
        else if (c == 'b') base = 2;

        if ((c == 'd') || (c == 'x') || (c == 'b')) {
            v = *((int *)ap);
            ap++;
            i--;
            while (v) {
                c = v % base;
                v /= base;
                if ((base == 16) && (c > 9)) {
                    c += 'a' - 10;
                } else {
                    c += '0';
                }
                b[i--] = c;
            }
            if (rpad) {
                i = sizeof(b) - (i + 1);
            } else {
                i = sizeof(b) - width;
            }
            while (width--) {
                if (i > sizeof(b)) {
                    *d++ = ' ';
                } else {
                    *d++ = b[i++];
                }
            }
            continue;
        }
        if (c == 's') {
            s = *ap;
            ap++;
            i = strlen(s);
            if (!rpad) {
                i = width - i;
                while (i--) {
                    *d++ = ' ';
                    width--;
                }
            }
            while (width--) {
                if (*s) {
                    *d++ = *s++;
                } else {
                    *d++ = ' ';
                }
            }
            continue;
        }
    }
}
#endif

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */

