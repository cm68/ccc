#include "ccc.h"

#define MAXPARMS    10

struct macro *macros;

/*
 * look up a macro in the macro table
 */
struct macro *
maclookup(char *name)
{
    struct macro *m;

    for (m = macros; m; m = m->next) {
        if (strcmp(m->name, name) == 0) {
            return m;
        }
    }
    return 0;
}

/*
 * remove a name from the macro list
 */
void
macundefine(char *s)
{
    int i;
    struct macro *m, *p;
    p = 0;

    for (m = macros; m; m = m->next) {
        if (strcmp(m->name, s) == 0) {
            break;
        }
        p = m;
    }
    if (m) {
        if (!p) {
            macros = m->next;
        } else {
            p->next = m->next;
        }
        for (i = 0; i < m->parmcount; i++) {
            free(m->parms[i]);
        }
        free(m->parms); 
        free(m->name);
        free(m);
    }
}


/*
 * read the macro definition and parse out the parameter names
 */
void
macdefine(char *s)
{
    int i;
    struct macro *m = malloc(sizeof(*m));
    char *parms[MAXPARMS];

    m->name = strdup(s);
    m->parmcount = 0;
    skipwhite1();

    /*
     * get the parameter names from the (<a>,<b>,...) list
     */
    if (curchar == '(') {
        getchar();
        while (1) {
            skipwhite1();
            if (issym(s)) {
                parms[m->parmcount++] = strdup(s);
                getchar();
                skipwhite1();
                if (curchar == ',') {
                    getchar();
                    continue;
                }
            }
            if (curchar == ')') {
                break;
            }
            err(ER_C_DP);
        }
        if (m->parmcount) {
            m->parms = malloc(sizeof(char *) * m->parmcount);
        }
        for (i = 0; i < m->parmcount; i++) {
            m->parms[i] = parms[i];
        }
        getchar();
        skipwhite1();
    }
    s = macbuffer;
    /* we copy to the macbuffer the entire logical line, spaces and tabs included */
    while (curchar != '\n') {
        if ((curchar == '\\') && (nextchar == '\n')) {
            getchar();
            curchar = ' ';
        }
        *s++ = curchar;
    }
    *s = 0;
    getchar();  /* eat the newline */
    m->mactext = strdup(macbuffer);
    m->next = macros;
    macros = m;
}

/*
 * if our symbol is a macro, expand it
 */
void
macexpand(char *s)
{
    struct macro *m;
    char plevel;
    char *d;
    char args;
    char *parms[MAXPARMS];
    char c;
    char *n;
    int i;

    for (m = macros; m; m = m->next) {
        if (strcmp(m->name, s) == 0) {
            break;
        }
    }
    if (!m) {
        return;
    }
    args = 0;
    d = macbuffer;
    skipwhite();
    /*
     * read the arguments
     */
    if (curchar == '(') {
        getchar();
        while (1) {
            skipwhite();
            /*
             * copy literals literally
             */
            if (curchar == '\'' || curchar == '\"') {
                c = curchar;
                getchar();
                *d++ = c;
                while (curchar != c) {
                    *d++ = curchar;
                    if (curchar == '\\') {
                        getchar();
                        *d++ = curchar;
                    }
                    getchar();
                }
            }
            if (curchar == '(') {
                plevel++;
            }
            if (curchar == ')') {
                plevel--;
            }
            /*
             * only advance when we have a non-parenthesized comma
             */
            if ((plevel == 0) && ((curchar == ',') || (curchar == ')'))) {
                *d++ = '\0';
                parms[args++] = strdup(macbuffer);
                if (curchar == ')') {
                    break;
                }
                d = macbuffer;
                getchar();
                continue;
            }
            *d++ = curchar;
            getchar();
        }
        getchar();
    }
    if (args != m->parmcount) {
        err(ER_C_DP);
    }

    d = macbuffer;
    s = m->mactext;

    while (*s) {
        c = *s;
        if ((c == '\'') || (c == '\"')) {
            *d++ = *s++;
            while (*s != c) {
                if (*s == '\\') {
                    *d++ = *s++;
                }
                *d++ = *s++;
            }
            *d++ = *s++;
            continue;
        }
        if (((c >= 'A') && (c <= 'z')) || ((c >= '_') && (c <= 'z'))) {
            n = strbuf;
            while (((c >= 'A') && (c <= 'z')) || 
                    ((c >= '_') && (c <= 'z')) ||
                    ((c >= '0') && (c <= '9'))) {
                *n++ = *s++;
            }
            *n++ = 0;
            n = strbuf;
            for (i = 0; i < args; i++) {
                if (strcmp(m->parms[i], strbuf) == 0) {
                    n = parms[i];
                    break;
                }
            }
            while (*n) {
                *d++ = *n++;
            }
            continue;
        }
        *d++ = *s++;
    }
    *d = 0;
    insertmacro(m->name);
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
