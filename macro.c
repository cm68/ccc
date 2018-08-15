#include "ccc.h"
#include "lex.h"
#define MAXPARMS    10

/*
 * read the macro definition and parse out the parameter names
 */
void
macdefine(char *s)
{
    int i;
    struct macro *m = malloc(sizeof(*m));
    char *parm[MAXPARMS];

    m->macnam = strdup(s);
    m->parmcount = 0;
    eatwhite1();

    /*
     * get the parameter names from the (<a>,<b>,...) list
     */
    if (curchar == '(') {
        getchar();
        while (1) {
            eatwhite1();
            if (issym(s)) {
                parm[m->parmcount++] = strdup(s);
                getchar();
                eatwhite1();
                if (curchar == ',') {
                    getchar();
                    continue;
                }
            }
            if (curchar == ')') {
                break;
            }
            lose("busted macro param list");
        }
        if (m->parmcount) {
            m->parms = malloc(sizeof(char *) * m->parmcount);
        }
        for (i = 0; i < m->parmcount; i++) {
            m->parms[i] = parms[i];
        }
        getchar();
        eatwhite1();
    }
    s = macbuffer;
    while (curchar != '\n') {
        if ((curchar == '\\') && (nextchar == '\n')) {
            getchar();
            curchar = ' ';
        }
        shrinkwhite1();
        *s++ = curchar;
    }
    *s = 0;
    getchar();
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
    char *parm[MAXPARMS];

    for (m = macros; m; m = m->next) {
        if (strcmp(m, s) == 0) {
            break;
        }
    }
    if (!m) {
        return 0;
    }
    args = 0;
    d = macbuffer;
    eatwhite();
    /*
     * read the arguments
     */
    if (curchar == '(') {
        getchar();
        eatwhite();
        while (1) {
            shrinkwhite();
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
                parm[args++] = strdup(macbuffer);
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
        lose("macro arg count mismatch");
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
            n = nsymbuf;
            while (((c >= 'A') && (c <= 'z')) || 
                    ((c >= '_') && (c <= 'z')) ||
                    ((c >= '0') && (c <= '9'))) {
                *n++ = *s++;
            }
            *n++ = 0;
            n = nsymbuf;
            for (i = 0; i < args; i++) {
                if (strcmp(m->parm, nsymbuf) == 0) {
                    n = a[i];
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
    insertmacro(m->macnam);
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
