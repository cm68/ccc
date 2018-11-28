/*
 * macros are done in a unified way with include file processing:
 *
 *
 */
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

    if (!macbuffer) {
        macbuffer = malloc(1024);
    }

    m->name = strdup(s);
    m->parmcount = 0;
    skipwhite1();

    /*
     * get the parameter names from the (<a>,<b>,...) list
     */
    if (curchar == '(') {
        getnext();
        while (1) {
            skipwhite1();
            if (issym(s)) {
                parms[m->parmcount++] = strdup(s);
                skipwhite1();
                if (curchar == ',') {
                    getnext();
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
        getnext();
        skipwhite1();
    }
    s = macbuffer;
    /* we copy to the macbuffer the entire logical line, spaces and tabs included */
    while (curchar != '\n') {
        if ((curchar == '\\') && (getnext == '\n')) {
            getnext();
            curchar = ' ';
        }
        *s++ = curchar;
        getnext();
    }
    *s = 0;
    getnext();  /* eat the newline */
    m->mactext = strdup(macbuffer);
    m->next = macros;
    macros = m;
}

/*
 * if our symbol is a macro, expand it
 */
int
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
    int stringify = 0;

    if (!macbuffer) {
        macbuffer = malloc(1024);
    }

    for (m = macros; m; m = m->next) {
        if (strcmp(m->name, s) == 0) {
            break;
        }
    }
    if (!m) {
        return 0;
    }
    args = 0;
    d = macbuffer;
    skipwhite();
    /*
     * read the arguments
     */
    if (curchar == '(') {
        plevel = 1;
        getnext();
        skipwhite();
        while (1) {
            /*
             * copy literals literally
             */
            if (curchar == '\'' || curchar == '\"') {
                c = curchar;
                getnext();
                *d++ = c;
                while (curchar != c) {
                    *d++ = curchar;
                    if (curchar == '\\') {
                        getnext();
                        *d++ = curchar;
                    }
                    getnext();
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
            if (((plevel == 1) && (curchar == ',')) || 
                ((plevel == 0) && (curchar == ')'))) {
                *d++ = '\0';
                parms[args++] = strdup(macbuffer);
                if (curchar == ')') {
                    break;
                }
                d = macbuffer;
                getnext();
                skipwhite();
                continue;
            }
            *d++ = curchar;
            getnext();
        }
        getnext();
    }
    if (args != m->parmcount) {
        err(ER_C_DP);
    }

    /*
     * now we copy the macro text to the macbuffer, expanding
     * the parameters whereever we find them
     */
    d = macbuffer;
    s = m->mactext;

    while (*s) {
        c = *s;
        /* literals go straight across */
        if ((c == '\'') || (c == '\"')) {
            *d++ = *s++;
            while (*s != c) {
                /* don't notice literal next quote */
                if (*s == '\\' && s[1] == c) {
                    *d++ = *s++;
                }
                *d++ = *s++;
            }
            *d++ = *s++;
            continue;
        }
        /* if the 'stringify' operator is present */
        if (c == '#' && s[1] != '#') {
            stringify = 1;
        }

        /* if macro text has something that looks like an arg */
        if (((c >= 'A') && (c <= 'z')) || ((c >= '_') && (c <= 'z'))) {
            n = strbuf;
            while ((c = *s) && 
                   (((c >= 'A') && (c <= 'z')) || 
                    ((c >= '_') && (c <= 'z')) ||
                    ((c >= '0') && (c <= '9')))) {
                *n++ = *s++;
            }
            *n++ = 0;
            n = strbuf;
            /* if it matches our declared arg name */
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
    return 1;
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
