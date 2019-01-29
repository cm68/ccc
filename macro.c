/*
 * macros are done in a unified way with include file processing:
 *
 * I think this implementation is a little more restrictive than normal
 * cpp, in that the values to substitute need to be single tokens
 * this can probably be relaxed at some point, but I need to get a good
 * handle on what the actual standard says
 *
 * these are the canonical examples:
 * #define k(a,b) a##b
 * #define k(a,b) if (a) b=0
 * #define k(a,b) if (a) s=#b
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
        advance();
        while (1) {
            skipwhite1();
            if (issym(s)) {
                parms[m->parmcount++] = strdup(s);
                skipwhite1();
                if (curchar == ',') {
                    advance();
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
        advance();
        skipwhite1();
    }
    s = macbuffer;
    /* we copy to the macbuffer the entire logical line, 
     * spaces and tabs included */
    while (curchar != '\n') {
        if ((curchar == '\\') && (nextchar == '\n')) {
            advance();
            curchar = ' ';
        }
        *s++ = curchar;
        advance();
    }
    *s = 0;
    advance();  /* eat the newline */
    m->mactext = strdup(macbuffer);
    m->next = macros;
    macros = m;
    printf("macro %s defined\n", m->name);
}

/*
 * if our symbol is a macro, expand it
 * the arguments are processed as follows:
 * we have attached to the macro structure the array of formal
 * parameters, and when in our invocation, we have the values to
 * substitute for them.  these formal parameters look like c symbol names,
 * and the substitution values are single comma delimited tokens.
 * so, when we hit, we'll consume the macro call of foo(x,y) in the input
 * stream, build the expansion into a buffer, and insert that buffer into
 * the input stream.   when that input stream is in turn processed, we'll
 * check that for macros just like file content.  in this way, macro calls
 * inside of macro calls just work, and we process them outside-in.
 * there are some cases that will be kinda bizarre, given this architecture.
 * also, there are two operators that only work inside a macro expansion:
 * stringify (#) and glom(##).
 * stringify must immediately precede a formal parameter name, and simply
 * will stringify the actual parameter.
 * #define baz(a) #a
 * glom, when encountered in a macro expansion, simply turns into nothing,
 * while allowing keyword expansion of any adjacent identifiers
 * #define foo(a,b) a##b
 * #define bar(c,d) c##d
 * foo(b,ar(xy,zzy)) generates xyzzy
 */
int
macexpand(char *s)	/* the symbol we are looking up as a macro */
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

    m = maclookup(s);
    if (!m) {
        return 0;
    }
    printf("macro %s called\n", m->name);

    args = 0;
    d = macbuffer;
    skipwhite();
    plevel = 0;
    /*
     * read the arguments from the invocation
     */
    if (curchar == '(') {
        plevel = 1;
        advance();
        skipwhite();
        while (1) {
            /*
             * copy literals literally
             */
            if (curchar == '\'' || curchar == '\"') {
                c = curchar;
                advance();
                *d++ = c;
                while (curchar != c) {
                    *d++ = curchar;
                    if (curchar == '\\') {
                        advance();
                        *d++ = curchar;
                    }
                    advance();
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
                advance();
                skipwhite();
                continue;
            }
            *d++ = curchar;
            advance();
        }
        advance();
    }
    if (args != m->parmcount) {
        err(ER_C_DP);
    }

    /*
     * now we copy the macro text to the macbuffer, expanding
     * the parameters where ever we find them
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
        stringify = 0;
        if (c == '#' && s[1] != '#') {
            stringify = 1;
            c = *++s;
        }

        /* if macro text has something that looks like an arg */
        if (((c >= 'A') && (c <= 'Z')) || 
            ((c >= 'a') && (c <= 'z')) ||
            (c == '_')) {
            n = strbuf;
            while ((c = *s) && 
                   (((c >= 'A') && (c <= 'Z')) || 
                    ((c >= 'a') && (c <= 'z')) ||
                    ((c >= '0') && (c <= '9')) ||
                    (c == '_'))) {
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
            if (stringify) {
                *d++ = '\"';
            }
            while (*n) {
                *d++ = *n++;
            }
            if (stringify) {
                *d++ = '\"';
            }
            continue;
        }
        *d++ = *s++;
    }
    *d = 0;
    insertmacro(m->name, macbuffer);
    return 1;
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
