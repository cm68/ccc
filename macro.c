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
#include "cc1.h"

char *macbuffer;
struct macro *macros;

/*
 * add a definition from the command line args
 * Format: "NAME=value" or just "NAME" (for empty macros)
 */
void
addDefine(char *s)
{
    struct macro *m;
    char *eq;
    int namelen;


    if (!s || !*s) {
        return;
    }

    m = malloc(sizeof(*m));

    /* Find '=' to separate name from value */
    eq = strchr(s, '=');

    if (eq) {
        /* NAME=value format */
        namelen = eq - s;
        m->name = malloc(namelen + 1);
        memcpy(m->name, s, namelen);
        m->name[namelen] = '\0';
        m->mactext = strdup(eq + 1);  /* value after '=' */
    } else {
        /* Just NAME with no value (like -DDEBUG) */
        m->name = strdup(s);
        m->mactext = strdup("1");  /* default to "1" */
    }

    m->parmcount = 0;
    m->parms = 0;
    m->next = macros;
    macros = m;
}

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
    unsigned char i;
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
        free(m->mactext);
        free(m);
    } else {
    }
}


/*
 * read the macro definition and parse out the parameter names
 */
void
macdefine(char *s)
{
    unsigned char i;
    struct macro *m = malloc(sizeof(*m));
    char *parms[MAXPARMS];


    if (!macbuffer) {
        macbuffer = malloc(1024);
    }

    m->name = strdup(s);
    m->parmcount = 0;

    /*
     * Check for function-like macro: '(' must be IMMEDIATELY after name
     * with NO whitespace (C standard requirement)
     * If there's whitespace before '(', it's part of the replacement text
     */
    if (curchar == '(') {
        advance();
        while (1) {
            skipws1();
            if (issym()) {
                advance();
                parms[m->parmcount++] = strdup(s);
                skipws1();
                if (curchar == ',') {
                    advance();
                    continue;
                }
            }
            if (curchar == ')') {
                break;
            }
            gripe(ER_C_DP);
            break;  /* Exit loop on error to avoid infinite loop */
        }
        if (m->parmcount) {
            m->parms = malloc(sizeof(char *) * m->parmcount);
        }
        for (i = 0; i < m->parmcount; i++) {
            m->parms[i] = parms[i];
        }
        advance();
        skipws1();
    } else {
        /* Object-like macro: skip whitespace before replacement text */
        skipws1();
    }
    s = macbuffer;
    /* we copy to the macbuffer the entire logical line,
     * spaces and tabs included, but stop at // comments */
    while (curchar != '\n') {
        /* Check for C++ style comment */
        if (curchar == '/' && nextchar == '/') {
            /* Skip rest of line - treat // as end of macro text */
            while (curchar != '\n') {
                advance();
            }
            break;
        }
        /* Check for C-style block comment */
        if (curchar == '/' && nextchar == '*') {
            /* Skip comment - do not include in macro text */
            advance();  /* skip '/' */
            advance();  /* skip '*' */
            while (1) {
                if (curchar == '*' && nextchar == '/') {
                    advance();  /* skip '*' */
                    advance();  /* skip '/' */
                    break;
                }
                if (curchar == '\n' || curchar == 0) {
                    /* Unterminated comment in macro - stop at newline */
                    break;
                }
                advance();
            }
            /* Replace comment with single space (to separate tokens) */
            if (s > macbuffer && s[-1] != ' ' && s[-1] != '\t') {
                *s++ = ' ';
            }
            continue;
        }
        if ((curchar == '\\') && (nextchar == '\n')) {
            advance();
            curchar = ' ';
        }
        *s++ = curchar;
        advance();
    }
    *s = 0;

    /* Trim trailing whitespace from macro text */
    while (s > macbuffer && (s[-1] == ' ' || s[-1] == '\t')) {
        s--;
        *s = 0;
    }

    advance();  /* eat the newline */
    m->mactext = strdup(macbuffer);
    m->next = macros;
    macros = m;

    // printf("macro %s defined\n", m->name);

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
 * 
 * when we are called, curchar is the last character of the macro name
 */
int
macexpand(char *s)	/* the symbol we are looking up as a macro */
{
    struct macro *m;
    unsigned char plevel;
    char *d;
    unsigned char args;
    char *parms[MAXPARMS];
    unsigned char c;
    char *n;
    unsigned char i;
    int stringify = 0;
    int saw_newline = 0;

    if (!macbuffer) {
        macbuffer = malloc(1024);
    }

    m = maclookup(s);
    if (!m) {
        return 0;
    }

    // printf("macro %s called\n", m->name);

    args = 0;
    d = macbuffer;
    /* this will stop after nextchar is not white space */
    /* Track if we see newline when asm capture is active */
    while (iswhite(nextchar)) {
        if (asmCbuf && nextchar == '\n') {
            saw_newline = 1;  /* Remember that we saw a newline */
        }
        advance();
    }
    plevel = 0;
    /*
     * read the arguments from the invocation
     */
    if (nextchar == '(') {
        advance();
        plevel = 1;
        advance();
        skipws();
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
                *d++ = 0;
                parms[args++] = strdup(macbuffer);
                if (curchar == ')') {
                    break;
                }
                d = macbuffer;
                advance();
                skipws();
                continue;
            }
            *d++ = curchar;
            *d = 0;
            advance();
        }
    } // curchar should be ')'

    if (args != m->parmcount) {
        gripe(ER_C_DP);
    }

    /*
     * now we copy the macro text to the macbuffer, expanding
     * the parameters where ever we find them
     */
    d = macbuffer;
    *d = '\0';
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

        /* is it a glom or stringify */
        stringify = 0;
        if (c == '#') {
            c = *++s;
            if (c == '#') {
                c = *++s;
            } else {
                stringify = 1;
            }
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

    /* If we saw a newline during asm capture, append semicolon to macro text */
    if (saw_newline && asmCbuf) {
        *d++ = ';';
        *d++ = ' ';
        *d = 0;
    }

    // printf("insertmacro: %s %s\n", m->name, macbuffer);

    insertmacro(m->name, macbuffer);
    return 1;
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
