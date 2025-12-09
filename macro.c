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
 * Add a macro definition from command-line argument
 *
 * Processes -D command-line arguments to define preprocessor macros before
 * parsing the source file. Supports both simple and value-defined macros.
 *
 * Formats accepted:
 *   - "NAME"       -> defines NAME as 1
 *   - "NAME=value" -> defines NAME as value
 *
 * The macro is added to the front of the macro list and becomes immediately
 * available for expansion during preprocessing.
 *
 * Object-like macros only:
 *   - No parameters (parmcount=0)
 *   - Direct text replacement
 *   - Cannot define function-like macros from command line
 *
 * Examples:
 *   -DDEBUG         -> DEBUG=1
 *   -DVERSION=2     -> VERSION=2
 *   -DMAX=100       -> MAX=100
 *
 * Parameters:
 *   s - Definition string in "NAME" or "NAME=value" format
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
 * Look up a macro by name
 *
 * Searches the macro list for a macro with the specified name. The macro
 * list is a simple linked list with most recently defined macros at the
 * front.
 *
 * Search order:
 *   - Linear search from front to back
 *   - First match wins (allows redefinition by prepending)
 *   - Case-sensitive name matching
 *
 * Used by:
 *   - macexpand() to check if identifier is a macro
 *   - #ifdef/#ifndef to test macro existence
 *   - #undef to find macro for removal
 *
 * Parameters:
 *   name - Macro name to search for
 *
 * Returns:
 *   Pointer to macro structure if found, NULL if not found
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
 * Remove a macro definition (#undef)
 *
 * Searches for and removes a macro from the macro list, freeing all
 * associated memory including parameter names and text. Implements the
 * #undef preprocessor directive.
 *
 * Memory cleanup:
 *   - Frees parameter name strings (if function-like macro)
 *   - Frees parameter array
 *   - Frees macro name string
 *   - Frees macro text string
 *   - Frees macro structure itself
 *
 * List maintenance:
 *   - Searches linearly for macro by name
 *   - Unlinks from list (updates prev->next or list head)
 *   - Handles both head and middle removal
 *
 * Silently succeeds if macro not found (standard C preprocessor behavior).
 *
 * Parameters:
 *   s - Name of macro to undefine
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
 * Parse and define a macro from #define directive
 *
 * Reads a macro definition from the input stream and creates a macro
 * structure. Handles both object-like and function-like macros with
 * parameter parsing.
 *
 * Macro forms:
 *   - Object-like:     #define NAME replacement_text
 *   - Function-like:   #define NAME(a,b) replacement_text
 *
 * Function-like detection:
 *   - '(' must IMMEDIATELY follow name (no whitespace) per C standard
 *   - Whitespace before '(' means object-like macro with '(' in text
 *
 * Parameter parsing:
 *   - Comma-separated identifiers: NAME(a,b,c)
 *   - Whitespace allowed around commas
 *   - Closing ')' terminates parameter list
 *   - Parameters stored in macro->parms array
 *
 * Replacement text processing:
 *   - Everything after parameters/whitespace until newline
 *   - Backslash-newline: Continues macro to next line (replaced with space)
 *   - C++ comments (//): Terminates macro text (not included)
 *   - C block comments: Removed, replaced with single space
 *   - Trailing whitespace: Trimmed
 *
 * Special operators in replacement text:
 *   - Hash (stringify): Must precede parameter name
 *   - Double-hash (token paste): Glues adjacent tokens together
 *   - Both handled during expansion, not definition
 *
 * Parameters:
 *   s - Macro name (already parsed from input stream)
 *
 * Side effects:
 *   - Consumes input stream through newline
 *   - Adds macro to front of macro list
 *   - Macro immediately available for expansion
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
 * Expand a macro invocation
 *
 * Checks if an identifier is a macro and expands it if so. Handles both
 * object-like and function-like macros with parameter substitution,
 * stringify (#), and token pasting (##) operators.
 *
 * Expansion process:
 *   1. Look up macro by name
 *   2. For function-like macros: parse argument list from input
 *   3. Build expansion text with parameter substitution
 *   4. Insert expansion into input stream via insertmacro()
 *   5. Recursive expansion happens when inserted text is processed
 *
 * Object-like macros:
 *   - Direct text replacement
 *   - No arguments parsed
 *   - Example: #define MAX 100
 *
 * Function-like macros:
 *   - Arguments parsed from (arg1, arg2, ...) in input
 *   - Parentheses levels tracked (nested calls handled)
 *   - String/character literals copied verbatim
 *   - Arguments matched to parameters by position
 *   - Parameter mismatch generates error
 *
 * Parameter substitution:
 *   - Formal parameters in macro text replaced with actual arguments
 *   - Identifiers in macro text matched against parameter names
 *   - Non-matching identifiers passed through unchanged
 *
 * Stringify operator (single hash):
 *   - Converts parameter to string literal
 *   - Adds quotes around parameter value in expansion
 *   - Example: STR(x) with "x" -> STR(foo) becomes "foo"
 *
 * Token paste operator (double hash):
 *   - Concatenates adjacent tokens
 *   - Simply removed during expansion (tokens already adjacent)
 *   - Example: CONCAT(a,b) with "a" and "b" -> CONCAT(foo,bar) becomes foobar
 *
 * Nested macro calls:
 *   - Arguments can contain macro invocations
 *   - Processed outside-in (outer expanded first)
 *   - Example: foo(bar(x)) expands foo first with bar(x) as argument
 *
 * Special asm block handling:
 *   - If newline seen during argument parsing in asm block
 *   - Appends semicolon to expansion text
 *   - Maintains asm statement separation
 *
 * Parameters:
 *   s - Identifier name to check and potentially expand
 *
 * Returns:
 *   1 if macro found and expanded, 0 if not a macro
 *
 * Side effects:
 *   - Consumes macro arguments from input stream
 *   - Inserts expansion text into input stream
 *   - Advances curchar/nextchar to start of expansion
 */
char
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
    while (iswhite(nextchar)) {
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
                /* Only count argument if we have content, OR if we already
                 * have args (to handle trailing comma case like FOO(a,)) */
                if (d > macbuffer + 1 || args > 0) {
                    parms[args++] = strdup(macbuffer);
                }
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

    // printf("insertmacro: %s %s\n", m->name, macbuffer);

    insertmacro(m->name, macbuffer);
    return 1;
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
