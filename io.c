/*
 * this i/o machinery is a unification of macro expansion, include files
 * and source file buffering.  these are stacked, so we can push
 * include files and save our position at each level
 *
 * we don't do any character processing at all at this level.
 * if there are nasty control characters, etc, we pass them up.
 * not our job; except nulls.  those are dirty; the first null is eof.
 */
#include "cc1.h"
#ifndef SDCC
#include <fcntl.h>
#include <unistd.h>
#endif

/*
 * the incoming character stream interface
 * a zero is EOF
 */
unsigned char curchar;               // the current character
unsigned char nextchar;              // the next char - can change if macro
int lineno;                 // line number for error messages
char *filename;             // current file name
int column;                 // this is reset to 0 when we see a newline
int nextcol = 0;
char namebuf[128];

/*
 * the formal definition of offset is the first unread character.
 * this is effectively the lookahead character, nextchar.
 * if we have not read anything from this buffer yet, it is zero.
 * advance() places this character into curchar.
 * if we do a macro insertion, it is after curchar
 */
struct textbuf *tbtop;

#ifdef DEBUG

void
cdump(char *tag)
{
    struct textbuf *t = tbtop;
    char cs[20];
    char ns[20];
    char tbuf[100];

    if (!(VERBOSE(V_IO))) {
        return;
    }

    if (tag) {
        fdprintf(2,"%s:\n", tag);
    }
    if (curchar <= ' ') {
        sprintf(cs, "0x%x", curchar);
    } else {
        sprintf(cs, "\'%c\'", curchar);
    }
    if (nextchar <= ' ') {
        sprintf(ns, "0x%x", nextchar);
    } else {
        sprintf(ns, "\'%c\'", nextchar);
    }
    if (t) {
    	sprintf(tbuf, "%s offset: 0x%x valid: 0x%x cs: %s ns: %s",
    			tag, tbtop->offset, tbtop->valid, cs, ns);
    	hexdump(tbuf, t->storage, t->valid);
    }
    
} 
#else
#define cdump(x)
#endif

struct include {
    char *path;
    struct include *next;
} *includes;

/* System include path for #include <foo.h> */
char *sys_include_path = "include";

/*
 * add a path to the include search list
 */
void
add_include(char *s)
{
    struct include *i, *ip;
    i = malloc(sizeof(*i));
    i->path = strdup(s);
    i->next = 0;
    if (includes) {
        ip = includes;
        while (ip->next) {
            ip = ip->next;
        }
        ip->next = i;
    } else {
        includes = i;
    }
#ifdef DEBUG
    if (VERBOSE(V_CPP)) {
        fdprintf(2,"add_include: %s\n", s);
    }
#endif
}

/*
 * if sys is true, then file was included using <> filename delimiters
 */
void
insertfile(char *name, int sys)
{
	struct textbuf *t;
    struct include *i;


#ifdef DEBUG
    if (VERBOSE(V_IO)) {
        fdprintf(2,
            "insertfile: %s sys=%d curchar='%c'(0x%x) "
            "nextchar='%c'(0x%x) column=%d offset=%d\n",
            name, sys,
            curchar >= 32 ? curchar : '?', curchar,
            nextchar >= 32 ? nextchar : '?', nextchar,
            column,
            tbtop ? tbtop->offset : -1);
    }
#endif

	t = malloc(sizeof(*t));
    t->fd = -1;  /* Initialize to indicate "not opened yet" */

    /*
     * For system includes (<foo.h>), try system include path first
     */
    if (sys && sys_include_path) {
        strcpy(namebuf, sys_include_path);
        strcat(namebuf, "/");
        strcat(namebuf, name);
        t->fd = open(namebuf, 0);
        if (t->fd > 0) {
            goto found;
        }
    }

    /*
     * try the filename in all the include path entries. first hit wins
     */
    for (i = includes; i; i = i->next) {
        if (i->path[0]) {
            strcpy(namebuf, i->path);
            strcat(namebuf, "/");
            strcat(namebuf, name);
        } else {
            /* Empty path means current directory - use name as-is */
            strcpy(namebuf, name);
        }
        t->fd = open(namebuf, 0);
        if (t->fd > 0) {
            break;
        }
    }
    if (t->fd == -1) {
        filename = name;  /* Set filename for error message */
        lineno = 1;
        fatal(ER_C_IF);
    }
found:
	t->name = strdup(namebuf);
	t->offset = t->valid = 0;
	t->lineno = 1;  /* New file starts at line 1 */
	t->storage = malloc(TBSIZE);
	t->saved_column = column;  /* Save parent's column */
	t->prev = tbtop;
	tbtop = t;
    filename = t->name;  /* Use resolved path */
    lineno = 1;  /* Start at line 1 for new file */
}

/*
 * when we encounter a macro invocation FOO(x,y) in the input stream, 
 * we replace it with the definition of FOO with parameter substitution
 * effectively pushing this text into the stream.  if that expansion in
 * turn has macro invocations, that causes another push.
 * there's an easy and effective optimization that checks to see if a macro
 * will fit in the portion of the buffer that we have read already.  if
 * so, we copy the macro before the current cursor and back up to the
 * start of the macro expansion.
 * important:  we push BETWEEN curchar and nextchar.
 */
void
insertmacro(char *name, char *macbuf)
{
	struct textbuf *t;
    int l;


    l = strlen(macbuf);         // our macro without the terminating null
#ifdef DEBUG
    if (VERBOSE(V_MACRO)) {
        fdprintf(2,"insert macro %s %d $%s$\n", name, l, macbuf);
    }
#endif
    t = tbtop;

    /* does it fit */
    if (t->offset > l) {
        cdump("before");
        t->offset -= l;
        strncpy(&t->storage[t->offset], macbuf, l);
        curchar = t->storage[t->offset++];
        nextchar = t->storage[t->offset];
        cdump("after");
        return;
    }
 
    /* if it does not */
	t = malloc(sizeof(*t));
	t->fd = -1;
	t->name = strdup(name);
	t->lineno = lineno;
	t->offset = 0;
	t->storage = strdup(macbuf);
	t->valid = strlen(t->storage);
	t->saved_column = column;  /* Save parent's column */
	t->prev = tbtop;
	tbtop = t;
    filename = name;
    /* Set curchar/nextchar to first characters of macro text */
    curchar = t->storage[t->offset++];
    nextchar = t->storage[t->offset];
}

void
tbdump(struct textbuf *t)
{
    fdprintf(2,"textbuf: %s fd: %d offset: %d valid: %d lineno %d\n",
        t->name, t->fd, t->offset, t->valid, t->lineno);
}

void
dump()
{
    struct textbuf *t = tbtop;
    while (t) {
        tbdump(t);
        t = t->prev;
    }
}

/*
 * read the next character into curchar
 * side effects: updating line and column
 */
void
advance()
{
	struct textbuf *t;

again:
    t = tbtop;

#ifdef DEBUG
    if (VERBOSE(V_IO)) {
        fdprintf(2,
            "Top of again: curchar='%c'(0x%x) nextchar='%c'(0x%x) "
            "offset=%d\n",
            curchar >= 32 ? curchar : '?', curchar,
            nextchar >= 32 ? nextchar : '?', nextchar,
            t ? t->offset : -1);
    }
#endif

    curchar = nextchar;

    /* if no textbuf, are at eof */
    if (!t) {
        nextchar = 0;
        goto done;
    }

    /* do we have a valid nextchar? */
	if (t->offset + 1 < t->valid) {
            nextchar = t->storage[++t->offset];
#ifdef DEBUG
            if (VERBOSE(V_IO)) {
                fdprintf(2,
                    "Read nextchar from buffer: '%c'(0x%x) at "
                    "offset %d\n",
                    nextchar >= 32 ? nextchar : '?', nextchar,
                    t->offset);
            }
#endif
            goto done;
	}

    /* if we have a file open, read some more of it */
    if (t->fd != -1) {
        t->valid = read(t->fd, t->storage, TBSIZE);
        t->offset = 0;
        if (t->valid > 0) { // read worked
            nextchar = t->storage[0];
            goto done;
        }
        close(t->fd);
    }
    /* closed file or empty macro buffer - pop */
    tbtop = t->prev;
    if (tbtop) {
#ifdef DEBUG
        if (VERBOSE(V_IO)) {
            fdprintf(2,"Popping from %s, restoring column from %d to %d\n",
                   t->name, column, t->saved_column);
        }
#endif
        /* Restore parent's state */
        column = t->saved_column;
        /* nextcol must match restored column */
        nextcol = t->saved_column;
        lineno = tbtop->lineno;
        filename = tbtop->name;

        /*
         * Read nextchar from parent buffer WITHOUT doing
         * curchar=nextchar again
         */
        if (tbtop->offset < tbtop->valid) {
            nextchar = tbtop->storage[tbtop->offset];
        } else if (tbtop->fd != -1) {
            /* Need to read more from parent file */
            tbtop->valid = read(tbtop->fd, tbtop->storage, TBSIZE);
            tbtop->offset = 0;
            if (tbtop->valid > 0) {
                nextchar = tbtop->storage[0];
            } else {
                nextchar = 0;
            }
        } else {
            nextchar = 0;
        }
    }
    free(t->storage);
    free(t->name);
    free(t);
    if (!tbtop) {
        /* No parent textbuf, we're at EOF */
        nextchar = 0;
    }
done:
    column = nextcol;
    if (curchar == 0) {
        nextcol = 0;
    } else if (curchar == '\n') {
        nextcol = 0;
        lineno++;
        if (tbtop) {
            tbtop->lineno = lineno;  /* Keep textbuf lineno in sync */
        }
    } else {
        nextcol++;
    }
    if (nextchar == '\t') nextchar = ' ';
#ifdef DEBUG
    if (VERBOSE(V_IO)) {
        fdprintf(2,
            "After done: curchar='%c'(0x%x) nextchar='%c'(0x%x) "
            "column=%d nextcol=%d\n",
            curchar >= 32 ? curchar : '?', curchar,
            nextchar >= 32 ? nextchar : '?', nextchar,
            column, nextcol);
    }
#endif
    cdump("advance");
}

/*
 * prime the pump
 */
void
ioinit()
{
    lineno = 1;
    advance();
    advance();
    column = 0;
}

struct textbuf *cpp;

#define CPP_BUF 256

void
cpp_flush()
{
    if (cpp->offset) {
        write(cpp_file, cpp->storage, cpp->offset);
    }
    cpp->offset = 0;
}

void
cpp_out(char *s, int len)
{
    if (!cpp) {
        cpp = malloc(sizeof(*cpp));
        cpp->storage = malloc(CPP_BUF);
        cpp->fd = cpp_file;
        cpp->name = cpp_file_name;
        cpp->offset = 0;
        cpp->valid = 0;
        cpp->prev = 0;
    }

    if (!s)
        return;

    if ((cpp->offset + len) > CPP_BUF) {
        cpp_flush();
    }
    memcpy(&cpp->storage[cpp->offset], s, len);
    cpp->offset += len;
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
