/*
 * this i/o machinery is a unification of macro expansion, include files
 * and source file buffering.  these are stacked, so we can push
 * include files and save our position at each level
 *
 * we don't do any character processing at all at this level.
 * if there are nasty control characters, etc, we pass them up.
 * not our job; except nulls.  those are dirty; the first null is eof.
 */
#include "ccc.h"
// #include <fcntl.h>

/*
 * the incoming character stream interface
 * a zero is EOF
 */
char curchar;               // the current character
char nextchar;              // the next char - can change if macro
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
#define	TBSIZE	1024		/* text buffer size */
struct textbuf {
	int fd;                 // if == -1, macro buffer
	char *name;             // filename or macro name
	char *storage;          // data - free when done
	short offset;           // always points at nextchar.
	short valid;            // total valid in buffer
	short lineno;           // current line # in file
	struct textbuf *prev;	// a stack
} *tbtop;

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
        printf("%s:\n", tag);
    }
    if (curchar <= ' ') {
        sprintf(cs, "0x%x", curchar);
    } else {
        sprintf(cs, "%c", curchar);
    }
    if (nextchar <= ' ') {
        sprintf(ns, "0x%x", nextchar);
    } else {
        sprintf(ns, "%c", nextchar);
    }
    if (t) {
    	sprintf(tbuf, "%s %x %x %s %s",
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
    if (VERBOSE(V_CPP)) {
        printf("add_include: %s\n", s);
    }
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
        printf("insertfile: %s\n", name);
    }
#endif

    if (!includes) {
        add_include(".");
    }

	t = malloc(sizeof(*t));
    /*
     * try the filename in all the include path entries. first hit wins
     */
    for (i = includes; i; i = i->next) {
        strcpy(namebuf, i->path);
        strcat(namebuf, "/");
        strcat(namebuf, name);
        t->fd = open(namebuf, 0);
        if (t->fd > 0) {
            break;
        }
    }
    if (t->fd == -1) {
        perror(namebuf);
        free(t);
        return;
    }
	t->name = strdup(namebuf);
	t->lineno = t->offset = t->valid = 0;
	t->storage = malloc(TBSIZE);
	t->prev = tbtop;
	tbtop = t;
    filename = name;
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
    if (VERBOSE(V_MACRO)) {
        printf("insert macro %s %d $%s$\n", name, l, macbuf);
    }
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
	t->prev = tbtop;
	tbtop = t;
    filename = name;
}

void
tbdump(struct textbuf *t)
{
    printf("textbuf: %s fd: %d data: %x offset: %d valid: %d lineno %d\n", 
        t->name, t->fd, t->storage, t->offset, t->valid, t->lineno);
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
	struct textbuf *t = tbtop;

    curchar = nextchar;

    /* if no textbuf, are at eof */
    if (!t) {
        nextchar = 0;
        goto done;
    }

    /* do we have a valid nextchar? */
	if (t->offset < t->valid) {
            nextchar = t->storage[++t->offset];
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
    free(t->storage);
    free(t->name);
    free(t);
    if (tbtop) {
        lineno = tbtop->lineno;
        filename = tbtop->name;
	}
done:
    column = nextcol;
    if (curchar == 0) {
        nextcol = 0;
    } else if (curchar == '\n') {
        nextcol = 0;
        lineno++;
    } else {
        nextcol++;
    }
    if (nextchar == '\t') nextchar = ' ';
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
    bcopy(s, &cpp->storage[cpp->offset], len);
    cpp->offset += len;
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
