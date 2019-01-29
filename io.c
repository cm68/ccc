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
#include <fcntl.h>

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

/*
 * the formal definition of offset is the first unread character.
 * this is effectively the lookahead character.  if we have not read anything
 * from this buffer yet, it is zero.  advance() places this character into
 * curchar.
 * if we do a macro insertion, it is after curchar
 */
#define	TBSIZE	1024		/* text buffer size */
struct textbuf {
	char fd;                // if == -1, macro buffer
	char *name;             // filename or macro name
	char *storage;          // data - free when done
	short offset;           // always points at nextchar.
	short valid;            // total valid in buffer
	short lineno;           // current line # in file
	struct textbuf *prev;	// a stack
} *tbtop;

/*
 * if sys is true, then file was included using <> filename delimiters
 */
void
insertfile(char *name, int sys)
{
	struct textbuf *t;

#ifdef DEBUG
    if (verbose & V_IO) {
        printf("insertfile: %s\n", name);
    }
#endif

	t = malloc(sizeof(*t));
	t->fd = open(name, O_RDONLY);
    if (t->fd < 0) {
        perror(name);
        free(t);
        return;
    }
	t->name = strdup(name);
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
    t = tbtop;

    /* does it fit */
    if (t->offset > l) {
#ifdef DEBUG
    if (verbose & V_IO) {
        printf("offset=%x curchar = %c nextchar = %c\n", t->offset, curchar, nextchar);
        hexdump(t->storage, t->valid);
    }
#endif
        t->offset -= l;
        strncpy(&t->storage[t->offset], macbuf, l);
        nextchar = t->storage[t->offset];
#ifdef DEBUG
    if (verbose & V_IO) {
        printf("offset=%x curchar = %c nextchar = %c\n", t->offset, curchar, nextchar);
        hexdump(t->storage, t->valid);
    }
#endif
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
iodump()
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
more:
    /* if no textbuf, are at eof */
    if (!t) {
        nextchar = 0;
        return;
    }

    /* do we have a valid nextchar? */
	if (t->offset < t->valid) {
            nextchar = t->storage[t->offset++];
            goto done;
	}

    /* if we have a file open, read some more of it */
    if (t->fd != -1) {
        t->valid = read(t->fd, t->storage, TBSIZE);
        t->offset = 0;
        if (t->valid > 0) { // read worked
            goto more;
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
    if (curchar == '\n') {
        nextcol = 0;
        lineno++;
    } else {
        nextcol++;
    }
    if (nextchar == '\t') nextchar = ' ';
}

void
ioinit()
{
    lineno = 1;
    advance();
    advance();
    column = 0;
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
