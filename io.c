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
extern int cpp_file;

/*
 * the incoming character stream interface
 * a zero is EOF
 */
char prevchar;
char curchar;
char nextchar;
int lineno;
char *filename;

#define	TBSIZE	1024		/* text buffer size */
struct textbuf {
	char fd;		// if == -1, macro buffer
	char *name;		// filename or macro name
	char *storage;		// data - free when done
	short offset;		// cursor
	short valid;		// total valid in buffer
	short lineno;		// current line # in file
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
 * we could save pushing and memory allocation if macbuf is smaller than
 * t_offset.  then we'd just copy it in and adjust t_offset backwards.
 * probably not worth it, unless there are large numbers of small macros.
 *
 * XXX - since some sources DO have a large number of macros, this is
 * probably worth it.
 *
 * when we encounter a macro invocation FOO(x,y) in the input stream, 
 * we replace it with the definition of FOO with parameter substitution
 * effectively pushing this text into the stream.  if that expansion in
 * turn has macro invocations, that causes another push.
 */
void
insertmacro(char *name, char *macbuf)
{
	struct textbuf *t;

#ifdef DEBUG
    if (verbose & V_IO) {
        printf("insert_macro: %s = \"%s\"\n", name, macbuf);
    }
#endif

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

/*
 * grab a character from the input machinery
 * if an input file has a null before EOF, then bizarre stuff happens.
 * handling this case is not worth it. - XXX
 */
char
readchar()
{
	struct textbuf *t;

	while ((t = tbtop) != 0) {
		if (t->offset < t->valid) {
			return t->storage[t->offset++];
		}
        if (t->fd != -1) {
            t->valid = read(t->fd, t->storage, TBSIZE);
            t->offset = 0;

#ifdef DEBUG
            if (verbose & V_IO) {
                printf("read file %s for %d\n", t->name, t->valid);
            }
#endif
            if (t->valid)
                continue;
            close(t->fd);
        }
        tbtop = t->prev;
        free(t->storage);
        free(t->name);
        free(t);
        if (tbtop) {
            lineno = tbtop->lineno;
            filename = tbtop->name;
        }
	}
	return 0;
}

/*
 * advance the current character.
 * this updates curchar, lineno, nextchar and prevchar
 */
char
getnext()
{
	prevchar = curchar;
	if (prevchar == '\n') lineno++;
	curchar = nextchar;
	if (curchar == 0) {
		nextchar = 0;
	} else {
		nextchar = readchar();
        if (nextchar == '\t') {
            nextchar = ' ';
        }
	}
	return curchar;
}

void
ioinit()
{
    nextchar = readchar();
    getnext();
}

/*
 * write to the cpp output file if requested
 *  */
void
cpp_out(char *s)
{
    if (s && cpp_file) {
        write(cpp_file, s, strlen(s));
        write(cpp_file, " ", 1);
    }
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
