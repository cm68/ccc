/*
 * this i/o machinery is a unification of macro expansion, include files
 * and source file buffering.  these are stacked, so we can push
 * include files and save our position at each level
 */

#include <fcntl.h>

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

void
pushfile(char *name)
{
	struct textbuf *t;

#ifdef DEBUG
    if (verbose & V_IO) {
        printf("pushfile: %s\n", name);
    }
#endif

	t = malloc(sizeof(*t));
	t->fd = open(name, O_RDONLY);
    if (t->fd < 0) {
        perror(name);
        return;
    }
	f->name = strdup(name);
	t->lineno = t->offset = t->valid = 0;
	t->storage = malloc(TBSIZE);
	t->prev = tbtop;
	tbtop = t;
}

/*
 * we could save pushing and memory allocation if macbuf is smaller than
 * t_offset.  then we'd just copy it in and adjust t_offset backwards.
 * probably not worth it.
 */
void
insert_macro(char *name)
{
	struct textbuf *t;

#ifdef DEBUG
    if (verbose & V_IO) {
        printf("insert_macro: %s = \"%s\"\n", name, macbuf);
    }
#endif

	t = malloc(sizeof(*t));
	t->fd = -1;
	f->name = strdup(name);
	t->lineno = lineno;
	t->offset = 0;
	t->storage = strdup(macbuf);
	t->valid = strlen(t->storage);
	t->prev = tbtop;
	tbtop = t;
}

char
getchar()
{
	prevchar = curchar;
	if (prevchar == '\n') lineno++;
	curchar = nextchar;
	if (curchar == EOF) {
		nextchar = EOF;
	} else {
		nextchar = readchar();
	}
	return curchar;
}

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
        lineno = tbtop->lineno;
	}
	return EOF;
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */

