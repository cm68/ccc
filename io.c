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

/*
 * Dump current character state for debugging
 *
 * Outputs the current and next characters along with the top textbuf
 * state to stderr for I/O debugging. Non-printable characters are shown
 * in hex format.
 *
 * Parameters:
 *   tag - Descriptive label for this dump point (e.g., "advance", "before")
 */
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
char *sysIncludePath = "include";

/*
 * Add a path to the include file search list
 *
 * Appends a directory path to the end of the include search list.
 * When processing #include directives, these paths are searched in order
 * to locate header files.
 *
 * The include list is searched for:
 *   - #include "file.h" directives (quoted form)
 *   - #include <file.h> directives (after trying sysIncludePath)
 *
 * Search order:
 *   1. System path (for <> includes only)
 *   2. Include list paths in order added (first match wins)
 *
 * Parameters:
 *   s - Directory path to add (string is duplicated)
 */
void
addInclude(char *s)
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
        fdprintf(2,"addInclude: %s\n", s);
    }
#endif
}

/*
 * Push an include file onto the input stack
 *
 * Opens and pushes a new include file onto the textbuf stack, pausing
 * processing of the current file. The include file is searched using the
 * configured include paths.
 *
 * Search strategy:
 *   - sys=1 (<file.h>): Try sysIncludePath first, then include list
 *   - sys=0 ("file.h"): Try include list paths only
 *   - Empty path in list means current directory
 *   - First file found is used (search stops)
 *
 * State preservation:
 *   - Current file position (offset) is saved in textbuf
 *   - Current column position is saved
 *   - Parent state restored when include file is exhausted
 *
 * New file initialization:
 *   - Line number starts at 1
 *   - Filename updated to resolved path
 *   - Empty buffer allocated (TBSIZE)
 *   - File descriptor opened for reading
 *
 * Error handling:
 *   - Fatal error if file not found in any search path
 *   - Sets filename and lineno for error message context
 *
 * Parameters:
 *   name - Include file name (relative or basename)
 *   sys  - 1 for <file.h> (system), 0 for "file.h" (user)
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
    if (sys && sysIncludePath) {
        strcpy(namebuf, sysIncludePath);
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
 * Insert macro expansion text into the input stream
 *
 * Pushes macro expansion text onto the textbuf stack, effectively inserting
 * it BETWEEN curchar and nextchar. This allows nested macro expansion where
 * macros within the expansion text are recursively expanded.
 *
 * Optimization:
 *   If the macro text fits in the already-read portion of the current buffer
 *   (before offset), the text is copied there and offset is backed up. This
 *   avoids allocating a new textbuf for short macros.
 *
 * Optimization conditions:
 *   - Current textbuf has space before offset (offset > macro length)
 *   - Macro text is copied to [offset-length ... offset-1]
 *   - Offset backed up to start of macro text
 *   - curchar/nextchar updated to first characters of macro
 *
 * New textbuf allocation (if optimization doesn't apply):
 *   - Allocate new textbuf with fd=-1 (not a file)
 *   - Duplicate macro text as storage
 *   - Set valid to text length
 *   - Push onto textbuf stack
 *   - Save parent's column position
 *
 * Insertion point:
 *   - Macro text appears BETWEEN curchar and nextchar
 *   - curchar remains unchanged (already consumed)
 *   - nextchar becomes first character of macro
 *   - Parent's nextchar restored after macro exhausted
 *
 * Parameters:
 *   name   - Macro name (for debugging/error context)
 *   macbuf - Expanded macro text (parameter substitutions already done)
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

/*
 * Dump a single textbuf for debugging
 *
 * Outputs textbuf state information to stderr including name, file
 * descriptor, buffer position, and line number.
 *
 * Parameters:
 *   t - Textbuf to dump
 */
void
tbdump(struct textbuf *t)
{
    fdprintf(2,"textbuf: %s fd: %d offset: %d valid: %d lineno %d\n",
        t->name, t->fd, t->offset, t->valid, t->lineno);
}

/*
 * Dump the entire textbuf stack for debugging
 *
 * Walks the textbuf stack from top to bottom, printing state of each
 * textbuf. Shows the nesting of include files and macro expansions.
 */
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
 * Advance to the next character in the input stream
 *
 * This is the core I/O function that implements the unified character stream
 * from files, include files, and macro expansions. It manages the textbuf
 * stack, handles EOF/buffer exhaustion, and updates line/column tracking.
 *
 * Character flow:
 *   1. Move nextchar -> curchar
 *   2. Read new nextchar from current textbuf
 *   3. If buffer exhausted, refill from file or pop textbuf
 *   4. Update line/column counters
 *
 * Textbuf stack management:
 *   - If current buffer has more data: read next character
 *   - If buffer exhausted and file open: refill buffer from file
 *   - If file exhausted or macro empty: pop textbuf, restore parent state
 *   - If no parent textbuf: nextchar=0 (EOF)
 *
 * State restoration when popping:
 *   - Restore parent's column position
 *   - Restore parent's line number
 *   - Restore parent's filename
 *   - Read nextchar from parent's current position
 *   - Parent's curchar remains unchanged (important!)
 *
 * Line/column tracking:
 *   - column: Position of curchar (current character)
 *   - nextcol: Position where nextchar will be (future column)
 *   - Newline: Increments lineno, resets nextcol to 0
 *   - Tab: Converted to space in nextchar
 *
 * Special handling:
 *   - Tabs normalized to spaces
 *   - Null bytes treated as EOF
 *   - Line numbers synchronized with textbuf
 *
 * Side effects:
 *   - Updates curchar, nextchar
 *   - Updates column, nextcol, lineno
 *   - May pop textbufs and free memory
 *   - Updates filename for error context
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
 * Initialize the I/O system for a new source file
 *
 * "Primes the pump" by calling advance() twice to load curchar and
 * nextchar with the first two characters from the input. This establishes
 * the two-character lookahead used throughout the lexer.
 *
 * Initialization sequence:
 *   1. Set line number to 1
 *   2. First advance(): loads curchar with first character
 *   3. Second advance(): loads nextchar with second character
 *   4. Reset column to 0 for start of file
 *
 * Called after insertfile() to begin reading the first (or included) file.
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

/*
 * Flush preprocessor output buffer to file
 *
 * Writes any pending data in the CPP output buffer to the preprocessor
 * output file (cppfile) and resets the buffer offset to 0. Used for
 * buffered I/O to reduce write() system calls.
 *
 * Called by:
 *   - cppOut() when buffer fills up
 *   - End of compilation to flush remaining data
 */
void
cppFlush()
{
    if (cpp->offset) {
        write(cppfile, cpp->storage, cpp->offset);
    }
    cpp->offset = 0;
}

/*
 * Write data to preprocessor output (buffered)
 *
 * Appends data to the CPP output buffer, flushing when full. This is used
 * when running in preprocessor-only mode (-E flag) to output the fully
 * preprocessed source code.
 *
 * Lazy initialization:
 *   - Buffer allocated on first call
 *   - Uses global cppfile descriptor and cppfname
 *
 * Buffering:
 *   - Data accumulated in CPP_BUF (256 byte) buffer
 *   - Flush triggered when buffer would overflow
 *   - Reduces write() system calls for efficiency
 *
 * Parameters:
 *   s   - Data to write (NULL-safe, returns immediately if NULL)
 *   len - Length of data in bytes
 */
void
cppOut(char *s, int len)
{
    if (!cpp) {
        cpp = malloc(sizeof(*cpp));
        cpp->storage = malloc(CPP_BUF);
        cpp->fd = cppfile;
        cpp->name = cppfname;
        cpp->offset = 0;
        cpp->valid = 0;
        cpp->prev = 0;
    }

    if (!s)
        return;

    if ((cpp->offset + len) > CPP_BUF) {
        cppFlush();
    }
    memcpy(&cpp->storage[cpp->offset], s, len);
    cpp->offset += len;
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
