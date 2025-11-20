/*
 * astio.h - Low-level I/O and token parsing for AST parser
 *
 * Handles buffered input, whitespace/comment skipping, and basic token reading.
 */
#ifndef ASTIO_H
#define ASTIO_H

#define BUFSIZE 4096  /* AST parser read buffer */

/*
 * Parser state for nested parsing (save/restore)
 * Note: saved_buf is malloc'd to avoid IX offset limit (>127 bytes)
 */
struct parser_state {
    char *saved_buf;        /* malloc'd buffer, size BUFSIZE */
    int saved_buf_pos;
    int saved_bufvld;
    int savedLineNum;
    unsigned char saved_curchar;
    int saved_in_fd;
};

/* Global parser state (managed by astio.c, accessible to parser) */
extern unsigned char curchar;
extern int lineNum;

/* Buffer initialization */
void initAstio(int fd);

/* Character-level I/O */
unsigned char nextchar(void);
void skip(void);
int expect(unsigned char c);

/* Token reading (return pointers to static buffers) */
char *readSymbol(void);
long readNumber(void);
char *readType(void);
char *readQuotedStr(void);

/* Parser state management for nested parsing */
void saveParseSt(struct parser_state *state);
void restoreParse(struct parser_state *state);
void freeParseSt(struct parser_state *state);
void setupStrInput(char *str, int len);

/* String utilities */
int isLabel(char *line);
char *trimLine(char *line);

#endif /* ASTIO_H */
