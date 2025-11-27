/*
 * astio.h - Low-level I/O and token parsing for AST parser
 *
 * Handles buffered input, whitespace/comment skipping, and basic token reading.
 */
#ifndef ASTIO_H
#define ASTIO_H

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

/* String utilities */
int isLabel(char *line);
char *trimLine(char *line);

#endif /* ASTIO_H */
