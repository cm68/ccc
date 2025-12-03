/*
 * astio.h - AST I/O functions for hex-based format
 */
#ifndef ASTIO_H
#define ASTIO_H

/* Parser state */
extern int lineNum;
extern unsigned char curchar;

/* Initialize with file descriptor */
void initAstio(unsigned char fd);

/* Read next character */
unsigned char nextchar(void);

/* Read 2 hex chars as byte value */
int readHex2(void);

/* Read hex number terminated by '.' */
long readNum(void);

/* Read hex-length-prefixed name (static buffer) */
char *readName(void);

/* Read hex-length-prefixed string (malloc'd) */
char *readStr(void);

/* Skip to next line */
void skipLine(void);

/* Skip newlines */
void skipNL(void);

#endif
