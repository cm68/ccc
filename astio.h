/*
 * astio.h - AST I/O functions for hex-based format
 */
#ifndef ASTIO_H
#define ASTIO_H

/* Parser state */
extern unsigned lineNum;
extern unsigned char curchar;

/* Initialize with file descriptor */
void initAstio(unsigned char fd);

/* Read next character */
unsigned char nextchar(void);

/* Read 2 hex chars as byte value */
int readHex2(void);

/* Read 4 hex chars as 16-bit value */
int readHex4(void);

/* Read 8 hex chars as 32-bit value (with optional -) */
long readHex8(void);

/* Read hex-length-prefixed name (static buffer) */
unsigned char *readName(void);

/* Read hex-length-prefixed string (malloc'd) */
unsigned char *readStr(void);

/* Skip to next line */
void skipLine(void);

/* Skip newlines */
void skipNL(void);

#endif
