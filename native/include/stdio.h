/* Minimal stdio.h stub for self-hosting */
#ifndef _STDIO_H
#define _STDIO_H

#ifndef _SIZE_T_DEF
#define _SIZE_T_DEF
typedef unsigned long size_t;
typedef long ssize_t;
#endif

typedef struct {
    int fd;
    int flags;
} FILE;

extern FILE *stdin;
extern FILE *stdout;
extern FILE *stderr;

#define NULL ((void *)0)
#define EOF (-1)

int printf(char *format, ...);
int fprintf(FILE *stream, char *format, ...);
int sprintf(char *str, char *format, ...);
int snprintf(char *str, size_t size, char *format, ...);

int putchar(int c);
int puts(char *s);
int fputs(char *s, FILE *stream);

FILE *fopen(char *pathname, char *mode);
int fclose(FILE *stream);
size_t fread(void *ptr, size_t size, size_t nmemb, FILE *stream);
size_t fwrite(void *ptr, size_t size, size_t nmemb, FILE *stream);
int fseek(FILE *stream, long offset, int whence);
long ftell(FILE *stream);

int getchar(void);
char *fgets(char *s, int size, FILE *stream);

void perror(char *s);

#endif /* _STDIO_H */
