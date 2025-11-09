/* Minimal stdio.h stub for self-hosting */
#ifndef _STDIO_H
#define _STDIO_H

typedef unsigned long size_t;
typedef long ssize_t;

typedef struct {
    int fd;
    int flags;
} FILE;

extern FILE *stdin;
extern FILE *stdout;
extern FILE *stderr;

#define NULL ((void *)0)
#define EOF (-1)

int printf(const char *format, ...);
int fprintf(FILE *stream, const char *format, ...);
int sprintf(char *str, const char *format, ...);
int snprintf(char *str, size_t size, const char *format, ...);

int putchar(int c);
int puts(const char *s);
int fputs(const char *s, FILE *stream);

FILE *fopen(const char *pathname, const char *mode);
int fclose(FILE *stream);
size_t fread(void *ptr, size_t size, size_t nmemb, FILE *stream);
size_t fwrite(const void *ptr, size_t size, size_t nmemb, FILE *stream);
int fseek(FILE *stream, long offset, int whence);
long ftell(FILE *stream);

int getchar(void);
char *fgets(char *s, int size, FILE *stream);

#endif /* _STDIO_H */
