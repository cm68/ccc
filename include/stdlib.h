/* Minimal stdlib.h stub for self-hosting */
#ifndef _STDLIB_H
#define _STDLIB_H

typedef unsigned long size_t;
typedef long ssize_t;

void *malloc(size_t size);
void *calloc(size_t nmemb, size_t size);
void *realloc(void *ptr, size_t size);
void free(void *ptr);

void exit(int status);
int atoi(const char *nptr);
long atol(const char *nptr);

char *getenv(const char *name);

#endif /* _STDLIB_H */
