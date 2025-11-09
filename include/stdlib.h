/* Minimal stdlib.h stub for self-hosting */
#ifndef _STDLIB_H
#define _STDLIB_H

#ifndef _SIZE_T_DEFINED
#define _SIZE_T_DEFINED
typedef unsigned long size_t;
typedef long ssize_t;
#endif

void *malloc(size_t size);
void *calloc(size_t nmemb, size_t size);
void *realloc(void *ptr, size_t size);
void free(void *ptr);

void exit(int status);
int atoi(char *nptr);
long atol(char *nptr);

char *getenv(char *name);

#endif /* _STDLIB_H */
