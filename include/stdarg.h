/*
 * stdarg.h - minimal stub for variadic argument handling
 */

#ifndef _STDARG_H
#define _STDARG_H

typedef char *va_list;

#define va_start(ap, last) ap = 0
#define va_arg(ap, type) 0
#define va_end(ap) ap = 0

#endif /* _STDARG_H */
