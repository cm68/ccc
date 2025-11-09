/*
 * stdarg.h - minimal stub for variadic argument handling
 */

#ifndef _STDARG_H
#define _STDARG_H

typedef char *va_list;

#define va_start(ap, last) ((void)0)
#define va_arg(ap, type) ((type)0)
#define va_end(ap) ((void)0)

#endif /* _STDARG_H */
