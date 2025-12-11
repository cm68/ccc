/*
 * stdarg.h - variadic argument handling for ccc
 *
 * Stack layout after framealloc:
 *   [locals]
 *   [saved IY]     <- IY+0
 *   [ret addr]     <- IY+2
 *   [arg 1]        <- IY+4  (first named param)
 *   [arg 2]        <- IY+4+sizeof(arg1)
 *   ...
 *
 * Arguments are contiguous at increasing addresses.
 * All args are 2-byte aligned (char pushed as word via push af).
 */

#ifndef _STDARG_H
#define _STDARG_H

typedef char *va_list;

/*
 * va_start: Initialize ap to point just past the last named parameter.
 * Usage: va_start(ap, last_named_param);
 * Uses __va_set helper to avoid compound assignment code gen issues.
 */
void __va_set(char **ap, char *val);

#define va_start(ap, last) \
    __va_set(&(ap), (char *)&(last) + sizeof(last))

/*
 * va_arg: Fetch next argument of given type, advance ap.
 * Usage: int x = va_arg(ap, int);
 *
 * cc1 applies default argument promotions: char/short -> int.
 * Uses __va_inc helper to avoid compound assignment code gen issues.
 */
char *__va_inc(char **ap, int size);

#define va_arg(ap, type) \
    (*(type *)__va_inc(&(ap), sizeof(type)))

/*
 * va_end: Clean up (no-op on this architecture).
 */
#define va_end(ap) ((void)0)

/*
 * va_copy: Copy va_list state.
 */
#define va_copy(dest, src) ((dest) = (src))

#endif /* _STDARG_H */
