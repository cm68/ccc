/*
 * setjmp.h - non-local jumps for ccc
 *
 * jmp_buf stores the context needed to restore execution:
 *   [0] SP (stack pointer)
 *   [1] return address
 *   [2] IY (frame pointer)
 */

#ifndef _SETJMP_H
#define _SETJMP_H

typedef int jmp_buf[3];

/*
 * setjmp: Save current execution context in env.
 * Returns 0 when called directly, non-zero when returning via longjmp.
 */
int setjmp(jmp_buf env);

/*
 * longjmp: Restore execution context saved in env.
 * Causes setjmp to return with value val (or 1 if val is 0).
 */
void longjmp(jmp_buf env, int val);

#endif /* _SETJMP_H */
