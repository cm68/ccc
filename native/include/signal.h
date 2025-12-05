/*
 * signal.h - minimal stub for signal handling
 */

#ifndef _SIGNAL_H
#define _SIGNAL_H

/* Signal numbers */
#define SIGALRM 14

/* Signal handler type */
typedef void (*sighandler_t)(int);

/* Stub declarations - actual implementations use system calls */
extern sighandler_t signal(int signum, sighandler_t handler);
extern unsigned int alarm(unsigned int seconds);

#endif /* _SIGNAL_H */
