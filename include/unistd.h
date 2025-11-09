/*
 * unistd.h - minimal stub for POSIX system calls
 */

#ifndef _UNISTD_H
#define _UNISTD_H

/* Standard file descriptors */
#define STDIN_FILENO  0
#define STDOUT_FILENO 1
#define STDERR_FILENO 2

/* Stub declarations - actual implementations use system calls */
extern int read(int fd, void *buf, unsigned int count);
extern int write(int fd, const void *buf, unsigned int count);
extern int close(int fd);
extern int fork(void);
extern int execv(const char *path, char *const argv[]);
extern unsigned int alarm(unsigned int seconds);
extern char *realpath(const char *path, char *resolved_path);
extern int mkstemp(char *template);

#endif /* _UNISTD_H */
