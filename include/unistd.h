/*
 * unistd.h - minimal stub for POSIX system calls
 */

#ifndef _UNISTD_H
#define _UNISTD_H

/* Standard file descriptors */
#define STDIN_FILENO  0
#define STDOUT_FILENO 1
#define STDERR_FILENO 2

/* access() mode flags */
#define F_OK 0
#define X_OK 1
#define W_OK 2
#define R_OK 4

/* Stub declarations - actual implementations use system calls */
extern int access(const char *pathname, int mode);
extern int read(int fd, void *buf, unsigned int count);
extern int write(int fd, const void *buf, unsigned int count);
extern int close(int fd);
extern int fork(void);
extern int execv(const char *path, char *const argv[]);
extern unsigned int alarm(unsigned int seconds);
extern char *realpath(const char *path, char *resolved_path);
extern int mkstemp(char *template);
extern int unlink(const char *pathname);

#endif /* _UNISTD_H */
