/*
 * sys/stat.h - minimal stub for file status
 */

#ifndef _SYS_STAT_H
#define _SYS_STAT_H

/* File mode bits */
#define S_IRUSR 0400
#define S_IWUSR 0200
#define S_IXUSR 0100
#define S_IRGRP 0040
#define S_IWGRP 0020
#define S_IXGRP 0010
#define S_IROTH 0004
#define S_IWOTH 0002
#define S_IXOTH 0001

/* Stub declarations */
struct stat {
    unsigned long st_size;
    unsigned int st_mode;
};

extern int stat(const char *path, struct stat *buf);
extern int fstat(int fd, struct stat *buf);

#endif /* _SYS_STAT_H */

/* vim: set tabstop=4 shiftwidth=4 noexpandtab: */
