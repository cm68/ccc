/*
 * this is a minimal unix library header file for use on compilers
 * that don't have unixlike libraries and includes
 */
char *strdup(char *s);
int open(char *name, int mode);
int close(int fd);
int creat(char *name, int mode);
void perror(char *msg);
void exit(int exitcode);
int read(int fd, char *buf, int len);
int write(int fd, char *buf, int len);
