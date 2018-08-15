/*
 * global defs for curt's c compiler
 */

#define	DEBUG

#ifdef DEBUG
extern int verbose;

#define V_LEX   (1 << 0)
#define	V_IO	(1 << 1)
#endif

extern void lossage(char *message);
extern char errmsg[];

/* io.c */
extern void pushfile(char *name);
extern void insert_macro(char *name);
extern char getchar();
extern char readchar();

