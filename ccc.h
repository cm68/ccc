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

/* main.c */
extern void err(char errcode);
extern void fatal(char errcode);
extern void recover(char errcode, char skipto);
extern void need(char check, char skipto, char errcode);
extern void err(char errcode);
void sprintf(char *s, char *fmt, ...);
int main(int argc, char **argv);
void process(char *f);
void usage(char *complaint, char *p);
