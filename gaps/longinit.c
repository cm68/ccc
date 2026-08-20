/* ccc: a long global initialised from a shift expression is allocated
 * two bytes and gets a wrong value.  `zero` below (a literal) is four
 * bytes; `true`/`false` (the same value built with <<16/<<24) are two. */
typedef long obj;
#define OBOOL 2
#define BTRUE 1
#define BFALSE 2
#define objmk(p,t,s) ((obj)(((long)(p) & 0xffffL) | ((long)(t) << 16) | ((long)(s) << 24)))
obj true  = objmk(0, OBOOL, BTRUE);
obj false = objmk(0, OBOOL, BFALSE);
obj zero  = 0x01020000L;
char *p = "end";
main() { return 0; }
