/* ccc: casting a long to a struct pointer at runtime takes the HIGH word
 * and drops the &0xffff mask.  The bug needs a runtime (non-constant)
 * long, so makeobj() returns one from a function.  objptr() prints 0x100
 * (the high word) instead of 0xd831 (the low word). */
typedef long obj;
struct cell { char *nval; char *sval; long fval; unsigned tval; struct cell *nextval; };
#define objptr(o) ((struct cell *)((o) & 0xffffL))
obj makeobj() { return 0x0100d831L; }
main()
{
	obj y = makeobj();
	printf("%x %x\n", (int)objptr(y), (int)(y & 0xffffL));  /* 100 d831 */
	return 0;
}
