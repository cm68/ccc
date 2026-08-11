/*
 * Initialising a union, in every spelling.  All are refused.
 *
 * K&R's book does not allow a union initializer at all - initializers
 * are for scalars, arrays and structs.  ANSI later admitted the braced
 * form for the union's first member, and the bare "= 0" that
 * Whitesmiths read as "zero fill" was never C anywhere.
 *
 * Both were accepted here and both came out two bytes long, whatever
 * the union's size, so the object was smaller than its type and
 * everything declared after it was laid down inside it.  Nothing was
 * said.  The Micronix boot loader's 512 byte sector buffer had two
 * bytes of storage and the disk spec sat nineteen bytes into it.
 *
 * Nothing is lost by refusing: a union DECLARED and not initialised
 * goes to bss at its full size, and crt0 clears bss, so
 *
 *	union u a;
 *
 * already means what "= 0" was reaching for.  That spelling is in
 * tests/run/rt_aggrinit.c, on the running side, along with the arrays
 * and structs - which are NOT refused and are now sized from the type.
 */
union u { char bytes[512]; };
union v { int i; char b[4]; };
typedef union u U;

union u a = 0;			/* the Whitesmiths spelling */
union u b = { 0 };		/* the ANSI spelling */
union v c = { 5 };		/* a non-zero first member */
U      d = 0;			/* and through a typedef */
