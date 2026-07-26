/*
 * Storing through a pointer held in a register variable.  The lvalue
 * of an assignment is the address, so "*p = 5" must store to memory at
 * p, not overwrite p itself.  Compare with "i = 5" on a register
 * variable, where the register IS the storage.
 */

int gi;

/* pointer alone, so it is the one that gets a register */
int pstore(p)
register int *p;
{
	*p = 5;		/* store through p, NOT to p */
	*p += 10;	/* read-modify-write through p */
	p = &gi;	/* this one really does assign p */
	return *p;
}

int store(p, i)
register int *p;
register int i;
{
	*p = 5;		/* memory at p */
	i = 6;		/* the register itself */
	*p += 10;	/* read-modify-write through p */
	i += 11;	/* register arithmetic */
	return i;
}
