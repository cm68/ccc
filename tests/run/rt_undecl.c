/*
 * A declaration of something the file never uses.
 *
 * c0 drops a file-scope declaration that nothing refers to - cpp
 * scores each name by how often the stream mentions it, and one
 * mention means the only one is the declaration itself.  Headers are
 * almost entirely this, and holding them was the bulk of what c0 ran
 * out of memory on.
 *
 * Dropping it in phase 1 left phase 2 with no entry to reuse, and
 * phase 2 does not rebuild a function type at file scope - it expects
 * phase 1 to have done it.  So the fresh entry kept the bare return
 * type and the declaration was emitted as STORAGE:
 *
 *	FILE *fopen();		->	_fopen:: .ds 2
 *	long ftell();		->	_ftell:: .ds 4
 *
 * two bytes of bss in every object that included stdio.h, each of
 * them defining what it meant only to declare.  Within one object
 * that is invisible - it is just unused bss - so this test has to
 * link: the collision is with the real definition in libc, and it is
 * the linker that says "duplicate symbol".
 *
 * Nothing below is called on purpose.  The declarations are the test,
 * and reaching main at all means they stayed declarations.
 */
#include "rt.h"

/*
 * Real libc names, declared and not used.  Invented ones would not
 * do: the bss is harmless inside one object, and what catches it is
 * the collision with the same name in another.  Every object that
 * included stdio.h grew the same bss, so the one this links against -
 * the exit-time stdio cleanup, which every program pulls in - held a
 * second definition of each of these.  Declared here without the
 * header, which the three toolchains do not agree on the path to.
 */
long	ftell();
char *	fgets();
char *	_bufallo();
int	_flsbuf();

/*
 * A definition IS still a definition, whatever its score: mentioned
 * once, here, and it has to keep its name to be emitted against.
 */
short
usedonce()
{
	return 7;
}

main()
{
	CHECK(1, usedonce(), 7);
	return 0;
}
