/*
 * ftell on a buffered write stream, around a seek.
 *
 * The pattern is write-back-and-return: write a file, remember the
 * end with ftell, seek back to patch something, seek forward to the
 * remembered end and continue.  ftell computed the write side as
 * BUFSIZ - _cnt, which is right only while the stream has never
 * seeked - fseek leaves _cnt at 0 with the buffer empty, and fflush
 * and _flsbuf both already carry the lesson that _cnt and _ptr part
 * company there.  ftell was the last one trusting the subtraction:
 * it answered a whole BUFSIZ past the truth, the return seek landed
 * a block beyond the end, and the file grew a run of zeros.  peep's
 * frameless patch found it by diverging from its host build.
 *
 * The writes run past one BUFSIZ first, so the kernel position and
 * the buffered tail are both nonzero when the telling starts.
 */
#include <stdio.h>
#include "rt.h"

extern int unlink();

char *name = "rtftw.tmp";
char *patch = "PATCH";
char *tail = "TAIL";

int
main()
{
	FILE *f;
	long end;
	short i, c, want;

	f = fopen(name, "w");
	if (!f)
		return 99;
	for (i = 0; i < 700; i++)
		fputc('a' + (i % 26), f);
	CHECK(1, ftell(f) == 700L, 1);

	end = 700L;
	CHECK(2, fseek(f, 100L, 0), 0);
	CHECK(3, ftell(f) == 100L, 1);	/* the bug said 612 here */
	fputs("PATCH", f);
	CHECK(4, ftell(f) == 105L, 1);
	CHECK(5, fseek(f, end, 0), 0);
	CHECK(6, ftell(f) == 700L, 1);
	fputs("TAIL", f);
	CHECK(7, ftell(f) == 704L, 1);
	fclose(f);

	f = fopen(name, "r");
	if (!f)
		return 98;
	for (i = 0; i < 704; i++) {
		c = fgetc(f);
		if (i >= 700)
			want = tail[i - 700];
		else if (i >= 100 && i < 105)
			want = patch[i - 100];
		else
			want = 'a' + (i % 26);
		CHECK(8, c, want);
	}
	CHECK(9, fgetc(f), EOF);	/* nothing past the tail: no zeros */
	fclose(f);
	unlink(name);
	return 0;
}
