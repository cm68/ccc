/*
 * the ANSI screen-size probe: park the cursor at the far corner,
 * ask where it landed, parse the answer.  RAW so the reply is
 * readable a byte at a time, off again after; the reply comes back
 * on fd 2, the keyboard fd by this tree's convention.
 */
#include <types.h>
#include <stdio.h>
#include <sys/sgtty.h>

main()
{
	struct sgtty omode, rmode;
	char c;
	char buf[32];
	int rows, cols, n, i;

	gtty(2, &omode);
	gtty(2, &rmode);
	rmode.mode |= RAW;
	rmode.mode &= ~ECHO;
	stty(2, &rmode);

	write(1, "\033[9999;9999H", 12);
	write(1, "\033[6n", 4);

	i = 0;
	while (i < sizeof buf - 1) {
		if (read(2, &c, 1) != 1)
			break;
		buf[i++] = c;
		if (c == 'R')
			break;
	}
	buf[i] = 0;
	stty(2, &omode);

	rows = cols = 0;
	for (i = 0; buf[i] && buf[i] != '['; i++)
		;
	if (buf[i] == '[') {
		i++;
		while (buf[i] >= '0' && buf[i] <= '9')
			rows = rows * 10 + buf[i++] - '0';
		if (buf[i] == ';') {
			i++;
			while (buf[i] >= '0' && buf[i] <= '9')
				cols = cols * 10 + buf[i++] - '0';
		}
	}
	printf("\nrows=%d cols=%d\n", rows, cols);
	return 0;
}
