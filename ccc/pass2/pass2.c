/*
 * pass2.c - Code generator main
 */
#include "pass2.h"
#include "../lib/libutil.h"
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#ifdef DEBUG
#include <stdio.h>
#endif

#ifdef DEBUG
#include "debugtags.c"
#endif

int in2fd;

#ifdef DEBUG
short verbose;
#endif

static void
errout(char *buf)
{
	write(2, buf, strlen(buf));
}

static char *progname;

static void
usage(char *complaint)
{
	errout(complaint);
	errout("usage: c1 [options] <.ast> <inits.s> <out.s>\n");
#ifdef DEBUG
	errout("  -v <mask>    Set verbosity (hex bitmask)\n");
#ifndef CCC
	{
		int i;
		for (i = 0; vopts[i]; i++) {
			char buf[64];
			sprintf(buf, "\t%x %s\n", 1 << i, vopts[i]);
			errout(buf);
		}
	}
#endif
#endif
	exit(1);
}

int
main(int argc, char **argv)
{
	register char *s;

	progname = *argv++;
	argc--;

	/* Parse options */
	while (argc) {
		s = *argv;
		if (*s++ != '-')
			break;
		argv++;
		argc--;

		while (*s) {
			switch (*s++) {
			case 'h':
				usage("");
				break;
#ifdef DEBUG
			case 'v':
				if (!argc--)
					usage("verbosity not specified\n");
				verbose = strtol(*argv++, 0, 0);
				break;
#endif
			default:
				{
					char buf[64];
					fmtstr(buf, "bad flag %c\n", s[-1]);
					errout(buf);
				}
				break;
			}
		}
	}

	if (argc != 3) {
		usage("requires 3 file arguments\n");
	}

#ifdef DEBUG
#ifndef CCC
	if (verbose) {
		int i, j = 0;
		char buf[128];

		for (i = 0; i < 32; i++) {
			if (!vopts[i])
				break;
			if (verbose & (1 << i))
				j |= (1 << i);
		}

		sprintf(buf, "verbose: %x (", j);
		errout(buf);
		for (i = 0; vopts[i]; i++) {
			if (j & (1 << i)) {
				errout(vopts[i]);
				j ^= (1 << i);
				if (j) {
					errout(" ");
				}
			}
		}
		errout(")\n");
	}
#endif
#endif

	infd = open(argv[0], O_RDONLY);
	if (infd < 0) {
		errout("cannot open AST file\n");
		exit(1);
	}

	in2fd = open(argv[1], O_RDONLY);
	if (in2fd < 0) {
		errout("cannot open init file\n");
		exit(1);
	}

	outfd = creat(argv[2], 0644);
	if (outfd < 0) {
		errout("cannot create output file\n");
		exit(1);
	}

	initOpTab();
	copyinit();
	parse();

#ifdef DEBUG
	dumphits();
#endif
	close(infd);
	close(in2fd);
	close(outfd);
	return 0;
}

/* vim: set tabstop=4 shiftwidth=4 noexpandtab: */
