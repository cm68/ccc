/*
 * cc2.c - Code generator main
 */
#include "cc2.h"
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

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

static void
usage(void)
{
	errout("usage: c1 -p <ast> -o <out.s> [options]\n");
	errout("  -p <file>    AST input file\n");
	errout("  -o <file>    Assembly output file\n");
	errout("  -h           Show this help\n");
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
	char *astfile = NULL;
	char *outfile = NULL;
	int i;

	/* Parse arguments */
	for (i = 1; i < argc; i++) {
		if (strcmp(argv[i], "-p") == 0) {
			if (++i >= argc) usage();
			astfile = argv[i];
		} else if (strcmp(argv[i], "-o") == 0) {
			if (++i >= argc) usage();
			outfile = argv[i];
		} else if (strcmp(argv[i], "-h") == 0) {
			usage();
		} else if (strcmp(argv[i], "-v") == 0) {
			if (++i >= argc) usage();
#ifdef DEBUG
			verbose = strtol(argv[i], 0, 0);
#endif
		} else if (argv[i][0] == '-') {
			char buf[64];
			sprintf(buf, "Unknown option: %s\n", argv[i]);
			errout(buf);
			usage();
		} else {
			errout("Unexpected argument\n");
			usage();
		}
	}

	if (!astfile || !outfile) {
		errout("Missing required arguments\n");
		usage();
	}

#ifdef DEBUG
#ifndef CCC
	if (verbose) {
		int j = 0;
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

	infd = open(astfile, O_RDONLY);
	if (infd < 0) {
		errout("cannot open AST file\n");
		exit(1);
	}

	outfd = creat(outfile, 0644);
	if (outfd < 0) {
		errout("cannot create output file\n");
		exit(1);
	}

	parse();

	close(infd);
	close(outfd);
	return 0;
}

/* vim: set tabstop=4 shiftwidth=4 noexpandtab: */
