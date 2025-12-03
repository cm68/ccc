/*
 * trasm main file - arg processing and file handling
 *
 * /usr/src/cmd/asz/main.c
 *
 * this file has interpolated the original sio.c
 * because it became almost trivial after the buffer stuff was
 * stripped out
 *
 * this file mostly rewritten because it had truly lame file name
 * handling:  output only went to a.out, and all specified files
 * were assembled into it.
 *
 * now, instead, for a file foo.s, we write foo.o as the gods intended
 *
 * on top of that, we assemble stdin to a.out
 *
 * Changed: <2025-11-19 17:16:21 curt>
 *
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */


#ifdef linux
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <signal.h>
#define INIT
#else
#define INIT = 0
#define void int
#endif

#include "asm.h"

char verbose INIT;
char m_flag INIT;
char n_flag INIT;
char no_relax INIT;  /* -8: no jp->jr relaxation (8080 mode) */

char *progname INIT;

int lineNum INIT;

int outfd;
int tmpfd;
int infd;
int inbuffd;

/*
 * outputs a byte onto output file
 *
 * out = byte to output
 */
void
outbyte(out)
char out;
{
    if (verbose > 4) 
        printf("outbyte: 0x%x\n", out); 
	write(outfd, &out, 1);
}

/*
 * writes a byte to the temp file
 *
 * tmp = byte to write to tmp
 */
void
outtmp(tmp)
char tmp;
{
    if (verbose > 4) 
        printf("outtmp: 0x%x\n", tmp);
	write(tmpfd, &tmp, 1);
}

/*
 * Signal handler for assembly timeout
 *
 * Catches SIGALRM to detect infinite loops or hangs during assembly.
 * The main() function sets a 5-second alarm that triggers this handler
 * if assembly doesn't complete in time.
 */
#ifdef linux
void
timeoutHdlr(int sig)
{
    fprintf(stderr, "\n\n*** TIMEOUT after 5 seconds ***\n");
    exit(1);
}
#endif

/*
 * print usage message
 */
void
usage()
{
	fprintf(stderr, "usage: %s [-vmn98] [ -o <objectfile> ] [<sourcefile>]\n", progname);
	fprintf(stderr, "\t-v\tincrease verbosity\n");
	fprintf(stderr, "\t-9\t9 character symbol names (default 15)\n");
	fprintf(stderr, "\t-8\t8080 mode (no jp->jr relaxation)\n");
	fprintf(stderr, "\t-n\tno timeout\n");
	exit(1);
}

char *infile;
char *outfile;
char tmpbuf[256];

int
main(argc, argv)
int argc;
char **argv;
{
    char *s;
    char tname[40];
    int i;

    progname = *argv;
    argv++;
    argc--;

	while (argc) {
        s = *argv;

		if (*s++ != '-') {
            break;
        }
        argv++;
        argc--;
 
	    while (*s) {
            switch (*s++) {

            case 'n':
                n_flag++;
                break;

            case '8':
                no_relax++;
                break;

			case '9':
			case 'm':
				m_flag++;
				break;

			case 'v':
				verbose++;
				break;

            case 'o':
                outfile = *argv++;
                argc--;
                break;

			default:
				usage();
			}
		}
	}

    if (verbose) {
        printf("verbose: %d\n", verbose);
    }

    if (argc) {
        infile = *argv;
        infd = open(infile, O_RDONLY);
		if (infd == -1) {
            printf("cannot open source file %s\n", infile);
            exit(1);
        } 
        if (!outfile) {
            outfile = malloc(strlen(infile)+2);
            strcpy(outfile, infile);
            s = strrchr(outfile, '.');
            if (!s) {
                s = &outfile[strlen(outfile)];
            }
            strcpy(s, ".o");
        }
    } else {
        /* no filename specified - use stdin */
        infd = 0;
        if (!outfile)
            outfile = "a.out";
        sprintf(tmpbuf, "/tmp/atmi%d", getpid());
        if ((inbuffd = open(tmpbuf, O_CREAT|O_TRUNC|O_RDWR, 0700)) == -1) {
            printf("cannot open tmp input buffer file %s\n", tmpbuf);
            exit(1);
        }
        unlink(tmpbuf);
    }


#ifdef linux
    if (infd && !n_flag) {
	    /* Set up timeout handler to catch infinite loops */
	    signal(SIGALRM, timeoutHdlr);
	    alarm(5);  /* 5 second timeout */
	}
#endif

	sprintf(tmpbuf, "/tmp/atm%d", getpid());
    if ((tmpfd = open(tmpbuf, O_CREAT|O_TRUNC|O_RDWR, 0700)) == -1) {
        printf("cannot open tmp file %s\n", tmpbuf);
        exit(1);
    }
    unlink(tmpbuf);

    if ((outfd = creat(outfile, 0700)) == -1) {
        printf("cannot open source file %s\n", outfile);
        exit(1);
    }

    assemble();

    lseek(tmpfd, 0, SEEK_SET);
    do {
        i = read(tmpfd, tmpbuf, sizeof(tmpbuf));
        if (i == -1) {
            perror("cannot read tmp file");
            exit(1);
        }
        if (write(outfd, tmpbuf, i) != i) {
            perror("cannot write object file");
            exit(1);
        }
    } while (i == sizeof(tmpbuf));
    close(outfd);
    close(infd);
    close(tmpfd);
}

/*
 * vim: tabstop=4 shiftwidth=4 expandtab:
 */
