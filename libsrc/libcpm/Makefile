#
# cp/m system call Library Makefile
# Builds the cp/m call library
#

ARCHIVE = libcpm.a

AS = asz
# zc3, not ccc: the installed archive is runtime code for the .com/.mx
# builds and pass2 codegen is still WIP (matches libc/libu).  The ccc
# compile coverage of these sources lives in the ccc-check target.
# override: the top level drives everything with CC=gcc on the
# command line, and a command-line variable beats a plain
# assignment in every sub-make it reaches.  These are runtime
# sources for the target and gcc cannot build them - a clean
# tree failed here, and only built at all because the objects
# from some earlier run were still lying about.
override CC = zc3
LIB = wslib

DEST = $(realpath ../../root/lib)

.SUFFIXES: .c .s .o

.c.o:
	$(CC) -I../include -c $<

.s.o:
	$(AS) $< -o $@

# fakecpcl.s was a stub __cpm_clean for when cleanup.c didn't compile
SRCS = bdos.s bdoshl.s exec.s execl.s exit.s _exit.s \
	fgetc.s getargs.s # zrrtcpm.s fakecpcl.s

CSRCS = abort.c chmod.c cleanup.c close.c creat.c dup.c \
	fcbname.c fseek.c getargs.c getfcb.c getch.c isatty.c \
	open.c read.c rename.c seek.c signal.c stat.c time.c \
	unlink.c write.c

OBJS = $(SRCS:.s=.o) $(CSRCS:.c=.o)

all: $(ARCHIVE)

$(ARCHIVE): $(OBJS)
	rm -f $(ARCHIVE)
	$(LIB) -cr $(ARCHIVE) $(OBJS)

install: $(ARCHIVE)
	@mkdir -p $(DEST)
	cp $(ARCHIVE) $(DEST)

clean:
	rm -f $(OBJS) *.1 *.2 *.x *.i *.pp
	rm -rf ccctest

clobber: clean
	rm -f $(ARCHIVE)

regression:
	../../tests/regress.sh --filter libsrc/libcpm/ $(REGRESS_FLAGS)

# Compile every C source with the self-hosted ccc pipeline (cpp/c0/c1/
# asz) as a stress test.  Objects go to ccctest/ and are thrown away -
# the installed archive is zc3-built.
ccc-check:
	@mkdir -p ccctest
	@set -e; cd ccctest; for f in $(CSRCS); do \
		printf "ccc %-14s" "$$f"; \
		../../../root/bin/ccc -I../../include -c ../$$f \
			> $$f.log 2>&1 && echo ok \
			|| { echo FAIL; cat $$f.log; exit 1; }; \
	done

.PHONY: regression ccc-check

#
# vim: tabstop=4 shiftwidth=4 noexpandtab:
#
