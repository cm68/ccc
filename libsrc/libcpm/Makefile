#
# cp/m system call Library Makefile
# Builds the cp/m call library
#

ARCHIVE = libcpm.a

AS = asz
CC = ccc
LIB = wslib

DEST ?= ../../root/lib

.SUFFIXES: .c .s .o

.c.o:
	$(CC) -I../include -c $<

.s.o:
	$(AS) $< -o $@

SRCS = bdos.s bdoshl.s exec.s execl.s exit.s _exit.s \
	fakecpcl.s fgetc.s getargs.s # zrrtcpm.s

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
	rm -f $(OBJS)

clobber: clean
	rm -f $(ARCHIVE)

#
# vim: tabstop=4 shiftwidth=4 noexpandtab:
#
