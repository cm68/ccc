#
# micronix system call Library Makefile
# Builds the micronix system call library
#

ARCHIVE = libu.a

AS = asz
# zc3, not ccc: pass2 codegen is still WIP (matches libc)
CC = zc3
LIB = wslib

DEST ?= ../../root/lib

.SUFFIXES: .c .s .o

.c.o:
	$(CC) -I../include -c $<

.s.o:
	$(AS) $< -o $@

SRCS = access.s alarm.s _break.s chdir.s chmod.s chown.s close.s \
	creat.s dup.s errno.s exec.s execv.s exit.s fdpos.s fork.s fstat.s \
	getpid.s getuid.s gtty.s kill.s link.s mknod.s mount.s nice.s open.s \
	pause.s perror.s pipe.s read.s seekraw.s setuid.s _signal.s sleep.s stat.s \
	stime.s sbrk.s stty.s sync.s time.s umount.s uname.s unlink.s wait.s \
	write.s

CSRCS = signal.c lseek.c seek.c

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

regression:
	../../tests/regress.sh --filter libsrc/libu/ $(REGRESS_FLAGS)

.PHONY: regression

#
# vim: tabstop=4 shiftwidth=4 noexpandtab:
#
