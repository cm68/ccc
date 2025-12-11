# micronix system call Library Makefile
# Builds the micronix system call library for programs compiled with ccc

CC = ../../root/bin/ccc
LIB = ../../root/bin/wslib
ROOTDIR ?= ../../root
DEST = $(ROOTDIR)/lib

# Source files - each in its own .s file for selective linking
LIBU_S_SRCS = access.s alarm.s break.s chdir.s chmod.s chown.s close.s create.s \
	creat.s dup.s errno.s exec.s execv.s exit.s fork.s fstat.s getpid.s \
	getuid.s gtty.s kill.s link.s lseek.s mknod.s mount.s nice.s open.s pause.s \
	perror.s pipe.s read.s sbreak.s seek.s setuid.s sleep.s stat.s stime.s stty.s \
	sync.s time.s umount.s uname.s unlink.s wait.s write.s

LIBU_C_SRCS = signal.c

# Object files
LIBU_OBJS = $(LIBU_S_SRCS:.s=.o) $(LIBU_C_SRCS:.c=.o)

# Library target
LIBU = libu.a

.PHONY: all clean clobber install

all: $(LIBU)

%.o: %.c
	$(CC) -c -o $@ $<

%.o: %.s
	$(CC) -c -o $@ $<

$(LIBU): $(LIBU_OBJS)
	$(LIB) -crv $@ $^

install: $(LIBU)
	mkdir -p $(DEST)
	cp $(LIBU) $(DEST)
	
clean:
	rm -f $(LIBU_OBJS)

clobber: clean
	rm -f $(LIBU)

#
# vim: tabstop=4 shiftwidth=4 noexpandtab:
#
