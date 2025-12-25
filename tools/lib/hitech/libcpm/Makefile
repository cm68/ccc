.SUFFIXES:	.c .s .o

.c.o:
	zc3 -c $<

.s.o:
	asz $< -o $@

SRCS = bdos.s bdoshl.s exec.s execl.s exit.s _exit.s \
	fakecpcl.s fgetc.s getargs.s zrrtcpm.s

CSRCS = abort.c c.c chmod.c cleanup.c close.c creat.c dup.c \
	fcbname.c fseek.c getargs.c getfcb.c getch.c isatty.c \
	open.c read.c rename.c seek.c signal.c stat.c time.c \
	unlink.c write.c

OBJS = bdos.o bdoshl.o exit.o _exit.o \
	fakecpcl.o \
	abort.o c.o chmod.o cleanup.o close.o creat.o dup.o \
	fcbname.o fseek.o getfcb.o getch.o isatty.o \
	open.o read.o rename.o seek.o signal.o stat.o time.o \
	unlink.o write.o

libcpm.a: $(OBJS)
	-rm -f libcpm.a
	wslib -cr libcpm.a $(OBJS)

# vim: tabstop=4 shiftwidth=4 noexpandtab:
