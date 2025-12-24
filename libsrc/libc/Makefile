#
# compiler support and standard c library without system call dependencies
#

ARCHIVE = libc.a

AS = asz
CC = ccc
LIB = wslib

DEST ?= ../../root/lib

.SUFFIXES: .c .s .as .o

.c.as:
	$(CC) -I../include -s $<

.c.o:
	$(CC) -I../include -c $<

.s.o:
	$(AS) $< -o $@

SRCS = abs.s allsh.s alrsh.s asallsh.s asalrsh.s asar.s \
	asdiv.s asladd.s asland.s asll.s asllrsh.s aslmul.s \
	aslor.s aslr.s aslsub.s aslxor.s asmod.s asmul.s \
	atoi.s bitfield.s blkclr.s bmove.s csv.s brelop.s \
	wrelop.s frelop.s getsp.s idiv.s imul.s index.s inout.s \
	iregset.s isalpha.s isdigit.s islower.s isspace.s isupper.s \
	ladd.s land.s ldiv.s linc.s llrsh.s lmul.s \
	longjmp.s lor.s lrelop.s lsub.s lxor.s max.s swap.s  \
	rcsv.s rindex.s sbrk.s shar.s fbcd.s \
	shll.s shlr.s strcat.s strchr.s strcmp.s strcpy.s \
	strlen.s strncat.s strncmp.s strncpy.s strrchr.s \
	tolower.s toupper.s xtoi.s float.s fputc.s fgetc.s

CSRCS = \
	rand.c srand1.c \
	timezone.c ctime.c assert.c calloc.c malloc.c \
	memcmp.c memcpy.c memset.c blkcpy.c ctype_.c exit.c cleanup.c \
	fclose.c fflush.c filbuf.c flsbuf.c puts.c \
	fopen.c fread.c freopen.c fwrite.c setbuf.c gets.c ungetc.c \
	buf.c getenv.c pnum.c qsort.c remove.c rewind.c fseek.c \
	fprintf.c sprintf.c printf.c doprnt.c putchar.c \
	atol.c fscanf.c scanf.c sscanf.c doscan.c \
	fnum.c acos.c asin.c atan2.c atan.c cos.c cosh.c sin.c sinh.c \
	tan.c tanh.c exp.c log.c sqrt.c fabs.c ceil.c floor.c evalpoly.c \
	fnum.c atof.c 

OBJS = $(SRCS:.s=.o) $(CSRCS:.c=.o)

all: $(ARCHIVE)

$(ARCHIVE): $(OBJS)
	rm -f $(ARCHIVE)
	$(LIB) -cr $(ARCHIVE) $(OBJS)

install: $(ARCHIVE)
	@mkdir -p $(DEST)
	cp $(ARCHIVE) $(DEST)

clean:
	rm -f $(OBJS) *.as

clobber: clean
	rm -f $(ARCHIVE)

# vim: tabstop=4 shiftwidth=4 noexpandtab:

