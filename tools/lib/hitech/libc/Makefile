.SUFFIXES:	.c .s .o

.c.o:
	zc3 -c $<

.s.o:
	asz $< -o $@

SRCS = abs.s allsh.s alrsh.s asallsh.s asalrsh.s asar.s \
	asdiv.s asladd.s asland.s asll.s asllrsh.s aslmul.s \
	aslor.s aslr.s aslsub.s aslxor.s asmod.s asmul.s \
	atoi.s bitfield.s blkclr.s bmove.s csv.s brelop.s \
	wrelop.s frelop.s getsp.s idiv.s imul.s index.s inout.s \
	iregset.s isalpha.s isdigit.s islower.s isspace.s isupper.s \
	ladd.s land.s ldiv.s linc.s llrsh.s lmul.s \
	longjmp.s lor.s lrelop.s lsub.s lxor.s max.s swap.s  \
	rcsv.s rindex.s sbrk.s shar.s \
	shll.s shlr.s strcat.s strchr.s strcmp.s strcpy.s \
	strlen.s strncat.s strncmp.s strncpy.s strrchr.s \
	tolower.s toupper.s xtoi.s

CSRCS = atol.c blkcpy.c calloc.c malloc.c memcmp.c memcpy.c \
	memset.c pnum.c qsort.c rand.c ctype.c ctype_.c 

OBJS	= memcpy.o memcmp.o memset.o \
	  abs.o asallsh.o allsh.o asalrsh.o asar.o \
	  asdiv.o asladd.o asland.o asll.o asllrsh.o \
	  aslmul.o aslor.o aslsub.o aslxor.o asmod.o \
	  atoi.o atol.o bitfield.o blkclr.o blkcpy.o calloc.o \
	  asmul.o ctype_.o getsp.o index.o strchr.o \
	  inout.o iregset.o isalpha.o isdigit.o \
	  islower.o isspace.o isupper.o ladd.o land.o \
	  linc.o llrsh.o longjmp.o lor.o lrelop.o frelop.o \
	  brelop.o wrelop.o lsub.o lxor.o malloc.o idiv.o max.o \
	  pnum.o ldiv.o qsort.o swap.o aslr.o bmove.o \
	  imul.o rand.o alrsh.o lmul.o rindex.o strrchr.o \
	  sbrk.o shar.o shll.o shlr.o strcat.o \
	  strcmp.o strcpy.o strlen.o strncat.o strncmp.o \
	  strncpy.o csv.o rcsv.o tolower.o \
	  toupper.o xtoi.o

libc.a:	$(OBJS)
	-rm -f libc.a
	wslib -cr libc.a $(OBJS)

install:	$(LIB)/libc.a

clean:
	rm *.o

clobber: clean
	rm libc.a

# vim: tabstop=4 shiftwidth=4 noexpandtab:
