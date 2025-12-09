# CCC Runtime Library Makefile
# Builds the Z80 runtime libraries for programs compiled with ccc

AS = ../../xbin/asz
LIB = ../../xbin/wslib
DEST = ../../lib

# Source files - each in its own .s file for selective linking
LIBC_SRCS = frame.s indexiy.s long.s long32.s switch.s \
	add32.s sub32.s and32.s or32.s xor32.s \
	add168.s mul16.s div16.s mod16.s and16.s or16.s shift16.s \
	mul32.s div32.s shift32.s \
	linc.s ldec.s lincp.s ldecp.s \
	cmp.s cmp8.s cmp16.s ucmp16.s cmpmix.s cmp32.s cmp3216.s

CRT0_SRC = crt0.s

# Object files
LIBC_OBJS = $(LIBC_SRCS:.s=.o)
CRT0 = crt0.o

# Library target
LIBC = libc.a

.PHONY: all clean

all: $(LIBC) $(CRT0)

%.o: %.s
	$(AS) $(ASFLAGS) -o $@ $<

crt0.o: crt0.s

$(LIBC): $(LIBC_OBJS)
	$(LIB) -crv $@ $^

install: $(LIBC) $(CRT0)
	mkdir -p $(DEST)
	cp $(LIBC) $(CRT0) $(DEST)
	
clean:
	rm -f $(LIBC_OBJS) $(CRT0) $(LIBC)

#
# vim: tabstop=4 shiftwidth=4 noexpandtab:
#
