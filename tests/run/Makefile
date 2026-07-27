#
# Runtime correctness tests.
#
# Everything else in this tree checks that code compiles and that the
# assembly looks right.  These check that it computes the right answer,
# which is the only thing that catches a wrong branch or an unsigned
# comparison standing in for a signed one - both of which generated
# perfectly clean code.
#
# Three paths over the same sources:
#   native  host compiler, run directly - the reference
#   zc3     Hi-Tech, run under the simulator
#   ccc     this compiler, run under the simulator
#
# A disagreement between ccc and either of the other two is a bug in
# ccc.  A disagreement between native and zc3 means the test is
# assuming something about widths it should not.
#

.PHONY: all native zc3 ccc one clean

all: native zc3 ccc

native:
	@sh runtests.sh native

zc3:
	@sh runtests.sh zc3

ccc:
	@sh runtests.sh ccc

# One file through one path, for narrowing a failure down:
#	make one MODE=ccc T=rt_cmp.c
# The file need not be an rt_ one, so a throwaway probe next to these
# gets the same build and run without being picked up by the suite.
MODE = ccc
one:
	@sh runtests.sh $(MODE) $(T)

# Snapshot the assembly ccc generates for a corpus, so that a change
# meant to leave code generation alone can be shown to have done so:
#	make asmsnap DIR=before
#	...change something...
#	make asmsnap DIR=after && diff -r before after
# Byte-identical output is a stronger statement than a passing test,
# because it covers the paths the tests do not reach.
SNAPSRC = $(wildcard rt_*.c) \
	  $(wildcard ../../libsrc/libcpm/*.c) \
	  ../../libsrc/libc/atol.c ../../libsrc/libc/qsort.c \
	  ../../libsrc/libc/doprnt.c ../../libsrc/libc/malloc.c
DIR = asmsnap
asmsnap:
	@mkdir -p $(DIR)
	@for f in $(SNAPSRC); do \
		b=`basename $$f .c`; \
		../../root/bin/ccc -s -I. -I../../libsrc/include $$f \
			>/dev/null 2>&1; \
		[ -f $$b.s ] && mv $$b.s $(DIR)/$$b.s; \
	done; ls $(DIR) | wc -l

clean:
	rm -rf out

# vim: tabstop=4 shiftwidth=4 noexpandtab:
