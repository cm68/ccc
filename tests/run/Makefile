#
# Runtime correctness tests.
#
# Everything else in this tree checks that code compiles and that the
# assembly looks right.  These check that it computes the right answer,
# which is the only thing that catches a wrong branch or an unsigned
# comparison standing in for a signed one - both of which generated
# perfectly clean code.
#
# Two paths over the same sources:
#   native  host compiler, run directly - the reference
#   ccc     this compiler, run under the simulator
#
# A disagreement between the two is a bug in ccc.  There was a third
# path once, Hi-Tech's zc3 under the simulator, to tell a ccc bug
# apart from a test making an assumption about widths it should not.
# It stopped being worth its upkeep - zc3 failed this suite on its
# own unsigned-char arithmetic - and went with the bootstrap.
#

include ../../cruft.mk

.PHONY: all native ccc one clean clobber

all: native ccc

native:
	@sh runtests.sh native

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
	  $(wildcard ../../src/libcpm/*.c) \
	  ../../src/libc/atol.c ../../src/libc/qsort.c \
	  ../../src/libc/doprnt.c ../../src/libc/malloc.c
DIR = asmsnap
asmsnap:
	@mkdir -p $(DIR)
	@for f in $(SNAPSRC); do \
		b=`basename $$f .c`; \
		../../unix/bin/ccc -s -I. -I../../src/include $$f \
			>/dev/null 2>&1; \
		[ -f $$b.s ] && mv $$b.s $(DIR)/$$b.s; \
	done; ls $(DIR) | wc -l

# gp_*.c is the generated production corpus - tests/gen writes it
# here - so it goes with the rest of the droppings.  rt_*.c are the
# hand-written cases and stay.
clean:
	rm -f $(CRUFT) $(CRUFTASM) gp_*.c
	rm -rf $(CRUFTDIRS)

clobber: clean
	rm -f $(CRUFTLOG)

# vim: tabstop=4 shiftwidth=4 noexpandtab:
