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

.PHONY: all native zc3 ccc clean

# zc3 is not in the default set yet.  Compiled and linked as below it
# produces a binary that makes CP/M system calls, so it is targeting
# CP/M rather than Micronix and the simulator gets nowhere.  The flag
# that selects the target has to be found in the zc3 driver; until
# then the target builds and runs but its results mean nothing.
all: native ccc

native:
	@sh runtests.sh native

zc3:
	@sh runtests.sh zc3

ccc:
	@sh runtests.sh ccc

clean:
	rm -rf out

# vim: tabstop=4 shiftwidth=4 noexpandtab:
