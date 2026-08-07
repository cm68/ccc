#
# What a build leaves behind, in one place.
#
# Every Makefile in the tree includes this and hands the lists to rm.
# There is one list to maintain instead of fifteen slightly different
# ones, and a directory that clobber leaves dirty stops being a
# per-directory oversight that nobody notices until the tree has a
# hundred core files in it.
#
# The chain is  .c -> .i/.x (cpp) -> .1/.2 (pass1) -> .s (pass2)
# -> .o (asz) -> image, and every one of those steps can be asked for
# on its own or can die part way and leave its output behind.  Add
# what falls out of debugging a compiler - core dumps, valgrind logs,
# the per-file .log and .err the loop rules write, assembler and
# linker listings - and that is CRUFT.
#
# Included as  include ../cruft.mk  (adjust the depth); make runs with
# its cwd set to the Makefile's directory in every -C sub-make here.
#

#
# Removed by clean.  Note that .s is NOT in this list: libsrc and its
# libraries, tools/tests and ccc/cpp all keep hand-written assembly
# under the suffix pass2 writes its output to.  Directories where .s
# is only ever output add CRUFTASM as well.
#
CRUFT = *.i *.x *.1 *.2 *.pp *.ast *.o *.obj *.rel *.a \
	*.n *.noi *.lk *.cdb *.d *.lst *.sym *.map *.mem \
	*.com *.ws *.bin *.hex *.ihx *.mx \
	*.log *.err *.out a.out tags \
	core core.[0-9]* vgcore.* tmp[0-9]*

#
# Assembly, for the directories that only ever generate it.  .s is
# ccc's suffix; .as and .asm were zc3's and sdcc's and are swept so
# that a tree built before the bootstrap ended comes out clean.
#
CRUFTASM = *.s *.as *.asm

#
# The generated half only, for directories whose .s files are sources.
#
CRUFTASMGEN = *.as *.asm

#
# Build areas.  Each is made by a rule - one per compiler for the
# size and native builds, one per stage for the bootstrap - and none
# is ever checked in, so clobber takes the whole directory.  comzc3,
# comsdcc and mxzc3 have no rule left to make them; they stay on the
# list so a tree that still has one loses it.
#
CRUFTDIRS = stage1 stage2 sizecheck sizecheck-* sizecheck1 \
	langtest valgrind comzc3 comsdcc comccc mxzc3 mxccc \
	ccctest fpwork out asmsnap

#
# Accumulating size reports.  A running record across builds, so
# clean leaves them and only clobber throws them away.
#
CRUFTLOG = sizefile* oldsize prev.size cur.size

#
# vim: tabstop=4 shiftwidth=4 noexpandtab:
#
