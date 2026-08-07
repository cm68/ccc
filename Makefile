#
# Top-level Makefile for ccc compiler
#
# Orchestrates builds across subdirectories
#

CC = gcc

include cruft.mk

# Installation destination - propagated to all submakes
DEST = $(abspath $(CURDIR)/root)

# Compiler implementation subdirectory.  ritchie and hitech were the
# two earlier front ends; both are in attic/ now.
COMPILER = ccc

# Subdirectories to build
DIRS = $(COMPILER) tools libsrc

# Subdirectories to clean.  tests is not built by "all" but it is
# where most of the tree's droppings land - a corpus compiled three
# ways leaves a .x, a .1, a .2, a .s, a .o and a binary per source -
# so clean and clobber have to reach it.
CLEANDIRS = $(DIRS) tests

# libsrc has no stage1: it is the runtime, built for the target by
# whichever compiler is driving, not compiled by ccc to be inspected.
# Looping over DIRS for stage1 meant the target always ended in an
# error after doing all of its work.
STAGE1DIRS = $(COMPILER) tools

SUBMAKE = $(MAKE) CC=$(CC) DEST=$(DEST)

#
# The build has two phases and the order is forced by what the thing
# is.  The target runtime - libsrc and the z80 halves of ccc/lib - is
# compiled BY this compiler: root/bin/ccc driving root/bin/{cpp,c0,
# c1,peep}, with root/bin/asz assembling and root/bin/wslib archiving
# the result.  So the host side has to be built AND installed before
# the target side can begin.
#
# Running the two in one pass, as this did, only ever worked on a
# machine that already had an earlier install of root/bin on its PATH.
# On a fresh checkout it died at the first asz, which is why every
# push has failed CI since December.
#
all: host target

# Phase one: the host tools, then the host compiler passes.  Both are
# installed as they are built, because phase two runs them.
host:
	@mkdir -p $(DEST)/bin $(DEST)/lib
	$(SUBMAKE) -C tools install
	$(SUBMAKE) -C $(COMPILER) install

# Phase two: the z80 runtime, compiled by what phase one installed.
target: host
	$(SUBMAKE) -C $(COMPILER) runtime
	$(SUBMAKE) -C libsrc install

# install is what all already did; kept because everything calls it.
install: all

clean:
	@for d in $(CLEANDIRS); do $(SUBMAKE) -C $$d clean; done
	rm -f $(CRUFT) $(CRUFTASM)

# root/ is deliberately left alone: it is the install destination, not
# a side effect, and root/sim is a hand-made symlink to the simulator
# outside this tree that no rule here knows how to put back.
clobber:
	@for d in $(CLEANDIRS); do $(SUBMAKE) -C $$d clobber; done
	rm -f $(CRUFT) $(CRUFTASM) $(CRUFTLOG) doc.pdf
	rm -rf $(CRUFTDIRS)

stage1: install
	@echo "Building stage1 with cross ccc"
	@for d in $(STAGE1DIRS); do $(MAKE) CC=ccc DEST=$(DEST) -C $$d stage1; done
	@echo "Stage1 build complete"

test: install
	$(SUBMAKE) -C tools test
	$(SUBMAKE) -C libsrc/libcpm ccc-check
	$(SUBMAKE) -C tests test

tests: install
	$(SUBMAKE) -C tests tests

# Leaks are not what this is for: these programs read a file, write a
# file and exit.  A field read before it is written is the thing that
# matters, because it changes what the compiler emits and does it
# differently depending on what was compiled before.  Needs stage1.
valgrind: stage1
	sh tests/vgsweep.sh

tags:
	ctags ccc/cpp/*.c ccc/pass1/*.c ccc/pass2/*.c tools/*.c

# Native (Z80) compile of cpp/c0/c1.
ZCC = ccc
sizecheck:
	$(MAKE) ZCC=$(ZCC) -C $(COMPILER) sizecheck

# Run the cpp regression harness over the full corpus.
# Pass REGRESS_FLAGS=--bless to regenerate the baseline.
regression:
	./tests/regress.sh $(REGRESS_FLAGS)

# The production-coverage suite: generate the operator x width x
# residence corpus, prove no shape lacks a rule, run it native and
# under the simulator, hold the rule-coverage baseline, and compile
# the corpus with the SIMULATED c0/c1, byte-compared against the host.
# Its footprint leg is the one that matters now - see tests/gen.
prodtest: install
	$(SUBMAKE) -C tests/gen

# The old top-level footprint target built cpp and c0 with Hi-Tech C
# and weighed them against ccc's, which is what tests/footprint.py
# reported on.  With zc3 gone there is no second column to print;
# tests/gen's footprint leg measures the passes under the simulator
# and is the live one.  The script is in attic/.

.PHONY: all host target install clean clobber stage1 test tests valgrind tags sizecheck regression prodtest
#
# vim: tabstop=4 shiftwidth=4 noexpandtab:
#
