#
# Top-level Makefile for ccc compiler
#
# Orchestrates builds across subdirectories
#

CC = gcc

# Installation destination - propagated to all submakes
DEST = $(realpath $(CURDIR)/root)

# Compiler implementation subdirectory
# COMPILER = ritchie 
# COMPILER = hitech 
COMPILER = ccc 

# Subdirectories to build
DIRS = $(COMPILER) tools libsrc

# libsrc has no stage1: it is the runtime, built for the target by
# whichever compiler is driving, not compiled by ccc to be inspected.
# Looping over DIRS for stage1 meant the target always ended in an
# error after doing all of its work.
STAGE1DIRS = $(COMPILER) tools

SUBMAKE = $(MAKE) CC=$(CC) DEST=$(DEST)

all:
	@for d in $(DIRS); do $(SUBMAKE) -C $$d all; done

install:
	@mkdir -p $(DEST)/bin $(DEST)/lib
	@for d in $(DIRS); do $(SUBMAKE) -C $$d install; done

clean:
	@for d in $(DIRS); do $(SUBMAKE) -C $$d clean; done
	rm -f *.ast *.s *.pp *.i *.x
	rm -rf stage1

clobber:
	@for d in $(DIRS); do $(SUBMAKE) -C $$d clobber; done
	rm -f tags doc.pdf prev.size cur.size

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
# Override Z80 compiler with: make sizecheck ZCC=zc3   (HiTech) or ZCC=ccc (default).
ZCC = ccc
sizecheck:
	$(MAKE) ZCC=$(ZCC) -C $(COMPILER) sizecheck

# Run the cpp regression harness over the full corpus.
# Pass REGRESS_FLAGS=--bless to regenerate the baseline.
regression:
	./tests/regress.sh $(REGRESS_FLAGS)

# Native-vs-Z80 equivalence + memory footprint matrix: every cpp and
# pass1 source through host cpp/c0 and sim cpp.mx/c0.mx, byte-compared,
# with heap/stack high-water for both Z80 programs.
# the production-coverage suite: generate the operator x width x
# residence corpus, prove no shape lacks a rule, run it native and
# under the simulator, hold the rule-coverage baseline, and compile
# the corpus with the SIMULATED c0/c1, byte-compared against the host
prodtest: install
	$(SUBMAKE) -C tests/gen

footprint:
	$(MAKE) -C ccc/cpp cpp xdump com-zc3
	$(MAKE) -C ccc/pass1 c0 mx-zc3
	python3 tests/footprint.py

.PHONY: all install clean clobber stage1 test tests valgrind tags sizecheck regression footprint
#
# vim: tabstop=4 shiftwidth=4 noexpandtab:
#
