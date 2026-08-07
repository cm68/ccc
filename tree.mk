#
# Where things are.
#
# The tree has one source directory and one install directory per
# system the compiler targets:
#
#   src/        every source - the compiler, the tools, the runtime
#   unix/       what runs on the build host: the cross compiler
#   micronix/   what runs on Micronix: the self-hosted passes
#   cpm/        what runs on CP/M
#
# Each install directory has the same three subdirectories - bin, lib
# and include - holding the programs that run on that system, the
# libraries a program linked for it needs, and the headers it is
# compiled against.  libc and libccc are system-independent and are
# built once and installed into both target trees; libu is Micronix's
# system-call layer and libcpm is CP/M's, so each goes to one.
#
# TOP is worked out from the path this file was included by, so a
# Makefile at any depth gets it right with the same one-line include
# and nothing has to be passed down a sub-make.  Override TOP on the
# command line to install somewhere else.
#
TOP ?= $(abspath $(dir $(lastword $(MAKEFILE_LIST))))

UNIXDIR = $(TOP)/unix
MXDIR   = $(TOP)/micronix
CPMDIR  = $(TOP)/cpm

# The host tools every other phase runs.  Named by path rather than
# found on PATH: they are built in this tree and a stale copy of an
# older install sitting on PATH is exactly the failure that made a
# clean checkout unbuildable.
CCC     = $(UNIXDIR)/bin/ccc
AS      = $(UNIXDIR)/bin/asz
LD      = $(UNIXDIR)/bin/wsld
LIB     = $(UNIXDIR)/bin/wslib
SIZE    = $(UNIXDIR)/bin/wssize

# The compiler proper.  A runtime rebuild depends on these, because
# these objects exist to be run and a codegen fix that leaves a stale
# one behind reads as a fix that did not work.
CCDEP   = $(UNIXDIR)/bin/cpp $(UNIXDIR)/bin/c0 \
	  $(UNIXDIR)/bin/c1 $(UNIXDIR)/bin/peep

#
# vim: tabstop=4 shiftwidth=4 noexpandtab:
#
