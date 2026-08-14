#
# Global definitions for the host build.
#
# THERE ARE THREE BUILDS, and they are not the same thing:
#
#   host    the compiler built by gcc, running on the build machine,
#           emitting Z80.  This is the cross compiler and it is what
#           desthost/ holds.
#   cross   the compiler built BY that compiler into Z80 binaries -
#           the passes as they will run on the target.  Driven from
#           the host, lands in destmicronix/ and destcpm/.
#   native  the same compiler built on Micronix by the tools already
#           there, real machine or simulator.
#
# The cross build and the native build must produce identical output.
# That is the whole point of a self-hosting compiler: one that runs on
# the target but emits something subtly different is worse than one
# that will not run at all, because its output looks right.  "make
# selfhost" is the gate that asserts it.
#
# THE TREE HAS TWO MAKEFILE NAMES.  GNU make looks for GNUmakefile
# before makefile before Makefile; the Micronix make looks for
# makefile then Makefile and has never heard of the first name.  So:
#
#   GNUmakefile   everything driven from the build host under GNU
#                 make - the host build and the cross build both.
#                 May use anything GNU make has.
#   Makefile      the native build.  Runs under the Micronix make,
#                 which is its own lineage and not GNU's, and is held
#                 to what that make understands: suffix rules, and
#                 none of GNU's include, pattern rules, -C or
#                 functions.
#
# Both kinds have all, install, clean and clobber.  Running "make" in
# any directory of this tree does the right thing for whichever
# machine you are standing on, without a flag.
#
# This file is the host side's shared half.  Every GNUmakefile in the
# tree begins with one line of
#
#	include ../../GNUmakefile.inc
#
# adjusted for depth, and gets from it the layout, the command names,
# the default rules and the cleanup lists.  The native Makefiles
# cannot include anything and carry their own small copies.
#
# vim: tabstop=4 shiftwidth=4 noexpandtab:
#

#
# TOP is worked out from the path this file was included by, so a
# GNUmakefile at any depth gets it right with the same one-line
# include and nothing has to be passed down a sub-make.  It has to be
# the first thing here and the include has to be the first include
# there, because it reads the end of MAKEFILE_LIST.  Override TOP on
# the command line to build against a tree somewhere else.
#
TOP ?= $(abspath $(dir $(lastword $(MAKEFILE_LIST))))

# ---------------------------------------------------------------
# Where things are.
#
# The tree has one source directory and one install directory per
# system the compiler targets:
#
#   src/        every source - the compiler, the tools, the runtime
#   desthost/   what runs on the build host: the cross compiler
#   destmicronix/  what runs on Micronix: the self-hosted passes
#   destcpm/    what runs on CP/M
#
# Each install directory has the same three subdirectories - bin, lib
# and include - holding the programs that run on that system, the
# libraries a program linked for it needs, and the headers it is
# compiled against.  libc and libccc are system-independent and are
# built once and installed into both target trees; libu is Micronix's
# system-call layer and libcpm is CP/M's, so each goes to one.
# ---------------------------------------------------------------

# Overridable, so "make install HOSTDIR=$$HOME/src/desthost" stages the
# toolchain outside the tree.  The default keeps it in the tree, where
# clobber can reach it and two checkouts cannot overwrite each other.
HOSTDIR ?= $(TOP)/desthost
MXDIR   ?= $(TOP)/destmicronix
CPMDIR  ?= $(TOP)/destcpm

# An installation is bin/ and lib/ beside each other: the driver is
# the only thing a user runs, and it finds everything else in the lib
# next to its own directory - see libdir in tools/ccc.c.  That is why
# the passes install to lib and not bin.  It is v7's arrangement, and
# it is what lets /usr/local/bin/ccc work without being rebuilt.
HOSTLIB = $(HOSTDIR)/lib
#
# Programs run by other programs.  The passes, the assembler and the
# linker go here and not in lib, which is for things to link against.
#
HOSTLIBEXEC = $(HOSTDIR)/libexec
MXLIBEXEC = $(MXDIR)/libexec

# ---------------------------------------------------------------
# Where "make install" puts a real toolchain.
#
# PREFIX is where it will finally live and is compiled into the driver
# as the fallback for a bare "ccc" typed at a shell - argv[0] is the
# typed command, so that case cannot be worked out at run time and
# there is no environment to ask.  Invoked by a path, the install
# stays relocatable regardless.
#
# DESTDIR stages it somewhere else for packaging; the driver still
# believes it lives at PREFIX, which is what you want.
# ---------------------------------------------------------------

PREFIX  ?= /usr/local
DESTDIR ?=

# The command that writes outside the tree.  Only "sysinstall" uses
# it.  Set SUDO= to test the copy into a DESTDIR you already own.
SUDO    ?= sudo

# ---------------------------------------------------------------
# Command names.
# ---------------------------------------------------------------

CC     ?= gcc
RM     = rm -f
CP     = cp
MKDIR  = mkdir -p

#
# How a build product gets into an install tree.
#
# -u, and it matters.  The passes are what the target runtime is
# compiled BY - see CCDEP below - so an unconditional cp that stamps
# lib/c0 with the time of day makes every Z80 object older than its
# compiler, and a no-op "make all" then recompiles the runtime.  It
# was 75 sources through the whole chain, every time, for nothing.
# Copy only what is actually newer and the second build has nothing
# to do.
#
INSTALL = cp -u

# The host tools every other phase runs.  Named by path rather than
# found on PATH: they are built in this tree and a stale copy of an
# older install sitting on PATH is exactly the failure that made a
# clean checkout unbuildable.
#
# The passes, the assembler and the linker are in LIBEXEC and not in
# lib - see HOSTLIBEXEC above and the comment in tools/ccc.c that says
# why.  These four moved with them and did not, which is what made
# "make clean ; make sysinstall" stop working: the install put pass0
# in libexec and every runtime object went on asking for lib/pass0,
# which no rule makes.
#
# The host linker is mxld.  "ld" here would be the build machine's
# own, so the file installed is called mxld and this has to ask for it
# by that name; the Micronix build gets ld, being that machine's.  The
# same goes for size, installed as mxsize.  wslib has not been renamed
# and is in bin, being a program a person uses.
#
CCC     = $(HOSTDIR)/bin/ccc
AS      = $(HOSTLIBEXEC)/asz
LD      = $(HOSTLIBEXEC)/mxld
#
# The librarian is ar, built for the host as mxar.  It was wslib,
# which wrote the whitesmiths archive format - a 14-byte name and a
# 2-byte length, ended by a null name.  ar writes the v7 header, with
# the date, uid, gid and mode as well, and ld reads both.
#
# The micronix tree switched to it some time ago and this one did not,
# so every library ccc built was still whitesmiths while every library
# beside it was v7 - and mxar could not so much as list its own libc.
#
LIB     = $(HOSTDIR)/bin/mxar
SIZE    = $(HOSTDIR)/bin/mxsize

# The compiler proper.  A runtime rebuild depends on these, because
# these objects exist to be run and a codegen fix that leaves a stale
# one behind reads as a fix that did not work.
#
# pass0 is what cpp installs as - the driver runs it by that name, and
# this list has to spell it the same way, and put it in the same
# DIRECTORY, or every target build asks for something no rule makes.
# It has been wrong both ways now: first lib/cpp, then lib/pass0 after
# the passes moved to libexec.
CCDEP   = $(HOSTLIBEXEC)/pass0 $(HOSTLIBEXEC)/c0 \
	  $(HOSTLIBEXEC)/c1 $(HOSTLIBEXEC)/peep

# ---------------------------------------------------------------
# Default rules.
#
# The plain host compile, for the directories that want nothing
# special.  A GNUmakefile that needs its own flags or its own recipe
# defines the same pattern rule after this include and GNU make takes
# the later one.
# ---------------------------------------------------------------

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

# ---------------------------------------------------------------
# What a build leaves behind, in one place.
#
# There is one list to maintain instead of fifteen slightly different
# ones, and a directory that clobber leaves dirty stops being a
# per-directory oversight that nobody notices until the tree has a
# hundred core files in it.
#
# The chain is  .c -> .x/.tok/.nam (pass0) -> .ast/.dat (c0)
# -> .s (c1)
# -> .o (asz) -> image, and every one of those steps can be asked for
# on its own or can die part way and leave its output behind.  Add
# what falls out of debugging a compiler - core dumps, valgrind logs,
# the per-file .log and .err the loop rules write, assembler and
# linker listings - and that is CRUFT.
# ---------------------------------------------------------------

#
# Removed by clean.  Note that .s is NOT in this list: libsrc and its
# libraries, tools/tests and ccc/cpp all keep hand-written assembly
# under the suffix pass2 writes its output to.  Directories where .s
# is only ever output add CRUFTASM as well.
#
CRUFT = *.tok *.x *.nam *.ast *.dat *.pp *.o *.obj *.rel *.a \
	*.noi *.lk *.cdb *.d *.lst *.sym *.map *.mem \
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
