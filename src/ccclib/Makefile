#
# libccc.a - support code the compiler passes share
#
# Built twice from one source.  The host archive is linked into cpp,
# c0, c1 and peep as they are compiled by gcc; the z80 archive is the
# same code compiled by ccc itself, for programs it builds.  The z80
# half is system-independent, so one build installs into both target
# trees.
#
# format.h lives here rather than beside astpp.c because pass2 reads
# it too - it is the shape of the AST both of them walk.
#

include ../../tree.mk
include ../../cruft.mk

CC = gcc

ifeq ($(CC),gcc)
DEFINES = -DDEBUG
WARNS = -Wall -Werror -Werror=declaration-after-statement
DEBUG = -g -O0
CFLAGS = -m32 $(DEBUG) $(DEFINES) $(WARNS)
HOSTLIB = ar
HOSTLIBFLAGS = rcs
endif

SOURCES = libutil.c
OBJECTS = $(SOURCES:.c=.o)

# host builds get permalloc from its single source in libc;
# z80 links resolve it from libc.a, so keep it out of the z80 archive
ifeq ($(CC),gcc)
OBJECTS += permalloc.o
endif

HEADERS = libutil.h format.h

ARCHIVE = libccc.a
Z80DIR = z80

# ---- host --------------------------------------------------------

all: $(ARCHIVE)

%.o: %.c $(HEADERS)
	$(CC) $(CFLAGS) -c $< -o $@

permalloc.o: ../libc/permaloc.c
	$(CC) $(CFLAGS) -c $< -o $@

$(ARCHIVE): $(OBJECTS)
	$(HOSTLIB) $(HOSTLIBFLAGS) $@ $(OBJECTS)

# The passes link this out of the build directory, so a host install
# is only for anything else that wants it.
install: all
	@mkdir -p $(UNIXDIR)/lib
	cp $(ARCHIVE) $(UNIXDIR)/lib

# ---- target ------------------------------------------------------

# Rebuilt when the compiler itself changes: these objects exist to be
# run, and a codegen change that leaves a stale one behind reads as a
# change that did not work.  This dependency was missing once and the
# 32-bit convention change is how that showed - libutil.o went on
# calling lld, arelop, aldiv and almod, so every image linked BOTH
# runtimes and c0 grew five hundred bytes that looked like they came
# from the new one.
$(Z80DIR)/libutil.o: libutil.c $(HEADERS) $(CCDEP)
	@mkdir -p $(Z80DIR)
	cd $(Z80DIR) && $(CCC) -O -I.. -I../../include -c ../libutil.c

$(Z80DIR)/$(ARCHIVE): $(Z80DIR)/libutil.o
	rm -f $@
	$(LIB) -cr $@ $(Z80DIR)/libutil.o

z80: $(Z80DIR)/$(ARCHIVE)

target-install: z80
	@mkdir -p $(MXDIR)/lib $(CPMDIR)/lib $(UNIXDIR)/lib
	cp $(Z80DIR)/$(ARCHIVE) $(MXDIR)/lib
	cp $(Z80DIR)/$(ARCHIVE) $(CPMDIR)/lib
	cp $(Z80DIR)/$(ARCHIVE) $(UNIXDIR)/lib

clean:
	rm -f $(OBJECTS) $(ARCHIVE) $(CRUFT) $(CRUFTASM)
	rm -rf $(Z80DIR) zc3 ccc

clobber: clean
	rm -f $(CRUFTLOG)
	rm -rf $(CRUFTDIRS)

regression:
	../../tests/regress.sh --filter src/ccclib/ $(REGRESS_FLAGS)

.PHONY: all install z80 target-install clean clobber regression

#
# vim: tabstop=4 shiftwidth=4 noexpandtab:
#
