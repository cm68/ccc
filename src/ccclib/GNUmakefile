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

include ../../GNUmakefile.inc

CC = gcc

ifeq ($(CC),gcc)
DEFINES = -DDEBUG
WARNS = -Wall -Werror -Werror=declaration-after-statement
DEBUG = -g -O0
CFLAGS = -m32 $(DEBUG) $(DEFINES) $(WARNS)
HOSTAR = ar
HOSTARFLAGS = rcs
endif

SOURCES = libutil.c fmtlong.c
MANPAGES = man3/fmtstr.3 man3/fmtlong.3 man3/permalloc.3
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

# The compile itself is the default rule in GNUmakefile.inc; this
# just says what every object here depends on besides its source.
$(OBJECTS): $(HEADERS)

permalloc.o: ../libc/permaloc.c
	$(CC) $(CFLAGS) -c $< -o $@

$(ARCHIVE): $(OBJECTS)
	$(HOSTAR) $(HOSTARFLAGS) $@ $(OBJECTS)

# The passes link this out of the build directory, so a host install
# is only for anything else that wants it.
install: all
	@mkdir -p $(HOSTDIR)/lib
	$(INSTALL) $(ARCHIVE) $(HOSTDIR)/lib

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

$(Z80DIR)/fmtlong.o: fmtlong.c $(HEADERS) $(CCDEP)
	@mkdir -p $(Z80DIR)
	cd $(Z80DIR) && $(CCC) -O -I.. -I../../include -c ../fmtlong.c

$(Z80DIR)/$(ARCHIVE): $(Z80DIR)/libutil.o $(Z80DIR)/fmtlong.o
	rm -f $@
	$(LIB) -cr $@ $(Z80DIR)/libutil.o $(Z80DIR)/fmtlong.o

z80: $(Z80DIR)/$(ARCHIVE)

target-install: z80 man-install
	@mkdir -p $(MXDIR)/lib $(CPMDIR)/lib $(HOSTDIR)/lib
	$(INSTALL) $(Z80DIR)/$(ARCHIVE) $(MXDIR)/lib
	$(INSTALL) $(Z80DIR)/$(ARCHIVE) $(CPMDIR)/lib
	$(INSTALL) $(Z80DIR)/$(ARCHIVE) $(HOSTDIR)/lib

# The manual goes with what it documents.  A library installed into a
# Micronix tree without its pages is only half shipped.  man(1) on that
# machine looks in /usr/man/man<n> - the path is in the binary - so the
# staged tree carries them under usr, the same as the headers.
man-install:
	@mkdir -p $(MXDIR)/usr/man/man3
	$(INSTALL) $(MANPAGES) $(MXDIR)/usr/man/man3

clean:
	rm -f $(OBJECTS) $(ARCHIVE) $(CRUFT) $(CRUFTASM)
	rm -rf $(Z80DIR) zc3 ccc

clobber: clean
	rm -f $(CRUFTLOG)
	rm -rf $(CRUFTDIRS)

regression:
	../../tests/regress.sh --filter src/ccclib/ $(REGRESS_FLAGS)

.PHONY: all install z80 target-install man-install clean clobber regression

#
# vim: tabstop=4 shiftwidth=4 noexpandtab:
#
