#
# libf - software floating point library
#
# ccc has no native float or double type, so FLOAT is an unsigned long
# holding the bits of a 1-sign/7-exp/24-mantissa (bias 64) value - see
# float.h.  This library is the arithmetic (float.c) and the elementary
# functions (mathf.c) on that type.  It lives apart from libc because a
# program that does not use floating point should not pay for it.
#
# Like libc and libu, it is compiled by ccc, so it is built twice: the
# Z80 archive and the Z280 archive (the frame-pointer convention differs).
# CC is deliberately unused: the top level drives everything with CC=gcc.

ARCHIVE = libf.a
ARCHIVE280 = libf280.a

# The .s files here are sources, so CRUFTASM is deliberately absent.
include ../../GNUmakefile.inc

.SUFFIXES:

SRCS = fbcd.s finc.s
CSRCS = float.c mathf.c

BUILD = z80
OBJS = $(addprefix $(BUILD)/,$(SRCS:.s=.o) $(CSRCS:.c=.o))

BUILD280 = z280
OBJS280 = $(addprefix $(BUILD280)/,$(SRCS:.s=.o) $(CSRCS:.c=.o))

all: $(BUILD)/$(ARCHIVE) $(BUILD280)/$(ARCHIVE280)

# Rebuilt when the compiler changes, the same reason libc's are.
$(BUILD)/%.o: %.c $(CCDEP)
	@mkdir -p $(BUILD)
	cd $(BUILD) && $(CCC) -O -I.. -I../../include -c ../$<

$(BUILD)/%.o: %.s
	@mkdir -p $(BUILD)
	$(AS) $< -o $@

$(BUILD)/$(ARCHIVE): $(OBJS)
	rm -f $@
	$(LIB) -cr $@ $(OBJS)

# Z280 build: .c via the Z280 passes, .s via the Z280 assembler.
$(BUILD280)/%.o: %.c $(CCDEP280)
	@mkdir -p $(BUILD280)
	cd $(BUILD280) && $(CCC) -O -m z280 -I.. -I../../include -c ../$<

$(BUILD280)/%.o: %.s
	@mkdir -p $(BUILD280)
	$(AS280) $< -o $@

$(BUILD280)/$(ARCHIVE280): $(OBJS280)
	rm -f $@
	$(LIB) -cr $@ $(OBJS280)

target-install: all
	@mkdir -p $(MXDIR)/lib $(CPMDIR)/lib $(HOSTDIR)/lib
	$(INSTALL) $(BUILD)/$(ARCHIVE) $(MXDIR)/lib
	$(INSTALL) $(BUILD)/$(ARCHIVE) $(CPMDIR)/lib
	$(INSTALL) $(BUILD)/$(ARCHIVE) $(HOSTDIR)/lib
	$(INSTALL) $(BUILD280)/$(ARCHIVE280) $(MXDIR)/lib
	$(INSTALL) $(BUILD280)/$(ARCHIVE280) $(HOSTDIR)/lib
install: target-install

clean:
	rm -rf $(BUILD) $(BUILD280)
	rm -f $(CRUFT) $(CRUFTASMGEN)

clobber: clean
	rm -f $(ARCHIVE) $(ARCHIVE280) $(CRUFTLOG)
	rm -rf $(CRUFTDIRS)

.PHONY: all install target-install clean clobber
