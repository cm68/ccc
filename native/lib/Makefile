# CCC Runtime Library Makefile
# Builds the Z80 runtime library for programs compiled with ccc

LIB = ../ws/wslib
AS = ../ws/asz
ASFLAGS = 

# Source files
SOURCES = ccclib.s

# Object files
OBJECTS = $(SOURCES:.s=.o)

# Library target
LIBRARY = ccclib.a

.PHONY: all clean

all: $(LIBRARY)
	
# Future build rules (uncomment when assembler available):
%.o: %.s
	$(AS) $(ASFLAGS) -o $@ $<

$(LIBRARY): $(OBJECTS)
	$(LIB) -crv $@ $^

clean:
	rm -f $(OBJECTS) $(LIBRARY)
