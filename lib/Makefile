# CCC Runtime Library Makefile
# Builds the Z80 runtime library for programs compiled with ccc

# Source files
SOURCES = ccclib.s

# Object files
OBJECTS = $(SOURCES:.s=.o)

# Library target
LIBRARY = ccclib.lib

AS = z80asm
ASFLAGS = 

.PHONY: all clean

all: $(OBJECTS)
	
# Future build rules (uncomment when assembler available):
%.o: %.s
	$(AS) $(ASFLAGS) --output=$@ $<

# $(LIBRARY): $(OBJECTS)
# 	ar rcs $@ $^

clean:
	rm -f $(OBJECTS) $(LIBRARY)
