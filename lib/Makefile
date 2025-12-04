# CCC Runtime Library Makefile
# Builds the Z80 runtime library for programs compiled with ccc

# Assembler - use our Whitesmith-compatible assembler
ASZ = ../ws/asz

# Source files
SOURCES = ccclib.s

# Object files (Whitesmith .O format)
OBJECTS = $(SOURCES:.s=.O)

.PHONY: all clean

all: $(OBJECTS)

# Assemble source to Whitesmith object format
%.O: %.s $(ASZ)
	$(ASZ) -o $@ $<

# Build assembler if needed
$(ASZ):
	$(MAKE) -C ../ws asz

clean:
	rm -f $(OBJECTS) *.lst
