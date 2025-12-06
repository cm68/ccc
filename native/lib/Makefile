# CCC Runtime Library Makefile
# Builds the Z80 runtime library for programs compiled with ccc

LIB = wslib
AS = asz
CC = ../../ccc
CCFLAGS = -i../include -I../..

ASFLAGS =

# Source files
SOURCES = ccclib.s unixlib.s

# Object files
OBJECTS = $(SOURCES:.s=.o)

# Library target
LIBRARY = ccclib.a

.PHONY: all clean

all: $(LIBRARY)

%.o: %.s
	$(AS) $(ASFLAGS) -o $@ $<

unixlib.s: unixlib.c
	$(CC) $(CCFLAGS) -s $<

$(LIBRARY): $(OBJECTS)
	$(LIB) -crv $@ $^

clean:
	rm -f $(OBJECTS) $(LIBRARY) unixlib.s
