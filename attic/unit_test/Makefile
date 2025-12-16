#
# Unit tests for ccc compiler components
#

# Compiler settings - inherit from parent or set defaults
CC ?= gcc
CFLAGS ?= -m32 -ggdb3 -O0 -DDEBUG -Wno-implicit-function-declaration -Wall
INCLUDES = -I..

# Parent directory objects that tests depend on
PARENT_DIR = ..
KW_OBJ = $(PARENT_DIR)/kw.o
IO_OBJ = $(PARENT_DIR)/io.o

# Test programs
TESTS = test_kw test_insertmacro

.PHONY: all tests clean

all: $(TESTS)

# Keyword lookup test
test_kw: test_kw.c $(KW_OBJ)
	$(CC) $(CFLAGS) $(INCLUDES) -o test_kw test_kw.c $(KW_OBJ)

# Macro insertion buffer test
test_insertmacro: test_insertmacro.c $(IO_OBJ)
	$(CC) $(CFLAGS) $(INCLUDES) -o test_insertmacro test_insertmacro.c $(IO_OBJ)

# Ensure parent objects are built
$(KW_OBJ):
	$(MAKE) -C $(PARENT_DIR) kw.o

$(IO_OBJ):
	$(MAKE) -C $(PARENT_DIR) io.o

# Run all tests
tests: all
	@echo "Running keyword lookup test..."
	./test_kw
	@echo ""
	@echo "Running macro insertion test..."
	./test_insertmacro
	@echo ""
	@echo "All unit tests passed!"

# Run individual tests
.PHONY: test-kw test-insertmacro
test-kw: test_kw
	./test_kw

test-insertmacro: test_insertmacro
	./test_insertmacro

# Run insertmacro test with verbose output
test-insertmacro-verbose: test_insertmacro
	./test_insertmacro -v

# Clean build artifacts
clean:
	rm -f $(TESTS) *.o
