#
# makefile for native and cross z80 compiler
#
# embedded tests are for incremental development
#
# this following is only to get an idea of size
# the idea eventual is that it is both cross and native.
#
# we self-generate some of the files to keep things consistent
#
CC = gcc
#CC = sdcc
#CC = ccc

ASM = asz
ASMOPTS =

CCCFLAGS = -DCCC -DASMKWLOOK -inative/include -I.

ifeq ($(CC),gcc)
DEFINES= -DDEBUG
WARNS = -Wdeclaration-after-statement -Werror=declaration-after-statement -Werror=implicit-function-declaration -Wall -Werror
DEBUG= -ggdb3 -O0
CFLAGS = -m32 $(DEBUG) $(DEFINES) $(WARNS)
LDFLAGS= -m32 $(DEBUG) -o
LD= gcc
endif

CC1OBJECTS = cc1.o error.o lex.o io.o macro.o kw.o util.o tokenlist.o \
	expr.o parse.o type.o declare.o outast.o

CC2OBJECTS = cc2.o util.o astio.o parseast.o codegen.o dumpast.o regcache.o emithelper.o emitexpr.o emitops.o emit.o

HEADERS = cc1.h token.h
GENERATED = enumlist.h tokenlist.c error.h debug.h debugtags.c op_pri.h trace2.h tracetags.c

# All C source files (generated + corresponding to .o files)
CFILES = cc1.c error.c lex.c io.c macro.c kw.c util.c \
	expr.c parse.c type.c declare.c outast.c \
	cc2.c astio.c parseast.c codegen.c dumpast.c regcache.c emithelper.c emitexpr.c emitops.c emit.c ccc.c \
	tokenlist.c debugtags.c

# All header files (manually written + generated)
HFILES = $(HEADERS) astio.h emithelper.h enumlist.h error.h debug.h op_pri.h trace2.h

# All source files
SOURCES = $(CFILES) $(HFILES)

LIBSRCS = native/lib/Makefile native/lib/ccclib.s

# Documentation files (in reading order)
DOCFILES = README.md TODO.md CLAUDE.md AST_FORMAT.md native/lib/README.md

BINS = cc1 cc2 ccc maketokens genop_pri

#VERBOSE=-v 3

all: cc1 cc2 ccc doc.pdf

cc1: $(CC1OBJECTS)
	$(LD) $(LDFLAGS) cc1 $(CC1OBJECTS)

cc2: $(CC2OBJECTS)
	$(LD) $(LDFLAGS) cc2 $(CC2OBJECTS)

ccc: ccc.o
	$(LD) $(LDFLAGS) ccc ccc.o

$(CC1OBJECTS): $(HFILES)

# Suffix rule to generate .ast files from .c files
%.ast: %.c cc1
	./cc1 $(CCCFLAGS) -o $@ $<

# Suffix rule to generate .s assembly files from .ast files
%.s: %.ast cc2
	./cc2 $<

# Suffix rule to generate .pp (pretty-printed) files from .ast files
%.pp: %.ast astpp.py
	python3 ./astpp.py $< > $@

# Suffix rule to assemble to .obj files
%.obj: %.s
	$(ASM) $(ASMOPTS) -o $@ $<

# Pattern rules for stage1 directory - always rebuild (FORCE dependency)
# Preserve intermediate files (make normally deletes .ast and .s after building .o)
.PRECIOUS: stage1/%.ast stage1/%.s

stage1/%.i: %.c cc1 FORCE
	@mkdir -p stage1
	./cc1 $(CCCFLAGS) -o stage1/$*.ast $<
	mv $*.i $@

stage1/%.ast: %.c cc1 FORCE
	@mkdir -p stage1
	./cc1 $(CCCFLAGS) -o $@ $<

stage1/%.pp: stage1/%.ast FORCE
	python3 ./astpp.py $< > $@

stage1/%.s: stage1/%.ast cc2 FORCE
	./cc2 -o $@ $<

stage1/%.o: stage1/%.s FORCE
	$(ASM) $(ASMOPTS) -o $@ $<

FORCE:

.PHONY: test tests valgrind valgrind-stage1 FORCE
test: cc1 tests/runtest.sh
	$(MAKE) -C tests test

tests: cc1 tests/runtest.sh
	$(MAKE) -C tests tests

valgrind: cc1 tests/runvalgrind.sh
	$(MAKE) -C tests valgrind

# Run valgrind on stage1 compilation - generates .vg files for each pass
# Output files per source:
#   stage1/<base>.cc1.vg  - valgrind log for cc1
#   stage1/<base>.cc1.err - stderr from cc1
#   stage1/<base>.cc2.vg  - valgrind log for cc2
#   stage1/<base>.cc2.err - stderr from cc2
valgrind-stage1: cc1 cc2
	@echo "Running valgrind on stage1 compilation"
	@mkdir -p stage1
	@for f in $(CFILES); do \
	  if [ -f "$$f" ]; then \
	    b=$$(basename $$f .c) ; \
	    printf "%-20s cc1: " "$$f"; \
	    valgrind --leak-check=full --log-file=stage1/$$b.cc1.vg \
	      ./cc1 $(CCCFLAGS) -o stage1/$$b.ast "$$f" \
	      2>stage1/$$b.cc1.err; \
	    grep -q "ERROR SUMMARY: 0 errors" stage1/$$b.cc1.vg && echo -n "OK " || echo -n "ERR "; \
	    mv $$b.i stage1/$$b.i 2>/dev/null; \
	    printf "cc2: "; \
	    valgrind --leak-check=full --log-file=stage1/$$b.cc2.vg \
	      ./cc2 -o stage1/$$b.s stage1/$$b.ast \
	      2>stage1/$$b.cc2.err; \
	    grep -q "ERROR SUMMARY: 0 errors" stage1/$$b.cc2.vg && echo "OK" || echo "ERR"; \
	  fi; \
	done
	@echo "Valgrind logs: stage1/*.vg, stderr: stage1/*.err"

.PHONY: unit-tests
unit-tests: $(GENERATED)
	$(MAKE) -C unit_test tests

.PHONY: stage1
stage1: cc1 cc2
	@echo "Building stage1 compiler binaries"
	@mkdir -p stage1
	@for f in $(CFILES); do \
	  if [ -f "$$f" ]; then \
	    b=$$(basename $$f .c) ; \
	    printf "%-30s" "$$f: "; \
	    ./cc1 $(CCCFLAGS) -E -o stage1/$$b.ast "$$f" \
		2>stage1/$$b.err ; \
	    ret1=$$?; \
	    mv $$b.i stage1/$$b.i; \
	    if [ $$ret1 -ne 0 ]; then \
	      echo "FAIL (parse error)"; \
	    else \
	      python3 ./astpp.py stage1/$$b.ast >stage1/$$b.pp 2>>stage1/$$b.err ; \
	      ./cc2 stage1/$$b.ast >stage1/$$b.s 2>>stage1/$$b.err ; \
	      ret2=$$?; \
	      if [ $$ret2 -ne 0 ]; then \
	        echo "FAIL (codegen error)"; \
	      else \
	        $(ASM) $(ASMOPTS) -o stage1/$$b.o stage1/$$b.s \
		    >>stage1/$$b.err 2>&1 ; \
	        ret3=$$?; \
	        if [ $$ret3 -ne 0 ]; then \
	          echo "FAIL (asm error)"; \
	        else \
	          echo "PASS"; \
	        fi; \
	      fi; \
	    fi; \
	  fi; \
	done
	@echo "Stage1 build complete: stage1/*.o files ready for linking"
	# Link disabled until library complete
	# $(MAKE) native/lib/ccclib.a
	# $(MAKE) stage1/cc1 stage1/cc2
	# @echo "Stage1 linking complete: stage1/cc1 stage1/cc2"

# Library dependency
native/lib/ccclib.a: native/lib/ccclib.s
	$(MAKE) -C native/lib ccclib.a

# Link stage1 binaries
WSLD = ws/wsld
STAGE1_CC1_OBJS = $(addprefix stage1/, $(CC1OBJECTS))
STAGE1_CC2_OBJS = $(addprefix stage1/, $(CC2OBJECTS))

stage1/cc1: $(STAGE1_CC1_OBJS) native/lib/ccclib.a
	$(WSLD) -o $@ $(STAGE1_CC1_OBJS) native/lib/ccclib.a

stage1/cc2: $(STAGE1_CC2_OBJS) native/lib/ccclib.a
	$(WSLD) -o $@ $(STAGE1_CC2_OBJS) native/lib/ccclib.a

#
# check size of compiled objects
#
sizecheck: $(STAGE1_CC1_OBJS) $(STAGE1_CC2_OBJS)
	@wssize $(STAGE1_CC1_OBJS) | \
	awk '($$1 != "text") {print substr($$6, 8), $$1, $$2, $$3; text+=$$1;data+=$$2;bss+=$$3}END{print "cc1 size: ", text, data, bss, "=", text+data+bss}' | tee current.size
	@wssize $(STAGE1_CC2_OBJS) | \
	awk '($$1 != "text") {print substr($$6, 8), $$1, $$2, $$3; text+=$$1;data+=$$2;bss+=$$3}END{print "cc2 size: ", text, data, bss, "=", text+data+bss}' | tee -a current.size
	@if [ -f prev.size ] ; then diff prev.size current.size ; fi ; mv current.size prev.size

#
# process the cc1.h file, extracting the enum tags for the tokens
#
enumlist.h: cc1.h Makefile
	@echo '/* generated from token.h - DO NOT EDIT */' >enumlist.h
	tr ',' '\n' < token.h | \
	sed -e '/\/\*/d' -e 's/=.*$$//' | \
	awk '/enum / { t=1;next } /;$$/ {t=0} {if (t) print}' | \
	tr -d '[:blank:]' | \
	awk '/[A-Z]+/ {printf("check(%s);\n", $$1);}' >>enumlist.h

#
# generate the operator priority table
#
op_pri.h: ./genop_pri
	./genop_pri

genop_pri: genop_pri.c
	cc -o genop_pri genop_pri.c

#
# generate token names from the enumlist.h file
#
tokenlist.c: enumlist.h maketokens
	./maketokens >tokenlist.c
	
maketokens: maketokens.c token.h enumlist.h
	cc -o maketokens maketokens.c

#
# generate the debug.h file from the shell script makedebug.sh
# which grunges through all the c sources looking for VERBOSE(tag)
#
debug.h debugtags.c: ./makedebug.sh
	./makedebug.sh

#
# generate the trace2.h file from makedebug2.sh
# which scans cc2 sources for TRACE(tag)
#
trace2.h tracetags.c: ./makedebug2.sh
	./makedebug2.sh

#
# process the errorcodes file, which generates error.h, containing the
# error codes and corresponding error strings
#
error.h: errorcodes ./makeerror.awk
	awk -f ./makeerror.awk < errorcodes > error.h

regen:
	rm -f $(GENERATED)
	make $(GENERATED)

tags:
	ctags *.c

doc.pdf: $(SOURCES) $(DOCFILES) Makefile
	{ for f in $(DOCFILES); do \
	    pandoc -f gfm -t plain "$$f" | \
		iconv -f utf-8 -t Latin1//TRANSLIT | \
		enscript -2rG --title="$$f" -p -; \
	  done; \
	  enscript -2rG -p - Makefile $(SOURCES) $(LIBSRCS) ; } | \
		ps2pdf - doc.pdf

clean:
	rm -f $(CC1OBJECTS) $(CC2OBJECTS) ccc.o $(GENERATED) tests/*.i \
		*.ast *.s *.pp *.asm *.lst *.sym *.map *.cdb *.ihx *.i
	rm -rf stage1
	$(MAKE) -C unit_test clean

clobber: clean
	rm -f $(BINS) tags doc.pdf prev.size

cc1.o: debugtags.c
parse.o: parse.c
type.o: type.c
main.o: main.c
io.o: io.c
macro.o: macro.c
kw.o: kw.c
error.o: error.c
util.o: util.c
tokenlist.o: tokenlist.c
expr.o: expr.c op_pri.h
outast.o: outast.c
astio.o: astio.c astio.h cc2.h
parseast.o: parseast.c astio.h cc2.h
$(CC2OBJECTS): trace2.h cc2.h
