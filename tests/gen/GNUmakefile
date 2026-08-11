#
# The production-coverage suite: every shape the language can put in
# front of the compiler, and every rule the compiler carries, each
# checking the other.
#
#   corpus    regenerate the gp_*.c programs into ../run (deterministic:
#             same seed, same bytes, so a failing check number means
#             the same thing everywhere)
#
#   complete  compile the corpus with -O and fail on any XXXXXX marker -
#             the comment pass2 leaves where NO rule named a shape and
#             code was silently not emitted.  This is the "no missing
#             productions" direction.
#
#   correct   run the corpus: native (gcc -m32, the oracle - the
#             expected values were computed against C semantics at Z80
#             widths) and ccc (the real compiler, executed under the
#             simulator, with the peephole on).  zc3 is not a leg: it
#             fails on its own unsigned-char arithmetic bugs, which
#             this suite was the first thing sharp enough to catch.
#
#   cover     which rewrite rules never fire over the corpus plus the
#             whole tree; the blessed list is unfired.ok and the
#             regression is that it must not GROW.  A new rule ships
#             with something that fires it or its name lands in the
#             report.
#
#   sim       the completeness leg again, but with the compiler itself
#             running under the simulator: c0.mx/c1.mx compile the
#             corpus and every output must be byte-identical to the
#             host compiler's.
#
#   footprint the tipping-point guard: the three simulated passes
#             compile the compiler's OWN sources and every run's gap
#             is read from the simulator.  Fails on out-of-memory, on
#             any gap under FLOOR (default 256), or on divergence
#             from the host - so a bug fix that grows a pass past
#             what Micronix can hold fails here, with the margin
#             table in hand, not in the field.
#
# "all" runs every leg, the simulated compiler included - one
# command, the whole claim.  The sim leg is the slow one; skip it
# with "make complete correct cover" when iterating.
#
ROOT = $(realpath ../..)
RUN = $(realpath ../run)

include ../../GNUmakefile.inc
GEN = python3 genprod.py

GPSRC = $(wildcard $(RUN)/gp_*.c)

all: corpus complete correct cover sim footprint

corpus:
	@$(GEN) | tail -1

complete: corpus
	@fail=0; cd $(RUN) && for f in gp_*.c; do \
		b=$${f%.c}; rm -f $$b.s; \
		$(ROOT)/desthost/bin/ccc -O -s -I. $$f >/dev/null 2>&1; \
		if [ ! -f $$b.s ]; then echo "$$b: DID NOT COMPILE"; fail=1; \
		elif grep -q XXXXXX $$b.s; then \
			echo "$$b: missing production:"; \
			grep -m 3 XXXXXX $$b.s; fail=1; fi; \
		rm -f $$b.s $$b.x $$b.n $$b.1 $$b.2 $$b.o; \
	done; \
	[ $$fail = 0 ] && echo "complete: no missing productions" || exit 1

correct: corpus
	@cd $(RUN) && sh runtests.sh native $$(cd $(RUN) && echo gp_*.c) | tail -1
	@cd $(RUN) && CCCFLAGS=-O sh runtests.sh ccc $$(cd $(RUN) && echo gp_*.c) | tail -1

cover: corpus
	@python3 $(ROOT)/tests/rulecover.py | tee cover.out | sed -n '1,3p'
	@sed -n '/=== never fired ===/,$$p' cover.out | tail -n +2 | \
		sed 's/^ *//' | sort > unfired.now
	@if [ -f unfired.ok ]; then \
		new=$$(comm -13 unfired.ok unfired.now); \
		if [ -n "$$new" ]; then \
			echo "cover: rules that STOPPED firing (or new rules"; \
			echo "nothing exercises) - extend the generator or"; \
			echo "bless with make bless:"; echo "$$new"; exit 1; \
		fi; \
		echo "cover: no rule lost its coverage"; \
	else echo "cover: no baseline - run make bless"; fi
	@rm -f cover.out

bless: unfired.now
	cp unfired.now unfired.ok
	@wc -l < unfired.ok | xargs echo "blessed never-fired rules:"

sim:
	@echo "sim: host c0/c1 vs the .mx pair over the corpus"
	@$(MAKE) -C $(ROOT)/src/pass1 mx-ccc >/dev/null
	@$(MAKE) -C $(ROOT)/src/pass2 mx-ccc >/dev/null
	@cp $(ROOT)/src/pass1/mxccc/c0.mx $(ROOT)/src/pass2/mxccc/c1.mx $(RUN)/
	@sh simcheck.sh $(ROOT) $(RUN)

footprint:
	@sh footprint.sh

# unfired.ok is the blessed baseline and stays; unfired.now is this
# run's answer.  The generated corpus lives in ../run and is removed
# from here because this is what puts it there.
clean:
	rm -f $(RUN)/gp_*.c unfired.now $(CRUFT) $(CRUFTASM)
	rm -rf $(CRUFTDIRS)

clobber: clean
	rm -f $(CRUFTLOG)

# A test directory produces no deliverable.  The rule is here because
# every directory in the tree answers to all, install, clean and
# clobber, and a recursive install should not stop on a missing one.
install:
	@echo "tests/gen: nothing to install"

.PHONY: all install corpus complete correct cover bless sim footprint clean clobber
