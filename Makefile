# Unison file synchronizer: Makefile
# See LICENSE for terms.

# IMPORTANT!
#
# This file is portable and compatible with GNU Make, (Net)BSD make,
# Solaris dmake and NMAKE. Do not make any changes in this file unless
# you are certain that the changes do not break this portability.

default: all
.PHONY: default

# Sub-makefiles are perfectly fine for parallel builds.
# This makefile is not, due to recursive invocations of make.
.NOTPARALLEL:

.PHONY: all
all: src manpage

.PHONY: src
src: FRC
	cd src && $(MAKE)
FRC: ;
# Not all make seem to work without FRC, even with .PHONY

.PHONY: tui gui macui fsmonitor manpage docs clean depend
tui gui macui fsmonitor manpage docs clean depend:
	cd src && $(MAKE) $@

.PHONY: test
test: ./src/unison
	./src/unison -ui text -selftest
# Note: unison binary is not built automatically for the test target.

prefix = /usr/local

.PHONY: install
install:
	ocaml src/make_tools.ml install
