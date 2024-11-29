# Unison file synchronizer: src/Makefile
# Copyright 1999-2024 (see ../LICENSE for terms).

# IMPORTANT!
#
# This file is portable and compatible with GNU Make, (Net)BSD make,
# Solaris dmake and NMAKE. Do not make any changes in this file unless
# you are certain that the changes do not break this portability.

########################################################################
########################################################################
#     (There should be no need to change anything from here on)       ##
########################################################################

default: all
.PHONY: default

# Executed only by BSD make
.BEGIN:
	@touch _mk.cfg
	@ocaml make_tools.ml conf > Makefile.cfg
	@ocaml make_tools.ml conf2 > Makefile2.cfg

# Executed only by Solaris dmake
.INIT:
	@touch _mk.cfg
	@ocaml make_tools.ml conf ASSUME_DMAKE=true > Makefile.cfg
	@ocaml make_tools.ml conf2 ASSUME_DMAKE=true > Makefile2.cfg

# Executed only by GNU Make and NMAKE
Makefile.cfg: _mk.cfg
	@ocaml make_tools.ml conf > Makefile.cfg
	@ocaml make_tools.ml conf2 > Makefile2.cfg
_mk.cfg: ;

.NOTPARALLEL:
.PHONY: all tui gui macui fsmonitor manpage docs clean depend dependgraph paths
all tui gui macui fsmonitor manpage docs clean depend dependgraph paths: Makefile.cfg
	$(MAKE) -f Makefile.OCaml $@

# Must use recursive make invocation here, in the name of portability.
# Including the generated Makefile.cfg will not work with GNU Make. To
# prevent it from going into infinite loop the generation would have
# to be guarded with something like ifndef MAKE_RESTARTS, and that
# would ruin the portability. Likewise, using "include" instead of
# "-include" will prevent BSD make from working when Makefile.cfg does
# not already exist (even when it is generated before the include
# statement). "-include", also, is not portable.


######################################################################
# For developers


######################################################################
# Tags

ETAGS=etags

# In Windows, tags and TAGS are the same, so make tags stops working
# after the first invocation.  The .PHONY declaration makes it work
# again.
.PHONY: tags
tags:
	@-if command -v $(ETAGS) > /dev/null ; then \
	    $(ETAGS) *.mli */*.mli *.ml */*.ml */*.m *.c */*.c *.txt \
		     *Makefile* \
	  ; fi

TAGS: tags
