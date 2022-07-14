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
	@ocaml make_tools.ml conf2 MAKE="$(MAKE)" > Makefile2.cfg
_mk.cfg: ;

TARGETS = all tui gui macui fsmonitor manpage docs clean depend dependgraph paths
FILES = unison-blob.o unison unison.exe

.NOTPARALLEL:
.PHONY: $(TARGETS)
$(TARGETS) $(FILES): Makefile.cfg
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



ifeq ($(OSARCH),SunOS)
  XATTR_LS=for f in test{1,2}.tmp/* ; do echo $$f ; for a in $$(runat $$f ls -1 | grep -v SUNWattr_r) ; do /usr/bin/echo $$a = \\c ; runat $$f cat $$a ; done ; echo ; done
  XATTR_SET1=echo "testxattrvalue1" | runat test1.tmp/a.txt tee unison_xattr_test_name1 ; echo "verylonglonglonglonglonglonglonglonglongtestxattrvalue2" | runat test1.tmp/a.txt tee unison_xattr_test_name2
  XATTR_SET2=echo "testxattrvalue2" | runat test2.tmp/b.txt tee unison_xattr_test_name2
  XATTR_SET3=echo "testxattrvalue3" | runat test1.tmp/a.txt tee unison_xattr_test_name3
  XATTR_CH1=echo "NEWVALUE" | runat test1.tmp/a.txt tee unison_xattr_test_name2
  XATTR_CH2=echo "verylonglonglonglonglonglonglonglonglongNEWVALUE" | runat test1.tmp/a.txt tee unison_xattr_test_name2
  XATTR_RM1=runat test1.tmp/a.txt rm unison_xattr_test_name1
  XATTR_RM_ALL=for f in test{1,2}.tmp/* ; do runat $$f rm unison_xattr_test_* ; done
endif
ifeq ($(OSARCH),Darwin)
  ## ls -l@ for listing just names
  XATTR_LS=for f in test{1,2}.tmp/* ; do echo $$f ; xattr -l $$f ; done
  XATTR_SET1=xattr -w unison_xattr_test_name1 testxattrvalue1 test1.tmp/a.txt ; xattr -w unison_xattr_test_name2 verylonglonglonglonglonglonglonglonglongtestxattrvalue2 test1.tmp/a.txt
  XATTR_SET2=xattr -w unison_xattr_test_name2 testxattrvalue2 test2.tmp/b.txt
  XATTR_SET3=xattr -w unison_xattr_test_name3 testxattrvalue3 test1.tmp/a.txt
  XATTR_CH1=xattr -w unison_xattr_test_name2 NEWVALUE test1.tmp/a.txt
  XATTR_CH2=xattr -w unison_xattr_test_name2 verylonglonglonglonglonglonglonglonglongNEWVALUE test1.tmp/a.txt
  XATTR_RM1=xattr -d unison_xattr_test_name1 test1.tmp/a.txt
  XATTR_RM_ALL=for f in test{1,2}.tmp/* ; do xattr -c $$f ; done
endif
ifeq ($(OSARCH),Linux)
  XATTR_LS=bash -c "for f in test{1,2}.tmp/* ; do getfattr -h -d \$$f ; done"
  XATTR_SET1=setfattr -h -n user.unison_xattr_test_name1 -v testxattrvalue1 test1.tmp/a.txt ; setfattr -h -n user.unison_xattr_test_name2 -v verylonglonglonglonglonglonglonglonglongtestxattrvalue2 test1.tmp/a.txt
  XATTR_SET2=setfattr -h -n user.unison_xattr_test_name2 -v testxattrvalue2 test2.tmp/b.txt
  XATTR_SET3=setfattr -h -n user.unison_xattr_test_name3 -v testxattrvalue3 test1.tmp/a.txt
  XATTR_CH1=setfattr -h -n user.unison_xattr_test_name2 -v NEWVALUE test1.tmp/a.txt
  XATTR_CH2=setfattr -h -n user.unison_xattr_test_name2 -v verylonglonglonglonglonglonglonglonglongNEWVALUE test1.tmp/a.txt
  XATTR_RM1=setfattr -x user.unison_xattr_test_name1 test1.tmp/a.txt
  XATTR_RM_ALL=setfattr -x user.unison_xattr_test_name2 test1.tmp/a.txt ; setfattr -x user.unison_xattr_test_name3 test1.tmp/a.txt ; setfattr -x user.unison_xattr_test_name2 test1.tmp/b.txt ; setfattr -x user.unison_xattr_test_name2 test2.tmp/a.txt ; setfattr -x user.unison_xattr_test_name3 test2.tmp/a.txt ; setfattr -x user.unison_xattr_test_name2 test2.tmp/b.txt
endif
ifeq ($(shell uname),FreeBSD)
  XATTR_BSD=1
else ifeq ($(OSARCH),NetBSD)
  XATTR_BSD=1
else
  XATTR_BSD=0
endif
ifeq ($(XATTR_BSD),1)
  ## lsextattr user filenames for listing just names
  XATTR_LS=sh -c "for f in test1.tmp/* test2.tmp/* ; do echo \$$f ; for a in \$$(lsextattr -q user \$$f) ; do echo -n \$$a '= ' ; getextattr -fhq user \$$a \$$f ; done ; done"
  XATTR_SET1=setextattr -h user unison_xattr_test_name1 testxattrvalue1 test1.tmp/a.txt ; setextattr -h user unison_xattr_test_name2 verylonglonglonglonglonglonglonglonglongtestxattrvalue2 test1.tmp/a.txt
  XATTR_SET2=setextattr -h user unison_xattr_test_name2 testxattrvalue2 test2.tmp/b.txt
  XATTR_SET3=setextattr -h user unison_xattr_test_name3 testxattrvalue3 test1.tmp/a.txt
  XATTR_CH1=setextattr -h user unison_xattr_test_name2 NEWVALUE test1.tmp/a.txt
  XATTR_CH2=setextattr -h user unison_xattr_test_name2 verylonglonglonglonglonglonglonglonglongNEWVALUE test1.tmp/a.txt
  XATTR_RM1=rmextattr -h user unison_xattr_test_name1 test1.tmp/a.txt
  XATTR_RM_ALL=sh -c "for f in test1.tmp/* test2.tmp/* ; do for a in unison_xattr_test_name1 unison_xattr_test_name2 unison_xattr_test_name3 ; do rmextattr -fhq user \$$a \$$f ; done ; done"
endif

XATTR_ROOTS=test1.tmp test2.tmp

.PHONY: testxattr
testxattr:
	-rm -rf test1.tmp test2.tmp
	mkdir test1.tmp test2.tmp
	@echo
	@echo -----------------------------------------------------------
	@echo
	## Set initial xattrs
	touch test1.tmp/a.txt
	$(XATTR_SET1)
	touch test2.tmp/b.txt
	$(XATTR_SET2)
	./$(NAME) -ui text -batch -ignorearchives -xattrs -debug props $(XATTR_ROOTS)
	@echo
	@echo -----------------------------------------------------------
	@echo
	## List xattrs test1.tmp/* test2.tmp/*
	$(XATTR_LS)
	@echo
	@echo -----------------------------------------------------------
	@echo
	./$(NAME) -ui text -batch -xattrs -debug props $(XATTR_ROOTS)
	@echo
	@echo -----------------------------------------------------------
	@echo
	## Remove one xattr
	$(XATTR_RM1)
	## Modify one xattr (reduce value length)
	$(XATTR_CH1)
	## Set one new xattr
	$(XATTR_SET3)
	./$(NAME) -ui text -batch -xattrs -debug props $(XATTR_ROOTS)
	@echo
	@echo -----------------------------------------------------------
	@echo
	## List xattrs test1.tmp/* test2.tmp/*
	$(XATTR_LS)
	@echo
	@echo -----------------------------------------------------------
	@echo
	## Modify one xattr (increase value length)
	$(XATTR_CH2)
	./$(NAME) -ui text -batch -xattrs -debug props $(XATTR_ROOTS)
	@echo
	@echo -----------------------------------------------------------
	@echo
	## List xattrs test1.tmp/* test2.tmp/*
	$(XATTR_LS)
	@echo
	@echo -----------------------------------------------------------
	@echo
	## Remove all xattrs
	$(XATTR_RM_ALL)
	## List xattrs test1.tmp/* test2.tmp/*
	$(XATTR_LS)
	./$(NAME) -ui text -batch -xattrs -debug props $(XATTR_ROOTS)
	@echo
	@echo -----------------------------------------------------------
	@echo
	-rm -rf test1.tmp test2.tmp


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
