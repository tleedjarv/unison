#######################################################################
# $I1: Unison file synchronizer: src/Makefile $
# $I2: Last modified by bcpierce on Sun, 22 Aug 2004 22:29:04 -0400 $
# $I3: Copyright 1999-2020 (see COPYING for details) $
#######################################################################
## User Settings

# Set NATIVE=false if you are not using the native code compiler (ocamlopt)
# This is not advised, though: Unison runs much slower when byte-compiled.
#
# If you set NATIVE=false, then make sure that the THREADS option below is
# also set to false unless your OCaml installation has true posix-compliant
# threads (i.e., -with-pthreads was given as an option to the config script).
NATIVE=true

# Use THREADS=false if your OCaml installation is not configured with the
# -with-pthreads option.  (Unison will crash when compiled with THREADS=true
# if the -with-pthreads configuration option was not used.)
THREADS=false

# User interface style.  For legal values, see Makefile.OCaml.
# You probably don't need to set this yourself -- it will be set to
# an appropriate value automatically, depending on whether the lablgtk
# library is available.
#
# UISTYLE=text

########################################################################
########################################################################
#     (There should be no need to change anything from here on)       ##
########################################################################

######################################################################
# Building installation instructions

all:: strings.ml buildexecutable

all:: INSTALL

.PHONY: all clean install doinstall installtext text \
	setupdemo-old setupdemo modifydemo demo \
	run runbatch runt rundebug runp runtext runsort runprefer \
	prefsdocs runtest repeattest \
	selftest selftestdebug selftestremote testmerge \
	testxattr testacl checkin installremote

.DELETE_ON_ERROR:
# to avoid problems when something fails to run

INSTALL: $(NAME)$(EXEC_EXT)
# file isn't made for OS X, so check that it's there first
	(if [ -f $(NAME) ]; then ./$(NAME) -doc install > INSTALLATION; fi)

########################################################################
## Miscellaneous developer-only switches
DEBUGGING=true
PROFILING=false
STATIC=false

# NAME, VERSION, and MAJORVERSION, automatically generated
-include Makefile.ProjectInfo

########################################################################
### Compilation rules

include Makefile.OCaml

######################################################################
# Installation

INSTALLDIR = $(HOME)/bin/

# This has two names because on OSX the file INSTALL shadows the target 'install'!
install: doinstall

installtext:
	$(MAKE) -C .. installtext

text:
	$(MAKE) -C .. text

# Note that this is not needed (and doesn't work) for the OSX GUI version.
# Just copy $(UIMACDIR)/build/Default/Unison.app to where you want it.
doinstall: buildexecutable
	@if [ ! $(NAME) ]; then \
	    echo "makefile variable NAME not bound"; \
	    exit 1 \
	  ; fi
	-mv $(INSTALLDIR)/$(NAME)$(EXEC_EXT) /tmp/$(NAME)-$(shell echo $$$$)
	cp $(NAME)$(EXEC_EXT) $(INSTALLDIR)
	cp $(NAME)$(EXEC_EXT) $(INSTALLDIR)$(NAME)-$(MAJORVERSION)$(EXEC_EXT)

######################################################################
# Demo

setupdemo-old: all
	-mkdir alice.tmp bob.tmp
	-touch alice.tmp/letter alice.tmp/curriculum
	-mkdir bob.tmp/curriculum
	-touch bob.tmp/curriculum/french
	-touch bob.tmp/curriculum/german
	-mkdir bob.tmp/good_friends
	-mkdir bob.tmp/good_friends/addresses
	-mkdir alice.tmp/good_friends
	-touch alice.tmp/good_friends/addresses
	-touch bob.tmp/good_friends/addresses/alice
	-mkdir alice.tmp/book
	-mkdir bob.tmp/book
	echo "first name:alice \n 2234 Chesnut Street \n Philadelphia" \
	   > bob.tmp/good_friends/addresses/alice
	echo "ADDRESS 1 : BOB \n firstName : bob \n 2233 Walnut Street" \
	   > alice.tmp/good_friends/addresses
	echo "Born in Paris in 1976 ..." > alice.tmp/curriculum
	echo "Ne a Paris en 1976 ..." > bob.tmp/curriculum/french
	echo "Geboren in Paris im jahre 1976 ..." > bob.tmp/curriculum/german
	echo "Dear friend, I received your letter ..."    > alice.tmp/letter
	echo "And then the big bad wolf" > bob.tmp/book/page3
	echo "Title : three little pigs" > alice.tmp/book/page1
	echo "there was upon a time ..." > alice.tmp/book/page2

setupdemo:
	rm -rf a.tmp b.tmp
	mkdir a.tmp
	touch a.tmp/a a.tmp/b a.tmp/c
	mkdir a.tmp/d
	touch a.tmp/d/f
	touch a.tmp/d/g
	cp -r a.tmp b.tmp

modifydemo:
	-rm a.tmp/a
	echo "Hello" > a.tmp/b
	echo "Hello" > b.tmp/b
	date > b.tmp/c
	echo "Hi there" > a.tmp/d/h
	echo "Hello there" > b.tmp/d/h

demo: all setupdemo
	@$(MAKE) run
	@$(MAKE) modifydemo
	@$(MAKE) run

run: all
	-mkdir a.tmp b.tmp
	-date > a.tmp/x
	-date > b.tmp/y
	./$(NAME) default a.tmp b.tmp

runbatch: all
	-mkdir a.tmp b.tmp
	-date > a.tmp/x
	-date > b.tmp/y
	./$(NAME) default a.tmp b.tmp -batch

runt: all
	-mkdir a.tmp b.tmp
	-date > a.tmp/x
	-date > b.tmp/y
	./$(NAME) default a.tmp b.tmp -timers

rundebug: all
	-date > a.tmp/x
	-date > b.tmp/y
	./$(NAME) a.tmp b.tmp -debug all  -ui text

runp: all
	-echo cat > a.tmp/cat
	-echo cat > b.tmp/cat
	-chmod 765 a.tmp/cat
	-chmod 700 b.tmp/cat
	./$(NAME) a.tmp b.tmp

runtext: all
	-mkdir a.tmp b.tmp
	-date > a.tmp/x
	-date > b.tmp/y
	./$(NAME) -ui text a.tmp b.tmp

runsort: all
	-mkdir a.tmp b.tmp
	-date > a.tmp/b
	-date > b.tmp/m
	-date > b.tmp/z
	-date > b.tmp/f
	-date >> b.tmp/f
	-date > b.tmp/c.$(shell echo $$$$)
	-date > b.tmp/y.$(shell echo $$$$)
	./$(NAME) default a.tmp b.tmp -debug sort

runprefer: all
	-mkdir a.tmp b.tmp
	-date > a.tmp/b
	-date > b.tmp/m
	-date > b.tmp/z
	-echo Hello > a.tmp/z
	-date > b.tmp/f
	-date >> b.tmp/f
	-date > b.tmp/c.$(shell echo $$$$)
	-date > b.tmp/y.$(shell echo $$$$)
	./$(NAME) default a.tmp b.tmp -force b.tmp

prefsdocs: all
	./$(NAME) -prefsdocs 2> prefsdocsjunk.tmp
	mv -f prefsdocsjunk.tmp prefsdocs.tmp

# For developers
runtest:
	$(MAKE) NATIVE=false DEBUG=true text
	./unison test

repeattest:
	$(MAKE) all NATIVE=false DEBUG=true UISTYLE=text
	./unison noprofile a.tmp b.tmp -repeat foo.tmp -debug ui

selftest:
	$(MAKE) all NATIVE=false DEBUG=true UISTYLE=text
	./unison -selftest -ui text -batch

selftestdebug:
	$(MAKE) all NATIVE=false DEBUG=true UISTYLE=text
	./unison -selftest -ui text -batch -debug all

selftestremote:
	$(MAKE) all NATIVE=false DEBUG=true UISTYLE=text
	./unison -selftest -ui text -batch test.tmp ssh://eniac.seas.upenn.edu/test.tmp

testmerge:
	$(MAKE) all NATIVE=false UISTYLE=text
	-rm -rf a.tmp b.tmp
	-rm -rf $(HOME)/.unison/backup/file.txt*
	mkdir a.tmp b.tmp
	@echo
	@echo -----------------------------------------------------------
	@echo
	./unison testmerge -ui text -batch
	echo 1OO >> a.tmp/file.txt
	echo 2oo >> a.tmp/file.txt
	echo 3oo >> a.tmp/file.txt
	echo 4oo >> a.tmp/file.txt
	echo 5oo >> a.tmp/file.txt
	echo 6oo >> a.tmp/file.txt
	echo 7oo >> a.tmp/file.txt
	echo 8oo >> a.tmp/file.txt
	echo 9oo >> a.tmp/file.txt
	echo 0oo >> a.tmp/file.txt
	echo 1oo >> a.tmp/file.txt
	echo 2oo >> a.tmp/file.txt
	echo 3oo >> a.tmp/file.txt
	echo 4oo >> a.tmp/file.txt
	echo 5oo >> a.tmp/file.txt
	echo 6oo >> a.tmp/file.txt
	echo 5oo >> a.tmp/file.txt
	echo 6oo >> a.tmp/file.txt
	echo 7oo >> a.tmp/file.txt
	echo 8oo >> a.tmp/file.txt
	echo 9oo >> a.tmp/file.txt
	echo 0oo >> a.tmp/file.txt
	echo 1oo >> a.tmp/file.txt
	echo 2oo >> a.tmp/file.txt
	echo 3OO >> a.tmp/file.txt
	echo 4oo >> a.tmp/file.txt
	./unison testmerge -ui text -batch
	rm a.tmp/file.txt b.tmp/file.txt
	echo 1OO >> a.tmp/file.txt
	echo second >> a.tmp/file.txt
	echo 3oo >> a.tmp/file.txt
	echo 4oo >> a.tmp/file.txt
	echo 5oo >> a.tmp/file.txt
	echo 6oo >> a.tmp/file.txt
	echo 7oo >> a.tmp/file.txt
	echo 8oo >> a.tmp/file.txt
	echo 9oo >> a.tmp/file.txt
	echo 0oo >> a.tmp/file.txt
	echo 1oo >> a.tmp/file.txt
	echo 2oo >> a.tmp/file.txt
	echo 3oo >> a.tmp/file.txt
	echo 4oo >> a.tmp/file.txt
	echo 5oo >> a.tmp/file.txt
	echo 6oo >> a.tmp/file.txt
	echo 5oo >> a.tmp/file.txt
	echo 6oo >> a.tmp/file.txt
	echo 7oo >> a.tmp/file.txt
	echo 8oo >> a.tmp/file.txt
	echo 9oo >> a.tmp/file.txt
	echo 0oo >> a.tmp/file.txt
	echo 1oo >> a.tmp/file.txt
	echo 2oo >> a.tmp/file.txt
	echo 3OO >> a.tmp/file.txt
	echo 4oo >> a.tmp/file.txt
	echo ---
	echo 1OO >> b.tmp/file.txt
	echo 2oo >> b.tmp/file.txt
	echo 3oo >> b.tmp/file.txt
	echo 4oo >> b.tmp/file.txt
	echo 5oo >> b.tmp/file.txt
	echo 6oo >> b.tmp/file.txt
	echo 7oo >> b.tmp/file.txt
	echo 8oo >> b.tmp/file.txt
	echo 9oo >> b.tmp/file.txt
	echo 0oo >> b.tmp/file.txt
	echo 1oo >> b.tmp/file.txt
	echo 2oo >> b.tmp/file.txt
	echo 3oo >> b.tmp/file.txt
	echo 4oo >> b.tmp/file.txt
	echo 5oo >> b.tmp/file.txt
	echo 6oo >> b.tmp/file.txt
	echo 5oo >> b.tmp/file.txt
	echo 6oo >> b.tmp/file.txt
	echo 7oo >> b.tmp/file.txt
	echo 8oo >> b.tmp/file.txt
	echo 9oo >> b.tmp/file.txt
	echo 0oo >> b.tmp/file.txt
	echo 1oo >> b.tmp/file.txt
	echo 2oo >> b.tmp/file.txt
	echo 3OO >> b.tmp/file.txt
	echo end >> b.tmp/file.txt
	@echo
	@echo -----------------------------------------------------------
	@echo
	./unison testmerge -ui text -batch -debug files -debug update -debug backup
	@echo
	@echo -----------------------------------------------------------
	@echo
	./unison testmerge -ui text -batch
	@echo
	@echo -----------------------------------------------------------
	@echo
	cat a.tmp/file.txt
	cat b.tmp/file.txt
	cat $(HOME)/.unison/backup/file.txt



ifeq ($(OSARCH),solaris)
  XATTR_LS=for f in test{1,2}.tmp/* ; do echo $$f ; runat $$f ls -l ; done
  XATTR_SET1=echo "testxattrvalue1" | runat test1.tmp/a.txt tee unison_xattr_test_name1 ; echo "testxattrvalue2" | runat test1.tmp/a.txt tee unison_xattr_test_name2
  XATTR_SET2=echo "testxattrvalue2" | runat test2.tmp/b.txt tee unison_xattr_test_name2
  XATTR_SET3=echo "testxattrvalue3" | runat test1.tmp/a.txt tee unison_xattr_test_name3
  XATTR_CH1=echo "NEWVALUE" | runat test1.tmp/a.txt tee unison_xattr_test_name2
  XATTR_RM1=runat test1.tmp/a.txt rm unison_xattr_test_name1
  XATTR_RM_ALL=for f in test{1,2}.tmp/* ; do runat $$f rm unison_xattr_test_* ; done
endif
ifeq ($(OSARCH),osx)
  ## ls -l@ for listing just names
  XATTR_LS=for f in test{1,2}.tmp/* ; do echo $$f ; xattr -l $$f ; done
  XATTR_SET1=xattr -w unison_xattr_test_name1 testxattrvalue1 test1.tmp/a.txt ; xattr -w unison_xattr_test_name2 testxattrvalue2 test1.tmp/a.txt
  XATTR_SET2=xattr -w unison_xattr_test_name2 testxattrvalue2 test2.tmp/b.txt
  XATTR_SET3=xattr -w unison_xattr_test_name3 testxattrvalue3 test1.tmp/a.txt
  XATTR_CH1=xattr -w unison_xattr_test_name2 NEWVALUE test1.tmp/a.txt
  XATTR_RM1=xattr -d unison_xattr_test_name1 test1.tmp/a.txt
  XATTR_RM_ALL=for f in test{1,2}.tmp/* ; do xattr -c $$f ; done
endif
ifeq ($(OSARCH),Linux)
  XATTR_LS=bash -c "for f in test{1,2}.tmp/* ; do getfattr -h -d \$$f ; done"
  XATTR_SET1=setfattr -h -n user.unison_xattr_test_name1 -v testxattrvalue1 test1.tmp/a.txt ; setfattr -h -n user.unison_xattr_test_name2 -v testxattrvalue2 test1.tmp/a.txt
  XATTR_SET2=setfattr -h -n user.unison_xattr_test_name2 -v testxattrvalue2 test2.tmp/b.txt
  XATTR_SET3=setfattr -h -n user.unison_xattr_test_name3 -v testxattrvalue3 test1.tmp/a.txt
  XATTR_CH1=setfattr -h -n user.unison_xattr_test_name2 -v NEWVALUE test1.tmp/a.txt
  XATTR_RM1=setfattr -x user.unison_xattr_test_name1 test1.tmp/a.txt
  XATTR_RM_ALL=setfattr -x user.unison_xattr_test_name2 test1.tmp/a.txt ; setfattr -x user.unison_xattr_test_name3 test1.tmp/a.txt ; setfattr -x user.unison_xattr_test_name2 test1.tmp/b.txt ; setfattr -x user.unison_xattr_test_name2 test2.tmp/a.txt ; setfattr -x user.unison_xattr_test_name3 test2.tmp/a.txt ; setfattr -x user.unison_xattr_test_name2 test2.tmp/b.txt
endif
ifeq ($(shell uname),FreeBSD)
  ## lsextattr user filenames for listing just names
  XATTR_LS=bash -c "for f in test{1,2}.tmp/* ; do echo \$$f ; for a in \`lsextattr -q user \$$f\` ; do echo -n \$$a '= ' ; getextattr -fhq user \$$a \$$f ; done ; done"
  XATTR_SET1=setextattr -h user unison_xattr_test_name1 testxattrvalue1 test1.tmp/a.txt ; setextattr -h user unison_xattr_test_name2 testxattrvalue2 test1.tmp/a.txt
  XATTR_SET2=setextattr -h user unison_xattr_test_name2 testxattrvalue2 test2.tmp/b.txt
  XATTR_SET3=setextattr -h user unison_xattr_test_name3 testxattrvalue3 test1.tmp/a.txt
  XATTR_CH1=setextattr -h user unison_xattr_test_name2 NEWVALUE test1.tmp/a.txt
  XATTR_RM1=rmextattr -h user unison_xattr_test_name1 test1.tmp/a.txt
  XATTR_RM_ALL=bash -c "for f in test{1,2}.tmp/* ; do for a in unison_xattr_test_name1 unison_xattr_test_name2 unison_xattr_test_name3 ; do rmextattr -fhq user \$$a \$$f ; done ; done"
endif

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
	./$(NAME) -ui text -batch -ignorearchives -xattrs -debug props test1.tmp test2.tmp
	@echo
	@echo -----------------------------------------------------------
	@echo
	## List xattrs test1.tmp/* test2.tmp/*
	$(XATTR_LS)
	@echo
	@echo -----------------------------------------------------------
	@echo
	./$(NAME) -ui text -batch -xattrs -debug props test1.tmp test2.tmp
	@echo
	@echo -----------------------------------------------------------
	@echo
	## Remove one xattr
	$(XATTR_RM1)
	## Modify one xattr (reduce value length)
	$(XATTR_CH1)
	## Set one new xattr
	$(XATTR_SET3)
	./$(NAME) -ui text -batch -xattrs -debug props test1.tmp test2.tmp
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
	./$(NAME) -ui text -batch -xattrs -debug props test1.tmp test2.tmp
	@echo
	@echo -----------------------------------------------------------
	@echo
	-rm -rf test1.tmp test2.tmp


ACL_CHMOD=echo
ACL_LS=ls -l
ifeq ($(OSARCH),solaris)
  ACL1=A+user:$(shell whoami):rw-pd---------:-------:allow,group@:----dD--------:-------:allow
  ACL2=A+user:$(shell whoami):r---d-a-R-c--s:-------:allow
  ACL3=A+user:$(shell whoami):-w-p----------:-------:deny
  ACLRM=A-
  ACL_CHMOD=chmod
  ACL_LS=ls -V
endif
ifeq ($(shell uname),FreeBSD)
  ACL_BSD=1
else ifeq ($(OSARCH),NetBSD)
  ACL_BSD=1
else
  ACL_BSD=0
endif
ifeq ($(ACL_BSD),1)
  ACL_ENABLED=0
  ACL_POSIX=$(shell getconf ACL_EXTENDED . 2> /dev/null)
  ACL_NFS4=$(shell getconf ACL_NFS4 . 2> /dev/null)
ifeq ($(ACL_POSIX),1)
    ACL_ENABLED=1
    ACL1=-m u:$(shell whoami):rwx,g::rw
    ACL2=-m u::---
    ACL3=-m u:$(shell whoami):rwx
else
ifeq ($(ACL_NFS4),1)
    ACL_ENABLED=1
    ACL1=-m user:$(shell whoami):rw-pd---------:-------:allow,group@:----dD--------:-------:allow
    ACL2=-m user:$(shell whoami):r---d-a-R-c--s:-------:allow
    ACL3=-m user:$(shell whoami):-w-p----------:-------:deny
endif
endif
ifeq ($(ACL_ENABLED),1)
    ACLRM=-b
    ACL_CHMOD=setfacl -h
    ACL_LS=getfacl -h
endif
endif
ifeq ($(OSARCH),osx)
  ACL1=+a '$(shell whoami) allow read,write,delete,add_file,add_subdirectory,file_inherit,directory_inherit'
  ACL2=+a '$(shell whoami) allow read'
  ACL3=+a '$(shell whoami) deny write'
  ACLRM=-N
  ACL_CHMOD=chmod
  ACL_LS=ls -le
endif

testacl:
	-rm -rf test1.tmp test2.tmp
	mkdir test1.tmp test2.tmp
	@echo
	@echo -----------------------------------------------------------
	@echo
	touch test1.tmp/a.txt
	$(ACL_CHMOD) $(ACL1) test1.tmp/a.txt
	touch test2.tmp/b.txt
	$(ACL_CHMOD) $(ACL2) test2.tmp/b.txt
	$(ACL_CHMOD) $(ACL3) test2.tmp/b.txt
	./$(NAME) -ui text -batch -ignorearchives -acl -debug props test1.tmp test2.tmp
	@echo
	@echo -----------------------------------------------------------
	@echo
	$(ACL_LS) test1.tmp/* test2.tmp/*
	@echo
	@echo -----------------------------------------------------------
	@echo
	./$(NAME) -ui text -batch -acl -debug props test1.tmp test2.tmp
	@echo
	@echo -----------------------------------------------------------
	@echo
	$(ACL_CHMOD) $(ACLRM) test2.tmp/b.txt
	./$(NAME) -ui text -batch -acl -debug props test1.tmp test2.tmp
	@echo
	@echo -----------------------------------------------------------
	@echo
	$(ACL_LS) test1.tmp/* test2.tmp/*
	@echo
	@echo -----------------------------------------------------------
	@echo
	$(ACL_CHMOD) $(ACLRM) test1.tmp/a.txt
	$(ACL_CHMOD) $(ACLRM) test1.tmp/b.txt
	$(ACL_CHMOD) $(ACLRM) test2.tmp/a.txt
	$(ACL_CHMOD) $(ACLRM) test2.tmp/b.txt
	$(ACL_LS) test1.tmp/* test2.tmp/*
	./$(NAME) -ui text -batch -acl -debug props test1.tmp test2.tmp
	@echo
	@echo -----------------------------------------------------------
	@echo
	-rm -rf test1.tmp test2.tmp


######################################################################
# Tags

# In Windows, tags and TAGS are the same, so make tags stops working
# after the first invocation.  The .PHONY declaration makes it work
# again.
.PHONY: tags

tags:
	-if [ -f "`which $(ETAGS)`" ]; then \
	    $(ETAGS) *.mli */*.mli *.ml */*.ml */*.m *.c */*.c *.txt \
		     *Makefile* \
	  ; fi

all:: TAGS

TAGS:
	$(MAKE) tags

######################################################################
# Misc

clean::
	-$(RM) *.log *.aux *.log *.dvi *.out *.bak
	-$(RM) -r obsolete
	-$(RM) $(NAME) $(NAME).exe
	-$(RM) $(NAME)-blob.o

clean::
	$(MAKE) -C ubase clean
	$(MAKE) -C lwt clean
	$(MAKE) -C fsmonitor/windows clean

ifneq ($(strip $(UIMACDIR)),)
clean::
	-(cd $(UIMACDIR); xcodebuild clean)
	-(cd $(UIMACDIR); $(RM) -r build ExternalSettings.xcconfig)
endif

checkin:
	$(MAKE) -C .. checkin

installremote:
	$(MAKE) UISTYLE=text
	-unison eniac -path current/unison/trunk/src -batch
	ssh eniac.seas.upenn.edu make -C current/unison/trunk/src installtext

####################################################################
# Documentation strings

# Cons up a fake strings.ml if necessary (the real one is generated when
# we build the documentation, but we need to be able to compile the
# executable here to do that!)
strings.ml:
	echo "(* Dummy strings.ml *)" > strings.ml
	echo "let docs = []" >> strings.ml
