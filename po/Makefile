# Unison file synchronizer: po/Makefile
# Copyright 2026 (see ../LICENSE for terms).

# This Makefile does not have to be completely portable but it
# would be nice if at least BSD make can work with it.

# [2026-04] Do we need LINGUAS? What tools make use of it?
# The only tool seems to be GNU gettext's msgfmt (requires its presence),
# which we only use as fallback. Might generate LINGUAS automatically
# in that case...
#LINGUAS != cat LINGUAS

# [2026-04] Do we need POTFILES? What tools make use of it?
# If it's not needed and we don't plan to use GNU gettext (even then, it seems
# only used by their build scripts) then it would be nice to convert it to Make
# dependency format and include it just as if it was a .depend file.
SOURCES != cat POTFILES

.PHONY: all
#all: unison.pot $(LINGUAS:=.po)
all:
	@echo
	@echo make refresh"      "- update the template
	@echo "                    "!! You do not need to do this as a translator.
	@echo
	@echo make lang_code.po - merge template to language file
	@echo make -C .. translations - install translations in source tree
	@echo To add a new language, copy unison.pot to lang_code.po
#	@echo and add lang_code to file LINGUAS
	@echo
	@echo !! Translators: do not commit unison.pot into source control.
	@echo

.PHONY: refresh
refresh: unison.pot

# [2026-04] ocaml-gettext has limitations and is overall not yet as good as
# GNU gettext's xgettext, so disable it for now and use xgettext. On the
# other hand, there is no reason to prefer GNU gettext's msgfmt for compiling
# MO files.

unison.pot: $(SOURCES)
# This doesn't work too well: #	ocaml-gettext --action extract --extract-pot $@ $^
# xgettext by default writes times in local timezone in the POT file. This
# makes no sense, so force UTC here.
	TZ=UTC xgettext --from-code=UTF-8 --add-comments=TRANSLATORS: -o $@ $^

%.po: unison.pot
	ocaml-gettext --action merge --merge-pot $< $@

clean:
	-$(RM) *.mo *.bak
