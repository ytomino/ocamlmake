HOST:=$(shell $(CC) -dumpmachine)
TARGET=$(HOST)

BUILDDIR=$(TARGET).noindex

DESTDIR=
PREFIX=/usr/local
BINDIR=$(PREFIX)/bin

ifneq ($(findstring mingw,$(TARGET)),)
 EXEEXT=.exe
else
 EXEEXT=
endif

OCAMLMAKE=ocamlmake$(EXEEXT)

OCAMLOPT=ocamlopt.opt

.PHONY: all clean install install-bin

all: $(BUILDDIR)/$(OCAMLMAKE)

$(BUILDDIR)/$(OCAMLMAKE): $(BUILDDIR)/ocamlmake.cmx
	$(OCAMLOPT) -o $@ unix.cmxa $<

$(BUILDDIR)/ocamlmake.cmx: ocamlmake.ml | $(BUILDDIR)
	$(OCAMLOPT) -c -w +a-4-9-70 -o $@ $<

$(BUILDDIR):
	mkdir $@

clean:
	-rm $(BUILDDIR)/ocamlmake.cm? $(BUILDDIR)/ocamlmake.o
	-rm $(BUILDDIR)/$(OCAMLMAKE)
ifeq ($(BUILDDIR),$(TARGET).noindex)
	-rmdir $(BUILDDIR)
endif

install: install-bin

install-bin: $(BUILDDIR)/$(OCAMLMAKE)
	install -s $(BUILDDIR)/$(OCAMLMAKE) $(DESTDIR)$(BINDIR)

$(DESTDIR)$(BINDIR):
	mkdir -p $@
