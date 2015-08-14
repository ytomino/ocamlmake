BUILDDIR=build

ifneq ($(HOMELOCAL),)
PREFIX=$(HOMELOCAL)
else
PREFIX=.
endif
BINDIR=$(PREFIX)/bin

empty=

ifneq ($(ComSpec),)
OCAMLMAKE=ocamlmake.exe
RM=del
DIRSEP=\$(empty)
else
OCAMLMAKE=ocamlmake
RM=rm
DIRSEP=/
endif

.PHONY: all clean install

all: $(BUILDDIR) $(BUILDDIR)/$(OCAMLMAKE)

$(BUILDDIR)/$(OCAMLMAKE): $(BUILDDIR)/ocamlmake.cmx
	ocamlopt -o $@ unix.cmxa $<

$(BUILDDIR)/ocamlmake.cmx: ocamlmake.ml
	ocamlopt -c -safe-string -w Aer -o $@ $<

$(BUILDDIR):
	mkdir $@

clean:
	-$(RM) $(BUILDDIR)$(DIRSEP)ocamlmake.cm? $(BUILDDIR)$(DIRSEP)ocamlmake.o
	-$(RM) $(BUILDDIR)$(DIRSEP)$(OCAMLMAKE)
ifeq ($(BUILDDIR),build)
	-rmdir $(BUILDDIR)
endif

install: all
	-mkdir -p $(BINDIR)
	install $(BUILDDIR)/$(OCAMLMAKE) $(BINDIR)