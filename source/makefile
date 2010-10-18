BUILDDIR=build
DESTDIR=~/bin

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
	ocamlopt -c -w Aer -o $@ $<

$(BUILDDIR):
	mkdir $@

clean:
	-$(RM) $(BUILDDIR)$(DIRSEP)ocamlmake.cm? $(BUILDDIR)$(DIRSEP)ocamlmake.o
	-$(RM) $(BUILDDIR)$(DIRSEP)$(OCAMLMAKE)
ifeq ($(BUILDDIR),build)
	-rmdir $(BUILDDIR)
endif

install: all
	-mkdir -p $(DESTDIR)
	install $(BUILDDIR)/$(OCAMLMAKE) $(DESTDIR)
