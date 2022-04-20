.PHONY: all clean install install-bin

all clean install install-bin:
	$(MAKE) -C source $@
